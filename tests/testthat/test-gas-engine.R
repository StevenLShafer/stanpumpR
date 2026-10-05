# Tests for the inhaled-gas engine (R/advanceClosedFormGas.R, R/gasProperties.R).
#
# The point of these tests is that the closed-form advance is checked against an
# INDEPENDENT integration of the same differential equations, written out again
# here by hand, plus analytic limits that can be computed on paper.  A bug that
# is shared between the engine and the reference would not be caught, so the
# reference below is deliberately written from the equations in the header
# comment rather than by reusing any engine code.
#
# These tests establish internal correctness only.  Fidelity to Gas Man 4.2 is a
# separate question and needs the exported fixture grid.

test_that("expmPade reproduces analytic matrix exponentials", {
  # Diagonal: exponentiate the diagonal
  A <- diag(c(-1, -2, -0.5))
  expect_equal(expmPade(A), diag(exp(c(-1, -2, -0.5))), tolerance = 1e-12)

  # Nilpotent: exp([[0,1],[0,0]]) = [[1,1],[0,1]] exactly
  N <- matrix(c(0, 0, 1, 0), 2, 2)
  expect_equal(expmPade(N), matrix(c(1, 0, 1, 1), 2, 2), tolerance = 1e-12)

  # Zero matrix gives the identity
  expect_equal(expmPade(matrix(0, 3, 3)), diag(3), tolerance = 1e-14)

  # Stiff, badly scaled case, using an actual gas system matrix: check against
  # an independent eigendecomposition-based exponential.
  #
  # Note that e^{A} e^{-A} = I is NOT a usable identity to test with here.  It
  # is true mathematically, but a stiff A makes e^{-A} enormous (entries of
  # order 1e22 for the matrices in this model), and the product is then
  # catastrophic cancellation rather than a test of the algorithm.
  body  <- getGasBody(70)
  props <- getGasProperties()
  sys   <- gasSystemSoluble(props[props$gas == "isoflurane", ],
                            body, Q = 2, VA = 4, Qco = 5.25, Ffgf = 1)
  M  <- sys$A * 5
  ev <- eigen(M)
  ref <- Re(ev$vectors %*% diag(exp(ev$values)) %*% solve(ev$vectors))
  expect_equal(expmPade(M), ref, tolerance = 1e-8)

  # The eigenvalues of a compartmental system are real and non-positive, which
  # is what makes the closed form well conditioned in the first place.
  expect_lt(max(Re(ev$values)), 1e-10)
  expect_lt(max(abs(Im(ev$values))), 1e-10)
})


test_that("closed-form advance matches independent RK4 integration (semi-closed circuit)", {
  body  <- getGasBody(70)
  props <- getGasProperties()
  p     <- props[props$gas == "sevoflurane", ]
  ltg   <- gasPartitionTissueGas(p)

  Q <- 2; VA <- 4; Qco <- body$Q_cardiac; Ffgf <- 2

  # Independent derivative, written straight from equations (1)-(3).
  deriv <- function(y) {
    Fc <- y[1]; Fa <- y[2]; Fb <- y[3]; Fm <- y[4]; Ff <- y[5]
    Fv <- body$f_brain * Fb + body$f_muscle * Fm + body$f_fat * Ff
    lb <- p$lambda_blood
    c(
      (Q * (Ffgf - Fc) + VA * (Fa - Fc)) / body$V_circuit,
      (VA * (Fc - Fa) - lb * Qco * (Fa - Fv)) / body$V_alveolar,
      body$f_brain  * Qco * lb * (Fa - Fb) / (body$V_brain  * ltg[["brain"]]),
      body$f_muscle * Qco * lb * (Fa - Fm) / (body$V_muscle * ltg[["muscle"]]),
      body$f_fat    * Qco * lb * (Fa - Ff) / (body$V_fat    * ltg[["fat"]])
    )
  }
  rk4 <- function(y, dt) {
    k1 <- deriv(y); k2 <- deriv(y + dt/2 * k1)
    k3 <- deriv(y + dt/2 * k2); k4 <- deriv(y + dt * k3)
    y + dt/6 * (k1 + 2*k2 + 2*k3 + k4)
  }

  sys <- gasSystemSoluble(p, body, Q, VA, Qco, Ffgf, circuit = "semi-closed")

  for (horizon in c(1, 10, 60)) {
    y <- rep(0, 5)
    steps <- horizon * 2000
    for (i in seq_len(steps)) y <- rk4(y, horizon / steps)
    closed <- advanceGasSegment(rep(0, 5), sys$A, sys$b, horizon)
    expect_equal(closed, y, tolerance = 1e-7,
                 info = paste("horizon =", horizon, "min"))
  }
})


test_that("alveolar oxygen steady state equals inspired minus 100*VO2/VA", {
  body <- getGasBody(70)
  # Pure oxygen at high flow: circuit is flushed, so inspired is ~100%.
  dose <- data.frame(
    Time = c(0, 0),
    Drug = c("oxygen", "ventilation"),
    Dose = c(10, 4),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, weight = 70, age = 40, maximum = 60)
  o2 <- sim$results[sim$results$Drug == "oxygen" & sim$results$Site == "Alveolar", ]
  final <- o2$Y[nrow(o2)]

  # Circuit reaches ~100% at 10 L/min; alveolar sits one VO2/VA step below.
  expected <- 100 - 100 * body$VO2 / 4
  expect_equal(final, expected, tolerance = 0.05)

  # And that is a physiologically sensible number, not just an algebraic one
  expect_gt(final, 90)
})


test_that("semi-closed circuit: steady state is the flow-weighted average of fresh and alveolar gas", {
  body <- getGasBody(70)
  props <- getGasProperties()
  p <- props[props$gas == "nitrogen", ]

  Q <- 1; VA <- 5; Qco <- body$Q_cardiac; Ffgf <- 79.07
  sys <- gasSystemSoluble(p, body, Q, VA, Qco, Ffgf, circuit = "semi-closed")

  # Run to steady state, then check F_circ = (Q Ffgf + VA F_alv)/(Q + VA)
  y <- advanceGasSegment(rep(0, 5), sys$A, sys$b, 100000)
  expect_equal(y[1], (Q * Ffgf + VA * y[2]) / (Q + VA), tolerance = 1e-8)
})


test_that("the system is linear in the vaporiser setting", {
  mk <- function(sevo) data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(2, 4, sevo),
    stringsAsFactors = FALSE
  )
  # Linearity holds only with the gases UNCOUPLED.  The uptake term makes the
  # system nonlinear on purpose -- see the second expectation below.
  a <- advanceClosedFormGas(mk(1), maximum = 30, uptakeEffect = FALSE)
  b <- advanceClosedFormGas(mk(2), maximum = 30, uptakeEffect = FALSE)

  ga <- a$results[a$results$Drug == "sevoflurane" & a$results$Site == "Brain", "Y"]
  gb <- b$results[b$results$Drug == "sevoflurane" & b$results$Site == "Brain", "Y"]

  # Doubling the dial doubles the entire brain trajectory, exactly.
  expect_equal(gb, 2 * ga, tolerance = 1e-9)
})


test_that("the uptake coupling makes the system nonlinear in the dial", {
  # This is not a defect, it is the physiology: a gas taken up in bulk
  # concentrates itself and everything beside it, so doubling the vaporiser
  # setting must give MORE than twice the tension.  With the coupling off the
  # test above shows the system is exactly linear, so the departure isolates
  # the uptake term as the cause.
  mk <- function(sevo) data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(2, 4, sevo),
    stringsAsFactors = FALSE
  )
  a <- advanceClosedFormGas(mk(1), maximum = 30, uptakeEffect = TRUE)
  b <- advanceClosedFormGas(mk(2), maximum = 30, uptakeEffect = TRUE)

  at <- function(r, t) {
    d <- r$results[r$results$Drug == "sevoflurane" & r$results$Site == "Brain", ]
    stats::approx(d$Time, d$Y, t)$y
  }
  expect_gt(at(b, 10), 2 * at(a, 10))
  # ...but only slightly: sevoflurane alone is not taken up in great volume.
  expect_lt(at(b, 10), 2.02 * at(a, 10))
})


test_that("the system is NOT linear in fresh gas flow", {
  mk <- function(o2) data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(o2, 4, 2),
    stringsAsFactors = FALSE
  )
  a <- advanceClosedFormGas(mk(0.5), maximum = 30)
  b <- advanceClosedFormGas(mk(1.0), maximum = 30)

  ga <- a$results[a$results$Drug == "sevoflurane" & a$results$Site == "Brain", "Y"]
  gb <- b$results[b$results$Drug == "sevoflurane" & b$results$Site == "Brain", "Y"]

  # Higher flow gives a faster rise (less rebreathing of depleted gas)...
  expect_gt(gb[length(gb)], ga[length(ga)])
  # ...but not proportionally: this is a change of shape, not of scale.
  ratio <- gb[-1] / ga[-1]
  expect_gt(stats::sd(ratio), 1e-6)
  expect_lt(max(ratio), 2)
})


test_that("nitrogen washes out when the patient is switched to oxygen", {
  dose <- data.frame(
    Time = c(0, 0),
    Drug = c("oxygen", "ventilation"),
    Dose = c(6, 4),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, maximum = 30)
  n2 <- sim$results[sim$results$Drug == "nitrogen" & sim$results$Site == "Alveolar", ]

  expect_equal(n2$Y[1], 78.07, tolerance = 1.5)   # starts at room air
  expect_lt(n2$Y[nrow(n2)], 5)                     # denitrogenated by 30 min
  expect_true(all(diff(n2$Y) <= 1e-9))             # monotone decreasing
})


test_that("wash-in is monotone and brain lags alveolar", {
  dose <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(2, 4, 2),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, maximum = 60)
  alv <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Alveolar", "Y"]
  brn <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Brain", "Y"]

  expect_true(all(diff(alv) >= -1e-9))
  expect_true(all(diff(brn) >= -1e-9))
  # The brain trails the alveolus at every point during wash-in
  expect_true(all(brn <= alv + 1e-9))
  # Neither exceeds the dial setting
  expect_lt(max(alv), 2 + 1e-9)
})


test_that("MAC sums the potent agents and adjusts for age", {
  props <- getGasProperties()

  # Mapleson: about 6% per decade
  expect_equal(macForAge(2.1, 40), 2.1, tolerance = 1e-12)
  expect_lt(macForAge(2.1, 80), macForAge(2.1, 40))
  expect_equal(macForAge(2.1, 50) / macForAge(2.1, 40), 10^(-0.00269 * 10),
               tolerance = 1e-12)

  # A long run at a fixed dial should approach brain/MAC for that agent alone
  dose <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(6, 4, 2),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, age = 40, maximum = 600)
  alv <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Alveolar", "Y"]
  mac <- sim$results[sim$results$Drug == "MAC", "Y"]

  # MAC is Minimum ALVEOLAR Concentration, and Gas Man's own CSV writes
  # GetALV / m_fMAC, so it comes from the alveolar series, not the brain.
  MACsevo <- macForAge(props$MAC40[props$gas == "sevoflurane"], 40)
  expect_equal(mac, alv / MACsevo, tolerance = 1e-9)
})


test_that("nitrous oxide contributes to MAC alongside a volatile", {
  dose <- data.frame(
    Time = c(0, 0, 0, 0),
    Drug = c("oxygen", "nitrousOxide", "ventilation", "sevoflurane"),
    Dose = c(2, 4, 4, 1),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, age = 40, maximum = 60)
  mac <- sim$results[sim$results$Drug == "MAC", "Y"]
  n2o <- sim$results[sim$results$Drug == "nitrousOxide" & sim$results$Site == "Brain", "Y"]

  expect_gt(max(n2o), 20)          # nitrous reaches the brain
  expect_gt(mac[length(mac)], 0.5) # combined MAC is clinically plausible
  expect_true(all(diff(mac) >= -1e-9))
})


test_that("settings persist until changed and take effect at the change point", {
  dose <- data.frame(
    Time = c(0, 0, 0, 20),
    Drug = c("oxygen", "ventilation", "sevoflurane", "sevoflurane"),
    Dose = c(2, 4, 2, 0),
    stringsAsFactors = FALSE
  )
  sim <- advanceClosedFormGas(dose, maximum = 60)
  r   <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Alveolar", ]

  peak <- max(r$Y)
  atEnd <- r$Y[nrow(r)]
  # Turning the vaporiser off at 20 min must produce a wash-out
  expect_gt(peak, 0.5)
  expect_lt(atEnd, peak / 2)
  # ...and the peak should occur at or just after the change point
  expect_equal(r$Time[which.max(r$Y)], 20, tolerance = 0.2)
})


test_that("the uptake coupling is on by default, as in Gas Man", {
  # Gas Man's m_bUptEnb defaults true, so the concentration and second gas
  # effect are present unless deliberately switched off.  This test previously
  # asserted the opposite -- that asking for the effect was an error -- which
  # was correct while it was unimplemented and is now wrong.
  DT <- data.frame(
    Time = c(0, 0, 0, 0),
    Drug = c("oxygen", "nitrousOxide", "ventilation", "sevoflurane"),
    Dose = c(2.4, 5.6, 4, 2), stringsAsFactors = FALSE)

  at <- function(r) {
    d <- r$results[r$results$Drug == "sevoflurane" & r$results$Site == "Alveolar", ]
    stats::approx(d$Time, d$Y, 5)$y
  }
  expect_gt(at(advanceClosedFormGas(DT, maximum = 30)),
            at(advanceClosedFormGas(DT, maximum = 30, uptakeEffect = FALSE)))
})


test_that("cardiac output defaults to Gas Man's 5 L/min, scaled allometrically, and changes uptake when overridden", {
  # gasman.ini [Defaults] CO=5 at 70 kg; GasDoc.cpp scales by (weight/70)^0.75.
  expect_equal(getGasBody(70)$Q_cardiac, 5, tolerance = 1e-12)
  expect_equal(getGasBody(140)$Q_cardiac, 5 * 2^0.75, tolerance = 1e-12)
  expect_equal(getGasBody(35)$Q_cardiac,  5 * 0.5^0.75, tolerance = 1e-12)

  dose <- data.frame(
    Time = c(0, 0, 0),
    Drug = c("oxygen", "ventilation", "sevoflurane"),
    Dose = c(2, 4, 2),
    stringsAsFactors = FALSE
  )
  low  <- advanceClosedFormGas(dose, maximum = 10, cardiacOutput = 2.5)
  high <- advanceClosedFormGas(dose, maximum = 10, cardiacOutput = 10)

  aLow  <- low$results[low$results$Drug == "sevoflurane" &
                         low$results$Site == "Alveolar", "Y"]
  aHigh <- high$results[high$results$Drug == "sevoflurane" &
                          high$results$Site == "Alveolar", "Y"]

  # Higher cardiac output removes more agent from the alveolus, so the
  # alveolar tension rises more slowly -- the classic Gas Man demonstration.
  expect_lt(aHigh[length(aHigh)], aLow[length(aLow)])
})


test_that("agent parameters are Gas Man's own, not literature substitutes", {
  # Read from gasman.ini in the Gas Man source (GPL-3.0,
  # github.com/rasman/gasmanonline).  Pinned because several differ from the
  # conventional published values this file previously carried.
  p <- getGasProperties()
  get <- function(gas, col) p[[col]][p$gas == gas]

  # blood:gas
  expect_equal(get("nitrousOxide", "lambda_blood"), 0.47)
  expect_equal(get("sevoflurane",  "lambda_blood"), 0.65)
  expect_equal(get("isoflurane",   "lambda_blood"), 1.3)   # not 1.4
  expect_equal(get("desflurane",   "lambda_blood"), 0.42)
  expect_equal(get("nitrogen",     "lambda_blood"), 0.014)

  # MAC
  expect_equal(get("nitrousOxide", "MAC40"), 110)          # not 104
  expect_equal(get("sevoflurane",  "MAC40"), 2.1)          # not 2.05
  expect_equal(get("isoflurane",   "MAC40"), 1.1)
  expect_equal(get("desflurane",   "MAC40"), 6.0)

  # tissue:GAS, stored directly rather than converted from tissue:blood
  expect_equal(get("desflurane", "tg_brain"),  0.54)
  expect_equal(get("desflurane", "tg_muscle"), 0.97)
  expect_equal(get("desflurane", "tg_fat"),    13)
  expect_equal(unname(gasPartitionTissueGas(p[p$gas == "desflurane", ])),
               c(0.54, 0.97, 13))

  # Nitrogen is excluded from summed MAC despite gasman.ini giving it MAC 200,
  # which would otherwise post 0.4 MAC on room air.
  expect_false(get("nitrogen", "potent"))
  expect_setequal(potentAgents(),
                  c("nitrousOxide", "sevoflurane", "isoflurane", "desflurane"))
})


test_that("desflurane simulates and washes in fastest of the volatiles", {
  mk <- function(agent) data.frame(
    Time = c(0, 0, 0), Drug = c("oxygen", "ventilation", agent),
    Dose = c(6, 4, 1), stringsAsFactors = FALSE)

  fa <- function(agent) {
    s <- advanceClosedFormGas(mk(agent), maximum = 30)
    r <- s$results[s$results$Drug == agent & s$results$Site == "Alveolar", ]
    approx(r$Time, r$Y, 10)$y
  }

  # Least soluble equilibrates fastest: desflurane 0.42 < sevoflurane 0.65 < isoflurane 1.3
  expect_gt(fa("desflurane"), fa("sevoflurane"))
  expect_gt(fa("sevoflurane"), fa("isoflurane"))

  # And it reaches the brain and contributes MAC
  s <- advanceClosedFormGas(mk("desflurane"), age = 40, maximum = 60)
  alv <- s$results[s$results$Drug == "desflurane" & s$results$Site == "Alveolar", "Y"]
  brn <- s$results[s$results$Drug == "desflurane" & s$results$Site == "Brain", "Y"]
  mac <- s$results[s$results$Drug == "MAC", "Y"]
  expect_gt(max(brn), 0)
  expect_equal(mac, alv / macForAge(6.0, 40), tolerance = 1e-9)

  # ...and it is NOT the brain series.  The two converge at equilibrium, so the
  # distinction only shows during wash-in, which is where it is checked.
  early <- which(s$timeLine > 1 & s$timeLine < 10)
  expect_gt(max(abs(alv[early] - brn[early])), 0.1)
  expect_false(isTRUE(all.equal(mac[early], brn[early] / macForAge(6.0, 40))))
})


test_that("Gas Man's numbers are used as they stand, with bad provenance flagged", {
  # Policy: use Gas Man's values so validation compares like with like, but flag
  # the ones known to be wrong rather than silently correcting them.
  props <- getGasProperties()

  # Nitrogen's MAC is recorded as Gas Man states it...
  expect_equal(props$MAC40[props$gas == "nitrogen"], 200)

  # ...and flagged, because Eger put the MAC of nitrogen at 110 ATMOSPHERES,
  # i.e. 11000% of one atmosphere.  Gas Man's 200 is low by about 55-fold.
  flagged <- flaggedGasParameters()
  expect_true("nitrogen" %in% flagged$gas)
  expect_match(flagged$flagNote[flagged$gas == "nitrogen"], "110 atm")

  # The arithmetic that makes it matter: room air is 0.79 atm nitrogen, so at
  # Eger's value it contributes a negligible fraction of a MAC, whereas at Gas
  # Man's it would contribute a large one.
  expect_lt(0.79 / 110, 0.01)     # Eger: about 0.007 MAC
  expect_gt(0.79 / 2, 0.3)        # Gas Man: about 0.4 MAC

  # Which is why nitrogen stays out of the summed MAC while that figure stands
  expect_false("nitrogen" %in% potentAgents())

  # An unflagged parameter means "not yet checked", not "verified", so the rest
  # of the table carrying no flag is expected rather than reassuring.
  expect_equal(nrow(flagged), 1)
})

test_that("oxygen never goes negative, even with no ventilation", {
  # Apnea: nothing replaces the oxygen being consumed.  The constant metabolic
  # sink would otherwise carry the alveolar fraction far below zero.
  apnea <- data.frame(Time = 0, Drug = c("oxygen", "nitrousOxide"), Dose = c(1, 2))
  sim <- advanceClosedFormGas(apnea, weight = 60, maximum = 60)
  o2 <- sim$results$Y[sim$results$Drug == "oxygen"]
  expect_gte(min(o2), 0)
  expect_equal(utils::tail(o2, 1), 0)
  expect_gte(min(sim$state[["oxygen"]]), 0)

  # The floor must not touch a run that never reaches it.
  ventilated <- rbind(apnea, data.frame(Time = 0, Drug = "ventilation", Dose = 4))
  sim <- advanceClosedFormGas(ventilated, weight = 60, maximum = 60)
  expect_gt(min(sim$results$Y[sim$results$Drug == "oxygen"]), 10)
})

test_that("default ventilation is a minute ventilation whose alveolar part is Gas Man's 4 L/min", {
  # gasman.ini [Defaults] VA=4; GasDoc.cpp: m_fVA = m_fDfltVA * (weight/70)^0.75.
  # That is ALVEOLAR ventilation.  The dose table takes MINUTE ventilation, with
  # a dead space of 30%, so the default is 4 / 0.7.
  expect_equal(GAS_DEAD_SPACE_FRACTION, 0.3)
  expect_equal(defaultGasVentilation(70), 5.7)
  expect_equal(defaultGasVentilation(70) * (1 - GAS_DEAD_SPACE_FRACTION), 4, tolerance = 0.02)
  expect_equal(defaultGasVentilation(60), round(4 * (60 / 70)^0.75 / 0.7, 1))   # 5.1
  expect_equal(defaultGasVentilation(140), round(4 * 2^0.75 / 0.7, 1))          # 9.6
  # With no dead space it is Gas Man's own number.
  expect_equal(defaultGasVentilation(70, deadSpace = 0), 4)
  # Missing or invalid weight falls back to the 70 kg standard.
  expect_equal(defaultGasVentilation(NA), 5.7)
  expect_equal(defaultGasVentilation(-5), 5.7)
})


# --- The ideal circuit, the engine's default since 2026-10-05 ----------------
# (Claude Code, Claude Fable 5.1; run on R 4.6.1.)

test_that("the ideal circuit is the default", {
  expect_equal(formals(advanceClosedFormGas)$circuit[[2]], "ideal")
  expect_equal(formals(simulateGases)$circuit[[2]], "ideal")
  expect_equal(formals(gasSystemSoluble)$circuit[[2]], "ideal")
})

test_that("ideal circuit: the fresh-gas fraction, with and without dead space", {
  # No dead space: Gas Man's ideal circuit, threshold at alveolar ventilation.
  expect_equal(gasFreshFraction(4, 4), 1)
  expect_equal(gasFreshFraction(10, 4), 1)
  expect_equal(gasFreshFraction(1, 4), 0.25)
  expect_equal(gasFreshFraction(0, 4), 0)
  expect_equal(gasFreshFraction(1, 4, "open"), 1)
  expect_equal(gasFreshFraction(2, 0), 1)          # no ventilation: nothing rebreathed

  # 30% dead space: minute ventilation 4, alveolar 2.8.  The threshold is at
  # MINUTE ventilation, and f = Q / (VA + Q d) runs continuously up to it.
  expect_equal(gasFreshFraction(4, 2.8, MV = 4), 1)
  expect_equal(gasFreshFraction(3.999, 2.8, MV = 4), 1, tolerance = 1e-3)
  expect_lt(gasFreshFraction(3.5, 2.8, MV = 4), 1)       # between VA and MV: still rebreathing
  expect_equal(gasFreshFraction(1, 2.8, MV = 4), 1 / (2.8 + 0.3))
  expect_equal(gasFreshFraction(0, 2.8, MV = 4), 0)
  # More of each fresh litre reaches the alveoli's inspired gas than with no
  # dead space at the same MV, because dead-space gas comes back unused.
  expect_gt(gasFreshFraction(1, 2.8, MV = 4), gasFreshFraction(1, 4))
})

test_that("ideal circuit: no rebreathing once fresh gas flow covers what is inspired", {
  mk <- function(Q) data.frame(Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"),
                               Dose = c(Q, 4, 2))
  final <- function(sim, j) utils::tail(sim$state$sevoflurane[, j], 1)

  # Comfortably above the minute ventilation the patient inspires the dial
  # setting from the first breath, and how far above makes no difference at all.
  at8  <- advanceClosedFormGas(mk(8),  maximum = 30)
  at15 <- advanceClosedFormGas(mk(15), maximum = 30)
  expect_equal(at8$state$sevoflurane[-1, 1], rep(2, nrow(at8$state$sevoflurane) - 1))
  expect_equal(at8$state$sevoflurane, at15$state$sevoflurane)

  # Exactly AT the minute ventilation a trace is still rebreathed while gas is
  # being taken up, because the patient inspires the minute ventilation PLUS
  # what the blood takes: the threshold is MV + uptake.  It is a trace.
  at4 <- advanceClosedFormGas(mk(4), maximum = 30)
  circ <- at4$state$sevoflurane[-1, 1]
  expect_true(all(circ < 2))
  expect_true(all(circ > 1.97))
  # With the gases uncoupled there is no uptake to add, and it is exactly 2.
  flat4 <- advanceClosedFormGas(mk(4), maximum = 30, uptakeEffect = FALSE)
  expect_equal(flat4$state$sevoflurane[-1, 1], rep(2, nrow(flat4$state$sevoflurane) - 1))

  # Between alveolar (2.8) and minute (4) ventilation there is real rebreathing.
  at3 <- advanceClosedFormGas(mk(3), maximum = 30)
  expect_true(all(at3$state$sevoflurane[-1, 1] < 1.95))
  expect_lt(final(at3, 2), final(at8, 2))

  # Below the threshold, with the gases uncoupled so that the weights are the
  # plain ones, the inspired gas is f fresh + (1 - f) alveolar at every moment.
  at1 <- advanceClosedFormGas(mk(1), maximum = 30, uptakeEffect = FALSE)
  y <- at1$state$sevoflurane[-1, ]
  f <- 1 / (2.8 + 1 * 0.3)
  expect_equal(y[, 1], f * 2 + (1 - f) * y[, 2])
  expect_lt(final(at1, 2), final(at3, 2))

  # With no dead space as well, the blend is Gas Man's f = Q / VA.
  gm1 <- advanceClosedFormGas(mk(1), maximum = 30, deadSpace = 0, uptakeEffect = FALSE)
  y <- gm1$state$sevoflurane[-1, ]
  expect_equal(y[, 1], 0.25 * 2 + 0.75 * y[, 2])

  # Alveolar ventilation is 70% of what is entered: the same alveolar
  # ventilation given directly, with no dead space, gives the same patient.
  alv <- advanceClosedFormGas(data.frame(Time = 0, Drug = c("oxygen", "ventilation", "sevoflurane"),
                                         Dose = c(15, 2.8, 2)), maximum = 30, deadSpace = 0)
  expect_equal(alv$state$sevoflurane[, 2:5], at15$state$sevoflurane[, 2:5])

  # The mixing box, for contrast, is still rebreathing at Q = VA: half and half.
  box <- advanceClosedFormGas(mk(4), maximum = 600, circuit = "semi-closed",
                              deadSpace = 0, oxygenUptake = FALSE)
  expect_equal(final(box, 1), (4 * 2 + 4 * final(box, 2)) / 8, tolerance = 1e-3)
  expect_lt(final(box, 1), 2)
  # And it only approaches the ideal circuit as the flow becomes enormous.
  flood <- advanceClosedFormGas(mk(1e6), maximum = 30, circuit = "semi-closed")
  expect_equal(final(flood, 2), final(at8, 2), tolerance = 1e-4)
})

test_that("the circuit blend: limits, and what uptake and absorbed carbon dioxide do to it", {
  # No uptake, no carbon dioxide: the two weights sum to one.
  bl <- gasCircuitBlend(1, 2.8, 4)
  expect_equal(bl$fresh, 1 / (2.8 + 0.3))
  expect_equal(bl$fresh + bl$alveolar, 1)
  expect_equal(gasCircuitBlend(4, 2.8, 4), list(fresh = 1, alveolar = 0))
  expect_equal(gasCircuitBlend(0, 2.8, 4)$fresh, 0)
  expect_equal(gasCircuitBlend(1, 2.8, 4, "open"), list(fresh = 1, alveolar = 0))

  # Uptake raises what is inspired, and so the flow needed to stop rebreathing.
  expect_lt(gasCircuitBlend(4, 2.8, 4, u = 0.5)$fresh, 1)
  expect_equal(gasCircuitBlend(4.5, 2.8, 4, u = 0.5)$fresh, 1)
  # Washout (negative uptake) does not lower it: only what is inspired counts.
  expect_equal(gasCircuitBlend(3, 2.8, 4, u = -0.5), gasCircuitBlend(3, 2.8, 4))

  # Carbon dioxide absorbed from rebreathed gas: the weights sum to more than
  # one, by exactly what makes inspired gas sum to 100 when the alveolar
  # fractions being carried sum to 100 less the carbon dioxide.
  VA <- 2.8; MV <- 4; VCO2 <- 0.2; cE <- VCO2 / MV
  bl <- gasCircuitBlend(1, VA, MV, u = 0.05, cE = cE)
  alveolarSum <- 100 * (1 - VCO2 / VA)
  expect_equal(bl$fresh * 100 + bl$alveolar * alveolarSum, 100)
})

test_that("ideal circuit: the closed-form advance matches an independent RK4 integration", {
  body  <- getGasBody(70)
  props <- getGasProperties()
  p     <- props[props$gas == "sevoflurane", ]
  ltg   <- gasPartitionTissueGas(p)
  Qco <- body$Q_cardiac; Ffgf <- 2; VA <- 4

  for (Q in c(1, 6)) {
    f <- min(1, Q / VA)
    # Written straight from the header: the circuit is not a state.
    deriv <- function(y) {
      Fa <- y[1]; Fb <- y[2]; Fm <- y[3]; Ff <- y[4]
      Fc <- f * Ffgf + (1 - f) * Fa
      Fv <- body$f_brain * Fb + body$f_muscle * Fm + body$f_fat * Ff
      lb <- p$lambda_blood
      c((VA * (Fc - Fa) - lb * Qco * (Fa - Fv)) / body$V_alveolar,
        body$f_brain  * Qco * lb * (Fa - Fb) / (body$V_brain  * ltg[["brain"]]),
        body$f_muscle * Qco * lb * (Fa - Fm) / (body$V_muscle * ltg[["muscle"]]),
        body$f_fat    * Qco * lb * (Fa - Ff) / (body$V_fat    * ltg[["fat"]]))
    }
    y <- rep(0, 4); h <- 0.001
    for (n in seq_len(20 / h)) {
      k1 <- deriv(y); k2 <- deriv(y + h / 2 * k1); k3 <- deriv(y + h / 2 * k2); k4 <- deriv(y + h * k3)
      y <- y + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
    }
    sys <- gasSystemSoluble(p, body, Q, VA, Qco, Ffgf)       # ideal by default
    expect_equal(sys$fresh, f)
    closed <- advanceGasSegment(rep(0, 5), sys$A, sys$b, 20)
    expect_equal(closed[2:5], y, tolerance = 1e-7, info = paste("Q =", Q))
  }
})

test_that("oxygen consumed shrinks the gas volume, and the fractions add up", {
  # The case that showed the problem (Shafer, 2026-10-05): 0.3 L/min of oxygen
  # with 1 L/min of nitrous oxide at 60 kg.  Oxygen consumption is 3.5 mL/kg/min,
  # 0.21 L/min, so only 0.09 L/min of oxygen is left over.
  body <- getGasBody(60)
  expect_equal(body$VO2, 0.0035 * 60)
  dose <- data.frame(Time = 0, Drug = c("nitrousOxide", "oxygen", "ventilation"),
                     Dose = c(1, 0.3, 5.1))
  sim <- advanceClosedFormGas(dose, weight = 60, maximum = 1440, resolution = 2881)
  last <- function(g, j) utils::tail(sim$state[[g]][, j], 1)
  gases <- c("oxygen", "nitrousOxide", "nitrogen")

  # Inspired gas sums to 100: the absorber has taken the carbon dioxide out.
  expect_equal(sum(vapply(gases, last, numeric(1), j = 1)), 100, tolerance = 1e-6)
  # Alveolar gas sums to 100 with its carbon dioxide, 100 VCO2 / VA.
  VA <- 5.1 * (1 - GAS_DEAD_SPACE_FRACTION)
  co2 <- 100 * GAS_RESPIRATORY_QUOTIENT * body$VO2 / VA
  expect_equal(co2, 4.7, tolerance = 0.01)
  expect_equal(sum(vapply(gases, last, numeric(1), j = 2)) + co2, 100, tolerance = 1e-6)

  # Mass balance.  What is vented is exhaled gas, and with the nitrous oxide
  # equilibrated it carries away the fresh gas less the oxygen consumed:
  # 1.09 L/min, 0.09 of it oxygen.
  exhaled <- vapply(gases, function(g)
    (1 - GAS_DEAD_SPACE_FRACTION) * last(g, 2) + GAS_DEAD_SPACE_FRACTION * last(g, 1), numeric(1))
  expect_equal(100 * exhaled[["oxygen"]] / sum(exhaled), 100 * 0.09 / 1.09, tolerance = 0.01)
  expect_equal(100 * exhaled[["nitrousOxide"]] / sum(exhaled), 100 * 1 / 1.09, tolerance = 0.01)

  # Without it, as before today, the fractions do not add up: most of the
  # volume the oxygen left behind is simply missing.
  old <- advanceClosedFormGas(dose, weight = 60, maximum = 1440, oxygenUptake = FALSE)
  oldSum <- sum(vapply(gases, function(g) utils::tail(old$state[[g]][, 2], 1), numeric(1)))
  expect_lt(oldSum, 85)
})

test_that("oxygen: pure oxygen at low flow, the old formula when switched off, and starvation", {
  body <- getGasBody(70)
  dose <- data.frame(Time = 0, Drug = c("oxygen", "ventilation"), Dose = c(1, 4))

  # Pure oxygen: once the nitrogen is gone, alveolar gas is oxygen and carbon
  # dioxide and nothing else, however low the flow.
  sim <- advanceClosedFormGas(dose, weight = 70, maximum = 1440, resolution = 2881)
  co2 <- 100 * GAS_RESPIRATORY_QUOTIENT * body$VO2 / (4 * (1 - GAS_DEAD_SPACE_FRACTION))
  expect_equal(utils::tail(sim$state$oxygen[, 2], 1) + co2, 100, tolerance = 2e-3)
  expect_equal(utils::tail(sim$state$oxygen[, 1], 1), 100, tolerance = 2e-3)

  # Switched off, oxygen is the plain sink it was: with no dead space the
  # alveolar steady state is F_fgf - 100 VO2 / Q.
  old <- advanceClosedFormGas(dose, weight = 70, maximum = 240, deadSpace = 0,
                              oxygenUptake = FALSE)
  alv <- utils::tail(old$state$oxygen[, 2], 1)
  expect_equal(alv, 100 - 100 * body$VO2 / 1, tolerance = 1e-3)
  expect_equal(utils::tail(old$state$oxygen[, 1], 1), 0.25 * 100 + 0.75 * alv, tolerance = 1e-6)

  # A flow below oxygen consumption cannot be survived, in the model as in life.
  starved <- advanceClosedFormGas(
    data.frame(Time = 0, Drug = c("oxygen", "ventilation"), Dose = c(0.1, 4)),
    weight = 70, maximum = 600)
  expect_lt(utils::tail(starved$state$oxygen[, 2], 1), 1)
  expect_gte(min(starved$state$oxygen), 0)
})
