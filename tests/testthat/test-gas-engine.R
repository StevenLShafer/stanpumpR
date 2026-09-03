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


test_that("closed-form advance matches independent RK4 integration", {
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

  sys <- gasSystemSoluble(p, body, Q, VA, Qco, Ffgf)

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


test_that("circuit steady state is the flow-weighted average of fresh and alveolar gas", {
  body <- getGasBody(70)
  props <- getGasProperties()
  p <- props[props$gas == "nitrogen", ]

  Q <- 1; VA <- 5; Qco <- body$Q_cardiac; Ffgf <- 79.07
  sys <- gasSystemSoluble(p, body, Q, VA, Qco, Ffgf)

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
  a <- advanceClosedFormGas(mk(1), maximum = 30)
  b <- advanceClosedFormGas(mk(2), maximum = 30)

  ga <- a$results[a$results$Drug == "sevoflurane" & a$results$Site == "Brain", "Y"]
  gb <- b$results[b$results$Drug == "sevoflurane" & b$results$Site == "Brain", "Y"]

  # Doubling the dial doubles the entire brain trajectory, exactly.
  expect_equal(gb, 2 * ga, tolerance = 1e-9)
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
  brn <- sim$results[sim$results$Drug == "sevoflurane" & sim$results$Site == "Brain", "Y"]
  mac <- sim$results[sim$results$Drug == "MAC", "Y"]

  MACsevo <- macForAge(props$MAC40[props$gas == "sevoflurane"], 40)
  expect_equal(mac[length(mac)], brn[length(brn)] / MACsevo, tolerance = 1e-9)
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


test_that("the concentration effect is refused rather than silently ignored", {
  dose <- data.frame(Time = 0, Drug = "oxygen", Dose = 2,
                     stringsAsFactors = FALSE)
  expect_error(
    advanceClosedFormGas(dose, concentrationEffect = TRUE),
    "not yet"
  )
})


test_that("cardiac output defaults to 75 mL/kg and changes uptake when overridden", {
  expect_equal(getGasBody(70)$Q_cardiac, 5.25, tolerance = 1e-12)
  expect_equal(getGasBody(100)$Q_cardiac, 7.5, tolerance = 1e-12)

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
  brn <- s$results[s$results$Drug == "desflurane" & s$results$Site == "Brain", "Y"]
  mac <- s$results[s$results$Drug == "MAC", "Y"]
  expect_gt(max(brn), 0)
  expect_equal(max(mac), max(brn) / macForAge(6.0, 40), tolerance = 1e-9)
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
