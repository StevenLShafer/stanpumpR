# ---------------------------------------------------------------------------
# Tests for the extravascular (oral / intramuscular / intranasal) engine:
#   R/advanceClosedFormPO_IM_IN.R  -- builds the timeline, splits the dose table
#                                    into bolus / infusion / PO / IM / IN lines,
#                                    and sums the per-exponential state variables
#   R/advanceStatePO.R             -- the one-state exponential recursion the
#                                    engine calls six times per simulation
#
# Everything is driven through simCpCe() with real shipped drugs, because that is
# how the engine is reached in production: simCpCe() converts dose units, sets the
# Bolus/PO/IM/IN flags, and dispatches to advanceClosedFormPO_IM_IN() as soon as
# any PO, IM, or IN row is present.  One test calls the engine directly so the
# entry point itself is covered without the unit-conversion layer in between.
#
# HOW THE EXPECTED VALUES ARE DERIVED (this matters -- a test that replays the
# code's own output proves nothing).  The reference curve is an independent
# solution of the underlying linear ODE system, obtained from the matrix
# exponential exp(M t) built out of an eigen-decomposition of M:
#
#     dAg/dt = -ka * Ag                                       Ag(0) = F * Dose
#     dA1/dt =  ka * Ag - (k10 + k12 + k13) * A1 + k21*A2 + k31*A3
#     dA2/dt =  k12 * A1 - k21 * A2
#     dA3/dt =  k13 * A1 - k31 * A3
#     Cp     =  A1 / v1
#
# and, where the effect site is involved, the extra state
#
#     dCe/dt = ke0 * (A1/v1 - Ce)
#
# Only the micro rate constants (k10, k12, k13, k21, k31), v1, ka, F and ke0 are
# taken from getDrugPK().  The eigen-decomposition never touches cube(), the
# lambda_i, or any of the p_coef_* / e_coef_* closed-form coefficients, so the
# comparison genuinely cross-checks stanpumpR's closed-form algebra and its state
# recursion against an independent solution of the same differential equations.
#
# KNOWN LIMITATION -- debug output.  advanceClosedFormPO_IM_IN() unconditionally
# runs cat("Structure of pkSet") and print(utils::str(pkSet)) on every call
# (reported on issue #65).  Every call below is wrapped in utils::capture.output()
# so the suite log stays readable.  These tests deliberately do NOT assert on that
# output: it is a defect and removing it must not break this file.
#
# KNOWN LIMITATION -- plotRecovery.  simCpCe() passes PK$endCe into the engine's
# `emerge` argument, but getDrugPK() returns that field under the name `emerge`
# (and populates it from drugDefaults$Emerge, a column that does not exist in
# inst/extdata/drugDefaults_global.csv -- the column is `endCe`).  PK$endCe is
# therefore always NULL and plotRecovery = TRUE errors inside recoveryCalc().
# That path is documented as broken in simCpCe()'s roxygen ("current broken,
# leave set to FALSE"), so it is not exercised here.  See the findings notes.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the pre-deployment
# test plan (extravascular PO/IM/IN engine, plan issue #283).  Expected values
# derived from first principles -- an independent eigen-decomposition solution of
# the compartmental ODE system, hand-evaluated recursions, and the clearance
# identity AUC(0-inf) = F * Dose / CL -- not from recorded stanpumpR output.
# Run and verified green against the working tree on R 4.6.1 / testthat 3.3.2.
# ---------------------------------------------------------------------------


# ---- shared fixtures ------------------------------------------------------

# simCpCe() only ever uses the first two columns of `events`; an empty table
# keeps every simulation on the single-PK-set (time-invariant) branch.
poEvents <- data.frame(Time = double(), Event = character(), Fill = character())

# Two shipped drugs carry extravascular units in drugDefaults_global.csv:
#   hydromorphone -- "mg, mg/kg, mg/hr, mg/kg/hr, mg PO, mg IM, mg IN"
#                    full three-compartment model, and the only drug with a
#                    non-zero absorption lag (tlag_IM = 90, tlag_IN = 180 min)
#   oxycodone     -- "mg PO" only; a two-compartment model (cl3 = 0, so
#                    lambda_3 = 0), which exercises the degenerate branch
poPKhydro <- getDrugPK("hydromorphone", 70, 170, 50, "male",
                       getDrugDefaults("hydromorphone"))
poPKoxy   <- getDrugPK("oxycodone",     70, 170, 50, "male",
                       getDrugDefaults("oxycodone"))
poPSh <- poPKhydro$PK[[1]]
poPSo <- poPKoxy$PK[[1]]

# Both drugs report in ng/ml, so simCpCe() divides mg doses by mg_Conv = 0.001,
# i.e. amounts are carried in mcg while volumes are in litres (ng/ml == mcg/l).
poAmountPerMg <- 1000

# Run a simulation and hand back one of simCpCe()'s output series (`what` selects
# the Site: "Plasma" or "Effect Site") together with the summary tables.
# capture.output() swallows the engine's unconditional str(pkSet) dump.
poSim <- function(PK, doseTable, maximum, what = "Plasma") {
  out <- NULL
  utils::capture.output(
    out <- simCpCe(doseTable, poEvents, PK, maximum, FALSE)
  )
  series <- out$results[out$results$Site == what, ]
  list(
    Time = series$Time,
    Y    = series$Y,
    max  = out$max,
    equiSpace = out$equiSpace
  )
}

# Dose-table row builder.  `Drug` is carried for realism; simCpCe() labels its
# output from PK$drug and never reads this column.
poRow <- function(drug, time, dose, units) {
  data.frame(Drug = drug, Time = time, Dose = dose, Units = units)
}

# --- the independent reference solution -----------------------------------
# expm(M t) applied to the initial condition, via eigen-decomposition of M.
# Returns the plasma curve; `effectSite = TRUE` appends the effect compartment
# as a fifth state and returns both Cp and Ce.
poReference <- function(ps, amount, bioavailability, ka, times, effectSite = FALSE) {
  elim <- ps$k10 + ps$k12 + ps$k13
  M <- rbind(
    c(-ka,      0,       0,        0),
    c( ka,  -elim,  ps$k21,   ps$k31),
    c(  0, ps$k12, -ps$k21,        0),
    c(  0, ps$k13,       0,  -ps$k31)
  )
  x0 <- c(bioavailability * amount, 0, 0, 0)
  if (effectSite) {
    # Ce is driven by A1/v1 and decays at ke0; append it as a fifth state.
    M  <- cbind(rbind(M, c(0, ps$ke0 / ps$v1, 0, 0)), c(0, 0, 0, 0, -ps$ke0))
    x0 <- c(x0, 0)
  }
  E <- eigen(M)
  coefficients <- solve(E$vectors, x0)
  pick <- function(rowIndex, scale) {
    vapply(
      times,
      function(t) Re(sum(E$vectors[rowIndex, ] * exp(E$values * t) * coefficients)) / scale,
      numeric(1)
    )
  }
  if (effectSite) {
    data.frame(Cp = pick(2, ps$v1), Ce = pick(5, 1))
  } else {
    pick(2, ps$v1)
  }
}


# ---- 1. advanceStatePO(): the one-state recursion -------------------------

test_that("advanceStatePO advances one exponential state by hand-checked algebra", {
  # The recursion is state_i = state_(i-1) * l_i + bolus_i + infusion_i +
  # PO_i + IM_i + IN_i, seeded at zero, with the seed dropped from the output.
  # Hand evaluation of the vectors below:
  #   i = 1:  0    * 0.50 + 1 = 1.0
  #   i = 2:  1.0  * 0.80 + 2 = 2.8
  #   i = 3:  2.8  * 0.25 + 3 = 3.7
  state <- advanceStatePO(
    l        = c(0.50, 0.80, 0.25),
    bolus    = c(1, 0, 0),
    infusion = c(0, 2, 0),
    PO       = c(0, 0, 3),
    IM       = c(0, 0, 0),
    IN       = c(0, 0, 0),
    L        = 3
  )
  expect_equal(state, c(1.0, 2.8, 3.7))

  # All five input channels enter the sum additively and symmetrically, so
  # moving the same input between channels cannot change the answer.
  viaIM <- advanceStatePO(
    l        = c(0.50, 0.80, 0.25),
    bolus    = c(0, 0, 0),
    infusion = c(1, 0, 0),
    PO       = c(0, 2, 0),
    IM       = c(0, 0, 3),
    IN       = c(0, 0, 0),
    L        = 3
  )
  expect_equal(viaIM, state)

  # The very first element of `l` multiplies the zero seed and is therefore
  # ignored; this documents that the engine's dt[1] = 0 convention is harmless.
  expect_equal(
    advanceStatePO(c(99, 0.8, 0.25), c(1, 0, 0), c(0, 2, 0), c(0, 0, 3),
                   c(0, 0, 0), c(0, 0, 0), 3),
    state
  )

  # A pure decay chain must reproduce exp(-lambda * t) exactly.
  lambda <- 0.037
  dt <- c(0, 5, 5, 5)
  decay <- advanceStatePO(exp(-lambda * dt), c(2, 0, 0, 0), rep(0, 4), rep(0, 4),
                          rep(0, 4), rep(0, 4), 4)
  expect_equal(decay, 2 * exp(-lambda * cumsum(dt)))
})


# ---- 2. the engine entry point, called directly ---------------------------

test_that("advanceClosedFormPO_IM_IN returns a well-formed frame and the right Cp", {
  # Dose table in the post-conversion form simCpCe() hands over: amounts already
  # in mcg, plus the four route flags.  4 mg oral hydromorphone.
  dose <- data.frame(
    Time  = 0,
    Dose  = 4 * poAmountPerMg,
    Units = "mg PO",
    Bolus = FALSE,
    PO    = TRUE,
    IM    = FALSE,
    IN    = FALSE
  )

  out <- NULL
  utils::capture.output(
    out <- advanceClosedFormPO_IM_IN(dose, poPSh, maximum = 720,
                                     plotRecovery = FALSE, emerge = 1)
  )

  expect_s3_class(out, "data.frame")
  expect_named(out, c("Time", "Cp", "Ce", "Recovery"))
  # The timeline is built with sort(unique(...)), so it must be strictly
  # increasing, start at 0, and end at `maximum`.
  expect_true(all(diff(out$Time) > 0))
  expect_equal(out$Time[1], 0)
  expect_equal(out$Time[nrow(out)], 720)
  # plotRecovery = FALSE leaves the Recovery column as the zero vector.
  expect_true(all(out$Recovery == 0))

  # Cp against the independent ODE solution.
  reference <- poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO,
                           poPSh$ka_PO, out$Time)
  expect_lt(max(abs(out$Cp - reference)), 1e-8 * max(reference))
})


# ---- 3. absorption-phase shape --------------------------------------------

test_that("an oral dose starts at zero and peaks later than the same IV bolus", {
  po <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  iv <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg"),    720)

  # Cp(0) is zero by construction: p_coef_PO_ka is defined as minus the sum of
  # the three lambda coefficients, so the four exponentials cancel at t = 0.
  # What survives is floating-point cancellation residue, ~1e-15 on a peak of
  # roughly 5 ng/ml, hence the relative-to-Cmax bound rather than == 0.
  expect_lt(abs(po$Y[1]), 1e-9 * max(po$Y))

  # An IV bolus is at its maximum the instant it is given; the oral curve is not.
  expect_equal(iv$Time[which.max(iv$Y)], 0)
  expect_gt(po$Time[which.max(po$Y)], iv$Time[which.max(iv$Y)])
  expect_gt(po$Time[which.max(po$Y)], 30)

  # Rise then fall: the profile is strictly increasing up to the grid maximum
  # and strictly decreasing after it.
  peak <- which.max(po$Y)
  expect_gt(peak, 1)
  expect_lt(peak, length(po$Y))
  expect_true(all(diff(po$Y[1:peak]) > 0))
  expect_true(all(diff(po$Y[peak:length(po$Y)]) < 0))

  # The discrete argmax must bracket the true tmax of the continuous curve.
  # tmax is located by optimising the independent ODE reference, not by reading
  # anything back out of the simulation.
  analyticTmax <- stats::optimize(
    function(t) -poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO,
                             poPSh$ka_PO, t),
    interval = c(0, 720),
    tol = 1e-10
  )$minimum
  expect_gt(analyticTmax, po$Time[peak - 1])
  expect_lt(analyticTmax, po$Time[peak + 1])
})


# ---- 4. pointwise agreement with the closed-form / ODE solution -----------

test_that("the simulated oral curve matches an independent ODE solution pointwise", {
  # hydromorphone: full three-compartment disposition, ka = 0.01/min, F = 0.6.
  # Note this drug is in flip-flop -- ka is slower than lambda_2 -- which makes
  # p_coef_PO_ka positive and p_coef_PO_l1/l2 negative, so the test also covers
  # the sign bookkeeping in getDrugPK()'s PO coefficient block.
  hydro <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  refH <- poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO,
                      poPSh$ka_PO, hydro$Time)
  expect_lt(max(abs(hydro$Y - refH)), 1e-8 * max(refH))
  expect_equal_rounded(refH, hydro$Y)

  # oxycodone: cl3 = 0, so lambda_3 = 0 and the third exponential drops out of
  # the closed form.  The reference matrix is still solved as a four-state
  # system (the third compartment simply never receives drug).
  oxy <- poSim(poPKoxy, poRow("oxycodone", 0, 20, "mg PO"), 720)
  refO <- poReference(poPSo, 20 * poAmountPerMg, poPSo$bioavailability_PO,
                      poPSo$ka_PO, oxy$Time)
  expect_lt(max(abs(oxy$Y - refO)), 1e-8 * max(refO))
  expect_equal_rounded(refO, oxy$Y)

  # Sanity on scale: 20 mg oral oxycodone should peak in the tens of ng/ml,
  # which is the range the shipped MEAC of 12 ng/ml is calibrated against.
  expect_gt(max(oxy$Y), 10)
  expect_lt(max(oxy$Y), 200)
})


# ---- 5. absorption lag ----------------------------------------------------

test_that("tlag holds concentrations at exactly zero until the lag has elapsed", {
  # hydromorphone is the shipped drug that carries lags: tlag_PO = 0,
  # tlag_IM = 90 min, tlag_IN = 180 min.  The engine implements the lag by
  # shifting the dose time, so everything before the lag is untouched zero --
  # not a small number, exactly zero.
  expect_equal(poPSh$tlag_PO, 0)
  expect_equal(poPSh$tlag_IM, 90)
  expect_equal(poPSh$tlag_IN, 180)

  for (route in list(list(units = "mg IM", lag = 90), list(units = "mg IN", lag = 180))) {
    lag <- route$lag
    sim <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, route$units), 720)

    before <- sim$Y[sim$Time < lag]
    after  <- sim$Y[sim$Time > lag]
    expect_gt(length(before), 0)
    expect_gt(length(after), 0)
    expect_true(all(before == 0))
    expect_true(all(after > 0))

    # At the lag itself the four exponentials cancel, exactly as at t = 0 for
    # an oral dose, so the value there is cancellation residue rather than zero.
    atLag <- sim$Y[sim$Time == lag]
    expect_equal(length(atLag), 1L)
    expect_lt(abs(atLag), 1e-9 * max(sim$Y))
  }

  # hydromorphone uses the same ka and bioavailability for PO, IM and IN, so the
  # IM curve must be the oral curve translated 90 minutes to the right.  The
  # reference is again the independent ODE solution, evaluated at t - tlag.
  im <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg IM"), 720)
  shifted <- ifelse(
    im$Time < poPSh$tlag_IM,
    0,
    poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_IM, poPSh$ka_IM,
                pmax(im$Time - poPSh$tlag_IM, 0))
  )
  expect_lt(max(abs(im$Y - shifted)), 1e-8 * max(shifted))

  # The lag delays but does not change the peak height.
  po <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  expect_equal(max(im$Y), max(po$Y), tolerance = 1e-9)
  expect_equal(im$Time[which.max(im$Y)] - poPSh$tlag_IM,
               po$Time[which.max(po$Y)], tolerance = 1e-9)
})


# ---- 6. linearity in dose -------------------------------------------------

test_that("the extravascular engine is exactly linear in dose", {
  # The timeline depends only on dose *times*, never on dose amounts, so the two
  # simulations land on identical grids and can be compared element by element.
  single <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  double <- poSim(poPKhydro, poRow("hydromorphone", 0, 8, "mg PO"), 720)
  expect_identical(single$Time, double$Time)
  expect_equal(double$Y, 2 * single$Y)

  # Ten times the dose, ten times the curve -- and a zero dose gives zero.
  tenFold <- poSim(poPKhydro, poRow("hydromorphone", 0, 40, "mg PO"), 720)
  expect_equal(tenFold$Y, 10 * single$Y)
  zero <- poSim(poPKhydro, poRow("hydromorphone", 0, 0, "mg PO"), 720)
  expect_true(all(zero$Y == 0))

  # Linearity holds for the effect site too, since Ce is a linear functional
  # of Cp (calculateCe() interpolates Cp, then convolves with a fixed ke0).
  singleCe <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720, "Effect Site")
  doubleCe <- poSim(poPKhydro, poRow("hydromorphone", 0, 8, "mg PO"), 720, "Effect Site")
  expect_equal(doubleCe$Y, 2 * singleCe$Y)
})


# ---- 7. superposition -----------------------------------------------------

test_that("two oral doses superpose onto the sum of the single-dose simulations", {
  # Grid-alignment trick: the timeline is derived from the set of dose times, so
  # each single-dose run is padded with a zero-dose row at the other dose time.
  # That makes all three simulations share one timeline exactly.
  both <- poSim(poPKhydro,
                rbind(poRow("hydromorphone", 0, 4, "mg PO"),
                      poRow("hydromorphone", 240, 6, "mg PO")), 720)
  first <- poSim(poPKhydro,
                 rbind(poRow("hydromorphone", 0, 4, "mg PO"),
                       poRow("hydromorphone", 240, 0, "mg PO")), 720)
  second <- poSim(poPKhydro,
                  rbind(poRow("hydromorphone", 0, 0, "mg PO"),
                        poRow("hydromorphone", 240, 6, "mg PO")), 720)

  expect_identical(both$Time, first$Time)
  expect_identical(both$Time, second$Time)
  expect_equal(both$Y, first$Y + second$Y)

  # And against first principles: the sum of two independently shifted ODE
  # solutions, which is what superposition means for a linear system.
  reference <-
    poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO, poPSh$ka_PO,
                both$Time) +
    ifelse(both$Time < 240, 0,
           poReference(poPSh, 6 * poAmountPerMg, poPSh$bioavailability_PO,
                       poPSh$ka_PO, pmax(both$Time - 240, 0)))
  expect_lt(max(abs(both$Y - reference)), 1e-8 * max(reference))

  # The second dose lands on the tail of the first, so the overall peak has to
  # sit after it -- a cheap guard against the dose being dropped.
  expect_gt(both$Time[which.max(both$Y)], 240)
  expect_gt(max(both$Y), max(first$Y))
})


# ---- 8. route comparison: PO versus IV ------------------------------------

test_that("the same milligram dose gives a lower, later peak orally than IV", {
  po <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  iv <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg"),    720)

  expect_lt(max(po$Y), max(iv$Y))
  expect_gt(po$Time[which.max(po$Y)], iv$Time[which.max(iv$Y)])

  # Both curves stay finite; the oral curve stays non-negative apart from the
  # t = 0 cancellation residue described above.
  expect_true(all(is.finite(po$Y)))
  expect_true(all(is.finite(iv$Y)))
  expect_gt(min(po$Y), -1e-9 * max(po$Y))
  expect_gte(min(iv$Y), 0)

  # The peak is knocked down by far more than bioavailability alone: absorption
  # (ka = 0.01/min) is slow enough that distribution removes most of the drug
  # from plasma before the oral dose has finished arriving.  So Cmax(oral) must
  # sit well below F * Cmax(IV), which is the ceiling a hypothetical
  # instantaneous absorption would give.
  expect_lt(max(po$Y), poPSh$bioavailability_PO * max(iv$Y))

  # Same dose times, so the two runs share one timeline and can be compared
  # element by element.  Early on the IV curve is far above the oral one; in
  # mid-course the oral curve overtakes it, because the bolus has distributed
  # into the peripheral compartments while absorption is still feeding the
  # central one.  Both statements together are the signature of an absorption
  # phase, and neither would survive if the PO coefficients were mis-signed.
  expect_identical(po$Time, iv$Time)
  expect_gt(iv$Y[2], po$Y[2])
  expect_gt(max(po$Y - iv$Y), 0)
})


# ---- 9. mixed IV and oral in one dose table -------------------------------

test_that("an IV bolus and an oral dose in one table add without interference", {
  # Padding each single-route run with a zero-dose row of the *other* route
  # does two jobs: it aligns the timelines, and it keeps the single-route runs
  # on the extravascular engine (an IV-only table would dispatch to
  # advanceClosedForm0, which builds a slightly different timeline).
  mixed <- NULL
  expect_no_error(
    mixed <- poSim(poPKhydro,
                   rbind(poRow("hydromorphone", 0, 1, "mg"),
                         poRow("hydromorphone", 120, 4, "mg PO")), 720)
  )
  ivOnly <- poSim(poPKhydro,
                  rbind(poRow("hydromorphone", 0, 1, "mg"),
                        poRow("hydromorphone", 120, 0, "mg PO")), 720)
  poOnly <- poSim(poPKhydro,
                  rbind(poRow("hydromorphone", 0, 0, "mg"),
                        poRow("hydromorphone", 120, 4, "mg PO")), 720)

  expect_identical(mixed$Time, ivOnly$Time)
  expect_identical(mixed$Time, poOnly$Time)
  expect_equal(mixed$Y, ivOnly$Y + poOnly$Y)

  # Against first principles: an IV bolus is the same disposition system with the
  # drug placed straight into the central compartment (no gut state, F = 1).
  ivElimination <- poPSh$k10 + poPSh$k12 + poPSh$k13
  ivMatrix <- rbind(
    c(-ivElimination, poPSh$k21,  poPSh$k31),
    c(     poPSh$k12, -poPSh$k21,         0),
    c(     poPSh$k13,          0, -poPSh$k31)
  )
  ivEigen <- eigen(ivMatrix)
  ivCoefficients <- solve(ivEigen$vectors, c(1 * poAmountPerMg, 0, 0))
  ivReference <- vapply(
    mixed$Time,
    function(t) {
      Re(sum(ivEigen$vectors[1, ] * exp(ivEigen$values * t) * ivCoefficients)) / poPSh$v1
    },
    numeric(1)
  )
  poReferenceCurve <- ifelse(
    mixed$Time < 120, 0,
    poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO, poPSh$ka_PO,
                pmax(mixed$Time - 120, 0))
  )
  expect_lt(max(abs(mixed$Y - (ivReference + poReferenceCurve))),
            1e-8 * max(mixed$Y))

  # All four routes may be mixed in a single table, and they still superpose.
  # Each single-route run carries zero-dose rows for the other three, so all
  # five simulations share one timeline.
  fourRoutes <- function(bolus, oral, intramuscular, intranasal) {
    rbind(
      poRow("hydromorphone", 0, bolus,         "mg"),
      poRow("hydromorphone", 0, oral,          "mg PO"),
      poRow("hydromorphone", 0, intramuscular, "mg IM"),
      poRow("hydromorphone", 0, intranasal,    "mg IN")
    )
  }
  combined <- poSim(poPKhydro, fourRoutes(1, 4, 2, 2), 720)
  pieces <- list(
    poSim(poPKhydro, fourRoutes(1, 0, 0, 0), 720),
    poSim(poPKhydro, fourRoutes(0, 4, 0, 0), 720),
    poSim(poPKhydro, fourRoutes(0, 0, 2, 0), 720),
    poSim(poPKhydro, fourRoutes(0, 0, 0, 2), 720)
  )
  for (piece in pieces) expect_identical(piece$Time, combined$Time)
  expect_equal(combined$Y, Reduce(`+`, lapply(pieces, function(p) p$Y)))
  expect_true(all(is.finite(combined$Y)))

  # The IM and IN lags (90 and 180 minutes) load the late tail: at 12 hours the
  # combined curve carries several times what the IV bolus alone leaves behind.
  last <- length(combined$Y)
  expect_gt(combined$Y[last], 4 * pieces[[1]]$Y[last])
})


# ---- 10. agreement with the IV-only engine --------------------------------

test_that("with no extravascular dose the PO engine reproduces advanceClosedForm0", {
  # A bolus plus an infusion that is turned off later, run twice: once as an
  # IV-only table (dispatches to advanceClosedForm0) and once with a single
  # zero-dose oral row appended (dispatches to advanceClosedFormPO_IM_IN).
  # The extravascular engine puts dose$Time - 0.01 into the timeline for every
  # row rather than for boluses only, so its grid is a strict superset; the
  # comparison is made on the shared points.
  ivTable <- rbind(
    poRow("hydromorphone",   0, 2,   "mg"),
    poRow("hydromorphone",  30, 0.5, "mg/hr"),
    poRow("hydromorphone", 150, 0,   "mg/hr")
  )
  viaIVEngine <- poSim(poPKhydro, ivTable, 480)
  viaPOEngine <- poSim(poPKhydro,
                       rbind(ivTable, poRow("hydromorphone", 0, 0, "mg PO")), 480)

  shared <- intersect(viaIVEngine$Time, viaPOEngine$Time)
  expect_equal(length(shared), length(viaIVEngine$Time))
  a <- viaIVEngine$Y[match(shared, viaIVEngine$Time)]
  b <- viaPOEngine$Y[match(shared, viaPOEngine$Time)]
  # The two engines run the identical bolus/infusion algebra, so plasma
  # concentrations agree to the last bit.
  expect_equal(b, a, tolerance = 1e-12)
  expect_gt(max(a), 0)

  # Effect site agrees only to the accuracy of calculateCe(), which integrates
  # a piecewise interpolation of Cp: the two grids differ by the extra
  # dose-time-minus-0.01 points, so the interpolation error differs slightly.
  ceIV <- poSim(poPKhydro, ivTable, 480, "Effect Site")
  cePO <- poSim(poPKhydro,
                rbind(ivTable, poRow("hydromorphone", 0, 0, "mg PO")), 480,
                "Effect Site")
  ceA <- ceIV$Y[match(shared, ceIV$Time)]
  ceB <- cePO$Y[match(shared, cePO$Time)]
  expect_lt(max(abs(ceA - ceB)), 1e-5 * max(ceA))
})


# ---- 11. AUC and the clearance identity -----------------------------------

test_that("oral AUC(0-inf) equals Dose * F / CL", {
  # Step 1 -- the algebraic identity, from the definition of clearance.
  # For a sum of exponentials Cp(t) = sum_j c_j exp(-r_j t) the integral to
  # infinity is sum_j c_j / r_j; for an extravascular dose that integral must
  # equal F * Dose / CL.  Terms whose rate constant is zero carry a zero
  # coefficient and are skipped (oxycodone has lambda_3 = 0).
  aucPerUnitDose <- function(ps) {
    coefficients <- c(ps$p_coef_PO_l1, ps$p_coef_PO_l2, ps$p_coef_PO_l3,
                      ps$p_coef_PO_ka)
    rates <- c(ps$lambda_1, ps$lambda_2, ps$lambda_3, ps$ka_PO)
    sum(coefficients[rates > 0] / rates[rates > 0])
  }
  expect_equal(aucPerUnitDose(poPSh), poPSh$bioavailability_PO / poPSh$cl1)
  expect_equal(aucPerUnitDose(poPSo), poPSo$bioavailability_PO / poPSo$cl1)
  # Bioavailability is not folded away anywhere: it appears as an explicit
  # multiplier in getDrugPK()'s PO coefficient block, and the identity above
  # recovers F / CL rather than 1 / CL.  That is only a meaningful distinction
  # because both drugs ship with F strictly between 0 and 1; the exact values
  # are model choices and are deliberately not pinned here.
  for (bioavailability in c(poPSh$bioavailability_PO, poPSo$bioavailability_PO)) {
    expect_gt(bioavailability, 0)
    expect_lt(bioavailability, 1)
  }

  # Step 2 -- the same number by numerical quadrature over the simulated curve.
  # oxycodone is used because its terminal half-life (ln 2 / lambda_2, about 3
  # hours) lets a 72-hour window capture the whole curve: the analytic tail
  # beyond 4320 minutes is under 1e-7 of the total, so no tail correction is
  # needed.  The dose table is padded with hourly zero-dose rows purely to
  # densify the timeline -- the engine's own gap filling is geometric and gets
  # very sparse late on, which would leave the trapezoid rule badly biased.
  horizon <- 4320
  padTimes <- seq(60, horizon, by = 60)
  doseTable <- rbind(
    poRow("oxycodone", 0, 20, "mg PO"),
    poRow("oxycodone", padTimes, 0, "mg PO")
  )
  sim <- poSim(poPKoxy, doseTable, horizon)

  trapezoid <- sum(diff(sim$Time) *
                     (utils::head(sim$Y, -1) + sim$Y[-1]) / 2)
  target <- 20 * poAmountPerMg * poPSo$bioavailability_PO / poPSo$cl1

  # Tolerance is set by the quadrature, not by the model: the trapezoid rule on
  # this grid is accurate to about 1.5e-4 relative (measured), so 1e-3 gives
  # roughly a factor of seven of headroom without being a rubber stamp.
  expect_equal(trapezoid / target, 1, tolerance = 1e-3)

  # And the residual tail really is negligible, which is what licenses omitting
  # a tail correction above.
  expect_lt(sim$Y[length(sim$Y)] / poPSo$lambda_2, 1e-5 * target)
})


# ---- 12. effect site output on the extravascular path ---------------------

test_that("Ce lags Cp and tracks the exact effect-site solution", {
  sim <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720)
  ce  <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg PO"), 720, "Effect Site")

  expect_identical(sim$Time, ce$Time)
  expect_equal(ce$Y[1], 0)
  expect_true(all(is.finite(ce$Y)))
  # The effect site is downstream of plasma, so its peak comes later and lower.
  expect_gt(ce$Time[which.max(ce$Y)], sim$Time[which.max(sim$Y)])
  expect_lt(max(ce$Y), max(sim$Y))

  # Compare with the exact five-state solution (four PK states plus the effect
  # compartment).  calculateCe() is *not* exact: it advances Ce assuming Cp is
  # linear (rising) or log-linear (falling) between grid points, so the residual
  # here is interpolation error on the engine's sparse geometric timeline, about
  # 0.13% of the peak as shipped.  The 1% bound below is that measured error
  # with headroom; it is a statement about the quadrature, not about the model.
  reference <- poReference(poPSh, 4 * poAmountPerMg, poPSh$bioavailability_PO,
                           poPSh$ka_PO, ce$Time, effectSite = TRUE)
  expect_lt(max(abs(ce$Y - reference$Ce)), 1e-2 * max(reference$Ce))
  expect_lt(max(abs(sim$Y - reference$Cp)), 1e-8 * max(reference$Cp))
})


# ---- 13. pinned quirk: an absorption lag past the end of the simulation ---

test_that("an absorption lag beyond `maximum` pushes the timeline past `maximum`", {
  # PINNED QUIRK.  advanceClosedFormPO_IM_IN() adds tlag to the dose time and
  # only then builds the timeline, so when the lag carries the dose beyond the
  # requested horizon the returned Time column runs past `maximum` -- here to
  # 180 minutes for an intranasal hydromorphone dose in a 120-minute window.
  # Every other engine stops at `maximum`.  Within the requested window the
  # answer is still correct (nothing has been absorbed yet, so Cp is zero), and
  # simCpCe()'s equispaced output and reported maxima are unaffected, which is
  # why this is pinned rather than treated as a blocking defect.  Fixing the
  # engine to clamp the timeline should deliberately update this test.
  sim <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg IN"), 120)

  expect_equal(max(sim$Time), poPSh$tlag_IN)   # 180, not the requested 120
  expect_gt(sum(sim$Time > 120), 0)

  # The physiologically meaningful part is right: nothing is absorbed inside the
  # window, so both the in-window curve and the reported maximum are zero.
  expect_true(all(sim$Y[sim$Time <= 120] == 0))
  expect_equal(sim$max$Cp, 0)
  expect_equal(sim$max$Ce, 0)
  expect_true(all(sim$equiSpace$Ce == 0))

  # A window that does contain the lag behaves normally, with the whole
  # timeline inside the requested horizon.
  ok <- poSim(poPKhydro, poRow("hydromorphone", 0, 4, "mg IN"), 720)
  expect_equal(max(ok$Time), 720)
  expect_gt(max(ok$Y), 0)
})
