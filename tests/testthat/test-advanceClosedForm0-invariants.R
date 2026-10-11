# test-advanceClosedForm0-invariants.R
#
# Physics invariants of the time-invariant closed-form engine (R/advanceClosedForm0.R),
# exercised through the production entry point simCpCe() so that unit conversion and
# the Bolus/PO/IM/IN flags are applied exactly as the app applies them. The existing
# test-advanceClosedForm0.R only checks an all-zero dose; this file adds real-dose
# behavior:
#
#   1. A single IV bolus reproduces the analytic sum of exponentials
#      Cp(t) = D * sum_i p_coef_bolus_i * exp(-lambda_i * t), including the
#      coefficient identities sum(p_coef_bolus) = 1/v1 and
#      sum(p_coef_infusion) = 1/cl1.
#   2. Superposition (linearity): bolus A at t=0 plus bolus B at t=10 equals the
#      pointwise sum of the two single-bolus simulations on an identical time grid.
#   3. A constant infusion approaches steady state Cp_ss = R / cl1 (mass balance).
#   4. After an infusion stops, Cp decays strictly monotonically.
#   5. The plotRecovery path: pinned quirk for the exported API (see below), plus a
#      direct engine call and the app-style patched call, with an independent
#      first-principles check that Recovery is the time for the analytic effect-site
#      curve to decay to the emergence target.
#   6. The effect-site (Ce) peak for a bolus at t=0 lands at the grid point nearest
#      tPeak (ke0 is fitted so that the analytic Ce peaks exactly at tPeak).
#
# All simulations use remifentanil, 70 kg / 170 cm / 50 y male. Remifentanil's
# Concentration.Units is "ng", so mcg doses pass through simCpCe's unit conversion
# unchanged and concentrations come out in mcg/L = ng/mL (v1, cl1 are in L, L/min).
#
# KNOWN LIMITATION / pinned quirk (also reported in the PR): calling the exported
# pipeline getDrugPK() -> simCpCe(..., plotRecovery = TRUE) errors with
# "argument is of length zero" (message text is locale-dependent, so the pin does
# not match on it). Root cause is a field-name mismatch chain:
#   - inst/extdata/drugDefaults_global.csv names the emergence-threshold column
#     "endCe";
#   - getDrugPK() (R/getDrugPK.R, `emerge = drugDefaults$Emerge`) reads a
#     nonexistent "Emerge" column, so the returned PK$emerge is always NULL;
#   - simCpCe() (R/simCpCe.R line 89) passes PK$endCe, which is not an element of
#     getDrugPK()'s return list either, so advanceClosedForm0() receives
#     emerge = NULL and recoveryCalc() fails at `if (target >= sum(state))`.
# The Shiny app works only because R/server-helpers.R (line 73) patches
# drugs[[drug]]$endCe onto the PK object after getDrugPK(). The roxygen for
# simCpCe() says "current broken, leave set to FALSE". Fixing the plumbing should
# deliberately update the pinned expectations in the "plotRecovery via the exported
# API" test below. The engine itself is fine when emerge is numeric, which the
# remaining recovery tests demonstrate.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the pre-deployment
# test plan (closed-form engine invariants, plan issue #283). Expected values are
# derived from first principles - the analytic three-compartment bolus/infusion
# solutions, the coefficient identities sum(p_coef_bolus) = 1/v1 and
# sum(p_coef_infusion) = 1/cl1, superposition of linear systems, and steady-state
# mass balance Cp_ss = R/cl1 - not by pasting back simulation output. The one
# approximate element is Ce: advanceClosedForm0 computes Ce with the stepwise
# calculateCe() integrator (piecewise linear/log interpolation of Cp), which is not
# exactly linear in Cp, so the Ce superposition assertion carries a stated loose
# tolerance. Verified against the working tree on 2026-08-14 (all passing).

# ---------------------------------------------------------------------------
# Shared fixtures (sourced once by testthat)
# ---------------------------------------------------------------------------

covWeight <- 70
PK <- getDrugPK("remifentanil", covWeight, 170, 50, "male",
                getDrugDefaults("remifentanil"))
pkSet <- PK$PK[["default"]]

# simCpCe() drops all but the first two event columns and keeps only events whose
# Event is in PK$pkEvents; remifentanil has the single "default" PK set, so any
# placeholder event table routes the simulation to advanceClosedForm0().
events <- data.frame(Time = 0, Event = "none", stringsAsFactors = FALSE)

# Pull one Site's (Time, Y) series out of simCpCe()'s long-format results.
extractSite <- function(res, site) {
  res$results[res$results$Site == site, c("Time", "Y")]
}

# Analytic unit responses for a 1 mcg IV bolus at t = 0 (ng/mL per mcg).
# These are the textbook closed-form solutions the engine is supposed to
# reproduce; advanceState()'s recursion
#   state[i] = state[i-1] * exp(-lambda * dt) + bolus[i] + infusion[i]
# propagates each exponential exactly, so the plasma comparison is exact to
# floating point.
bolusUnitCp <- function(t) {
  with(pkSet,
    p_coef_bolus_l1 * exp(-lambda_1 * t) +
    p_coef_bolus_l2 * exp(-lambda_2 * t) +
    p_coef_bolus_l3 * exp(-lambda_3 * t)
  )
}
# Analytic effect-site unit response (used to independently verify Recovery;
# the four coefficients sum to zero, so Ce(0) = 0).
bolusUnitCe <- function(t) {
  with(pkSet,
    e_coef_bolus_l1 * exp(-lambda_1 * t) +
    e_coef_bolus_l2 * exp(-lambda_2 * t) +
    e_coef_bolus_l3 * exp(-lambda_3 * t) +
    e_coef_bolus_ke0 * exp(-ke0 * t)
  )
}

# One shared single-bolus run (100 mcg at t = 0), reused by tests 1 and 6.
bolusDose <- 100  # mcg
singleBolusTable <- data.frame(
  Drug = "remifentanil", Time = 0, Dose = bolusDose, Units = "mcg",
  stringsAsFactors = FALSE
)
singleBolusRun <- simCpCe(singleBolusTable, events, PK,
                          maximum = 60, plotRecovery = FALSE)

# ---------------------------------------------------------------------------
# 1. Single IV bolus: analytic sum of exponentials
# ---------------------------------------------------------------------------

test_that("a single IV bolus reproduces the analytic sum of exponentials", {
  # Coefficient identities, from the standard 3-compartment algebra:
  # at t = 0+ all the drug is in V1, so Cp(0) = D/v1, i.e. the bolus
  # coefficients must sum to 1/v1; and integrating the unit disposition
  # function gives sum(p_coef_bolus_i / lambda_i) = sum(p_coef_infusion_i)
  # = 1/cl1 (total exposure = dose / clearance).
  expect_equal_rounded(
    1 / pkSet$v1,
    pkSet$p_coef_bolus_l1 + pkSet$p_coef_bolus_l2 + pkSet$p_coef_bolus_l3
  )
  expect_equal_rounded(
    1 / pkSet$cl1,
    pkSet$p_coef_infusion_l1 + pkSet$p_coef_infusion_l2 + pkSet$p_coef_infusion_l3
  )

  cp <- extractSite(singleBolusRun, "Plasma")

  # The engine applies a t = 0 bolus instantaneously at the first grid point
  # (dt = 0 there), so the simulation starts at the mixed-in-V1 concentration.
  expect_equal_rounded(bolusDose / pkSet$v1, cp$Y[cp$Time == 0])

  # First post-dose grid point (the engine's exponential gap-filling puts it at
  # ~0.69/ke0/4 ~ 0.3 min) must equal the closed form there ...
  expect_equal_rounded(bolusDose * bolusUnitCp(cp$Time[2]), cp$Y[2])

  # ... and in fact the whole plasma series is the analytic solution evaluated
  # on the grid (observed agreement ~1e-15 relative; default tolerance).
  expect_equal_rounded(bolusDose * bolusUnitCp(cp$Time), cp$Y)
})

# ---------------------------------------------------------------------------
# 2. Superposition (linearity)
# ---------------------------------------------------------------------------

test_that("two boluses superpose: combined run equals the sum of single-bolus runs", {
  # The engine's time grid is built from the dose times (plus each bolus time
  # minus 0.01 and exponential gap-filling). To compare the three runs pointwise
  # we give every run the SAME dose-time skeleton by padding with zero-dose
  # bolus rows: a 0 mcg bolus adds its grid points but no drug, so all three
  # simulations land on an identical timeline (asserted below, not assumed).
  doseA  <- 100  # mcg at t = 0
  doseB  <- 50   # mcg at t = 10
  mkDose <- function(d0, d10) {
    data.frame(Drug = "remifentanil", Time = c(0, 10), Dose = c(d0, d10),
               Units = c("mcg", "mcg"), stringsAsFactors = FALSE)
  }
  resAB <- simCpCe(mkDose(doseA, doseB), events, PK, maximum = 60, plotRecovery = FALSE)
  resA  <- simCpCe(mkDose(doseA, 0),     events, PK, maximum = 60, plotRecovery = FALSE)
  resB  <- simCpCe(mkDose(0, doseB),     events, PK, maximum = 60, plotRecovery = FALSE)

  cpAB <- extractSite(resAB, "Plasma")
  cpA  <- extractSite(resA,  "Plasma")
  cpB  <- extractSite(resB,  "Plasma")

  # Identical grids -- required for an exact pointwise comparison.
  expect_identical(cpA$Time, cpAB$Time)
  expect_identical(cpB$Time, cpAB$Time)

  # Plasma superposition is exact: each lambda state advances by a recursion
  # that is linear in the dose sequence (observed agreement ~1e-15).
  expect_equal_rounded(cpA$Y + cpB$Y, cpAB$Y)

  # Sanity check against the analytic answer as well, so this test cannot pass
  # by all three runs being wrong the same way: Cp_AB(t) = A*f(t) + B*f(t-10)
  # for t >= 10, with f the unit bolus disposition function.
  expected <- doseA * bolusUnitCp(cpAB$Time) +
    ifelse(cpAB$Time >= 10, doseB * bolusUnitCp(pmax(cpAB$Time - 10, 0)), 0)
  expect_equal_rounded(expected, cpAB$Y)

  # Effect-site superposition is only approximate: calculateCe() integrates Ce
  # from Cp using piecewise interpolation that switches between linear and
  # log-linear branches, which is not exactly linear in Cp. Observed deviation
  # is ~2e-4 relative on this scenario; 1e-3 pins the property without
  # overstating the integrator's accuracy.
  ceAB <- extractSite(resAB, "Effect Site")
  ceA  <- extractSite(resA,  "Effect Site")
  ceB  <- extractSite(resB,  "Effect Site")
  expect_equal(ceAB$Y, ceA$Y + ceB$Y, tolerance = 1e-3)
})

# ---------------------------------------------------------------------------
# 3. Constant infusion: steady state Cp_ss = R / cl1
# ---------------------------------------------------------------------------

test_that("a constant infusion approaches Cp_ss = rate / cl1", {
  # 0.5 mcg/kg/min * 70 kg = 35 mcg/min. simCpCe converts the per-kg rate using
  # PK$weight, so the engine sees exactly this rate.
  ratePerKg <- 0.5
  rate <- ratePerKg * covWeight  # mcg/min
  doseInf <- data.frame(Drug = "remifentanil", Time = 0, Dose = ratePerKg,
                        Units = "mcg/kg/min", stringsAsFactors = FALSE)

  # Remifentanil's terminal half-life here is 0.693/lambda_3 ~ 28 min; 600 min
  # is > 21 terminal half-lives, so the remaining approach-to-steady-state
  # transient is exp(-lambda_3*600) * (p_inf_3/sum(p_inf)) ~ 2e-8 relative --
  # far inside the default tolerance.
  expect_true(0.693 / pkSet$lambda_3 < 40)  # guards the "600 min is enough" claim
  resInf <- simCpCe(doseInf, events, PK, maximum = 600, plotRecovery = FALSE)
  cpInf <- extractSite(resInf, "Plasma")

  # Steady-state mass balance: infusion rate in = cl1 * Cp_ss out.
  expect_equal_rounded(rate / pkSet$cl1, cpInf$Y[nrow(cpInf)])

  # While the infusion runs, Cp must rise monotonically toward Cp_ss
  # (all plasma infusion coefficients are positive).
  expect_true(all(diff(cpInf$Y) >= 0))
  expect_true(all(cpInf$Y <= rate / pkSet$cl1 * (1 + 1e-12)))
})

# ---------------------------------------------------------------------------
# 4. Infusion stop: monotone decay
# ---------------------------------------------------------------------------

test_that("after an infusion stops, Cp peaks at the stop time and decays monotonically", {
  # Run 0.5 mcg/kg/min from t = 0, stop at t = 30 (a rate-0 row), watch to 120.
  doseStop <- data.frame(
    Drug = "remifentanil", Time = c(0, 30), Dose = c(0.5, 0),
    Units = c("mcg/kg/min", "mcg/kg/min"), stringsAsFactors = FALSE
  )
  resStop <- simCpCe(doseStop, events, PK, maximum = 120, plotRecovery = FALSE)
  cpStop <- extractSite(resStop, "Plasma")

  # The global maximum is at the stop time: rising before, falling after.
  expect_equal(30, cpStop$Time[which.max(cpStop$Y)])

  # After the stop, Cp is a positive combination of decaying exponentials,
  # so it must fall strictly at every step.
  after <- cpStop[cpStop$Time >= 30, ]
  expect_true(nrow(after) > 10)  # the grid actually samples the decay
  expect_true(all(diff(after$Y) < 0))
})

# ---------------------------------------------------------------------------
# 5. plotRecovery path
# ---------------------------------------------------------------------------

test_that("plotRecovery = TRUE via the exported API errors (pinned quirk)", {
  # Pinned quirk -- see the header block. getDrugPK() never populates a usable
  # emergence threshold (it reads the nonexistent drugDefaults$Emerge; the CSV
  # column is "endCe"), and simCpCe() reads PK$endCe, which getDrugPK() does not
  # return. Both NULLs are asserted here so that fixing EITHER end of the
  # plumbing surfaces in this test and its expectations can be updated
  # deliberately (the error pin below should then become a success path).
  expect_null(PK$emerge)
  expect_null(PK$endCe)

  # recoveryCalc() then evaluates `if (NULL >= sum(state))` and errors.
  # No message match: R condition messages are locale-dependent.
  expect_error(
    simCpCe(singleBolusTable, events, PK, maximum = 60, plotRecovery = TRUE)
  )
})

test_that("the recovery engine itself works when emerge is numeric", {
  # Call advanceClosedForm0() directly, constructing the dose frame exactly as
  # simCpCe() would have after unit conversion (100 mcg bolus, flagged Bolus),
  # and pass the emergence threshold the app would patch in: remifentanil's
  # endCe of 1 ng/mL from drugDefaults_global.csv.
  emergeTarget <- getDrugDefaults("remifentanil")$endCe
  expect_true(is.finite(emergeTarget) && emergeTarget > 0)

  doseConverted <- data.frame(
    Time = 0, Dose = bolusDose, Units = "mcg", Bolus = TRUE,
    PO = FALSE, IM = FALSE, IN = FALSE, stringsAsFactors = FALSE
  )
  r <- NULL
  expect_no_error(
    r <- advanceClosedForm0(doseConverted, pkSet, 60, TRUE, emergeTarget)
  )

  # Recovery must be a well-formed non-negative time everywhere.
  expect_true(all(is.finite(r$Recovery)))
  expect_true(all(r$Recovery >= 0))
  expect_true(max(r$Recovery) > 0)  # the path actually computed something

  # At the instant of the bolus the effect-site state sums to zero (the four
  # e-coefficients cancel by construction), so recovery is 0 by definition ...
  expect_equal(0, r$Recovery[1])
  # ... and by 60 min Ce has decayed far below the 1 ng/mL target
  # (analytic Ce(60) ~ 0.015 ng/mL), so recovery is 0 again.
  expect_equal(0, r$Recovery[nrow(r)])

  # Independent first-principles check: for a single bolus, the effect-site
  # state components at grid time t are exactly D*e_coef_j*exp(-lambda_j*t)
  # (the recursion is exact), so recoveryCalc()'s answer must satisfy
  # Ce_analytic(t + Recovery(t)) = emergeTarget wherever Recovery > 0.
  # recoveryCalc() runs optimize() with tol = 0.1 min; |dCe/dt| near the
  # crossing is ~0.1 ng/mL/min, so the residual is ~0.01 ng/mL; 0.05 gives
  # comfortable headroom without accepting a wrong crossing.
  live <- r$Recovery > 0
  ceAtRecovery <- bolusDose * bolusUnitCe(r$Time[live] + r$Recovery[live])
  expect_true(all(abs(ceAtRecovery - emergeTarget) < 0.05))

  # The app-style call (server-helpers.R patches PK$endCe after getDrugPK)
  # must succeed and agree with the direct engine call above.
  PKapp <- PK
  PKapp$endCe <- emergeTarget
  resApp <- NULL
  expect_no_error(
    resApp <- simCpCe(singleBolusTable, events, PKapp,
                      maximum = 60, plotRecovery = TRUE)
  )
  recApp <- extractSite(resApp, "Recovery")
  expect_identical(r$Time, recApp$Time)
  expect_equal_rounded(r$Recovery, recApp$Y)
})

# ---------------------------------------------------------------------------
# 6. Ce peak lands at tPeak
# ---------------------------------------------------------------------------

test_that("the effect-site peak after a t=0 bolus lands at the grid point nearest tPeak", {
  # ke0 is fitted (getDrugPK -> tPeakError -> optimize) precisely so that the
  # analytic Ce after a bolus peaks at tPeak (1.6 min for remifentanil). The
  # simulation reports Ce on a discrete exponentially-spaced grid, so the
  # observed argmax can only be resolved to the local grid spacing (~0.3-0.4
  # min around t = 1.6). Assert the grid argmax is interior and within one
  # local grid step of tPeak.
  ce <- extractSite(singleBolusRun, "Effect Site")
  i <- which.max(ce$Y)
  expect_true(i > 1 && i < nrow(ce))              # a genuine interior peak
  localStep <- max(ce$Time[i] - ce$Time[i - 1],   # grid resolution at the peak
                   ce$Time[i + 1] - ce$Time[i])
  expect_true(abs(ce$Time[i] - PK$tPeak) <= localStep)

  # And the peak value is sane: below the initial plasma peak (effect site
  # never overshoots plasma for a bolus) and above the emergence threshold.
  expect_true(ce$Y[i] < bolusDose / pkSet$v1)
  expect_true(ce$Y[i] > 1)
})
