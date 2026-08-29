# test-simCpCe-units.R -------------------------------------------------------
#
# What this file tests: the front half of simCpCe() (R/simCpCe.R) -- the part
# that turns a user-facing dose table into the base units the closed-form
# engines expect, classifies each dose by route, dispatches to the right
# engine, and post-processes the engine output (normalization columns, the
# equiSpace table, MEAC scaling, and the max table).  The engines themselves
# (advanceClosedForm0 / advanceClosedFormPO_IM_IN / advanceClosedForm1) are
# only exercised as far as needed to prove the conversion and dispatch were
# correct.  The existing test-simCpCe.R covers only a zero-dose remifentanil
# case; this file adds non-zero doses across units and routes.
#
# Unit-conversion contract being tested (from the switch at the top of
# simCpCe):  concentrations are amount / volume(L), reported in the drug's
# Concentration.Units per ml.  So:
#   Concentration.Units == "mcg"  (mcg/ml == mg/L)  -> base dose unit is mg
#     ("mg" doses /1, "mcg" doses /1000, "ng" doses /1e6)
#   Concentration.Units == "ng"   (ng/ml == mcg/L)  -> base dose unit is mcg
#     ("mg" doses /0.001, "mcg" doses /1, "ng" doses /1000)
# then "kg" in the unit string multiplies by PK$weight, and "hr" divides by
# 60 (per-hour rate -> per-minute rate).  Route classification is grepl-based:
# a dose is a Bolus unless its Units contain "min", "hr", "PO", "IM", or "IN".
#
# KNOWN LIMITATIONS / pinned quirks (each marked at its assertion):
#  1. equiSpace is built with Time listed twice in the data.frame() call, so
#     its names are Drug, Time, Ce, Time.1, Recovery, MEAC.  Already visible
#     in test-simCpCe.R; pinned again here deliberately.
#  2. advanceClosedFormPO_IM_IN() unconditionally prints debug output
#     ("Structure of pkSet" + str(pkSet)) via cat/print.  This file uses that
#     output as an observable marker of which engine simCpCe dispatched to.
#     If those debug lines are removed (a reasonable cleanup -- repo style
#     says use outputComments()), update the dispatch test to use another
#     marker (e.g. tracing) rather than deleting it.
#  3. A dose whose Units name a route the drug has no absorption PK for
#     (e.g. oxycodone "mg IM": ka_IM == 0) silently simulates to all-zero
#     concentrations -- the dose disappears without warning.  The UI's
#     per-drug unit lists prevent this today, but simCpCe itself does not
#     validate.  Pinned so a future validation layer knowingly updates it.
#  4. The PO engine can return a tiny negative Plasma value at t == 0
#     (~ -4e-15, floating-point roundoff).  Only equiSpace$Ce[1] is clamped
#     to zero; results$Plasma is not.  We assert |Cp(0)| < 1e-9 rather than
#     == 0.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (simCpCe unit conversion and dispatch).  Expected
# values derived by hand unit algebra (the equivalences), and independently
# from the closed-form sums of exponentials: Cp(t) reconstructed directly
# from the getDrugPK() bolus/PO coefficients, peak concentration = dose/v1
# for an IV bolus at t = 0, and the analytic PO tmax found by optimizing the
# closed form.  Equivalence tests compare two dose tables that must convert
# to the identical internal dose, so they never depend on engine internals.
# Verified against the working tree (R 4.6.1) on 2026-08-14.
# ----------------------------------------------------------------------------

# --- Shared fixtures --------------------------------------------------------
# getDrugPK is deterministic (the only numeric search, stats::optimize for
# ke0, is deterministic), so building these once at source time is safe.

emptyEvents <- data.frame(Time = double(), Event = character(), Fill = character())

# Minimal dose-table constructor matching the columns simCpCe consumes.
mkDose <- function(drug, time, dose, units) {
  data.frame(Drug = drug, Time = time, Dose = dose, Units = units,
             stringsAsFactors = FALSE)
}

# Run simCpCe while capturing anything printed to stdout.  The PO/IM/IN
# engine currently prints debug output (pinned quirk 2); capturing keeps the
# test log clean AND gives us the dispatch marker.
runSim <- function(dose, PK, maximum = 60) {
  out <- capture.output(sim <- simCpCe(dose, emptyEvents, PK, maximum, FALSE))
  list(sim = sim, printed = out)
}

# Convenience: pull one Site's series out of the gathered results table.
siteY <- function(sim, site) sim$results[sim$results$Site == site, "Y"]
siteT <- function(sim, site) sim$results[sim$results$Site == site, "Time"]

covWeight <- 70  # kg; used by the per-kg conversion (PK$weight)
pkProp <- getDrugPK("propofol",  covWeight, 170, 50, "male", getDrugDefaults("propofol"))
pkFent <- getDrugPK("fentanyl",  covWeight, 170, 50, "male", getDrugDefaults("fentanyl"))
pkOxy  <- getDrugPK("oxycodone", covWeight, 170, 50, "male", getDrugDefaults("oxycodone"))

test_that("fixture drugs carry the units/MEAC this file assumes", {
  # Guard assertions: if drugDefaults_global.csv changes these, the tests
  # below are testing something different and should be revisited.
  expect_identical(pkProp$Concentration.Units, "mcg")  # base dose unit: mg
  expect_identical(pkFent$Concentration.Units, "ng")   # base dose unit: mcg
  expect_identical(pkOxy$Concentration.Units,  "ng")
  expect_identical(pkProp$MEAC, 0)    # propofol has no MEAC -> MEAC column 0
  expect_identical(pkFent$MEAC, 0.6)  # ng/ml
  expect_identical(pkProp$weight, covWeight)
  expect_gt(pkOxy$PK$default$ka_PO, 0)   # oxycodone has oral PK ...
  expect_identical(pkOxy$PK$default$ka_IM, 0)  # ... but no IM PK (quirk 3)
})

# --- 1. Mass-unit equivalence ----------------------------------------------

test_that("mg and mcg doses of the same drug convert to the same base amount", {
  # propofol (mcg/ml == mg/L): 10 mg and 10000 mcg are the same dose, so the
  # entire return value (results, equiSpace, max) must match.
  a <- runSim(mkDose("propofol", 0, 10,    "mg"),  pkProp)$sim
  b <- runSim(mkDose("propofol", 0, 10000, "mcg"), pkProp)$sim
  expect_equal(a, b)

  # fentanyl (ng/ml == mcg/L): 0.1 mg == 100 mcg.
  f1 <- runSim(mkDose("fentanyl", 0, 0.1, "mg"),  pkFent)$sim
  f2 <- runSim(mkDose("fentanyl", 0, 100, "mcg"), pkFent)$sim
  expect_equal(f1, f2)

  # Absolute anchor (first principles, not relational): an IV bolus at t = 0
  # peaks at t = 0 with Cp = dose / v1.  100 mcg into v1 liters gives
  # 100/v1 mcg/L == 100/v1 ng/ml.  This proves "100 mcg" became the internal
  # amount 100 (mcg), not merely that mg and mcg agree with each other.
  expect_equal_rounded(100 / pkFent$PK$default$v1, f1$max$Cp)
})

test_that("a converted bolus reproduces the closed-form sum of exponentials", {
  # Independent reconstruction: for a single IV bolus D at t = 0 with no
  # other input, the closed form is
  #   Cp(t) = D * (A1 e^(-lambda_1 t) + A2 e^(-lambda_2 t) + A3 e^(-lambda_3 t))
  # with the Ai and lambdas taken from getDrugPK (which is tested elsewhere).
  # Matching this at every reported time verifies both the absolute dose
  # amount after unit conversion (D = 10 mg for propofol) and that the
  # engine's stepwise state advance did not distort the profile.
  sim <- runSim(mkDose("propofol", 0, 10, "mg"), pkProp)$sim
  pk  <- pkProp$PK$default
  tt  <- siteT(sim, "Plasma")
  expCp <- 10 * (pk$p_coef_bolus_l1 * exp(-pk$lambda_1 * tt) +
                 pk$p_coef_bolus_l2 * exp(-pk$lambda_2 * tt) +
                 pk$p_coef_bolus_l3 * exp(-pk$lambda_3 * tt))
  # Observed agreement is ~4e-16 relative; 1.5e-6 (helper default) is ample.
  expect_equal_rounded(expCp, siteY(sim, "Plasma"))
})

# --- 2. Per-kg and per-hour conversions ------------------------------------

test_that("per-kg doses scale by PK$weight", {
  # 1 mg/kg at 70 kg is exactly a 70 mg bolus ("mg/kg" has no min/hr/PO/IM/IN
  # so it stays classified as a bolus).
  a <- runSim(mkDose("propofol", 0, 1,  "mg/kg"), pkProp)$sim
  b <- runSim(mkDose("propofol", 0, 70, "mg"),    pkProp)$sim
  expect_equal(a, b)
})

test_that("per-hour infusion rates equal the per-minute rate divided by 60", {
  # fentanyl: 60 mcg/kg/hr == 1 mcg/kg/min (60/60 = 1, weight cancels).
  # Note mcg/kg/min is not in fentanyl's UI unit list, but the conversion
  # logic is unit-string generic, which is what we are testing.
  h1 <- runSim(mkDose("fentanyl", 0, 60, "mcg/kg/hr"),  pkFent)$sim
  h2 <- runSim(mkDose("fentanyl", 0, 1,  "mcg/kg/min"), pkFent)$sim
  expect_equal(h1, h2)

  # propofol, using two units both offered in its UI list, crossing the mass
  # conversion and the hr conversion at once:
  #   60 mg/kg/hr  -> 60 * 70 / 60      = 70 mg/min
  #   1000 mcg/kg/min -> 1000/1000 * 70 = 70 mg/min
  h3 <- runSim(mkDose("propofol", 0, 60,   "mg/kg/hr"),   pkProp)$sim
  h4 <- runSim(mkDose("propofol", 0, 1000, "mcg/kg/min"), pkProp)$sim
  expect_equal(h3, h4)
})

# --- 3. Route classification and engine dispatch ---------------------------

test_that("PO doses dispatch to the PO engine, IV doses to advanceClosedForm0", {
  iv <- runSim(mkDose("oxycodone", 0, 10, "mg"),    pkOxy, maximum = 240)
  po <- runSim(mkDose("oxycodone", 0, 10, "mg PO"), pkOxy, maximum = 240)

  # Dispatch marker (pinned quirk 2): only advanceClosedFormPO_IM_IN prints
  # "Structure of pkSet".  Its presence/absence tells us which engine ran.
  expect_true(any(grepl("Structure of pkSet", po$printed)))
  expect_false(any(grepl("Structure of pkSet", iv$printed)))

  ivT <- siteT(iv$sim, "Plasma"); ivY <- siteY(iv$sim, "Plasma")
  poT <- siteT(po$sim, "Plasma"); poY <- siteY(po$sim, "Plasma")

  # IV bolus: peak at t = 0, height dose/v1 (10 mg -> 10000 mcg base units,
  # so Cp(0) = 10000/v1 ng/ml).  First-principles anchor for the PO
  # comparisons below.
  expect_identical(ivT[which.max(ivY)], 0)
  expect_equal_rounded(10000 / pkOxy$PK$default$v1, max(ivY))

  # PO: nothing in plasma at t = 0 (absorption has not started).  Pinned
  # quirk 4: the engine returns ~ -4e-15 rather than exactly 0, so bound it.
  expect_lt(abs(poY[poT == 0][1]), 1e-9)

  # PO peak is delayed and attenuated: first-pass bioavailability is 0.5 and
  # absorption (ka_PO = 0.06/min) is slow, so the oral peak must be well
  # under half the IV peak (observed ~27.8 vs ~110.9 ng/ml).
  expect_lt(max(poY), 0.5 * max(ivY))

  # The engine reports its peak on a discrete timeline, so compare the
  # observed argmax against the analytic tmax found by optimizing the PO
  # closed form directly (independent of the engine).  The timeline is
  # coarse near 30 min, hence the 5-minute window (observed gap ~1.1 min).
  pk <- pkOxy$PK$default
  analyticTmax <- stats::optimize(
    function(t) -(pk$p_coef_PO_l1 * exp(-pk$lambda_1 * t) +
                  pk$p_coef_PO_l2 * exp(-pk$lambda_2 * t) +
                  pk$p_coef_PO_l3 * exp(-pk$lambda_3 * t) +
                  pk$p_coef_PO_ka * exp(-pk$ka_PO    * t)),
    c(0, 240))$minimum
  expect_lt(abs(poT[which.max(poY)] - analyticTmax), 5)

  # Strongest dispatch proof: the whole PO plasma curve equals the PO closed
  # form D * (sum of PO coefficients times exponentials), D = 10/0.001 =
  # 10000 mcg.  Only the PO engine applies these coefficients, so agreement
  # here (observed ~4e-16 relative) is conclusive.
  expPO <- 10000 * (pk$p_coef_PO_l1 * exp(-pk$lambda_1 * poT) +
                    pk$p_coef_PO_l2 * exp(-pk$lambda_2 * poT) +
                    pk$p_coef_PO_l3 * exp(-pk$lambda_3 * poT) +
                    pk$p_coef_PO_ka * exp(-pk$ka_PO    * poT))
  expect_equal_rounded(expPO, poY)
})

test_that("a route the drug has no PK for silently yields zero (pinned quirk)", {
  # Pinned quirk 3: oxycodone has no IM absorption PK (ka_IM == 0), and its
  # UI unit list offers only "mg PO".  Feeding "mg IM" through simCpCe
  # anyway classifies the dose as IM and the dose then vanishes -- every
  # concentration is zero, with no warning.  If simCpCe (or a caller) gains
  # route validation, this test should be updated to expect that error.
  im <- runSim(mkDose("oxycodone", 0, 10, "mg IM"), pkOxy, maximum = 240)$sim
  expect_identical(im$max$Cp, 0)
  expect_identical(im$max$Ce, 0)
})

# --- 4. Normalization columns ----------------------------------------------

test_that("normalized series are percent-of-maximum and peak at exactly 100", {
  sim <- runSim(mkDose("propofol", 0, 10, "mg"), pkProp)$sim
  P <- siteY(sim, "Plasma")
  E <- siteY(sim, "Effect Site")

  # With plotRecovery = FALSE the gathered results contain exactly these six
  # series (the MEAC series lives in equiSpace, not here).
  expect_setequal(unique(sim$results$Site),
                  c("Plasma", "Effect Site",
                    "CpNormCp", "CeNormCp", "CpNormCe", "CeNormCe"))

  # Self-normalized series peak at exactly 100 by construction (x/max(x)*100
  # hits 100 where x == max(x), with no floating-point division residue
  # worth a tolerance -- max(x)/max(x) is exactly 1).
  expect_identical(max(siteY(sim, "CpNormCp")), 100)
  expect_identical(max(siteY(sim, "CeNormCe")), 100)

  # All four columns are the documented ratios of the raw series.
  expect_equal(siteY(sim, "CpNormCp"), P / max(P) * 100)
  expect_equal(siteY(sim, "CeNormCp"), E / max(P) * 100)
  expect_equal(siteY(sim, "CpNormCe"), P / max(E) * 100)
  expect_equal(siteY(sim, "CeNormCe"), E / max(E) * 100)

  # Cross-normalized Ce/maxCp stays below 100 for a bolus: the effect-site
  # peak is always lower than the plasma peak (Ce lags and is damped).
  expect_lt(max(siteY(sim, "CeNormCp")), 100)
})

test_that("an all-zero dose table produces zero normalized columns, not NaN", {
  # maxCp == 0 would make x/max(x) = 0/0 = NaN; simCpCe special-cases this to
  # hard zeros.  Guard that branch.
  sim <- runSim(mkDose("propofol", 0, 0, "mg"), pkProp)$sim
  normY <- sim$results$Y[sim$results$Site %in%
                           c("CpNormCp", "CeNormCp", "CpNormCe", "CeNormCe")]
  expect_true(all(normY == 0))
  expect_false(anyNA(sim$results$Y))
  expect_identical(sim$max$Cp, 0)
  expect_identical(sim$max$Ce, 0)
  expect_true(all(sim$equiSpace$Ce == 0))
})

# --- 5. MEAC scaling --------------------------------------------------------

test_that("equiSpace$MEAC is Ce as a percent of MEAC; zero when MEAC is zero", {
  # fentanyl: MEAC = 0.6 ng/ml, so MEAC column = Ce / 0.6 * 100 (percent of
  # the minimum effective analgesic concentration).
  f <- runSim(mkDose("fentanyl", 0, 100, "mcg"), pkFent)$sim
  expect_equal(f$equiSpace$MEAC, f$equiSpace$Ce / 0.6 * 100)
  expect_gt(max(f$equiSpace$MEAC), 0)  # non-degenerate: dose was non-zero

  # propofol: MEAC = 0 -> the column is hard zero (not Ce/0 = Inf).
  p <- runSim(mkDose("propofol", 0, 10, "mg"), pkProp)$sim
  expect_true(all(p$equiSpace$MEAC == 0))
})

# --- 6. equiSpace structure and the max table ------------------------------

test_that("equiSpace has RESOLUTION rows on an even grid spanning 0..maximum", {
  sim <- runSim(mkDose("propofol", 0, 10, "mg"), pkProp, maximum = 60)$sim
  eq <- sim$equiSpace

  # RESOLUTION is the package-internal grid size (100, R/globalVariables.R).
  # Note RESOLUTION is stored as a double there, while nrow() returns an
  # integer, so this must be expect_equal, not expect_identical.
  expect_equal(nrow(eq), RESOLUTION)
  expect_equal(eq$Time, seq(from = 0, to = 60, length.out = RESOLUTION))

  # Pinned quirk 1: Time appears twice in the data.frame() constructor, so
  # data.frame() deduplicates the second to "Time.1".  Both carry the grid.
  # Fixing simCpCe to emit a single Time column should update this test.
  expect_identical(names(eq),
                   c("Drug", "Time", "Ce", "Time.1", "Recovery", "MEAC"))
  expect_identical(eq$Time.1, eq$Time)

  # simCpCe clamps the first interpolated Ce to exactly 0 (approx() can
  # produce a tiny negative there).
  expect_identical(eq$Ce[1], 0)

  # With plotRecovery = FALSE the Recovery column is all zeros.
  expect_true(all(eq$Recovery == 0))

  # Interpolation sanity: away from the clamped first point, equiSpace$Ce
  # must lie on the results curve.  Check the last grid point, which is an
  # exact node of the simulation timeline (t == maximum).
  E <- siteY(sim, "Effect Site"); tt <- siteT(sim, "Effect Site")
  expect_equal(eq$Ce[RESOLUTION], E[tt == 60][1])
})

test_that("the max table reports the maxima of the simulated series", {
  sim <- runSim(mkDose("fentanyl", 0, 100, "mcg"), pkFent)$sim
  expect_identical(sim$max$Drug, "fentanyl")
  expect_equal(sim$max$Cp, max(siteY(sim, "Plasma")))
  expect_equal(sim$max$Ce, max(siteY(sim, "Effect Site")))
  expect_identical(sim$max$Recovery, 0)  # plotRecovery = FALSE
})
