# ---------------------------------------------------------------------------
# test-getDrugPK-invariants.R
#
# WHAT THIS FILE TESTS
#
# `getDrugPK()` is the covariate -> parameter compiler for the whole engine.
# It calls the per-drug model function, forms the micro rate constants
# (k10 = cl1/v1, k12 = cl2/v1, k21 = cl2/v2, k13 = cl3/v1, k31 = cl3/v3),
# hands those to `cube()` for the three eigenvalues of the mammillary
# rate matrix, fits `ke0` from `tPeak` by `stats::optimize(tPeakError, ...)`,
# and finally emits the closed-form coefficients that `simCpCe()` and the
# `advanceClosedForm*()` family consume.
#
# The existing `test-getDrugPK.R` is a golden master: one drug (propofol) at
# one covariate set, ~66 hard-coded numbers. That catches a change in
# propofol and nothing else. This file is the complement: it asserts the
# *structural and physical invariants* that must hold for EVERY drug in
# `inst/extdata/drugDefaults_global.csv`, at more than one covariate set, and
# derives every expected value from the pharmacokinetic algebra rather than
# from what the code happened to print.
#
# The invariants, and where each comes from:
#
#   1. Micro constants are exactly the cl/v ratios that define them.
#   2. The three lambdas are the roots of the characteristic polynomial of
#      the rate matrix, so their elementary symmetric functions equal that
#      polynomial's coefficients (Vieta). They are real, ordered, and
#      non-negative.
#   3. Sum of the bolus plasma coefficients = 1/v1, because Cp(0) = dose/V1.
#      Sum of the infusion plasma coefficients = 1/cl1, because at steady
#      state input rate = CL1 x Css.
#   4. The four effect-site bolus coefficients sum to zero, because
#      Ce(0) = 0. The effect-site infusion coefficients sum to 1/cl1,
#      because Ce = Cp at steady state.
#   5. Every emitted number is finite; volumes and clearances are physical.
#   6. ke0 > 0 whenever tPeak > 0, and the fitted ke0 really does put the
#      effect-site maximum at tPeak.
#   7. A drug not in the CSV allowlist is rejected.
#   8. cl1 follows the weight-scaling law the published model actually
#      implements (per-kg, 3/4-power allometric, or weight-independent).
#   9. For the extravascular routes, C(0) = 0 and AUC = F/cl1.
#
# KNOWN LIMITATION - remifentanil and the BMI 30 model switch.
# `drugs_remifentanil.R` deliberately switches from the Eleveld model to the
# Kim model at BMI 30 ("# NIH Obesity cutoff"). That makes several returned
# parameters discontinuous in weight: at height 170 cm, age 50, male, going
# from 86 kg to 87 kg drops v1 by 11.6% and v2 by 45.8%. It is a modelling
# decision rather than a defect, but it means no cross-BMI-30 monotonicity
# claim can be made, so the monotonicity check for remifentanil is confined
# to weights that stay inside the Eleveld branch. See the findings note in
# the accompanying pull request.
#
# KNOWN LIMITATION - dexmedetomidine at age exactly 0.
# The infant model's post-bypass event scales clearance by a postnatal
# maturation term, cl1 <- 623 * (weight/70)^0.75 * (age*365)/(1.77 + age*365)
# / 1000 (R/drugs_dexmedetomidine.R). At age 0 - which the app permits, since
# MIN_AGE is 0 - that numerator is zero, so getDrugPK() emits cl1 = 0, k10 = 0
# and lambda_2 = 0 for the CPBEnd event: a drug that never leaves the body.
# The steady-state identities in sections 3 and 4 are then vacuous (1/cl1 is
# infinite) and the cl1 > 0 check in section 5 would fail. This file therefore
# uses age 0.5 for the infant covariate set, exercising the multi-event branch
# without standing on the degenerate point. If the maturation term is given a
# floor, widen the infant covariate set down to age 0.
#
# KNOWN LIMITATION - the `emerge` field is not asserted.
# getDrugPK() returns `emerge = drugDefaults$Emerge`, but the defaults CSV has
# no `Emerge` column, so `emerge` is NULL for every drug and is handed to the
# advanceClosedForm*() functions as their `emerge` argument. Pinning NULL here
# would cement what looks like a column-name slip, so this file asserts
# nothing about it; the field is reported separately.
#
# KNOWN LIMITATION - the reference / citation strings are deliberately NOT
# asserted anywhere in this file. They are prose, they are being revised on a
# separate branch, and pinning them would make this file a merge hazard.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (getDrugPK structural and physical invariants).
# Expected values derived from first principles - the closed-form solution of
# the three-compartment mammillary model with an effect site, Vieta's
# relations for the characteristic cubic, and the weight-scaling exponents
# read directly out of each `R/drugs_*.R` model definition - not from the
# current output of the code. Numerical tolerances were calibrated by
# measuring the actual residual of each identity across every drug and every
# covariate set used here, then leaving roughly three orders of magnitude of
# head-room; the measured maxima are quoted beside each tolerance.
# ---------------------------------------------------------------------------


## --------------------------------------------------------------------------
## Fixtures
## --------------------------------------------------------------------------

# The drug allowlist is the CSV's Drug column - the same list getDrugPK()
# validates against - so a newly added drug is swept by every invariant below
# without touching this file. The one place a new drug does need a line is the
# weight-scaling partition in section 8, and that is on purpose.
drugNames <- getDrugDefaultsGlobal()$Drug

# Two adult covariate sets that differ in every covariate. Only two, because
# getDrugPK() runs a nested stats::optimize() per PK event and there is no
# value in a combinatorial sweep: the invariants below are exact identities,
# not curve fits.
covariateSets <- list(
  standardAdult = list(weight =  70, height = 170, age = 50, sex = "male"),
  largerYounger = list(weight = 100, height = 185, age = 25, sex = "female")
)

# A few drug model functions cat() progress lines; capture them so the suite
# log stays clean.
buildPK <- function(drug, cv) {
  result <- NULL
  utils::capture.output(
    result <- getDrugPK(
      drug, cv$weight, cv$height, cv$age, cv$sex, getDrugDefaults(drug)
    )
  )
  result
}

# Flatten everything into one list of (drug, covariate set, PK event) blocks.
# A "PK event" is one entry of X$PK: most drugs return a single "default"
# event, but dexmedetomidine's infant model returns nine (default, the
# cardiopulmonary-bypass temperature steps, and CPBEnd). Including the infant
# dexmedetomidine covariates is what exercises getDrugPK()'s `for (event in
# events)` loop with more than one pass - otherwise that loop is never tested.
pkBlocks <- list()
for (csName in names(covariateSets)) {
  for (drug in drugNames) {
    pk <- buildPK(drug, covariateSets[[csName]])
    for (ev in pk$pkEvents) {
      pkBlocks[[paste(drug, csName, ev, sep = "/")]] <-
        list(p = pk$PK[[ev]], tPeak = pk$tPeak,
             drug = drug, covariateSet = csName, event = ev)
    }
  }
}
# Age 0.5, not 0: see the dexmedetomidine KNOWN LIMITATION in the header - at
# age exactly 0 the post-bypass event's clearance collapses to zero and the
# model becomes degenerate.
infantCovariates <- list(weight = 8, height = 70, age = 0.5, sex = "female")
pkInfantDex <- buildPK("dexmedetomidine", infantCovariates)
for (ev in pkInfantDex$pkEvents) {
  pkBlocks[[paste("dexmedetomidine", "infant", ev, sep = "/")]] <-
    list(p = pkInfantDex$PK[[ev]], tPeak = pkInfantDex$tPeak,
         drug = "dexmedetomidine", covariateSet = "infant", event = ev)
}

# Unit-bolus effect-site concentration and its time derivative, written out
# from the closed form Ce(t) = sum_i e_i exp(-lambda_i t) + e_ke0 exp(-ke0 t).
# Used only to locate the effect-site peak independently of tPeakError().
ceBolus <- function(p, t) {
  p$e_coef_bolus_l1  * exp(-p$lambda_1 * t) +
  p$e_coef_bolus_l2  * exp(-p$lambda_2 * t) +
  p$e_coef_bolus_l3  * exp(-p$lambda_3 * t) +
  p$e_coef_bolus_ke0 * exp(-p$ke0      * t)
}
dCeBolus <- function(p, t) {
  -p$lambda_1 * p$e_coef_bolus_l1  * exp(-p$lambda_1 * t) -
   p$lambda_2 * p$e_coef_bolus_l2  * exp(-p$lambda_2 * t) -
   p$lambda_3 * p$e_coef_bolus_l3  * exp(-p$lambda_3 * t) -
   p$ke0      * p$e_coef_bolus_ke0 * exp(-p$ke0      * t)
}


## --------------------------------------------------------------------------
## 0. The block set really does cover what it claims to cover
## --------------------------------------------------------------------------

test_that("the invariant sweep covers every drug and a multi-event model", {
  # Every drug in the allowlist must have contributed at least one PK event
  # block at each of the two adult covariate sets, plus the infant
  # dexmedetomidine model. Written as a set comparison rather than a count so
  # that adding a drug, or giving an existing drug extra PK events, does not
  # produce a spurious failure here. (What *does* force attention when a drug
  # is added is the weight-scaling partition in section 8, which is
  # deliberate: a new drug has to be classified.)
  expectedCoverage <- c(
    as.vector(outer(drugNames, names(covariateSets), paste, sep = "/")),
    "dexmedetomidine/infant"
  )
  actualCoverage <- unique(vapply(
    pkBlocks, function(b) paste(b$drug, b$covariateSet, sep = "/"), character(1)
  ))
  expect_equal(sort(actualCoverage), sort(expectedCoverage))

  # The multi-event branch is the one that would silently stop being tested
  # if the infant model were restructured, so assert it explicitly.
  expect_equal(
    pkInfantDex$pkEvents,
    c("default", "CPBStart", "CPB36", "CPB35", "CPB34",
      "CPB33", "CPB32", "CPB31", "CPBEnd")
  )
  # $PK must be keyed by exactly the event names getDrugPK() advertises.
  expect_equal(names(pkInfantDex$PK), pkInfantDex$pkEvents)
})

test_that("getDrugPK echoes the covariates it was called with", {
  for (csName in names(covariateSets)) {
    cv <- covariateSets[[csName]]
    pk <- buildPK("propofol", cv)
    expect_identical(pk$drug,   "propofol", label = paste(csName, "drug"))
    expect_identical(pk$weight, cv$weight,  label = paste(csName, "weight"))
    expect_identical(pk$height, cv$height,  label = paste(csName, "height"))
    expect_identical(pk$age,    cv$age,     label = paste(csName, "age"))
    expect_identical(pk$sex,    cv$sex,     label = paste(csName, "sex"))
    # tPeak is passed straight through from the drug model function.
    expect_identical(pk$tPeak, propofol(cv$weight, cv$height, cv$age, cv$sex)$tPeak,
                     label = paste(csName, "tPeak"))
  }
})


## --------------------------------------------------------------------------
## 1. Micro rate constants
## --------------------------------------------------------------------------

test_that("micro rate constants are exactly the cl/v ratios that define them", {
  # k10 = CL1/V1, k12 = CL2/V1, k13 = CL3/V1, k21 = CL2/V2, k31 = CL3/V3 is
  # the definition of a mammillary model's inter-compartmental rate constants
  # from its clearance/volume parameterisation. getDrugPK() returns both the
  # volumes/clearances and the rate constants, so the two must agree - and
  # since both come from the same double-precision divisions, they must agree
  # bit for bit, not merely to a tolerance. `expect_identical` is therefore
  # the right strength here: any drift means a genuine inconsistency in what
  # the function reports, not a rounding artefact.
  for (key in names(pkBlocks)) {
    p <- pkBlocks[[key]]$p
    expect_identical(p$k10, p$cl1 / p$v1, label = paste(key, "k10"))
    expect_identical(p$k12, p$cl2 / p$v1, label = paste(key, "k12"))
    expect_identical(p$k13, p$cl3 / p$v1, label = paste(key, "k13"))
    expect_identical(p$k21, p$cl2 / p$v2, label = paste(key, "k21"))
    expect_identical(p$k31, p$cl3 / p$v3, label = paste(key, "k31"))
  }
})


## --------------------------------------------------------------------------
## 2. Eigenvalue invariants
## --------------------------------------------------------------------------

test_that("the lambdas are real, ordered, non-negative roots of the characteristic cubic", {
  # `cube()` returns the three exponential rate constants of the mammillary
  # model, i.e. the negated eigenvalues of the rate matrix. They are the
  # roots of
  #
  #     lambda^3 - a2 lambda^2 + a1 lambda - a0 = 0
  # with
  #     a2 = k10 + k12 + k13 + k21 + k31
  #     a1 = k10 k31 + k21 k31 + k21 k13 + k10 k21 + k31 k12
  #     a0 = k10 k21 k31
  #
  # so Vieta's relations give the three elementary symmetric functions of the
  # lambdas directly. This is the same algebra cube() solves, but approached
  # from the opposite end: cube() goes coefficients -> trigonometric form ->
  # roots, and this check goes roots -> coefficients, so a sign slip or a
  # mis-transcribed a1 term cannot satisfy both.
  #
  # The two-compartment drugs (cl3 = 0, hence k13 = k31 = 0) are covered by
  # exactly the same three relations: a1 collapses to k10 k21, a0 to 0, and
  # the returned lambda_3 = 0 satisfies them.
  #
  # Residuals are normalised by a2^k, the natural scale for the k-th
  # elementary symmetric function of rate constants that sum to a2. Measured
  # maximum across all blocks: 4.9e-16, 4.0e-16, 2.7e-17.
  vietaTol <- 1e-12

  for (key in names(pkBlocks)) {
    p <- pkBlocks[[key]]$p
    lambdas <- c(p$lambda_1, p$lambda_2, p$lambda_3)

    # Real and finite. cube() clamps the cosine argument to [-1, 1], so a
    # NaN here would mean the clamp was removed or the discriminant went bad.
    expect_true(all(is.finite(lambdas)), label = paste(key, "lambdas finite"))

    # Sorted descending: downstream code assumes lambda_1 is the fastest.
    expect_gte(lambdas[1], lambdas[2], label = paste(key, "lambda_1 >= lambda_2"))
    expect_gte(lambdas[2], lambdas[3], label = paste(key, "lambda_2 >= lambda_3"))

    # Non-negative: a negative rate constant would mean a concentration term
    # that grows without bound.
    expect_gte(lambdas[3], 0, label = paste(key, "lambda_3 >= 0"))

    # Every drug in the library is at least two-compartment, so the two
    # fastest rate constants are strictly positive.
    expect_gt(lambdas[1], 0, label = paste(key, "lambda_1 > 0"))
    expect_gt(lambdas[2], 0, label = paste(key, "lambda_2 > 0"))

    # A third exponential exists if and only if there is a third compartment.
    expect_identical(lambdas[3] > 0, p$k31 > 0,
                     label = paste(key, "third exponential iff k31 > 0"))

    a2 <- p$k10 + p$k12 + p$k13 + p$k21 + p$k31
    a1 <- p$k10 * p$k31 + p$k21 * p$k31 + p$k21 * p$k13 +
          p$k10 * p$k21 + p$k31 * p$k12
    a0 <- p$k10 * p$k21 * p$k31

    expect_lt(abs(sum(lambdas) - a2) / a2, vietaTol,
              label = paste(key, "Vieta e1"))
    expect_lt(abs(lambdas[1] * lambdas[2] + lambdas[1] * lambdas[3] +
                  lambdas[2] * lambdas[3] - a1) / a2 ^ 2, vietaTol,
              label = paste(key, "Vieta e2"))
    expect_lt(abs(prod(lambdas) - a0) / a2 ^ 3, vietaTol,
              label = paste(key, "Vieta e3"))
  }
})


## --------------------------------------------------------------------------
## 3. Plasma coefficient identities
## --------------------------------------------------------------------------

test_that("bolus plasma coefficients sum to 1/v1 and infusion coefficients to 1/cl1", {
  # BOLUS. For a unit bolus into the central compartment,
  #     Cp(t) = A1 exp(-l1 t) + A2 exp(-l2 t) + A3 exp(-l3 t).
  # At t = 0 none of the dose has left V1 yet, so Cp(0) = dose/V1 = 1/V1, and
  # setting t = 0 in the sum gives A1 + A2 + A3 = 1/V1.
  #
  # The same result falls out of the algebra getDrugPK() actually uses:
  #     A_i = (k21 - l_i)(k31 - l_i) / [ prod_{j != i} (l_i - l_j) ] / V1,
  # and for distinct nodes the Lagrange/divided-difference identity
  #     sum_i P(l_i) / prod_{j != i} (l_i - l_j) = leading coefficient of P
  # holds for any polynomial P of degree n-1 = 2. Here P(x) = (k21-x)(k31-x)
  # has leading coefficient 1, so the sum is exactly 1/V1. Measured maximum
  # residual across all blocks: 3.5e-16.
  #
  # INFUSION. p_coef_infusion_i = A_i / l_i, and the closed form for a unit
  # infusion rate is Cp(t) = sum_i (A_i / l_i) (1 - exp(-l_i t)), so the
  # coefficients sum to Cp(infinity) = Css. Mass balance at steady state
  # gives rate in = rate out = CL1 x Css, hence Css = 1/CL1 for unit rate.
  # (Equivalently, sum_i A_i/l_i is the AUC of a unit bolus, = dose/CL.)
  # Dividing by the lambdas amplifies cube()'s trigonometric round-off - the
  # worst case is methadone, whose slowest lambda is 7e-5/min - so this one
  # gets a looser bound. Measured maximum residual: 1.2e-12.
  bolusTol    <- 1e-12
  infusionTol <- 1e-9

  for (key in names(pkBlocks)) {
    p <- pkBlocks[[key]]$p

    bolusSum <- p$p_coef_bolus_l1 + p$p_coef_bolus_l2 + p$p_coef_bolus_l3
    expect_lt(abs(bolusSum - 1 / p$v1) * p$v1, bolusTol,
              label = paste(key, "sum of bolus plasma coefficients = 1/v1"))

    infusionSum <- p$p_coef_infusion_l1 + p$p_coef_infusion_l2 +
                   p$p_coef_infusion_l3
    expect_lt(abs(infusionSum - 1 / p$cl1) * p$cl1, infusionTol,
              label = paste(key, "sum of infusion plasma coefficients = 1/cl1"))
  }
})


## --------------------------------------------------------------------------
## 4. Effect-site coefficient identities
## --------------------------------------------------------------------------

test_that("effect-site coefficients satisfy Ce(0) = 0 and Ce(steady state) = 1/cl1", {
  # The effect site is a massless compartment fed from plasma at rate ke0 and
  # emptied at the same rate, so it holds no drug at the instant of a bolus:
  # Ce(0) = 0. With
  #     Ce(t) = e1 exp(-l1 t) + e2 exp(-l2 t) + e3 exp(-l3 t)
  #                                          + e_ke0 exp(-ke0 t),
  # setting t = 0 gives e1 + e2 + e3 + e_ke0 = 0, equivalently
  # e_ke0 = -(e1 + e2 + e3).
  #
  # getDrugPK() *constructs* e_coef_bolus_ke0 as -e1 - e2 - e3, so this holds
  # to the last bit rather than to a tolerance (measured maximum scaled
  # residual: 1.4e-16). Checking it anyway is still worth the line: it is the
  # identity that guarantees the effect-site curve starts at zero, and it
  # would break the moment someone gave the effect compartment a volume or
  # reordered the terms. The residual is scaled by the largest coefficient
  # because the coefficients themselves span several orders of magnitude
  # across the drug library.
  #
  # At steady state the effect site has equilibrated with plasma, so
  # Ce(infinity) = Cp(infinity) = 1/CL1 per unit infusion rate, and the four
  # infusion effect-site coefficients must sum to that. This one is a real
  # numerical check rather than a construction check: e_coef_infusion_ke0 is
  # e_coef_bolus_ke0/ke0 while the others are e_i/lambda_i, so the identity
  # only closes if the fitted ke0 and the lambdas are mutually consistent.
  # Measured maximum residual: 1.2e-12.
  zeroTol     <- 1e-12
  infusionTol <- 1e-9

  for (key in names(pkBlocks)) {
    p <- pkBlocks[[key]]$p

    effectBolus <- c(p$e_coef_bolus_l1, p$e_coef_bolus_l2,
                     p$e_coef_bolus_l3, p$e_coef_bolus_ke0)
    expect_lt(abs(sum(effectBolus)) / max(abs(effectBolus)), zeroTol,
              label = paste(key, "effect-site bolus coefficients sum to zero"))

    # Stated the other way round, exactly as the implementation phrases it:
    # the ke0 coefficient is minus the sum of the three lambda coefficients.
    expect_lt(
      abs(p$e_coef_bolus_ke0 +
          (p$e_coef_bolus_l1 + p$e_coef_bolus_l2 + p$e_coef_bolus_l3)) /
        max(abs(effectBolus)),
      zeroTol,
      label = paste(key, "e_coef_bolus_ke0 = -(l1 + l2 + l3 coefficients)")
    )

    # Ce(0) itself, evaluated through the closed form.
    expect_lt(abs(ceBolus(p, 0)) / max(abs(effectBolus)), zeroTol,
              label = paste(key, "Ce(0) = 0"))

    effectInfusionSum <- p$e_coef_infusion_l1 + p$e_coef_infusion_l2 +
                         p$e_coef_infusion_l3 + p$e_coef_infusion_ke0
    expect_lt(abs(effectInfusionSum - 1 / p$cl1) * p$cl1, infusionTol,
              label = paste(key, "sum of infusion effect-site coefficients = 1/cl1"))
  }
})


## --------------------------------------------------------------------------
## 5. Finiteness, physicality, and the emitted parameter contract
## --------------------------------------------------------------------------

# The exact set of names each PK event block must carry. `simCpCe()` and the
# `advanceClosedForm*()` functions read these by name, so this is a pinned
# interface contract, not a derived quantity: if a field is renamed or
# dropped, this test is supposed to fail and be updated deliberately together
# with its consumers. `reference` is intentionally absent - it lives at the
# top level of the return value and is not asserted anywhere in this file.
expectedPKFields <- c(
  "v1", "v2", "v3", "cl1", "cl2", "cl3",
  "k10", "k12", "k13", "k21", "k31",
  "ka_PO", "bioavailability_PO", "tlag_PO",
  "ka_IM", "bioavailability_IM", "tlag_IM",
  "ka_IN", "bioavailability_IN", "tlag_IN",
  "customFunction",
  "lambda_1", "lambda_2", "lambda_3", "ke0",
  "p_coef_bolus_l1", "p_coef_bolus_l2", "p_coef_bolus_l3",
  "e_coef_bolus_l1", "e_coef_bolus_l2", "e_coef_bolus_l3", "e_coef_bolus_ke0",
  "p_coef_infusion_l1", "p_coef_infusion_l2", "p_coef_infusion_l3",
  "e_coef_infusion_l1", "e_coef_infusion_l2", "e_coef_infusion_l3",
  "e_coef_infusion_ke0",
  "p_coef_PO_l1", "p_coef_PO_l2", "p_coef_PO_l3", "p_coef_PO_ka",
  "e_coef_PO_l1", "e_coef_PO_l2", "e_coef_PO_l3", "e_coef_PO_ke0",
  "e_coef_PO_ka",
  "p_coef_IM_l1", "p_coef_IM_l2", "p_coef_IM_l3", "p_coef_IM_ka",
  "e_coef_IM_l1", "e_coef_IM_l2", "e_coef_IM_l3", "e_coef_IM_ke0",
  "e_coef_IM_ka",
  "p_coef_IN_l1", "p_coef_IN_l2", "p_coef_IN_l3", "p_coef_IN_ka",
  "e_coef_IN_l1", "e_coef_IN_l2", "e_coef_IN_l3", "e_coef_IN_ke0",
  "e_coef_IN_ka"
)

test_that("every emitted parameter is finite, scalar, and physically admissible", {
  for (key in names(pkBlocks)) {
    p <- pkBlocks[[key]]$p

    expect_identical(names(p), expectedPKFields, label = paste(key, "field names"))

    # Everything except customFunction is a finite numeric scalar. A NaN or
    # Inf here propagates silently through simCpCe() into a blank plot, so it
    # is worth checking every single field rather than a sample.
    numericFields <- setdiff(names(p), "customFunction")
    isGood <- vapply(
      numericFields,
      function(nm) is.numeric(p[[nm]]) && length(p[[nm]]) == 1L && is.finite(p[[nm]]),
      logical(1)
    )
    expect_true(all(isGood),
                info = paste(key, "bad fields:",
                             paste(numericFields[!isGood], collapse = ", ")),
                label = paste(key, "all numeric fields finite scalars"))

    expect_true(is.character(p$customFunction) && length(p$customFunction) == 1L,
                label = paste(key, "customFunction is a character scalar"))

    # Volumes are strictly positive. Note v3 is 1 L (a placeholder), not 0,
    # for the two-compartment drugs, precisely so that k31 = cl3/v3 is a
    # well-defined 0 rather than 0/0.
    expect_gt(p$v1, 0, label = paste(key, "v1 > 0"))
    expect_gt(p$v2, 0, label = paste(key, "v2 > 0"))
    expect_gt(p$v3, 0, label = paste(key, "v3 > 0"))

    # Elimination and the first inter-compartmental clearance are strictly
    # positive for every drug in the library; cl3 is exactly zero for the
    # two-compartment models and positive otherwise, never negative.
    expect_gt(p$cl1, 0, label = paste(key, "cl1 > 0"))
    expect_gt(p$cl2, 0, label = paste(key, "cl2 > 0"))
    expect_gte(p$cl3, 0, label = paste(key, "cl3 >= 0"))

    # Absorption parameters: zero means "route not supported", never negative,
    # and a bioavailability is a fraction.
    for (route in c("PO", "IM", "IN")) {
      ka   <- p[[paste0("ka_", route)]]
      biov <- p[[paste0("bioavailability_", route)]]
      tlag <- p[[paste0("tlag_", route)]]
      expect_gte(ka,   0, label = paste(key, route, "ka >= 0"))
      expect_gte(tlag, 0, label = paste(key, route, "tlag >= 0"))
      expect_gte(biov, 0, label = paste(key, route, "bioavailability >= 0"))
      expect_lte(biov, 1, label = paste(key, route, "bioavailability <= 1"))
      # getDrugPK() defaults bioavailability to 0 exactly when the route is
      # absent, so a supported route always carries a positive fraction.
      expect_identical(biov > 0, ka > 0,
                       label = paste(key, route, "bioavailability > 0 iff ka > 0"))
    }
  }
})


## --------------------------------------------------------------------------
## 6. ke0 sanity
## --------------------------------------------------------------------------

test_that("ke0 is positive and places the effect-site peak at tPeak", {
  # Every drug in the library declares tPeak > 0, so every drug must get a
  # positive ke0 out of the stats::optimize(tPeakError, ...) fit. A zero ke0
  # would collapse the effect site onto plasma and is the failure mode if the
  # optimiser bracket or tPeakError() signature ever drifts.
  for (key in names(pkBlocks)) {
    b <- pkBlocks[[key]]
    expect_gt(b$tPeak, 0, label = paste(key, "tPeak > 0"))
    expect_gt(b$p$ke0, 0, label = paste(key, "ke0 > 0"))
  }

  # And the fit has to mean something: the maximum of the closed-form Ce(t)
  # for a unit bolus must land on tPeak. Located here by root-finding on
  # dCe/dt to 1e-12, independently of the optimiser tPeakError() uses.
  #
  # The 0.5% tolerance is honest looseness, not a fudge: tPeakError() locates
  # the peak with stats::optimize(CE, c(0, 100), ...) at the default tol of
  # .Machine$double.eps^0.25 (about 1.2e-4), and getDrugPK() then searches ke0
  # with stats::optimize(..., c(0, 200)) at the same tol, so the recovered
  # peak time carries roughly a hundredth of a minute of optimiser noise.
  # Measured worst case across all blocks: 0.12% (morphine, tPeak 93.8 min,
  # recovered peak 93.69 min). The resulting sensitivity was measured by
  # rebuilding propofol's effect-site coefficients from a deliberately wrong
  # ke0: a 1% error in ke0 moves the peak by 0.53% and does fail this
  # assertion, a 0.5% error moves it by 0.27% and does not. Deliberately kept
  # as a single assertion; the detailed behaviour of the fit itself belongs to
  # the tPeakError() tests.
  # Ce starts at zero, rises, and decays, so dCe/dt changes sign exactly once
  # on (0, 400] minutes. Check that before root-finding, both because it is
  # itself the statement that the effect-site curve is unimodal and because
  # it turns a uniroot "endpoints not of opposite sign" error into a readable
  # test failure.
  bracketed <- vapply(pkBlocks, function(b) {
    dCeBolus(b$p, 1e-9) > 0 && dCeBolus(b$p, 400) < 0
  }, logical(1))
  expect_true(all(bracketed),
              info = paste("not unimodal:",
                           paste(names(bracketed)[!bracketed], collapse = ", ")),
              label = "dCe/dt brackets one sign change on (0, 400] min")

  peakResiduals <- vapply(names(pkBlocks), function(key) {
    b <- pkBlocks[[key]]
    peak <- stats::uniroot(function(t) dCeBolus(b$p, t), c(1e-9, 400),
                           tol = 1e-12)$root
    abs(peak - b$tPeak) / b$tPeak
  }, numeric(1))

  expect_lt(max(peakResiduals), 5e-3,
            label = paste0("worst effect-site peak mismatch (",
                           names(which.max(peakResiduals)), ")"))
})


## --------------------------------------------------------------------------
## 7. Allowlist guard
## --------------------------------------------------------------------------

test_that("getDrugPK rejects a drug that is not in the drug table", {
  # getDrugPK() dispatches by eval(call(drug, ...)), so an unchecked name
  # would call an arbitrary function in scope. The guard must fire before
  # that happens. No message pattern is matched: R translates error messages,
  # so a pattern would make this test locale-dependent.
  propofolDefaults <- getDrugDefaults("propofol")

  expect_error(getDrugPK("notADrug", 70, 170, 50, "male", propofolDefaults))

  # The allowlist is case-sensitive, and "Propofol" is not on it even though
  # "propofol" is.
  expect_error(getDrugPK("Propofol", 70, 170, 50, "male", propofolDefaults))

  # `sum` is a real function in scope; it must still be refused.
  expect_error(getDrugPK("sum", 70, 170, 50, "male", propofolDefaults))

  expect_error(getDrugPK("", 70, 170, 50, "male", propofolDefaults))

  # Conversely, every name on the allowlist must resolve to a working model -
  # otherwise the guard is protecting a list that the app cannot actually use.
  for (drug in drugNames) {
    expect_silent(pk <- buildPK(drug, covariateSets$standardAdult))
    expect_identical(pk$drug, drug, label = paste(drug, "resolves"))
  }
})


## --------------------------------------------------------------------------
## 8. Weight scaling
## --------------------------------------------------------------------------

test_that("cl1 follows the weight-scaling law each published model implements", {
  # These exponents are read out of the model definitions in R/drugs_*.R, not
  # inferred from output, so this is a real check of the covariate code path
  # rather than a restatement of it:
  #
  #   exponent 1    - per-kg models where every volume and every clearance is
  #                   a constant times weight (v1 <- c * weight; cl1 <- v1 *
  #                   k10 with fixed micro constants), e.g. drugs_morphine.R,
  #                   drugs_ketamine.R, drugs_naloxone.R.
  #   exponent 0.75 - 3/4-power allometric clearance at otherwise fixed
  #                   covariates: fentanyl (cl1 = 0.632 * (weight/70)^0.75),
  #                   remimazolam (cl1 = 1.12 * Fsize^0.75, Fsize = weight/70)
  #                   and propofol, whose Eleveld M4 term is
  #                   (weight/70)^0.75 * KCL * DCL with KCL and DCL functions
  #                   of age alone.
  #   exponent 0    - models with no weight term at all in the adult branch.
  #                   dexmedetomidine belongs here only for age > 1; its
  #                   infant branch does scale with weight, which is why the
  #                   age is held at 50 throughout this test.
  #
  # remifentanil is deliberately absent: see the BMI 30 note below.
  #
  # Measured maximum residual of |cl1(w)/cl1(70) - (w/70)^exponent| across all
  # 19 drugs and all weights below: 2.2e-16, i.e. exact in double precision.
  scalingLaws <- list(
    "1" = c("morphine", "pethidine", "hydromorphone", "methadone", "ketamine",
            "etomidate", "lidocaine", "rocuronium", "naloxone"),
    "0.75" = c("propofol", "fentanyl", "remimazolam"),
    "0" = c("alfentanil", "sufentanil", "dexmedetomidine", "midazolam",
            "oxytocin", "oxycodone", "oliceridine")
  )
  # Every drug except remifentanil is accounted for exactly once.
  expect_equal(sort(c(unlist(scalingLaws, use.names = FALSE), "remifentanil")),
               sort(drugNames))

  testWeights <- c(40, 55, 85, 110, 140)
  lawTol <- 1e-12

  for (exponentName in names(scalingLaws)) {
    exponent <- as.numeric(exponentName)
    for (drug in scalingLaws[[exponentName]]) {
      cl1At70 <- buildPK(drug, list(weight = 70, height = 170,
                                    age = 50, sex = "male"))$PK$default$cl1
      for (w in testWeights) {
        got <- buildPK(drug, list(weight = w, height = 170,
                                  age = 50, sex = "male"))$PK$default$cl1
        expect_lt(abs(got / cl1At70 - (w / 70) ^ exponent), lawTol,
                  label = sprintf("%s cl1(%g)/cl1(70) = (w/70)^%s", drug, w, exponentName))
        # Monotonicity is the weaker claim the exponents imply; assert it
        # explicitly so the intent survives if an exponent is ever changed.
        if (exponent > 0) {
          if (w > 70) expect_gt(got, cl1At70, label = sprintf("%s cl1(%g) > cl1(70)", drug, w))
          if (w < 70) expect_lt(got, cl1At70, label = sprintf("%s cl1(%g) < cl1(70)", drug, w))
        }
      }
    }
  }
})

test_that("per-kg models leave the eigenvalues and ke0 weight-invariant", {
  # In a model where every volume and every clearance is proportional to
  # weight, the micro rate constants k = CL/V are weight-free, so the whole
  # exponential structure - all three lambdas and the ke0 fitted from tPeak -
  # must be identical at any body weight. Only the coefficients (which carry
  # the 1/V1 and 1/CL1 factors) may move. This is a stronger statement than
  # monotonicity and would catch, for instance, a weight term accidentally
  # applied to a volume but not its paired clearance.
  #
  # Measured maxima at 50 vs 110 kg: micro constants 2.4e-16, lambdas 7.7e-14,
  # ke0 1.9e-11. ke0 gets the loosest bound because it comes out of a nested
  # stats::optimize() whose convergence path is sensitive to last-bit changes
  # in its objective.
  perKgDrugs <- c("morphine", "pethidine", "hydromorphone", "methadone",
                  "ketamine", "etomidate", "lidocaine", "rocuronium", "naloxone")

  for (drug in perKgDrugs) {
    light <- buildPK(drug, list(weight =  50, height = 170, age = 50, sex = "male"))$PK$default
    heavy <- buildPK(drug, list(weight = 110, height = 170, age = 50, sex = "male"))$PK$default

    for (k in c("k10", "k12", "k13", "k21", "k31")) {
      if (light[[k]] > 0) {
        expect_lt(abs(heavy[[k]] - light[[k]]) / light[[k]], 1e-12,
                  label = paste(drug, k, "weight-invariant"))
      } else {
        expect_identical(heavy[[k]], light[[k]], label = paste(drug, k, "both zero"))
      }
    }
    for (l in c("lambda_1", "lambda_2")) {
      expect_lt(abs(heavy[[l]] - light[[l]]) / light[[l]], 1e-10,
                label = paste(drug, l, "weight-invariant"))
    }
    expect_lt(abs(heavy$ke0 - light$ke0) / light$ke0, 1e-8,
              label = paste(drug, "ke0 weight-invariant"))

    # Volumes, by contrast, must scale exactly with weight.
    expect_lt(abs(heavy$v1 / light$v1 - 110 / 50), 1e-12,
              label = paste(drug, "v1 scales with weight"))
  }
})

test_that("remifentanil cl1 increases with weight inside one model branch", {
  # PINNED QUIRK. drugs_remifentanil.R switches from the Eleveld model to the
  # Kim model at BMI 30 ("# NIH Obesity cutoff"), so its parameters are
  # discontinuous in weight and no monotonicity claim can span the cutoff.
  # At height 170 cm, age 50, male, 86 kg -> 87 kg crosses BMI 30 and v1 falls
  # 11.6% while v2 falls 45.8%. This test therefore stays strictly inside the
  # Eleveld branch, where the model genuinely implies monotonicity: clearance
  # there is exp(THETA04) * bsize^0.75 * kcl * ksex * KMAT, and both the
  # Al-sallami fat-free-mass term bsize and the maturation term KMAT are
  # increasing functions of weight at fixed height, age, and sex.
  #
  # If the BMI 30 switch is ever removed or smoothed, this test should be
  # deliberately widened to span the whole weight range rather than quietly
  # left as is.
  height <- 175  # 85 kg at 175 cm is BMI 27.8, still below the cutoff
  weights <- seq(45, 85, by = 5)
  cl1 <- vapply(
    weights,
    function(w) buildPK("remifentanil",
                        list(weight = w, height = height, age = 40, sex = "male"))$PK$default$cl1,
    numeric(1)
  )
  expect_true(all(weights / (height / 100) ^ 2 < 30),
              label = "test weights stay inside the Eleveld branch")
  expect_true(all(diff(cl1) > 0),
              info = paste("cl1:", paste(signif(cl1, 6), collapse = " ")),
              label = "remifentanil cl1 strictly increasing in weight below BMI 30")
})


## --------------------------------------------------------------------------
## 9. Extravascular routes
## --------------------------------------------------------------------------

test_that("oral, IM and intranasal coefficients satisfy C(0) = 0 and AUC = F/cl1", {
  # For a first-order absorption route the drug starts outside the body, so
  # the concentration at the moment of dosing is zero:
  #     C(t) = sum_i c_i exp(-l_i t) + c_ka exp(-ka t),  C(0) = 0
  # forces sum_i c_i + c_ka = 0. getDrugPK() builds c_ka as minus the sum of
  # the others, so this holds by construction; asserting it documents which
  # physical fact that construction encodes.
  #
  # The AUC identity is the independent one. Integrating the same closed form
  # from 0 to infinity gives sum_i c_i/l_i + c_ka/ka, and mass balance says
  # that must equal (fraction absorbed)/(clearance) = F/CL1. That only closes
  # if the absorption-route coefficients, the lambdas and the bioavailability
  # are all mutually consistent. Measured maximum residual: 1.2e-12
  # (hydromorphone, whose slow lambda amplifies cube()'s round-off).
  zeroTol <- 1e-12
  aucTol  <- 1e-9

  routesTested <- character(0)
  for (csName in names(covariateSets)) {
    for (drug in drugNames) {
      p <- buildPK(drug, covariateSets[[csName]])$PK$default
      for (route in c("PO", "IM", "IN")) {
        ka <- p[[paste0("ka_", route)]]
        if (ka <= 0) next
        routesTested <- union(routesTested, paste(drug, route))
        key <- paste(drug, csName, route, sep = "/")

        plasma <- c(p[[paste0("p_coef_", route, "_l1")]],
                    p[[paste0("p_coef_", route, "_l2")]],
                    p[[paste0("p_coef_", route, "_l3")]],
                    p[[paste0("p_coef_", route, "_ka")]])
        effect <- c(p[[paste0("e_coef_", route, "_l1")]],
                    p[[paste0("e_coef_", route, "_l2")]],
                    p[[paste0("e_coef_", route, "_l3")]],
                    p[[paste0("e_coef_", route, "_ke0")]],
                    p[[paste0("e_coef_", route, "_ka")]])

        expect_lt(abs(sum(plasma)) / max(abs(plasma)), zeroTol,
                  label = paste(key, "Cp(0) = 0"))
        expect_lt(abs(sum(effect)) / max(abs(effect)), zeroTol,
                  label = paste(key, "Ce(0) = 0"))

        # AUC of the plasma curve. Terms whose rate constant is zero (the
        # absent third exponential of a two-compartment model) carry a zero
        # coefficient and contribute nothing.
        auc <- plasma[4] / ka
        for (i in 1:3) {
          lambda <- p[[paste0("lambda_", i)]]
          if (lambda > 0) auc <- auc + plasma[i] / lambda
        }
        biov <- p[[paste0("bioavailability_", route)]]
        expect_lt(abs(auc - biov / p$cl1) * p$cl1 / biov, aucTol,
                  label = paste(key, "AUC = F/cl1"))
      }
    }
  }

  # Guard against this whole test silently becoming a no-op if the CSV or the
  # drug models change: hydromorphone (PO/IM/IN) and oxycodone (PO) are the
  # extravascular routes the library currently offers.
  expect_equal(
    sort(routesTested),
    sort(c("hydromorphone PO", "hydromorphone IM", "hydromorphone IN",
           "oxycodone PO"))
  )
})
