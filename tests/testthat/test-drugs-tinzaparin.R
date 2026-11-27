# Tinzaparin: Barrett 2001 subcutaneous two-compartment anti-Xa model (DVT
# patients).  See the header of R/drugs_tinzaparin.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

# A self-contained matrix exponential (scaling and squaring a Taylor series;
# the same helper the compartment tests use), so the independent reference
# needs no package.
expmTaylor <- function(M) {
  s <- max(0, ceiling(log2(max(abs(M)) * nrow(M))) + 1)
  A <- M / 2^s
  E <- diag(nrow(M)); term <- E
  for (k in 1:30) { term <- term %*% A / k; E <- E + term }
  for (i in seq_len(s)) E <- E %*% E
  E
}

# The percent above ideal body weight Barrett used (Devine; Eqs 1-3).
pibwOf <- function(weight, height, sex) {
  ibw <- (if (sex == "male") 50 else 45) + (2.3 / 2.5) * max(0, height - 152)
  (weight - ibw) / ibw * 100
}

test_that("returns the reference parameters with the switch off", {
  # 70 kg, 170 cm, 35 y man at the median creatinine 1.0 mg/dL: the creatinine
  # term is 1, so only the PIBW term moves clearance.  Built by hand from
  # Barrett's Table 3, not from the code under test.
  pibw <- pibwOf(70, 170, "male")
  clf  <- exp(-0.006 * (pibw - 14))                       # creatinine term = 1
  actual <- tinzaparin(70, 170, 35, "male", adjustToFFM = FALSE, creatinine = 1.0)
  expected <- list(
    PK = list(default = list(
      v1 = 0.0981 * 70, v2 = 0.864 * 70, v3 = 1,
      cl1 = 0.0176 * 70 * clf / 60, cl2 = 0.007 * 70 / 60, cl3 = 0,
      ka_SC = 0.206 / 60, tlag_SC = 0, bioavailability_SC = 1
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("every disposition parameter is per kilogram (linear in weight)", {
  # All of CL, Vc, Q and V3 are weight-normalised, so doubling weight (at a
  # fixed covariate contribution) doubles each.  Hold the covariates fixed by
  # keeping the same height-scaled PIBW is not possible, so compare at equal
  # PIBW by scaling height is awkward; instead strip the clearance covariate
  # by reading V1, V2, Q (which carry no covariate) and read CL at a fixed
  # creatinine with PIBW held by using the same patient shape.
  a <- tinzaparin(60, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default
  b <- tinzaparin(120, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default
  expect_equal(b$v1 / a$v1, 2, tolerance = 1e-8)
  expect_equal(b$v2 / a$v2, 2, tolerance = 1e-8)
  expect_equal(b$cl2 / a$cl2, 2, tolerance = 1e-8)   # Q, no covariate
  # CL carries weight x the PIBW covariate, which differs between the two
  # weights, so its ratio is 2 x the ratio of the PIBW factors.
  clfRatio <- exp(-0.006 * (pibwOf(120, 170, "male") - pibwOf(60, 170, "male")))
  expect_equal(b$cl1 / a$cl1, 2 * clfRatio, tolerance = 1e-8)
})

test_that("clearance falls with plasma creatinine as Table 5 gives (77% at 2.2 mg/dL)", {
  base <- tinzaparin(70, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default$cl1
  hi   <- tinzaparin(70, 170, 50, "male", adjustToFFM = FALSE, creatinine = 2.2)$PK$default$cl1
  expect_equal(hi / base, exp(-0.213 * (2.2 - 1.0)), tolerance = 1e-8)
  expect_equal(hi / base, 0.7745, tolerance = 1e-3)      # Table 5 fraction of median
})

test_that("clearance falls with obesity (PIBW term)", {
  # A 70 kg, 170 cm man (PIBW ~5%) against a 110 kg, 170 cm man (PIBW ~65%);
  # the heavier man clears less PER KILOGRAM, but the per-kg CL still rises
  # with weight overall.  Compare the covariate factor itself.
  lean  <- tinzaparin(70, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default
  obese <- tinzaparin(110, 170, 50, "male", adjustToFFM = FALSE, creatinine = 1.0)$PK$default
  # cl1 = 0.0176 x weight x exp(-0.006 (PIBW - 14)); divide out the weight term
  leanCovar  <- lean$cl1  / (0.0176 * 70  / 60)
  obeseCovar <- obese$cl1 / (0.0176 * 110 / 60)
  expect_equal(obeseCovar / leanCovar,
               exp(-0.006 * (pibwOf(110, 170, "male") - pibwOf(70, 170, "male"))),
               tolerance = 1e-8)
  expect_lt(obeseCovar, leanCovar)                       # per-kg clearance lower
})

test_that("a blank creatinine is the assumed normal value for age and sex", {
  blank <- tinzaparin(70, 170, 40, "male", adjustToFFM = FALSE)$PK$default$cl1
  filled <- tinzaparin(70, 170, 40, "male", adjustToFFM = FALSE,
                       creatinine = assumedCreatinine(40, "male"))$PK$default$cl1
  expect_equal(blank, filled, tolerance = 1e-12)
})

test_that("the plotted output is anti-Xa activity only, with no effect site", {
  r <- getDrugPK("tinzaparin", 70, 170, 50, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
  expect_equal(r$typical, 0)
})

test_that("a subcutaneous IU dose matches an independent 2-compartment solution", {
  # 175 IU/kg in a 70 kg, 170 cm, 35 y man at creatinine 1.0 mg/dL.  Build the
  # depot/central/peripheral system by hand (in minutes) and compare the peak
  # and the whole curve with the engine's.
  wt <- 70; ht <- 170
  pibw <- pibwOf(wt, ht, "male")
  clf  <- exp(-0.006 * (pibw - 14))                      # creatinine 1.0: term 1
  CL <- 0.0176 * wt * clf / 60; V1 <- 0.0981 * wt
  Q  <- 0.007 * wt / 60;        V2 <- 0.864 * wt
  ka <- 0.206 / 60
  k10 <- CL / V1; k12 <- Q / V1; k21 <- Q / V2
  A <- matrix(c(-ka,       0,          0,
                 ka, -(k10 + k12),    k21,
                  0,     k12,       -k21), 3, 3, byrow = TRUE)
  dose <- 175 * wt
  refConc <- function(t) (as.numeric(expmTaylor(A * t) %*% c(dose, 0, 0)))[2] / V1 / 1000

  d <- data.frame(Drug = "tinzaparin", Time = 0, Dose = 175 * wt, Units = "IU SC")
  r <- simulateDrugsWithCovariates(d, noEvents, wt, ht, 35, "male",
                                   1440, TRUE, creatinine = 1.0,
                                   adjustToFFM = FALSE)$tinzaparin
  pl <- r$results[r$results$Site == "Plasma", ]
  ref <- vapply(pl$Time, refConc, numeric(1))
  expect_equal(pl$Y, ref, tolerance = 1e-4)
  expect_gt(max(pl$Y), 0.55)                              # paper's model ~0.58
  expect_lt(max(pl$Y), 0.62)
})

test_that("a mass unit is refused for this activity drug", {
  dose <- data.frame(Drug = "tinzaparin", Time = 0, Dose = 10, Units = "mg SC")
  expect_error(
    simulateDrugsWithCovariates(dose, noEvents, 70, 170, 50, "male", 1440, TRUE,
                                creatinine = 1.0, adjustToFFM = FALSE),
    "international units"
  )
})

test_that("scales to fat-free mass with the switch on", {
  # The whole model is per kilogram, so with the switch on every disposition
  # parameter is evaluated at the pharmacokinetic weight; the covariate (PIBW,
  # creatinine) reads the entered patient and is unchanged by the switch.
  wt <- 120; ht <- 170; age <- 50
  pk <- pkSizeFactors(wt, ht, age, "male", TRUE, legacyVolume = 1)$pkWeight
  pibw <- pibwOf(wt, ht, "male")
  clf  <- exp(-0.213 * (assumedCreatinine(age, "male") - 1) - 0.006 * (pibw - 14))
  actual <- tinzaparin(wt, ht, age, "male")$PK$default
  expect_equal_rounded(actual[c("v1", "v2", "cl1", "cl2")], list(
    v1  = 0.0981 * pk,
    v2  = 0.864  * pk,
    cl1 = 0.0176 * pk * clf / 60,
    cl2 = 0.007  * pk / 60
  ))
})
