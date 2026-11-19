# clindamycin: see the header of R/drugs_clindamycin.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- clindamycin(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 66.2,
        v2 = 1,
        v3 = 1,
        cl1 = 0.25333333,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.016116667,
        bioavailability_PO = 0.876,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 3.1, upperTypical = 9.9, lowerTypical = 3.1,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- clindamycin(120, 170, 50, "male")
  expected <- list(
    v1 = 86.38482 / cohortFactorsIndependent(CLINDAMYCIN_COHORT)$volume,  # V anchored (A2-F4)
    v2 = 1,
    v3 = 1,
    cl1 = 0.28915807,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the final-table vector is used, not the abstract's", {
  x <- clindamycin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 15.2)     # not 16.2
  expect_equal(x$v1, 66.2)           # not 70.2
  expect_equal(x$ka_PO * 60, 0.967)  # not 0.92
  expect_equal(x$bioavailability_PO, 0.876)
  # The published clearance exponent is 0.497 on total weight with the switch off
  y <- clindamycin(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$cl1 / x$cl1, 0.5^0.497)
  expect_equal(y$v1, x$v1)
})

test_that("an oral dose carries the bioavailability once", {
  PK <- getDrugPK("clindamycin", 70, 171, 50, "male")$PK$default
  aucIV <- PK$p_coef_bolus_l1 / PK$lambda_1
  aucPO <- PK$p_coef_PO_l1 / PK$lambda_1 + PK$p_coef_PO_ka / PK$ka_PO
  expect_equal(aucPO / aucIV, 0.876)
})

test_that("anchored at Bouazza's cohort: its typical patient receives the published V (A2-F4)", {
  # Text p. 972: 30 men and 20 women, 70.9 kg, 58.7 y; heights from NCD-RisC
  # (France, born 1950, less the loss to 58.7 y), 173.1 and 160.4 cm.  V has
  # no sex term and is anchored at the pooled patient, the cohort's man and
  # woman on the geometric mean; CL keeps its own 0.497 power, unanchored.
  kc <- cohortFactorsIndependent(CLINDAMYCIN_COHORT)
  expect_equal(kc$volume, 0.9207, tolerance = 1e-3)
  m <- clindamycin(70.9, 173.1, 58.7, "male")$PK$default
  w <- clindamycin(70.9, 160.4, 58.7, "female")$PK$default
  pooled <- exp((30 * log(m$v1) + 20 * log(w$v1)) / 50)
  expect_equal(pooled, 66.2, tolerance = 1e-9)
  expect_equal(m$v1, 66.2 * pkSizeFactors(70.9, 173.1, 58.7, "male")$volume / kc$volume,
               tolerance = 1e-6)
  expect_equal(m$cl1 * 60, 15.2 * pkSizeFactors(70.9, 173.1, 58.7, "male")$volume^0.497,
               tolerance = 1e-9)
  # The switch off is unchanged
  off <- clindamycin(70.9, 173.1, 58.7, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 66.2)
  expect_equal(off$cl1 * 60, 15.2 * (70.9 / 70)^0.497)
})
