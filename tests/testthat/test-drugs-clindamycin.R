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
    tPeak = 0, MEAC = 0, typical = 2, upperTypical = 4, lowerTypical = 1,
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
    v1 = 86.38482,
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
