# glycopyrrolate: see the header of R/drugs_glycopyrrolate.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- glycopyrrolate(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 14.13538,
        v2 = 23.767453,
        v3 = 89.440678,
        cl1 = 0.93610406,
        cl2 = 0.53372526,
        cl3 = 0.17158433
      )
    ),
    tPeak = 0, MEAC = 0, typical = 3, upperTypical = 10, lowerTypical = 1,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- glycopyrrolate(120, 170, 50, "male")
  expected <- list(
    v1 = 18.445351,
    v2 = 31.014307,
    v3 = 116.71173,
    cl1 = 1.1429013,
    cl2 = 0.6516319,
    cl3 = 0.20948947
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the published cation parameters are recovered through the bromide factor", {
  x <- glycopyrrolate(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  f <- 318.43 / 398.33
  expect_equal(f, 0.7994125, tolerance = 1e-6)
  expect_equal(x$v1 * f, 11.3); expect_equal(x$v2 * f, 19.0); expect_equal(x$v3 * f, 71.5)
  expect_equal(x$cl1 * 60 * f, 44.9); expect_equal(x$cl2 * 60 * f, 25.6); expect_equal(x$cl3 * 60 * f, 8.23)
  # Rate constants are unchanged by the rescaling, so the eigenvalue
  # half-times are the specification's: 0.0927, 0.809, 7.21 h
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, x$cl3 / x$v1, x$cl2 / x$v2, x$cl3 / x$v3)
  expect_equal(sort(log(2) / r / 60), c(0.0927082, 0.8089118, 7.2062445), tolerance = 1e-5)
  # A dose of 120 mcg of CATION (= 120 / f mcg of bromide) has AUC 120 / 44.9 mcg.h/L
  PK <- getDrugPK("glycopyrrolate", 70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  auc <- sum(c(PK$p_coef_bolus_l1, PK$p_coef_bolus_l2, PK$p_coef_bolus_l3) /
             c(PK$lambda_1, PK$lambda_2, PK$lambda_3))
  expect_equal(auc / 60 * 120 / f, 120 / 44.9, tolerance = 1e-8)   # auc is min/L per mcg
})
