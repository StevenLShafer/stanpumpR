# ceftriaxone: see the header of R/drugs_ceftriaxone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- ceftriaxone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.76,
        v2 = 2.97,
        v3 = 1,
        cl1 = 0.020166667,
        cl2 = 0.048666667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 20, upperTypical = 50, lowerTypical = 10,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- ceftriaxone(120, 170, 50, "male")
  expected <- list(
    v1 = 7.5162623,
    v2 = 3.8755727,
    v3 = 1,
    cl1 = 0.024621738,
    cl2 = 0.059417747,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the total-plasma vector is Sanz-Codina's", {
  x <- ceftriaxone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 1.21); expect_equal(x$v1, 5.76)
  expect_equal(x$cl2 * 60, 2.92); expect_equal(x$v2, 2.97)
  PK <- getDrugPK("ceftriaxone", 70, 171, 50, "male")$PK$default
  expect_equal(PK$p_coef_bolus_l1 / PK$lambda_1 + PK$p_coef_bolus_l2 / PK$lambda_2, 1 / PK$cl1)
})
