# gentamicin: see the header of R/drugs_gentamicin.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- gentamicin(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 16.6,
        v2 = 13.4,
        v3 = 1,
        cl1 = 0.072972888,
        cl2 = 0.024666667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 8, upperTypical = 10, lowerTypical = 1,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- gentamicin(120, 170, 50, "male")
  expected <- list(
    v1 = 21.66145,
    v2 = 17.485749,
    v3 = 1,
    cl1 = 0.081364711,
    cl2 = 0.030115845,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Smit's equations are reproduced with de-indexed eGFR", {
  x <- gentamicin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  G <- egfrDeindexed(70, 171, 50, "male")
  expect_equal(x$cl1 * 60, 3.53 * (G / 74))
  expect_equal(x$v1, 16.6); expect_equal(x$v2, 13.4); expect_equal(x$cl2 * 60, 1.48)
  # Weight scales Vc linearly and leaves Vp and Q fixed with the switch off
  y <- gentamicin(140, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1, 33.2); expect_equal(y$v2, 13.4); expect_equal(y$cl2, x$cl2)
})
