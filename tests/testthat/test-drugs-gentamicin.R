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
        cl1 = 0.082886221,
        cl2 = 0.024666667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 2, upperTypical = 8, lowerTypical = 0.5,
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
    cl1 = 0.092418069,
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

test_that("the Pregnant field applies the review's factor (R/pregnancy.R)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  eGFR x 1.5, so CL x 1.5.
  # The values are those the model gives with the flag on; test-pregnancy.R
  # checks the ratio against the flag off and that nothing else moves.
  actual <- gentamicin(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(13.38117788, actual$v1)
  expect_equal_rounded(10.80167371, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.1102026358, actual$cl1)
  expect_equal_rounded(0.02098457399, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
  expect_equal(gentamicin(70, 165, 30, "female")$PK$default,
               gentamicin(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(gentamicin(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(gentamicin(70, 165, 30, "female")$reference, "Ansari")
})
