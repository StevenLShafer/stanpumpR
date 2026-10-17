# sugammadex: see the header of R/drugs_sugammadex.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- sugammadex(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.4904106,
        v2 = 6.7861043,
        v3 = 1,
        cl1 = 0.079718088,
        cl2 = 0.206,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 10, upperTypical = 30, lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- sugammadex(120, 170, 50, "male")
  expected <- list(
    v1 = 5.423784,
    v2 = 8.0897291,
    v3 = 1,
    cl1 = 0.10302249,
    cl2 = 0.251508,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the source's derived covariate scenario is reproduced", {
  # W = 74.5 kg and CR = 119 mL/min: Cockcroft-Gault at the assumed creatinine
  # of 0.9 mg/dL gives 119 for a 74.5 kg man of (140 - 119 x 72 x 0.9 / 74.5)
  # = 36.5 years.
  x <- sugammadex(74.5, 175, 140 - 119 * 72 * SCR_ASSUMED_MALE / 74.5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 5.58, tolerance = 1e-9)
  expect_equal(x$v1, 4.704143, tolerance = 1e-6)
  expect_equal(x$cl2 * 60, 12.951264, tolerance = 1e-6)
  expect_equal(x$v2, 6.758214, tolerance = 1e-6)
  expect_equal(x$v1 + x$v2, 11.462357, tolerance = 1e-6)
})
