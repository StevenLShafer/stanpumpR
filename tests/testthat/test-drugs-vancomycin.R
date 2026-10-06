# vancomycin: see the header of R/drugs_vancomycin.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- vancomycin(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 47.25,
        v2 = 51.24,
        v3 = 1,
        cl1 = 0.06633315,
        cl2 = 0.038,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 15, upperTypical = 20, lowerTypical = 10,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- vancomycin(120, 170, 50, "male")
  expected <- list(
    v1 = 61.656839,
    v2 = 66.863417,
    v3 = 1,
    cl1 = 0.086807758,
    cl2 = 0.04639468,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Thomson's covariate equations are reproduced, Q as a clearance", {
  x <- vancomycin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  crcl <- creatinineClearanceCG(70, 50, "male")
  expect_equal(x$cl1 * 60, 2.99 * (1 + 0.0154 * (crcl - 66)))
  expect_equal(x$v1, 0.675 * 70); expect_equal(x$v2, 0.732 * 70)
  expect_equal(x$cl2 * 60, 2.28)
  # The source's own reference point: CrCL 66 gives CL 2.99 L/h.  That is a
  # 70 kg man of 72.1 years at the assumed creatinine.
  y <- vancomycin(70, 171, 140 - 66 * 72 / 70, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$cl1 * 60, 2.99, tolerance = 1e-9)
  # Refuses rather than clamps where the equation is not defined
  expect_error(vancomycin(70, 171, 139.5, "male", adjustToFFM = FALSE), "not defined")
})
