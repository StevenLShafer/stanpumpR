# methylprednisolone: see the header of R/drugs_methylprednisolone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- methylprednisolone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 78.4,
        v2 = 1,
        v3 = 1,
        cl1 = 0.38,
        cl2 = 0,
        cl3 = 0,
        ka_PO = 0.02129034,
        bioavailability_PO = 0.82,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 0.5, upperTypical = 1.5, lowerTypical = 0.2,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- methylprednisolone(120, 170, 50, "male")
  expected <- list(
    v1 = 102.30468,
    v2 = 1,
    v3 = 1,
    cl1 = 0.4639468,
    cl2 = 0,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Hong's one-compartment vector, Al-Habet's F, and a 90 min oral peak", {
  x <- methylprednisolone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 22.8); expect_equal(x$v1, 78.4)
  expect_equal(x$bioavailability_PO, 0.82)
  expect_equal(log(2) / (x$cl1 / x$v1) / 60, 2.38345, tolerance = 1e-5)
  # The provisional absorption constant puts the oral plasma peak at 90 min
  k <- x$cl1 / x$v1
  expect_equal(log(x$ka_PO / k) / (x$ka_PO - k), 90, tolerance = 1e-6)
  expect_equal(x$ka_PO, METHYLPREDNISOLONE_KA_PO)
})
