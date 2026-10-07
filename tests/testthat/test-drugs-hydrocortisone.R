# hydrocortisone: see the header of R/drugs_hydrocortisone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- hydrocortisone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 2.15,
        v2 = 11.980583,
        v3 = 1,
        cl1 = 0.34304207,
        cl2 = 0.29093851,
        cl3 = 0,
        ka_PO = 0.01832173,
        bioavailability_PO = 0.88,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 0.5, upperTypical = 1, lowerTypical = 0.2,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- hydrocortisone(120, 170, 50, "male")
  expected <- list(
    v1 = 2.8055493,
    v2 = 15.633542,
    v3 = 1,
    cl1 = 0.41882439,
    cl2 = 0.3552105,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the linearisation divides the free-cortisol parameters by 1 + NS", {
  x <- hydrocortisone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1, 2.15)
  expect_equal(x$cl1 * 60, 106 / 5.15); expect_equal(x$cl2 * 60, 89.9 / 5.15)
  expect_equal(x$v2, 61.7 / 5.15)
  expect_equal(HYDROCORTISONE_NS, 4.15)
  # Oral: mean input time of the source transit chain plus depot, F 0.88
  expect_equal(1 / x$ka_PO / 60, 0.868 + 1 / 24)
  expect_equal(x$bioavailability_PO, 0.88)
  # Allometry on total weight with the switch off
  y <- hydrocortisone(35, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1 / x$v1, 0.5); expect_equal(y$cl1 / x$cl1, 0.5^0.75)
})
