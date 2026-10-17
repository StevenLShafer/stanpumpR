# Unfractionated heparin: Jia 2015 two-compartment anti-IIa model during
# cardiopulmonary bypass.  See the header of R/drugs_heparin.R.  Pins are the
# published Table 2 values.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- heparin(70, 170, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 3.04, v2 = 8.01, v3 = 1,
      cl1 = 1.18 / 60, cl2 = 0.171 / 60, cl3 = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- heparin(120, 170, 50, "male")$PK$default
  expected <- list(
    v1 = 3.04 * 1.3049067, v2 = 8.01 * 1.3049067,
    cl1 = 1.18 / 60 * 1.2209126, cl2 = 0.171 / 60 * 1.2209126
  )
  expect_equal_rounded(actual[names(expected)], expected)
})

test_that("the curve is anti-IIa activity in IU/mL and reproduces the reported levels", {
  # 375 IU/kg into V1 3.04 L (reference man) is about 8.6 IU/mL at time 0.
  dose <- data.frame(Drug = "heparin", Time = 0, Dose = 375, Units = "IU/kg")
  r <- simulateDrugsWithCovariates(dose, noEvents, 70, 170, 50, "male",
                                   120, TRUE, adjustToFFM = FALSE)$heparin
  cp0 <- r$results$Y[r$results$Site == "Plasma"][1]
  expect_equal(cp0, 375 * 70 / 3.04 / 1000, tolerance = 1e-6)
  # Terminal half-life of about 37 h, the slow peripheral return the authors
  # used to explain heparin rebound.
  # Rates per hour (L/h clearances over L volumes), so the half-life is
  # log(2) / lambda hours directly.
  ev <- -Re(eigen(matrix(c(-(1.18 / 3.04 + 0.171 / 3.04), 0.171 / 3.04,
                            0.171 / 8.01, -0.171 / 8.01), 2))$values)
  expect_equal(log(2) / min(ev), 37.4, tolerance = 0.1)        # hours
})

test_that("heparin takes only international units, and no other drug does", {
  expect_error(
    simulateDrugsWithCovariates(
      data.frame(Drug = "heparin", Time = 0, Dose = 50, Units = "mg"),
      noEvents, 70, 170, 50, "male", 120, TRUE),
    "international units")
  expect_error(
    simulateDrugsWithCovariates(
      data.frame(Drug = "bivalirudin", Time = 0, Dose = 50, Units = "IU"),
      noEvents, 70, 170, 50, "male", 120, TRUE),
    "international units")
})
