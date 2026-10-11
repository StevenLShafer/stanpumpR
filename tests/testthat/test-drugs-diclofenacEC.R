# Enteric-coated diclofenac: Bartels 2010 (PAGE poster) apparent two-compartment
# model, enteric-coated arm.  See the header of R/drugs_diclofenacEC.R.  Pins
# are the poster's numbers (Tables 2 and 3).

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  actual <- diclofenacEC(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 23.5, v2 = 21.3, v3 = 1,
        cl1 = 40.3 / 60, cl2 = 10.6 / 60, cl3 = 0,
        ka_PO = 0.503 / 60,
        tlag_PO = (0.932 + 0.02) * 60,   # lag plus two transits at 100 /h
        bioavailability_PO = 0.784
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # No size covariate in the source: unscaled with the switch off
  off <- diclofenacEC(140, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 23.5)
  expect_equal(off$cl1, 40.3 / 60)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- diclofenacEC(120, 170, 50, "male")$PK$default
  expected <- list(
    v1 = 23.5 * 1.3049067, v2 = 21.3 * 1.3049067,
    cl1 = 40.3 / 60 * 1.2209126, cl2 = 10.6 / 60 * 1.2209126
  )
  expect_equal_rounded(actual[names(expected)], expected)
  # Absorption is not scaled
  expect_equal(actual$ka_PO, 0.503 / 60)
  expect_equal(actual$bioavailability_PO, 0.784)
})

test_that("a 50 mg tablet matches the closed-form apparent model", {
  x <- simulateDrugsWithCovariates(
    data.frame(Drug = "diclofenacEC", Time = 0, Dose = 50, Units = "mg PO"),
    noEvents, 70, 171, 50, "male", 24 * 60, TRUE, adjustToFFM = FALSE
  )$diclofenacEC$results
  x <- x[x$Site == "Plasma", ]
  # Independent closed form: one first-order input into two compartments,
  # times in hours
  k10 <- 40.3 / 23.5; k12 <- 10.6 / 23.5; k21 <- 10.6 / 21.3; ka <- 0.503
  b <- k10 + k12 + k21
  l1 <- (b + sqrt(b^2 - 4 * k10 * k21)) / 2
  l2 <- (b - sqrt(b^2 - 4 * k10 * k21)) / 2
  cp <- function(t) {
    t <- t - 0.952
    ifelse(t <= 0, 0, 50 * 0.784 * ka / 23.5 * (
      (k21 - l1) / ((ka - l1) * (l2 - l1)) * exp(-l1 * t) +
      (k21 - l2) / ((ka - l2) * (l1 - l2)) * exp(-l2 * t) +
      (k21 - ka) / ((l1 - ka) * (l2 - ka)) * exp(-ka * t)))
  }
  expect_equal(x$Y, cp(x$Time / 60), tolerance = 1e-6)
  # Nothing before the lag
  expect_true(all(x$Y[x$Time < 0.95 * 60] == 0))
  expect_equal(max(x$Y), 0.256, tolerance = 0.01)
})
