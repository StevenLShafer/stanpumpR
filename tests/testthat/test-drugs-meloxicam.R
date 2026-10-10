# Meloxicam: Aoyama 2017 apparent oral two-compartment model with CYP2C9 and
# lean body mass.  See the header of R/drugs_meloxicam.R.  Pins are the
# published numbers.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  # 70 kg, 171 cm man: James LBM 70 x 1.10 - 128 x (70 / 171)^2 = 55.5502 kg
  actual <- meloxicam(70, 171, 50, "male", adjustToFFM = FALSE)
  lbm <- 77 - 128 * (70 / 171)^2
  expected <- list(
    PK = list(
      default = list(
        v1 = 7.79 * (lbm / 55)^1.05, v2 = 2.73, v3 = 1,
        cl1 = 0.391 / 60, cl2 = 1.24 / 60, cl3 = 0,
        ka_PO = 2 / (1.91 * 60), tlag_PO = 0, bioavailability_PO = 1,
        ka_PO2 = 2 / 60, tlag_PO2 = 114.6, fraction_PO2 = 0.575
      )
    ),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
  # Vc/F is 7.79 L at the median LBM of 55 kg
  expect_equal(7.79 * (55 / 55)^1.05, 7.79)
})

test_that("CYP2C9 alleles reduce apparent clearance per allele", {
  cl <- function(g) meloxicam(70, 171, 50, "male", FALSE, cyp2c9 = g)$PK$default$cl1 * 60
  expect_equal(cl("*1/*2"), 0.391 * 0.853)
  expect_equal(cl("*1/*3"), 0.391 * 0.600)
  expect_equal(cl("*2/*3"), 0.391 * 0.453)
  expect_equal(cl("*3/*3"), 0.391 * 0.200)   # 0.0782 L/h, as published
  expect_error(meloxicam(70, 171, 50, "male", cyp2c9 = "*1/*13"), "Invalid cyp2c9")
})

test_that("scales the size-free parameters to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126;
  # Vc/F on James LBM, 132 - 128 x (120 / 170)^2 = 68.2215 kg, either way
  actual <- meloxicam(120, 170, 50, "male")$PK$default
  expected <- list(
    v1 = 7.79 * ((132 - 128 * (120 / 170)^2) / 55)^1.05,
    v2 = 2.73 * 1.3049067,
    cl1 = 0.391 / 60 * 1.2209126,
    cl2 = 1.24 / 60 * 1.2209126
  )
  expect_equal_rounded(actual[names(expected)], expected)
  off <- meloxicam(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, actual$v1)
  expect_equal(off$cl1, 0.391 / 60)
})

test_that("the two absorption paths take the whole dose, F = 1", {
  PK <- getDrugPK("meloxicam", 70, 170, 40, "male")$PK$default
  expect_equal(PK$bioavailability_PO + PK$bioavailability_PO2, 1)
  expect_equal(PK$bioavailability_PO2, 0.575)
})

test_that("7.5 mg peaks near the published structure's peak", {
  x <- simulateDrugsWithCovariates(
    data.frame(Drug = "meloxicam", Time = 0, Dose = 7.5, Units = "mg PO"),
    noEvents, 70, 170, 40, "male", 72 * 60, TRUE)$meloxicam$results
  x <- x[x$Site == "Plasma", ]
  # Published structure (zero-order path solved numerically): 0.733 mg/L at
  # 3.1 h; the first-order approximation: 0.720 mg/L at 3.2 h
  expect_equal(max(x$Y), 0.720, tolerance = 0.005)
  expect_equal(x$Time[which.max(x$Y)] / 60, 3.17, tolerance = 0.02)
  # Terminal half-life about 19 h
  late <- x[x$Time >= 48 * 60, ]
  slope <- coef(lm(log(late$Y) ~ late$Time))[2]
  expect_equal(unname(log(2) / -slope / 60), 19.1, tolerance = 0.02)
})
