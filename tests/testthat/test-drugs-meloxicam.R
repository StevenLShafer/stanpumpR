# Meloxicam: Aoyama 2017 apparent oral two-compartment model with CYP2C9 and
# lean body mass, and the ANJESO intravenous three-compartment model (FDA
# review 2020) as a parallel system that takes the intravenous doses.  See the
# header of R/drugs_meloxicam.R.  Pins are the published numbers.

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
    routes = "PO",
    parallelSystems = actual$parallelSystems,
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

test_that("the intravenous ANJESO system has the published parameters", {
  # 70 kg: every weight factor is 1.  Creatinine 0.9 mg/dL in a 50 y man:
  # CKD-EPI 2009 141 x (0.9 / 0.9)^-0.411 x 0.993^50 = 99.2 mL/min/1.73 m^2
  x <- meloxicam(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = 0.9)
  expect_length(x$parallelSystems, 1)
  iv <- x$parallelSystems[[1]]
  expect_equal(iv$routes, "IV")
  egfr <- 141 * 0.993^50
  expected <- list(
    v1 = 4.16, v2 = 2.06, v3 = 3.28,
    cl1 = 0.416 * (egfr / 91)^0.554 / 60,
    cl2 = 6.171 / 60, cl3 = 0.835 / 60
  )
  expect_equal_rounded(iv$PK$default[names(expected)], expected)
  # Same absorption as the oral system, so both run on one time line
  oral <- c("ka_PO", "tlag_PO", "bioavailability_PO", "ka_PO2", "tlag_PO2", "fraction_PO2")
  expect_equal(iv$PK$default[oral], x$PK$default[oral])
})

test_that("the intravenous system scales on total weight in both switch positions", {
  # 120 kg: clearances x (120 / 70)^0.761 = 1.5064, volumes x (120 / 70)^0.776
  on  <- meloxicam(120, 170, 50, "male", creatinine = 1)$parallelSystems[[1]]$PK$default
  off <- meloxicam(120, 170, 50, "male", FALSE, creatinine = 1)$parallelSystems[[1]]$PK$default
  expect_equal(on, off)
  expect_equal(on$v1, 4.16 * (120 / 70)^0.776)
  expect_equal(on$cl2, 6.171 * (120 / 70)^0.761 / 60)
  # Halving eGFR (by creatinine) lowers CL by 2^-0.554 relative to eGFR's ratio
  hi <- meloxicam(70, 170, 50, "male", creatinine = 3)$parallelSystems[[1]]$PK$default
  ratio <- (egfrCKDEPI2009(50, "male", 3) / egfrCKDEPI2009(50, "male", 1))^0.554
  lo <- meloxicam(70, 170, 50, "male", creatinine = 1)$parallelSystems[[1]]$PK$default
  expect_equal(hi$cl1 / lo$cl1, ratio)
})

test_that("each route goes to its own fit and the two add", {
  sim <- function(d) {
    x <- simulateDrugsWithCovariates(d, noEvents, 70, 170, 40, "male", 72 * 60,
                                     TRUE)$meloxicam$results
    x[x$Site == "Plasma", ]
  }
  iv <- sim(data.frame(Drug = "meloxicam", Time = 0, Dose = 30, Units = "mg"))
  # 30 mg bolus on the ANJESO model alone: the AUC to 72 h, by the closed
  # form, is 30 / CL less the tail
  PK <- getDrugPK("meloxicam", 70, 170, 40, "male")$parallelSystems[[1]]$PK$default
  auc <- sum(diff(iv$Time) * (head(iv$Y, -1) + tail(iv$Y, -1)) / 2)
  tail72 <- iv$Y[nrow(iv)] / min(PK$lambda_1, PK$lambda_2, PK$lambda_3)
  expect_equal(auc + tail72, 30 / PK$cl1, tolerance = 0.01)
  expect_equal(iv$Y[which.min(abs(iv$Time - 60))], 3.91, tolerance = 0.005)

  po <- sim(data.frame(Drug = "meloxicam", Time = 60, Dose = 7.5, Units = "mg PO"))
  both <- sim(data.frame(Drug = "meloxicam", Time = c(0, 60), Dose = c(30, 7.5),
                         Units = c("mg", "mg PO")))
  t <- both$Time[both$Time > 0]
  at <- function(x) approx(x$Time, x$Y, t)$y
  expect_equal(at(both), at(iv) + at(po), tolerance = 0.002)
})
