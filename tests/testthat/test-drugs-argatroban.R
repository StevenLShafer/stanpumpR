# Argatroban: Cox 2004 two-compartment intravenous model in PCI patients.
# See the header of R/drugs_argatroban.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters at the 87 kg median with the switch off", {
  actual <- argatroban(87, 176, 63, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 11.0, v2 = 13.0, v3 = 1,
      cl1 = 22.0 / 60, cl2 = 10.0 / 60, cl3 = 0
    )),
    tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("clearance carries the linear weight covariate about 87 kg", {
  # CL = 22.0 + 0.018 * (WT - 87); a 100 kg patient clears slightly faster.
  cl <- argatroban(100, 176, 63, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl, 22.0 + 0.018 * (100 - 87), tolerance = 1e-8)
  cl70 <- argatroban(70, 176, 63, "male", adjustToFFM = FALSE)$PK$default$cl1 * 60
  expect_equal(cl70, 22.0 + 0.018 * (70 - 87), tolerance = 1e-8)
})

test_that("the fat-free-mass switch evaluates the weight covariate at the pk weight", {
  on  <- argatroban(120, 176, 40, "male", adjustToFFM = TRUE)
  size <- pkSizeFactors(120, 176, 40, "male", TRUE, legacyVolume = 1)
  expect_equal(on$PK$default$cl1 * 60, 22.0 + 0.018 * (size$pkWeight - 87),
               tolerance = 1e-8)
  # V1, V2 and Q have no size term in the source, so they take the library
  # fat-free-mass factors, divided by Cox's cohort factors (ARGATROBAN_COHORT;
  # A2-F4).  The clearance's own weight term above is not anchored.
  kc <- cohortFactorsIndependent(ARGATROBAN_COHORT)
  expect_equal(kc$volume, 1.0675920, tolerance = 1e-6)
  expect_equal(kc$clearance, 1.0502773, tolerance = 1e-6)
  expect_equal(on$PK$default$v1, 11.0 * size$volume / kc$volume, tolerance = 1e-8)
  expect_equal(on$PK$default$v2, 13.0 * size$volume / kc$volume, tolerance = 1e-8)
  expect_equal(on$PK$default$cl2 * 60, 10.0 * size$clearance / kc$clearance, tolerance = 1e-8)
})

test_that("anchored at Cox's cohort: its typical patient receives the published V1, V2 and Q (A2-F4)", {
  # pp. 983-984: 106 men and 46 women, median 87 kg, mean 63 years; NCD-RisC
  # heights for the USA, born 1941, 175.6 cm (men) and 161.6 cm (women).  V1,
  # V2 and Q carry no sex term, so they are anchored at the pooled patient: the
  # cohort's man and woman, weighted 106:46 geometrically, receive the
  # published values.
  m <- argatroban(87, 175.6, 63, "male")$PK$default
  w <- argatroban(87, 161.6, 63, "female")$PK$default
  pooled <- function(x, y) exp((106 * log(x) + 46 * log(y)) / 152)
  expect_equal(pooled(m$v1, w$v1), 11.0, tolerance = 1e-6)
  expect_equal(pooled(m$v2, w$v2), 13.0, tolerance = 1e-6)
  expect_equal(pooled(m$cl2, w$cl2) * 60, 10.0, tolerance = 1e-6)
  # The switch off is unchanged: the published values
  off <- argatroban(87, 175.6, 63, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(off$v1, off$v2, off$cl2 * 60), c(11.0, 13.0, 10.0))
})

test_that("the half-lives at the 87 kg median are about 13 and 86 min", {
  p <- getDrugPK("argatroban", 87, 176, 63, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(log(2) / c(p$lambda_1, p$lambda_2), c(13.0, 86.4), tolerance = 0.005)
})

test_that("the plotted concentration is plasma only, with no effect site", {
  r <- getDrugPK("argatroban", 87, 176, 63, "male")
  expect_equal(r$tPeak, 0)
  expect_equal(r$MEAC, 0)
})

test_that("a weight-based infusion simulates a sensible steady state", {
  # 2 mcg/kg/min in an 87 kg patient approaches rate/CL = 2*87/(22/60) ng/mL.
  dose <- data.frame(Drug = "argatroban", Time = 0, Dose = 2, Units = "mcg/kg/min")
  r <- simulateDrugsWithCovariates(dose, noEvents, 87, 176, 63, "male",
                                   600, TRUE, adjustToFFM = FALSE)$argatroban
  pl <- r$results[r$results$Site == "Plasma", ]
  css <- 2 * 87 / (22 / 60)
  expect_equal(max(pl$Y[pl$Time > 540]), css, tolerance = 0.05 * css)
})
