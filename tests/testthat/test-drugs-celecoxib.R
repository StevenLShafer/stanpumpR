# Celecoxib: two-compartment apparent oral model fitted to the FDA mean curve
# (NDA 211759 review, Study 915/22), weight exponents from Krishnaswami 2012,
# ke0 from Hannam 2023.  See the header of R/drugs_celecoxib.R.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the fitted parameters for a 70 kg adult with the switch off", {
  actual <- celecoxib(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(default = list(
      v1 = 237, v2 = 413, v3 = 1,
      cl1 = 36.9 / 60, cl2 = 72.0 / 60, cl3 = 0,
      ka_PO = 0.985 / 60, bioavailability_PO = 1, tlag_PO = 0.853 * 60
    )),
    tPeak = 0, ke0 = log(2) / (1.12 * 60), MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("weight scales clearances by ^0.265 and volumes by ^0.499 (Krishnaswami)", {
  off <- celecoxib(120, 170, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$cl1, 36.9 / 60 * (120 / 70)^0.265)
  expect_equal(off$cl2, 72.0 / 60 * (120 / 70)^0.265)
  expect_equal(off$v1, 237 * (120 / 70)^0.499)
  expect_equal(off$v2, 413 * (120 / 70)^0.499)
  # with the switch on, at the pharmacokinetic weight
  on <- celecoxib(120, 170, 50, "male")$PK$default
  pkw <- pkSizeFactors(120, 170, 50, "male", TRUE)$pkWeight
  expect_equal(on$cl1, 36.9 / 60 * (pkw / 70)^0.265)
  expect_equal(on$v1, 237 * (pkw / 70)^0.499)
  # the label: 10 kg and 25 kg have 40% and 24% lower CL/F than 70 kg
  cl <- function(w) celecoxib(w, 120, 8, "female", adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(cl(10) / cl(70), 0.60, tolerance = 0.01)
  expect_equal(cl(25) / cl(70), 0.76, tolerance = 0.01)
})

test_that("200 mg reproduces the digitized FDA mean curve", {
  # The 19 points the fit was made to (Figure 1, Celebrex capsule, fasting)
  d <- data.frame(
    t = c(1.26, 1.47, 1.89, 2.57, 3.24, 4.17, 4.58, 5.10, 6.04,
          8, 10, 12, 14, 24, 36, 48),
    C = c(257, 359, 404, 412, 404, 361, 333, 264, 224,
          167, 138, 127, 114, 82, 45, 23))
  x <- simulateDrugsWithCovariates(
    data.frame(Drug = "celecoxib", Time = 0, Dose = 200, Units = "mg PO"),
    noEvents, 70, 171, 35, "male", 72 * 60, FALSE, adjustToFFM = FALSE)$celecoxib$results
  cp <- x[x$Site == "Plasma", ]
  pred <- approx(cp$Time / 60, cp$Y, d$t)$y
  expect_true(all(abs(pred / d$C - 1) < 0.16))
  expect_lt(mean(abs(pred / d$C - 1)), 0.07)
  # AUC(0-72 h) against the digitized 5424 and the review's 5379 ng.h/mL
  auc <- sum(diff(cp$Time) * (head(cp$Y, -1) + tail(cp$Y, -1)) / 2) / 60
  expect_equal(auc, 5379, tolerance = 0.05)
  # terminal half-life against the review's 14.4 h
  late <- cp[cp$Time >= 36 * 60, ]
  slope <- coef(lm(log(late$Y) ~ late$Time))[2]
  expect_equal(unname(log(2) / -slope / 60), 14.4, tolerance = 0.06)
})

test_that("apparent clearance matches the independent studies", {
  # Itthipanichpong 2005: 35.9 L/h in men averaging 63 kg
  expect_equal(celecoxib(63, 170, 21, "male", FALSE)$PK$default$cl1 * 60, 35.9,
               tolerance = 0.01)
  # NCT04526197: 200 mg, AUC(0-inf) 6743 ng.h/mL (CV 38%) at 75 kg
  auc <- 200000 / (celecoxib(75, 169, 34, "male", FALSE)$PK$default$cl1 * 60)
  expect_gt(auc, 6743 * (1 - 0.38))
  expect_lt(auc, 6743)
})

test_that("the effect site has Hannam's 1.12 h equilibration half-time", {
  PK <- getDrugPK("celecoxib", 70, 170, 40, "male")$PK$default
  expect_equal(log(2) / PK$ke0 / 60, 1.12)
  expect_equal(getDrugDefaults("celecoxib")$endCe, 242)
})
