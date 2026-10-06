# Naloxone: Dowling 2008 intravenous disposition with clearance on lean body
# weight, a nasal-spray route derived from Laffont 2024, and ke0 from Yassen
# 2007.  See the header of R/drugs_naloxone.R.  Pins were worked out from the
# published numbers by hand (Python), not from the code under test.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # Clearance uses the Janmahasatian lean body weight in either switch
  # position (70 kg, 171 cm male: 54.75 kg), so it is not 91 L/h here.
  actual <- naloxone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 2.87,
        v2 = 1.49,
        v3 = 33.6,
        cl1 = 1.2615039,
        cl2 = 0.094333333,
        cl3 = 0.49666667,
        ka_IN = 0.019188856,
        bioavailability_IN = 0.19040215,
        tlag_IN = 4.302
      )
    ),
    tPeak = 0,
    ke0 = 0.1066380,
    MEAC = 0,
    typical = 3,
    upperTypical = 10,
    lowerTypical = 1,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg.  Clearance follows its own lean
  # body weight covariate, 91 x (71.09/70)^0.75 L/h; Vc sees the
  # pharmacokinetic weight 91.34 kg; the size-free peripheral parameters take
  # the library factors 1.3049067 and 1.2209126.
  actual <- naloxone(120, 170, 50, "male")
  expected <- list(
    v1 = 3.7450821,
    v2 = 1.9443109,
    v3 = 43.844863,
    cl1 = 1.5342649,
    cl2 = 0.11517276,
    cl3 = 0.6063866
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Dowling's vector is recovered at a lean body weight of 70 kg", {
  # CL is normalised to LBW 70 kg, not to the reference man.  A man whose
  # Janmahasatian lean body weight is 70 kg has CL 91 L/h.
  x <- naloxone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  lbw <- ffmAlSallami(70, 171, 50, "male")
  expect_equal(x$cl1 * 60, 91 * (lbw / 70)^0.75)
  expect_equal(x$v1, 2.87); expect_equal(x$v2, 1.49); expect_equal(x$v3, 33.6)
  expect_equal(x$cl2 * 60, 5.66); expect_equal(x$cl3 * 60, 29.8)
  # With the switch off, Vc scales with total weight and the rest is fixed
  y <- naloxone(140, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(y$v1, 5.74); expect_equal(y$v3, 33.6); expect_equal(y$cl3, x$cl3)
})

test_that("the nasal route reproduces Laffont's fitted exposure for the reference man", {
  # Laffont 2024: CL/F 396 L/h for the 4 mg spray, so AUC = D / 396.  F is
  # anchored on the reference man's clearance, 91 x (54.48/70)^0.75 = 75.4 L/h.
  x <- naloxone(70, 170, 35, "male")$PK$default
  expect_equal(x$cl1 * 60, 75.39925, tolerance = 1e-6)
  expect_equal(x$bioavailability_IN, 75.39925 / 396, tolerance = 1e-6)
  # Lag 0.0717 h; ka matches the mean input time of the whole mixture, 0.94 h
  expect_equal(x$tlag_IN, 0.0717 * 60)
  expect_equal(x$tlag_IN + 1 / x$ka_IN, 0.9402597 * 60, tolerance = 1e-6)
  PK <- getDrugPK("naloxone", 70, 170, 35, "male", getDrugDefaults("naloxone"))$PK$default
  aucIN <- sum(c(PK$p_coef_IN_l1, PK$p_coef_IN_l2, PK$p_coef_IN_l3, PK$p_coef_IN_ka) /
               c(PK$lambda_1, PK$lambda_2, PK$lambda_3, PK$ka_IN))
  # per unit dose in mcg (ng/mL concentration), aucIN is in min/L: 1/396 h/L
  expect_equal(aucIN / 60, 1 / 396, tolerance = 1e-6)
})

test_that("ke0 is Yassen's, and the effect site peaks a few minutes after a bolus", {
  PK <- getDrugPK("naloxone", 70, 170, 35, "male", getDrugDefaults("naloxone"))
  pk <- PK$PK$default
  expect_equal(pk$ke0, 6.39828 / 60)
  expect_equal(log(2) / pk$ke0, 6.5, tolerance = 1e-3)      # half-time, min
  tPeak <- effectSitePeakTime(
    c(pk$p_coef_bolus_l1, pk$p_coef_bolus_l2, pk$p_coef_bolus_l3),
    c(pk$lambda_1, pk$lambda_2, pk$lambda_3), pk$ke0)
  expect_equal(tPeak, 3.43, tolerance = 0.01)
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "naloxone", Time = 0, Dose = 0.4, Units = "mg"),
    noEvents, 70, 170, 35, "male", 120, TRUE)
  w <- o$naloxone$wide
  expect_false(any(is.na(w$"Effect Site")))
  expect_gt(max(w$"Effect Site"), 0)
})

test_that("a nasal dose is accepted and peaks after the lag", {
  o <- simulateDrugsWithCovariates(
    data.frame(Drug = "naloxone", Time = 0, Dose = 4, Units = "mg IN"),
    noEvents, 70, 170, 35, "male", 240, FALSE)
  w <- o$naloxone$wide
  expect_gt(w$Time[which.max(w$Plasma)], 4.3)
  # Narcan 4 mg: label peak about 4.8 ng/mL; this input is a little faster
  expect_gt(max(w$Plasma), 3)
  expect_lt(max(w$Plasma), 10)
})
