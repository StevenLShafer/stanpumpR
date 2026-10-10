# olanzapine: see R/drugs_olanzapine.R (Sun 2021).  Hand-worked pins.

noEvents <- data.frame(Time = numeric(0), Event = character(0))

test_that("the reference patient receives Sun's published values", {
  # 70 kg, 36-year-old man with the switch off: every covariate term is 1
  actual <- olanzapine(70, 170, 36, "male", adjustToFFM = FALSE)
  expected <- list(
      PK = list(default = list(
        v1 = 656, v2 = 225, v3 = 1,
        cl1 = 15.5 / 60, cl2 = 6.15 / 60, cl3 = 0,
        ka_PO = 0.861 / 60, bioavailability_PO = 1, tlag_PO = 0.782 * 60
      )),
      tPeak = 0, MEAC = 0,
      typical = 0, upperTypical = 0, lowerTypical = 0,
      reference = actual$reference
    )
  expect_equal_rounded(actual, expected)
})

test_that("weight, age and sex enter as Sun printed them", {
  x <- olanzapine(100, 170, 60, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(x$v1, 656 * (100 / 70) * (60 / 36)^0.356)
  expect_equal(x$cl1, 15.5 / 60 * (100 / 70)^0.75 * 0.862)
  # Vp and Q carry no covariates in the source
  expect_equal(x$v2, 225)
  expect_equal(x$cl2, 6.15 / 60)
})

test_that("with the switch on the model runs at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pkWeight = 70 x 1.3049067
  x <- olanzapine(120, 170, 50, "male")$PK$default
  expect_equal_rounded(x$v1, 656 * 1.3049067 * (50 / 36)^0.356)
  expect_equal_rounded(x$cl1, 15.5 / 60 * 1.2209126)
})

test_that("nothing reaches plasma before the 0.782 h lag", {
  w <- simulateDrugsWithCovariates(
    data.frame(Drug = "olanzapine", Time = 0, Dose = 10, Units = "mg PO"),
    noEvents, 70, 170, 36, "male", 600, FALSE, adjustToFFM = FALSE
  )$olanzapine$wide
  expect_true(all(w$Plasma[w$Time < 46.9] == 0))
  expect_gt(max(w$Plasma[w$Time > 60]), 0)
  # Peak about 5 h after 10 mg, 11 to 13 ng/mL: the label's 10 mg Cmax range
  expect_gt(w$Time[which.max(w$Plasma)], 240)
  expect_lt(w$Time[which.max(w$Plasma)], 360)
})
