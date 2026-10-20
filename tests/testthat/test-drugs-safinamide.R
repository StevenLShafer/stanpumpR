# safinamide: see R/drugs_safinamide.R (FDA Pharmacometrics Review of NDA
# 207145, Table 1, Model 136).

test_that("Table 1 at the reference (70 kg, 60 y), White relative F", {
  p <- safinamide(70, 170, 60, "male", FALSE)$PK$default
  expect_equal(p$cl1 * 60, 4.54 / 1.04)
  expect_equal(p$v1, 168 / 1.04)
  expect_equal(p$ka_PO * 60, 1.19)
})

test_that("the review's quoted ranges: weight 35-134 kg, age 30-80", {
  cl <- function(w, a) safinamide(w, 170, a, "male", FALSE)$PK$default$cl1
  v  <- function(w) safinamide(w, 170, 60, "male", FALSE)$PK$default$v1
  # "CL/F ranged from 41% lower to 63% higher, and Vd/F ... 50% lower to 91% higher"
  expect_equal(round(cl(35, 60) / cl(70, 60) - 1, 2), -0.41)
  expect_equal(round(cl(134, 60) / cl(70, 60) - 1, 2), 0.63)
  expect_equal(round(v(35) / v(70) - 1, 2), -0.50)
  expect_equal(round(v(134) / v(70) - 1, 2), 0.91)
  # "Over the age range of 30 to 80 years, there was an 18% decrease"
  expect_equal(round(1 - cl(70, 80) / cl(70, 30), 2), 0.18)
  # the negative power of age sees at least ADULT_AGE_TERM_FLOOR
  expect_equal(cl(70, 5), cl(70, ADULT_AGE_TERM_FLOOR))
})

test_that("half-life, peak and steady state against the label and Jing 2023", {
  expect_equal(pdTerminalHalfLife("safinamide", 70, 170, 60, "male", adjustToFFM = FALSE),
               25.6, tolerance = 0.005)
  one <- pdSimulate(pdRegimen("safinamide", "mg PO", 100, 1440, 1), maximum = 1440)
  expect_gt(pdPeak(one)[["t"]], 2.5); expect_lt(pdPeak(one)[["t"]], 4)
  ss <- pdSimulate(pdRegimen("safinamide", "mg PO", 100, 1440, 14), maximum = 14 * 1440)
  # 100 mg/day: dose / CL/F = 22,900 ng.h/mL (healthy Chinese adults: 23,100)
  expect_equal(pdAUC(ss, 13 * 1440, 14 * 1440), 100 / (4.54 / 1.04) * 1000, tolerance = 0.005)
})
