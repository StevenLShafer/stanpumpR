# istradefylline: see R/drugs_istradefylline.R (Knebel 2011, Table 1 of the
# 2007-8 FDA pharmacometrics review in the NDA 022075 file).

test_that("Table 1 at the reference subject (James lean body mass)", {
  p <- istradefylline(70, 170, 62, "male", FALSE)$PK$default
  lbm <- lbmJames(70, 170, "male")
  expect_equal(p$cl1 * 60, 5.76 * (lbm / 55.6)^0.106)
  expect_equal(p$v1, 198)
  expect_equal(p$cl2 * 60, 21.6 * (lbm / 55.6)^1.23)
  expect_equal(p$v2, 307)
  expect_equal(p$ka_PO * 60, 0.464)
  # the James lean body mass of the reference man is close to 55.6 kg
  expect_equal(lbm, 55.6, tolerance = 0.01)
})

test_that("age and weight terms", {
  p <- istradefylline(100, 180, 80, "female", FALSE)$PK$default
  lbm <- lbmJames(100, 180, "female")
  expect_equal(p$cl1 * 60, 5.76 * (lbm / 55.6)^0.106 * (80 / 62)^-0.111)
  expect_equal(p$v1, 198 * (100 / 70)^-0.0521)
  expect_equal(p$v2, 307 * (100 / 70)^1.69)
  young <- istradefylline(70, 170, 5, "male", FALSE)$PK$default
  floor <- istradefylline(70, 170, ADULT_AGE_TERM_FLOOR, "male", FALSE)$PK$default
  expect_equal(young$cl1, floor$cl1)
})

test_that("the smoker and CYP3A4 factors are recorded but not applied", {
  expect_equal(round(1 - 1 / ISTRADEFYLLINE_SMOKER, 2), 0.39)          # AUC -38% (review)
  expect_equal(round(1 / ISTRADEFYLLINE_CYP3A4_INHIBITOR - 1, 2), 0.36) # AUC +35% (review)
  expect_false(any(c("smoker", "cyp3a4") %in% names(formals(istradefylline))))
})

test_that("half-life about 67 h (label 83) and a peak about 4 h after a tablet", {
  expect_equal(pdTerminalHalfLife("istradefylline", 70, 170, 62, "male", adjustToFFM = FALSE),
               67.0, tolerance = 0.01)
  pk <- pdPeak(pdSimulate(pdRegimen("istradefylline", "mg PO", 40, 1440, 1), maximum = 1440, age = 62))
  expect_gt(pk[["t"]], 3); expect_lt(pk[["t"]], 5)
})
