# tirzepatide: see R/drugs_tirzepatide.R (Schneck and Urva 2024).
test_that("the 70 kg reference gets Table 3's values", {
  # FFM + 0.482 FM = 70 is not the 70 kg man exactly; check the formula
  w <- 70; h <- 170; bmi <- w / (h / 100)^2
  ffm <- 9270 * w / (6680 + 216 * bmi)
  pk <- tirzepatide(w, h, 40, "male")$PK$default
  expect_equal(pk$cl1, 0.0329 / 60)
  expect_equal(pk$cl2, 0.126 / 60)
  expect_equal(pk$v1, 2.47 * (ffm + 0.482 * (w - ffm)) / 70)
  expect_equal(pk$v2, 3.98 * (ffm + 0.482 * (w - ffm)) / 70)
  expect_equal(pk$ka_SC, 0.0373 / 60)
  expect_equal(pk$bioavailability_SC, 0.8 * (1 - 0.181))
})
test_that("the model carries its own size covariates: the switch changes nothing", {
  expect_identical(tirzepatide(110, 165, 55, "female", TRUE),
                   tirzepatide(110, 165, 55, "female", FALSE))
})
test_that("the derived F reproduces the paper's 0.061 L/h apparent clearance at 90 kg", {
  pk <- tirzepatide(90, 170, 55, "female")$PK$default
  expect_equal(pk$cl1 * 60 / pk$bioavailability_SC, 0.061, tolerance = 0.01)
})
test_that("terminal half-life is about five and a half days", {
  pk <- getDrugPK("tirzepatide", 90, 170, 55, "female")$PK$default
  expect_equal(log(2) / pk$lambda_2 / MINS_PER_DAY, 5.4, tolerance = 0.1)
})
