# liraglutide: see R/drugs_liraglutide.R (Roge 2015).
test_that("per kilogram at total weight with the switch off", {
  pk <- liraglutide(80, 175, 55, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$v1, 0.112 * 80)
  expect_equal(pk$cl1, 1.83e-4 * 80)
  expect_equal(pk$ka_SC, 9.41e-4)
})
test_that("at the pharmacokinetic weight with the switch on", {
  size <- pkSizeFactors(80, 175, 55, "male", TRUE)
  pk <- liraglutide(80, 175, 55, "male")$PK$default
  expect_equal(pk$v1, 0.112 * size$pkWeight)
  expect_equal(pk$cl1, 1.83e-4 * size$pkWeight)
})
test_that("flip-flop: the curve peaks about 13 hours after a dose", {
  PK <- getDrugPK("liraglutide", 70, 170, 40, "male")
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  w <- simCpCe(data.frame(Drug = "liraglutide", Time = 0, Dose = 1.8, Units = "mg SC"),
               noEvents, PK, 2 * MINS_PER_DAY, FALSE)$wide
  k <- 1.83e-4 / 0.112; ka <- 9.41e-4
  expect_equal(log(ka / k) / (ka - k) / 60, 13.3, tolerance = 0.01)
  expect_equal(w$Time[which.max(w$Plasma)] / 60, 13.3, tolerance = 0.02)
})
