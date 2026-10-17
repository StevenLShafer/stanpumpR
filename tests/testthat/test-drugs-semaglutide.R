# semaglutide: see R/drugs_semaglutide.R (Carlsson Petri 2018).
test_that("the reference subject, switch off, has the published parameters", {
  pk <- semaglutide(85, 165, 50, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$cl1, 0.0478 / 60)
  expect_equal(pk$v1, 12.2)
  expect_equal(pk$ka_SC, 0.0286 / 60)
  expect_equal(pk$bioavailability_SC, 1)
})
test_that("weight acts on clearance as (WT/85)^0.78, reproducing the paper's exposures", {
  cl <- function(w) semaglutide(w, 170, 50, "female", adjustToFFM = FALSE)$PK$default$cl1
  expect_equal(cl(85) / cl(55), 1.40, tolerance = 0.01)   # 55 kg: 40% more exposure
  expect_equal(cl(85) / cl(127), 0.73, tolerance = 0.01)  # 127 kg: 27% less
})
test_that("switch on: the pharmacokinetic weight and the library volume factor", {
  size <- pkSizeFactors(85, 165, 50, "female", TRUE, legacyVolume = 1)
  pk <- semaglutide(85, 165, 50, "female")$PK$default
  expect_equal(pk$cl1, 0.0478 * (size$pkWeight / 85)^0.78 / 60)
  expect_equal(pk$v1, 12.2 * size$volume)
  expect_equal_rounded(pk$cl1 * 60, 0.0375863)
})
test_that("steady state at 1 mg weekly matches the published 29.8 nmol/L", {
  PK <- getDrugPK("semaglutide", 85, 165, 50, "female", adjustToFFM = FALSE)
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  d <- data.frame(Drug = "semaglutide", Time = 0, Dose = 1, Units = "mg SC qwk")
  w <- simCpCe(d, noEvents, PK, 20 * MINS_PER_WEEK, FALSE)$wide
  last <- w$Time >= 19 * MINS_PER_WEEK
  t <- w$Time[last]; cp <- w$Plasma[last]
  cavg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_WEEK
  nmol <- cavg / 4.11358
  expect_equal(nmol, 30.3, tolerance = 0.01)     # model; paper 29.8 (CI 29.4-30.2)
  expect_equal(nmol, 29.8, tolerance = 0.03)
})
test_that("units: SC only, weekly offered", {
  units <- getDrugDefaults("semaglutide")$Units[[1]]
  expect_equal(units, c("mg SC", "mg SC qwk"))
  expect_true("semaglutide" %in% LONG_TERM_DRUGS)
})
