# insulinGlargine: see R/insulinTham.R (Tham 2017, Basaglar fit).
test_that("82.2% first-order at 0.0830/h, 17.8% zero-order over 0.612 h", {
  m <- insulinGlargine(70, 170, 35, "male", FALSE)
  expect_equal(m$PK$default$ka_SC, 0.0830 / 60)
  expect_equal(m$PK$default$v1, 768); expect_equal(m$PK$default$cl1, 74.5 / 60)
  expect_equal(m$zeroOrderSC$fraction, 1 - 0.822)
  expect_equal(m$zeroOrderSC$duration, 0.612 * 60)
})
test_that("once daily: average steady-state concentration is dose / (CL x 24 h)", {
  PK <- getDrugPK("insulinGlargine", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinGlargine", Time = 0, Dose = 28, Units = "U SC qd"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 14 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 13 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  cavg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_DAY
  expect_equal(cavg, 28000 / (74.5 * 24), tolerance = 0.01)
  expect_false(any(getDrugDefaults("insulinGlargine")$Units[[1]] %in% insulinRateUnits))
})
