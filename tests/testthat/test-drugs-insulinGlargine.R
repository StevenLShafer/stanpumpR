# insulinGlargine: see R/insulinTham.R (Tham 2017, Basaglar fit).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
test_that("82.2% first-order at 0.0830/h, 17.8% zero-order over 0.612 h", {
  m <- insulinGlargine(71.4, 170, 35, "male", FALSE)
  expect_equal(m$PK$default$ka_SC, 0.0830 / 60)
  expect_equal(m$PK$default$v1, 768); expect_equal(m$PK$default$cl1, 74.5 / 60)
  expect_equal(m$zeroOrderSC$fraction, 1 - 0.822)
  expect_equal(m$zeroOrderSC$duration, 0.612 * 60)
})
test_that("weight on V (linear, from 71.4 kg) and dose on F (from 35 U)", {
  expect_equal(insulinGlargine(90, 170, 35, "male", FALSE)$PK$default$v1,
               768 * (1 + 0.00728 * (90 - 71.4)))
  expect_equal(insulinGlargine(30, 150, 35, "male", FALSE)$PK$default$v1,
               768 * (1 + 0.00728 * (42.8 - 71.4)))      # held at the studied 42.8 kg
  f <- insulinGlargine(71.4, 170, 35, "male", FALSE)$scDoseBioavailability
  expect_equal(doseSizeFactor(c(14, 35, 58), f), (c(14, 35, 58) / 35)^-0.300)
  expect_equal(doseSizeFactor(c(5, 100), f), (c(14, 58) / 35)^-0.300)
})
test_that("once daily: average steady-state concentration is F x dose / (CL x 24 h)", {
  PK <- getDrugPK("insulinGlargine", 71.4, 170, 35, "male", adjustToFFM = FALSE)
  avg <- function(D) {
    w <- simCpCe(data.frame(Drug = "insulinGlargine", Time = 0, Dose = D, Units = "U SC qd"),
                 noEvents, PK, 14 * MINS_PER_DAY, FALSE)$wide
    last <- w$Time >= 13 * MINS_PER_DAY
    t <- w$Time[last]; cp <- w$Plasma[last]
    sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_DAY
  }
  expect_equal(avg(35), 35000 / (74.5 * 24), tolerance = 0.01)
  expect_equal(avg(20), (20 / 35)^-0.300 * 20000 / (74.5 * 24), tolerance = 0.01)
  expect_false(any(getDrugDefaults("insulinGlargine")$Units[[1]] %in% insulinRateUnits))
})
