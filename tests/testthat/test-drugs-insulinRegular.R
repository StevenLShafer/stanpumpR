# insulinRegular: see R/insulinTham.R (Tham 2017, Table 2).
test_that("Table 2 values, scaled by the library factors", {
  off <- insulinRegular(90, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(off$v1, 178); expect_equal(off$cl1, 127 / 60)
  expect_equal(off$ka_SC, 0.67 / 60); expect_equal(off$tlag_SC, 0)
  size <- pkSizeFactors(90, 170, 40, "male", TRUE, legacyVolume = 1)
  on <- insulinRegular(90, 170, 40, "male")$PK$default
  expect_equal(on$v1, 178 * size$volume); expect_equal(on$cl1, 127 * size$clearance / 60)
  expect_null(insulinRegular(70, 170, 40, "male")$zeroOrderSC)
})
test_that("10 U peaks at about 20 mcU/mL some 87 minutes after injection", {
  PK <- getDrugPK("insulinRegular", 70, 170, 35, "male")
  w <- simCpCe(data.frame(Drug = "insulinRegular", Time = 0, Dose = 10, Units = "U SC"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 600, FALSE)$wide
  ka <- 0.67 / 60; k <- 127 / 178 / 60
  tmax <- log(ka / k) / (ka - k)
  cmax <- 10000 * ka / (178 * (ka - k)) * (exp(-k * tmax) - exp(-ka * tmax))
  expect_equal(max(w$Plasma), cmax, tolerance = 1e-3)
  expect_equal(cmax, 20.0, tolerance = 0.01)
  expect_equal(tmax, 86.8, tolerance = 0.01)
  # units offered: injections and schedules, no pump, no mass, no IV
  units <- getDrugDefaults("insulinRegular")$Units[[1]]
  expect_false(any(units %in% c(insulinRateUnits, bolusUnits, infusionUnits)))
})
