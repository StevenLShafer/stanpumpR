# dapagliflozinKobuchi: see R/drugs_dapagliflozinKobuchi.R (Kobuchi 2025, Table 2).
test_that("Table 2 at the 77 kg reference, switch off", {
  pk <- dapagliflozinKobuchi(77, 167, 59, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(pk$cl1 * MINS_PER_DAY, 229.3)
  expect_equal(pk$v1, 73.9)
  expect_equal(pk$ka_PO * MINS_PER_DAY, 57.4)
  heavy <- dapagliflozinKobuchi(118, 167, 59, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(heavy$cl1 / pk$cl1, (118 / 77)^0.41)
})
test_that("switch on: the pharmacokinetic weight and the library volume factor", {
  size <- pkSizeFactors(90, 167, 59, "female", TRUE, legacyVolume = 1)
  pk <- dapagliflozinKobuchi(90, 167, 59, "female")$PK$default
  expect_equal(pk$cl1 * MINS_PER_DAY, 229.3 * (size$pkWeight / 77)^0.41)
  expect_equal(pk$v1, 73.9 * size$volume)
})
test_that("the steady-state trough on 5 mg daily is the fitted quantity", {
  PK <- getDrugPK("dapagliflozinKobuchi", 77, 167, 59, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "dapagliflozinKobuchi", Time = 0, Dose = 5, Units = "mg PO qd"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 7 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 6 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  cavg <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / MINS_PER_DAY
  expect_equal(cavg, 5000 / 229.3, tolerance = 0.005)   # dose / (CL/F x 1 day), ng/mL
})
