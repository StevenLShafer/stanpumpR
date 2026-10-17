# insulinLispro: see R/insulinTham.R (Tham 2017, Table 2; zero-order routing assumed).
test_that("72.9% through a lagged depot, 27.1% zero-order over 1.06 h", {
  m <- insulinLispro(80, 175, 35, "male", FALSE)
  pk <- m$PK$default
  expect_equal(pk$ka_SC, 0.989 / 60); expect_equal(pk$tlag_SC, 0.265 * 60)
  expect_equal(pk$v1, 43.0); expect_equal(pk$cl1, 30.5 / 60)
  expect_equal(m$zeroOrderSC$fraction, 1 - 0.729)
  expect_equal(m$zeroOrderSC$duration, 1.06 * 60)
  expect_equal(m$zeroOrderSC$into, "central")
  p <- getDrugPK("insulinLispro", 70, 170, 35, "male", adjustToFFM = FALSE)
  expect_equal(p$PK$default$bioavailability_RA, 0.729)
  # weight on V/F, (BW/80)^2.48, held within the studied 52.6 to 86 kg
  expect_equal(insulinLispro(70, 170, 35, "male", FALSE)$PK$default$v1, 43 * (70 / 80)^2.48)
  expect_equal(insulinLispro(120, 170, 35, "male", FALSE)$PK$default$v1, 43 * (86 / 80)^2.48)
  expect_equal(insulinLispro(40, 150, 35, "male", FALSE)$PK$default$v1, 43 * (52.6 / 80)^2.48)
  expect_false(insulinLispro(70, 170, 35, "male")$doseDependent)
})
test_that("mass balance: AUC of an injection is dose / CL", {
  PK <- getDrugPK("insulinLispro", 70, 170, 35, "male", adjustToFFM = FALSE)
  w <- simCpCe(data.frame(Drug = "insulinLispro", Time = 0, Dose = 10, Units = "U SC"),
               data.frame(Time = numeric(0), Event = character(0)), PK, 2880, FALSE)$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60  # mcU.h/mL
  expect_equal(auc, 10000 / 30.5, tolerance = 0.01)
})
test_that("lispro alone offers the pump", {
  expect_true("U/hr SC" %in% getDrugDefaults("insulinLispro")$Units[[1]])
})
