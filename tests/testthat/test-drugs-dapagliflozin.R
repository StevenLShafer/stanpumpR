# dapagliflozin: see R/drugs_dapagliflozin.R (Melin 2023, Supplementary
# Table 4 and its control stream).
noEvents <- data.frame(Time = numeric(0), Event = character(0))
# creatinine giving a CKD-EPI 2009 eGFR of 83 (the reference) at 50 years
scr83 <- uniroot(function(s) egfrCKDEPI2009(50, "male", s) - 83, c(0.3, 3), tol = 1e-12)$root

test_that("Supplementary Table 4 for the reference man (89 kg, eGFR 83)", {
  pk <- dapagliflozin(89, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr83)$PK$default
  expect_equal(pk$cl1 * 60, 23.0, tolerance = 1e-8)
  expect_equal(c(pk$v1, pk$v2), c(73.9, 143))
  expect_equal(pk$cl2 * 60, 8.92)
  expect_equal(pk$ka_PO * 60, 2.39)
  expect_equal(pk$tlag_PO, 0.248 * 60)
})

test_that("eGFR, sex and weight as the control stream writes them", {
  f <- dapagliflozin(60, 165, 50, "female", FALSE, creatinine = 1.2)$PK$default
  egfr <- egfrCKDEPI2009(50, "female", 1.2)
  expect_equal(f$cl1 * 60, 23.0 * (egfr / 83)^0.584 * exp(-0.174))
  expect_equal(f$v1, 73.9 * (60 / 89)^0.570)
  # adolescents: bedside Schwartz at their own creatinine
  teen <- dapagliflozin(55, 160, 14, "male", FALSE, creatinine = 0.7)$PK$default
  expect_equal(teen$cl1 * 60, 23.0 * (0.413 * 160 / 0.7 / 83)^0.584)
  # switch on: the pharmacokinetic weight
  size <- pkSizeFactors(110, 175, 50, "male", TRUE)
  expect_equal(dapagliflozin(110, 175, 50, "male", creatinine = scr83)$PK$default$v1,
               73.9 * (size$pkWeight / 89)^0.570)
  expect_equal_rounded(dapagliflozin(70, 170, 40, "male")$PK$default$cl1 * 60, 26.5989588)
})

test_that("10 mg daily: the steady-state AUC is dose / CL/F, as the control stream's AUCS", {
  PK <- getDrugPK("dapagliflozin", 89, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr83)
  w <- simCpCe(data.frame(Drug = "dapagliflozin", Time = 0, Dose = 10, Units = "mg PO qd"),
               noEvents, PK, 10 * MINS_PER_DAY, FALSE)$wide
  last <- w$Time >= 9 * MINS_PER_DAY
  t <- w$Time[last]; cp <- w$Plasma[last]
  auc <- sum(diff(t) * (head(cp, -1) + tail(cp, -1)) / 2) / 60
  expect_equal(auc, 10000 / 23.0, tolerance = 0.005)
  expect_true(all(is.na(w$`Effect Site`)))
})

test_that("units: oral, daily", {
  expect_equal(getDrugDefaults("dapagliflozin")$Units[[1]], c("mg PO", "mg PO qd"))
  expect_true("creatinine" %in% names(formals(dapagliflozin)))
})
