# Fluconazole (R/drugs_fluconazole.R): Vuong 2025, Table 2 and Equations 3-4.
# Drafted by Claude Code, 2026-11-26.  The expectations are computed here from
# the paper's equations and Inker 2021's CKD-EPI 2021 equation, not from the
# drug file.

events <- data.frame(Time = numeric(0), Event = character(0), Fill = character(0))
ckdepi2021 <- function(age, sex, scr) {
  k <- if (sex == "female") 0.7 else 0.9
  a <- if (sex == "female") -0.241 else -0.302
  142 * min(scr / k, 1)^a * max(scr / k, 1)^-1.200 * 0.9938^age *
    (if (sex == "female") 1.012 else 1)
}
vuongCL <- function(egfr) 0.614 * (egfr / 80)^0.532
vuongVc <- function(w) 39.4 * (w / 80)^0.908

test_that("CKD-EPI 2021 is Inker's equation", {
  expect_equal(egfrCKDEPI2021(50, "male", 1.2), ckdepi2021(50, "male", 1.2))
  expect_equal(egfrCKDEPI2021(40, "female", 0.6), ckdepi2021(40, "female", 0.6))
})

test_that("Vuong's equations, switch off", {
  set <- fluconazole(85, 175, 60, "male", adjustToFFM = FALSE, creatinine = 1.2)$PK$default
  expect_equal(set$cl1 * 60, vuongCL(ckdepi2021(60, "male", 1.2)))
  expect_equal(set$v1, vuongVc(85))
  expect_equal(set$cl2 * 60, 12.1)
  expect_equal(set$v2, 8.47)
  set <- fluconazole(60, 160, 45, "female", adjustToFFM = FALSE, creatinine = 0.7)$PK$default
  expect_equal(set$cl1 * 60, vuongCL(ckdepi2021(45, "female", 0.7)))
  expect_equal(set$v1, vuongVc(60))
})

test_that("the source's typical patient gets the published values", {
  # eGFR 80, 80 kg, switch off
  scr <- uniroot(function(s) ckdepi2021(62, "male", s) - 80, c(0.5, 2))$root
  set <- fluconazole(80, 175, 62, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(set$cl1 * 60, 0.614, tolerance = 1e-6)
  expect_equal(set$v1, 39.4)
})

test_that("the switch on reads the pharmacokinetic weight and the size factors", {
  size <- pkSizeFactors(120, 170, 50, "female", TRUE)
  on <- fluconazole(120, 170, 50, "female", creatinine = 0.8)$PK$default
  expect_equal(on$v1, vuongVc(size$pkWeight))
  expect_equal(on$cl1 * 60, vuongCL(ckdepi2021(50, "female", 0.8)) * size$clearance)
  expect_equal(on$v2, 8.47 * size$volume)
  expect_equal(on$cl2 * 60, 12.1 * size$clearance)
  ref <- fluconazole(70, 170, 35, "male")$PK$default
  expect_equal_rounded(0.7420773, ref$cl1 * 60)
  expect_equal_rounded(34.90113, ref$v1)
})

test_that("the oral route: the label's F and McLachlan's ka", {
  set <- fluconazole(70, 170, 35, "male")$PK$default
  expect_equal(set$bioavailability_PO, 0.9)
  expect_equal(set$ka_PO * 60, 3.07)
  dt <- data.frame(Drug = "fluconazole", Time = 0, Dose = 400, Units = "mg PO")
  w <- simulateDrugsWithCovariates(dt, events, 70, 170, 35, "male", 72 * 60, FALSE)$fluconazole$wide
  auc <- sum(diff(w$Time) * (head(w$Plasma, -1) + tail(w$Plasma, -1)) / 2) / 60
  # AUC to infinity = F x dose / CL; AUC to 72 h is below it
  expect_lt(auc, 0.9 * 400 / (set$cl1 * 60))
  expect_gt(max(w$Plasma), 0)
})

test_that("pregnancy raises the eGFR only when the creatinine is blank", {
  base <- fluconazole(70, 165, 30, "female")$PK$default
  preg <- fluconazole(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(preg$cl1 / base$cl1, PREGNANCY_GFR_FACTOR^0.532)
  expect_equal(preg$v1, base$v1)
  expect_equal(fluconazole(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK,
               fluconazole(70, 165, 30, "female", creatinine = 0.6)$PK)
})

test_that("fluconazole offers intravenous and oral units, and no band", {
  dd <- getDrugDefaultsGlobal()
  units <- unlist(dd$Units[dd$Drug == "fluconazole"])
  expect_true(all(c("mg", "mg/hr", "mg PO", "mg PO qd", "mg/kg PO qd") %in% units))
  expect_equal(dd$Upper[dd$Drug == "fluconazole"], 0)
})
