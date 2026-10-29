# Methylene blue (R/drugs_methyleneBlue.R): two compartments derived exactly
# from the ProvayBlue label and the Proveblue SmPC, checked against
# Walter-Sack 2009.  Drafted by Claude Code, 2026-10-28.

events <- data.frame(Time = double(), Event = character())
trapezoid <- function(t, y) sum(diff(t) * (head(y, -1) + tail(y, -1)) / 2)

test_that("the derived model reproduces the label's and the SmPC's four numbers", {
  p <- methyleneBlueFromLabel()
  dose <- 2 * 70
  # Vss and clearance
  expect_equal(p$v1 + p$v2, 255, tolerance = 1e-8)
  expect_equal(p$cl1 * 60, dose / 13.977, tolerance = 1e-8)
  # terminal half-life 24.7 h
  expect_equal(log(2) / p$lambda[2] / 60, 24.7, tolerance = 1e-8)
  # the exponents and coefficients are the model's own
  set <- getDrugPK("methyleneBlue", 70, 175, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(c(set$lambda_1, set$lambda_2), p$lambda, tolerance = 1e-6)
  # the published values, to the precision the header and help page print
  expect_equal(round(c(p$v1, p$v2, p$cl1 * 60, p$cl2 * 60), 2),
               c(46.83, 208.17, 10.02, 17.77))
  expect_equal(round(log(2) / p$lambda[1] / 60, 2), 1.07)
  # 30% of the area in the fast phase
  expect_equal(round(p$A[1] / p$lambda[1] / sum(p$A / p$lambda), 2), 0.30)
})

test_that("the label's infusion gives the label's Cmax and AUC through the engine", {
  # 2 mg/kg as a 5-minute infusion at 70 kg, plotted to 30 days
  sim <- simulateDrugsWithCovariates(
    data.frame(Drug = "methyleneBlue", Time = c(0, 5), Dose = c(140 / 5, 0),
               Units = "mg/min"),
    events, 70, 175, 40, "male", 43200, FALSE, adjustToFFM = FALSE)$methyleneBlue$wide
  expect_equal(sim$Plasma[sim$Time == 5], 2917, tolerance = 1e-4)
  expect_equal(max(sim$Plasma), 2917, tolerance = 1e-4)
  # AUC: 30 days is 29 half-lives, so the tail beyond is negligible
  expect_equal(trapezoid(sim$Time, sim$Plasma) / 60, 13977, tolerance = 0.01)
})

test_that("Walter-Sack 2009 (not fitted) measured a third lower clearance", {
  # 50 mg over 9.5 min, mean weight 67.1 kg; Table 1 plasma AUC(0-24) 4928,
  # AUC(0-inf) 7639 ng h/mL.  Time 0 in the paper is the end of the injection.
  sim <- simulateDrugsWithCovariates(
    data.frame(Drug = "methyleneBlue", Time = c(0, 9.5), Dose = c(50 / 9.5, 0),
               Units = "mg/min"),
    events, 67.1, 173, 26, "male", 9.5 + 24 * 60, FALSE,
    adjustToFFM = FALSE)$methyleneBlue$wide
  in24 <- sim$Time >= 9.5
  auc24 <- trapezoid(sim$Time[in24], sim$Plasma[in24]) / 60
  expect_equal(round(auc24), 3263, tolerance = 0.005)
  expect_equal(round(auc24 / 4928, 2), 0.66)
  # and the infinite area, 50 mg over this patient's clearance
  set <- getDrugPK("methyleneBlue", 67.1, 173, 26, "male", adjustToFFM = FALSE)$PK$default
  aucInf <- 50 / (set$cl1 * 60) * 1000
  expect_equal(round(aucInf), 5208, tolerance = 0.005)
  expect_equal(round(aucInf / 7639, 2), 0.68)
  # per kilogram: 0.143 against Walter-Sack's 0.098 L/h/kg
  expect_equal(round(set$cl1 * 60 / 67.1, 3), 0.143)
  expect_equal(round(50 / 7.639 / 67.1, 3), 0.098)
  # The model's plasma peaks at the end of the injection; Walter-Sack's peaked
  # 30 min later (mean Cmax 748 ng/mL), which no mammillary model reproduces
  expect_equal(sim$Time[which.max(sim$Plasma)], 9.5)
})

test_that("the fat-free-mass switch scales the 70-kg values", {
  p <- methyleneBlueFromLabel()
  size <- pkSizeFactors(85, 180, 50, "male", TRUE)
  set <- getDrugPK("methyleneBlue", 85, 180, 50, "male")$PK$default
  expect_equal_rounded(set$v1, p$v1 * size$volume)
  expect_equal_rounded(set$cl1, p$cl1 * size$clearance)
  off <- getDrugPK("methyleneBlue", 85, 180, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(off$v1, p$v1 * 85 / 70)
  expect_equal_rounded(off$cl2, p$cl2 * 85 / 70)
})

test_that("methylene blue is a vasoactive drug with no band and no effect site", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "methyleneBlue", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("methyleneBlue", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "PROVAYBLUE")
  expect_match(PK$reference, "Walter-Sack")
  # no renal term is applied
  expect_false("creatinine" %in% names(formals(methyleneBlue)))
})
