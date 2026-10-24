# Hydralazine (R/drugs_hydralazine.R): the mean of Reece 1980's ten
# intravenous two-compartment fits.  Drafted by Claude Code, 2026-10-24.

k <- HYDRALAZINE_REECE

test_that("each subject's printed half-lives, Varea and Vss follow from his constants", {
  # Table II: t1/2 alpha (min), t1/2 beta (h), Vd area and Vss (L/kg)
  tha <- c(8.76, 3.30, 3.06, 4.02, 4.86, 5.34, 4.26, 4.14, 4.56, 8.64)
  thb <- c(0.805, 0.523, 0.518, 0.701, 0.840, 0.893, 0.450, 0.474, 0.828, 0.675)
  varea <- c(7.67, 8.11, 6.84, 6.61, 7.03, 10.64, 7.57, 6.26, 7.98, 8.33)
  vss <- c(6.56, 5.58, 5.39, 4.90, 6.10, 7.86, 5.96, 4.44, 6.52, 7.08)
  expect_lt(max(abs(log(2) / k$alpha * 60 / tha - 1)), 0.01)
  expect_lt(max(abs(log(2) / k$beta / thb - 1)), 0.01)
  expect_lt(max(abs(k$CL / k$beta / varea - 1)), 0.01)
  s <- hydralazineSubjects()
  expect_true(all(s$v2 > 0 & s$cl2 > 0))
  expect_lt(max(abs((s$v1 + s$v2) / vss - 1)), 0.05)
})

test_that("the typical model reproduces the paper's means", {
  p <- hydralazineTypical()
  # Table II means over all ten: CL 8.315 L/h/kg, V1 2.768 L/kg, Vss 6.04
  expect_equal(p[["cl1"]] * 60, mean(k$CL))
  expect_equal(p[["v1"]], mean(k$V1))
  expect_lt(abs((p[["v1"]] + p[["v2"]]) / 6.04 - 1), 0.03)
  set <- getDrugPK("hydralazine", 70, 175, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(set$v1, p[["v1"]] * 70)
  expect_equal_rounded(set$cl1, p[["cl1"]] * 70)
  # half-lives against the means 5.1 min and 0.67 h
  expect_lt(abs(log(2) / set$lambda_1 / 5.09 - 1), 0.05)
  expect_lt(abs(log(2) / set$lambda_2 / 60 / 0.671 - 1), 0.06)
  expect_identical(getDrugPK("hydralazine", 70, 175, 30, "male")$Concentration.Units, "ng")
})

test_that("Reece's infusion gives the paper's AUC", {
  # 0.375 mg/kg over 5 min to a 73-kg subject; Table III AUC of hydralazine
  # 0.318 and 0.273 umol h/L in the two groups (mean 0.296), 160.17 g/mol
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "hydralazine", Time = c(0, 5), Dose = c(0.375 * 73 / 5, 0),
               Units = "mg/min"),
    data.frame(Time = double(), Event = character()),
    73, 178, 27, "male", 600, FALSE, adjustToFFM = FALSE)$hydralazine$wide
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2)
  aucUmolH <- auc / 60 / 160.17          # ng min/mL -> umol h/L
  expect_lt(abs(aucUmolH / 0.296 - 1), 0.1)
  # by construction, dose over the mean clearance
  expect_equal(aucUmolH, 0.375 / mean(k$CL) / 160.17 * 1000, tolerance = 0.02)
})

test_that("hydralazine is a vasoactive and resuscitation drug, plasma only, no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "hydralazine", ]
  expect_identical(row$Category, VASOACTIVE_DRUG_CATEGORY)
  expect_equal(row$MEAC, 0)
  expect_equal(row$Lower, 0)
  PK <- getDrugPK("hydralazine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_equal(PK$PK$default$ke0, 0)
  expect_match(PK$reference, "Reece")
})
