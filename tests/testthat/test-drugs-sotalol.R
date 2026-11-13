# Sotalol (R/drugs_sotalol.R): PEAKS (Yellepeddi 2025) disposition with
# Cockcroft-Gault on clearance; oral absorption from the FDA review of NDA
# 22-306.  Drafted by Claude Code, 2026-11-12.  Expectations from the printed
# tables, not from the drug file.

events <- data.frame(Time = double(), Event = character())

test_that("Table 2's parameters at the reference creatinine clearance and weight", {
  # a 104 kg, 69-year-old man whose creatinine makes the CrCl exactly 92.4
  scr <- (140 - 69) * 104 / (72 * 92.4)
  set <- sotalol(104, 180, 69, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(set$cl1, 9.5 / 60)
  expect_equal(set$v1, 38.2)
  expect_equal(set$cl2, 71.3 / 60)
  expect_equal(set$v2, 89.1)
  expect_equal(set$ka_PO, 0.605 / 60)
  expect_equal(set$tlag_PO, 0.231 * 60)
  expect_equal(set$bioavailability_PO, 1)
})

test_that("creatinine clearance and weight enter as published", {
  set <- sotalol(70, 170, 75, "female", adjustToFFM = FALSE, creatinine = 1.8)$PK$default
  crcl <- (140 - 75) * 70 / (72 * 1.8) * 0.85
  expect_equal(set$cl1, 9.5 / 60 * (crcl / 92.4)^0.7)
  expect_equal(set$v1, 38.2 * (70 / 104)^1.1)
  expect_equal(set$v2, 89.1)
})

test_that("an 80 mg oral dose reproduces the FDA review's healthy-subject AUC", {
  # FDA review, Table 3 (study 12103, not in the PEAKS fit): 80 mg sotalol
  # hydrochloride by mouth, mean 69.6 kg, age 32: AUC 7323 +/- 1897 ng.h/mL,
  # tmax 2.40 +/- 0.78 h
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "sotalol", Time = 0, Dose = 80, Units = "mg PO"),
    events, 70, 175, 32, "male", 72 * 60, FALSE, adjustToFFM = FALSE
  )$sotalol$wide
  auc <- sum(diff(wide$Time) * (head(wide$Plasma, -1) + tail(wide$Plasma, -1)) / 2) / 60
  expect_gt(auc, 7323 - 1897)
  expect_lt(auc, 7323 + 1897)
  tmax <- wide$Time[which.max(wide$Plasma)] / 60
  expect_true(tmax > 1.5 && tmax < 4)
})

test_that("pregnancy raises creatinine clearance by 1.5 only when it is blank", {
  off <- sotalol(70, 165, 30, "female")$PK$default
  on  <- sotalol(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(on$cl1 / off$cl1, 1.5^0.7)
  expect_equal(sotalol(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default,
               sotalol(70, 165, 30, "female", creatinine = 0.6)$PK$default)
})
