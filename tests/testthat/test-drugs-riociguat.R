# Riociguat (R/drugs_riociguat.R): Saleh 2016's one-compartment model in
# pulmonary hypertension.  Drafted by Claude Code, 2026-11-15.  Expectations
# are computed here from Table 2, part A, not from the drug file.

KA <- 2.17; CL <- 1.81; V <- 32.3
cg <- function(wt, age, scr, female = FALSE) (140 - age) * wt / (72 * scr) * (if (female) 0.85 else 1)
clOf <- function(crcl) CL * (1 + 0.00394 * (min(max(crcl, 15.4), 187) - 81.2))

test_that("Table 2, switch off", {
  set <- riociguat(68, 165, 50, "female", adjustToFFM = FALSE, creatinine = 0.8)$PK$default
  expect_equal(set$v1, V)                              # at the median 68 kg
  expect_equal(set$cl1, clOf(cg(68, 50, 0.8, TRUE)) / 60)
  expect_equal(set$ka_PO, KA / 60)
  expect_equal(set$bioavailability_PO, 1)
  set <- riociguat(90, 180, 60, "male", adjustToFFM = FALSE, creatinine = 1.4)$PK$default
  expect_equal(set$v1, V * (1 + 0.00619 * (90 - 68)))
  expect_equal(set$cl1, clOf(cg(90, 60, 1.4)) / 60)
})

test_that("at the median creatinine clearance, CL is 1.81 L/h; the label's half-life", {
  expect_equal(clOf(81.2), CL)
  # label: about 12 hours in patients
  expect_equal(round(log(2) * V / CL, 1), 12.4)
})

test_that("creatinine clearance is held within 15.4-187 mL/min", {
  lo <- riociguat(70, 170, 80, "male", FALSE, creatinine = 15)$PK$default$cl1
  expect_equal(lo, clOf(15.4) / 60)
  hi <- riociguat(70, 170, 20, "male", FALSE, creatinine = 0.3)$PK$default$cl1
  expect_equal(hi, clOf(187) / 60)
  expect_gt(lo, 0)
})

test_that("switch on: the weight terms at the pharmacokinetic weight", {
  # 120 kg, 170 cm, 50 y man: pharmacokinetic weight 70 x 1.3049067
  w <- 70 * 1.3049067
  set <- riociguat(120, 170, 50, "male", creatinine = 1)$PK$default
  expect_equal_rounded(set$v1, V * (1 + 0.00619 * (w - 68)))
  expect_equal_rounded(set$cl1, clOf(cg(w, 50, 1)) / 60)
})
