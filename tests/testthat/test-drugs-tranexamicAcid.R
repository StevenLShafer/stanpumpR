# Tranexamic acid (R/drugs_tranexamicAcid.R): Lanoiselee 2018's
# two-compartment model with Cockcroft-Gault creatinine clearance on CL.
# Drafted by Claude Code, 2026-11-12.
#
# The expectations are built from Table 2 of the source (PMC5777664), not
# from the drug file.

CL <- 5.32; CRCL_EXP <- 0.548; VC <- 6.09; Q <- 23.3; VP <- 9.55

events <- data.frame(Time = double(), Event = character())

test_that("Table 2's parameters, with the switch off", {
  # A 70 kg, 35-year-old man whose creatinine makes the CrCL exactly 85
  scr <- (140 - 35) * 70 / (72 * 85)
  set <- tranexamicAcid(70, 170, 35, "male", adjustToFFM = FALSE,
                        creatinine = scr)$PK$default
  expect_equal(set$cl1, CL / 60)
  expect_equal(set$v1, VC)
  expect_equal(set$cl2, Q / 60)
  expect_equal(set$v2, VP)
  expect_equal(c(set$v3, set$cl3), c(1, 0))
})

test_that("creatinine clearance and weight enter as published", {
  # a 90 kg, 70-year-old woman with a creatinine of 1.6 mg/dL
  set <- tranexamicAcid(90, 165, 70, "female", adjustToFFM = FALSE,
                        creatinine = 1.6)$PK$default
  crcl <- (140 - 70) * 90 / (72 * 1.6) * 0.85
  expect_equal(set$cl1, CL / 60 * (crcl / 85)^CRCL_EXP)
  expect_equal(set$v1, VC * 90 / 70)
  expect_equal(set$v2, VP)
  # renal impairment slows the fall below 10 mg/L
  normal <- tranexamicAcid(90, 165, 70, "female", adjustToFFM = FALSE,
                           creatinine = 0.7)$PK$default
  expect_lt(set$cl1, normal$cl1)
})

test_that("the time above 10 mg/L after 1 g is in the range the study saw", {
  # Lanoiselee 2018, Table 3: after a 1 g bolus alone, 5.6 h (3.3-11.8)
  # above 10 mg/L, Cmax after the bolus 148 mg/L (65-311) at 3 min
  wide <- simulateDrugsWithCovariates(
    data.frame(Drug = "tranexamicAcid", Time = 0, Dose = 1, Units = "g"),
    events, 75, 165, 66, "male", 1440, FALSE, adjustToFFM = FALSE,
    creatinine = 1.0)$tranexamicAcid$wide
  above <- max(wide$Time[wide$Plasma > 10]) / 60
  expect_gt(above, 3.3)
  expect_lt(above, 11.8)
  expect_equal(max(wide$Plasma), 1000 / (VC * 75 / 70), tolerance = 1e-6)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only when it is blank", {
  off <- tranexamicAcid(70, 165, 30, "female")$PK$default
  on  <- tranexamicAcid(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(on$cl1 / off$cl1, 1.5^CRCL_EXP)
  measured <- tranexamicAcid(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)$PK$default
  expect_equal(measured, tranexamicAcid(70, 165, 30, "female", creatinine = 0.6)$PK$default)
})

test_that("the defaults row reads the 10 mg/L threshold and draws no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tranexamicAcid", ]
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$endCe), c(0, 0, 0, 10))
})
