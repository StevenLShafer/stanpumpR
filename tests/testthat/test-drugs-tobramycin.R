# Tobramycin (R/drugs_tobramycin.R): Conil 2011's two compartments in ICU
# patients, clearance additive in Cockcroft-Gault creatinine clearance and
# height.  Drafted by Claude Code, 2026-11-12.  Expectations from Table 2 and
# the control stream of the source (PMC3018027), not from the drug file.

test_that("Table 2's parameters at the reference covariates", {
  # a 172 cm, 60-year-old man of 80 kg whose creatinine gives CrCl 94 mL/min
  scr <- (140 - 60) * 80 / (72 * 94)
  set <- tobramycin(80, 172, 60, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(set$cl1, 3.83 / 60)
  expect_equal(set$v1, 25.50)
  expect_equal(set$cl2, 4.74 / 60)
  expect_equal(set$v2, 30.60)
})

test_that("the covariates add as published, with the floors", {
  set <- tobramycin(70, 160, 75, "female", adjustToFFM = FALSE, creatinine = 2)$PK$default
  crcl <- (140 - 75) * 70 / (72 * 2) * 0.85
  expect_equal(set$cl1, (3.83 + 0.020 * (crcl - 94) + 0.052 * (160 - 172)) / 60)
  # height floored at 150 cm, so a child-sized height cannot drive it negative
  short <- tobramycin(40, 120, 30, "male", adjustToFFM = FALSE, creatinine = 5)$PK$default
  expect_gt(short$cl1, 0)
  expect_equal(tobramycin(40, 120, 30, "male", adjustToFFM = FALSE, creatinine = 5)$PK$default$cl1,
               tobramycin(40, 150, 30, "male", adjustToFFM = FALSE, creatinine = 5)$PK$default$cl1)
})

test_that("the MIC line and the MBC band", {
  mic <- antibioticMic("tobramycin")
  expect_equal(c(mic$MIC, mic$FreeFraction, mic$Threshold), c(2, 1, 2))
  band <- antibioticMbcBand("tobramycin")$plotted
  expect_equal(unname(band), c(0.5, 4, 8))
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "tobramycin", ]
  expect_equal(c(row$Lower, row$Typical, row$Upper, row$endCe), c(0.5, 4, 8, 2))
})
