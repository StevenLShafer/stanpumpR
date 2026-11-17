# Fludarabine (R/drugs_fludarabine.R): Langenhorst 2019, three compartments
# allometric in weight, eGFR on clearance; doses as fludarabine phosphate, the
# model in F-ara-A.  Drafted by Claude Code, 2026-11-07.

f <- 285.24 / 365.21

test_that("the model is Table 2 per 70 kg, run in F-ara-A for a phosphate dose", {
  # 30-year-old man, 70 kg, creatinine chosen for a Cockcroft-Gault of 120
  scr <- (140 - 30) * 70 / (72 * 120)
  X <- fludarabine(70, 170, 30, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal_rounded(X$v1 * f, 39)
  expect_equal_rounded(X$v2 * f, 20)
  expect_equal_rounded(X$v3 * f, 50)
  expect_equal_rounded(X$cl2 * 60 * f, 8.6)
  expect_equal_rounded(X$cl3 * 60 * f, 3.8)
  expect_equal_rounded(X$cl1 * 60 * f, 3.24 + 0.782 * 120 * 0.06)
  # eGFR read in L/h per 70 kg: the paper's "approximately 65%" renal share
  expect_equal(round(0.782 * 120 * 0.06 / (X$cl1 * 60 * f), 2), 0.63)
  expect_equal(round(FLUDARABINE_FARAA_FRACTION, 3), 0.781)
  PK <- getDrugPK("fludarabine", 70, 170, 30, "male", adjustToFFM = FALSE,
                  creatinine = scr)$PK$default
  expect_equal(round(log(2) / c(PK$lambda_1, PK$lambda_2, PK$lambda_3) / 60, 2),
               c(0.86, 3.50, 14.89))
})

test_that("the adult median cumulative AUC: 160 mg/m2 of phosphate at 1.9 m2, 78 kg, eGFR 110", {
  scr <- (140 - 54) * 78 / (72 * 110)
  X <- fludarabine(78, 175, 54, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(round(160 * 1.9 / (X$cl1 * 60), 1), 26.1)    # paper's median 26 mg.h/L
})

test_that("eGFR: Schwartz in children, Cockcroft-Gault in adults, capped by age", {
  expect_equal(fludarabineEGFR(10, 75, 0, "male", 0.3), 35)        # cap at birth
  expect_equal(fludarabineEGFR(10, 75, 0.75, "male", 0.3), 87.5)   # cap half-way to 1.5 y
  expect_equal(fludarabineEGFR(30, 140, 8, "female", 0.7), 0.55 * 140 / 0.7)
  expect_equal(fludarabineEGFR(50, 160, 13, "male", 1.0), 0.70 * 160 / 1.0)
  expect_equal(fludarabineEGFR(50, 160, 15, "male", 1.0),
               creatinineClearanceCG(50, 15, "male", 1.0))
  expect_equal(fludarabineEGFR(50, 160, 15, "female", 1.0), 0.55 * 160 / 1.0)
  expect_equal(fludarabineEGFR(70, 170, 80, "male", 0.5), 100)     # 140 - 8 per decade after 30
})

test_that("pregnancy raises the eGFR by 1.5 only with the creatinine blank", {
  cl <- function(...) fludarabine(70, 165, 30, "female", ...)$PK$default$cl1
  expect_gt(cl(pregnant = TRUE), cl())
  expect_equal(cl(creatinine = 0.8, pregnant = TRUE), cl(creatinine = 0.8))
})

test_that("both switch positions are pinned", {
  on <- fludarabine(70, 170, 35, "male")$PK$default
  off <- fludarabine(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  expect_equal_rounded(on$cl1, off$cl1)
  expect_lt(fludarabine(120, 170, 35, "male")$PK$default$v1,
            fludarabine(120, 170, 35, "male", adjustToFFM = FALSE)$PK$default$v1)
})

test_that("fludarabine is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "fludarabine", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("fludarabine", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Langenhorst")
  expect_identical(therapeuticRange("fludarabine")$band_status, "not_applicable")
})
