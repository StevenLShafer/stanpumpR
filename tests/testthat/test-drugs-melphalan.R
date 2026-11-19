# Melphalan (R/drugs_melphalan.R): Nath 2010.  Drafted by Claude Code,
# 2026-11-06.

# Creatinine giving a Cockcroft-Gault clearance of 88 mL/min per 70 kg at
# age 57 in a man
scr88 <- (140 - 57) * 70 / (72 * 88)

test_that("Table 5 at the reference: CL 28.1 L/h, 39.5% renal, Table 7's half-lives", {
  pk <- function(w, h, sex) melphalanNath(w, h, 57, sex, creatinine = scr88)$PK$default
  # find a man whose FFM is 50 kg: search on weight at 170 cm
  w50 <- uniroot(function(w) ffmAlSallami(w, 170, 57, "male") - 50, c(40, 90))$root
  set <- getDrugPK("melphalan", w50, 170, 57, "male", adjustToFFM = FALSE,
                   creatinine = scr88)$PK$default
  expect_equal(round(set$cl1 * 60, 1), 28.1)
  expect_equal_rounded(set$v1, 13.2)
  expect_equal_rounded(set$cl2 * 60, 30.6)
  expect_equal_rounded(set$v2, 15)
  expect_equal(round(11.1 / (set$cl1 * 60), 3), 0.395)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives, 3), c(0.121, 0.914))
})

test_that("the renal term is Cockcroft-Gault per 70 kg: weight does not enter it", {
  cl <- function(w, scr) melphalanNath(w, 170, 57, "male", adjustToFFM = FALSE,
                                   creatinine = scr)$PK$default$cl1 * 60
  ffm <- function(w) ffmAlSallami(w, 170, 57, "male")
  renal <- function(w, scr) cl(w, scr) - 17 * (ffm(w) / 50)^0.75
  expect_equal(renal(60, 1.2), renal(110, 1.2))
  expect_equal(renal(70, 2 * scr88), 11.1 / 2)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  m <- function(...) melphalanNath(70, 165, 30, "female", adjustToFFM = FALSE, ...)$PK$default$cl1
  nonRenal <- 17 * (ffmAlSallami(70, 165, 30, "female") / 50)^0.75 / 60
  expect_equal(m(pregnant = TRUE) - nonRenal, 1.5 * (m() - nonRenal))
  expect_equal(m(creatinine = 0.6, pregnant = TRUE), m(creatinine = 0.6))
})

test_that("melphalan is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "melphalan", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("melphalan", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Nath")
  expect_identical(therapeuticRange("melphalan")$band_status, "not_applicable")
})

test_that("Nath's size-free parameters are anchored at her 100 patients (A2-F4)", {
  # Table 2: 78 kg, 168 cm, 57 years, 59 men and 41 women.  With the switch
  # on, Q and V2 are the published values divided by the cohort's pooled
  # factors, so that its men and women, pooled geometrically, receive the
  # published values; with the switch off they are fixed.  V1 and the
  # non-renal clearance carry their own fat-free mass and do not move.
  kc <- cohortFactorsIndependent(MELPHALAN_NATH_COHORT)
  pk <- function(sex, ffm = TRUE)
    melphalanNath(78, 168, 57, sex, adjustToFFM = ffm, creatinine = 1)$PK$default
  for (sex in c("male", "female")) {
    own <- pkSizeFactors(78, 168, 57, sex)
    on <- pk(sex)
    off <- pk(sex, FALSE)
    expect_equal(off$v2, 15, tolerance = 1e-12)
    expect_equal(off$cl2 * 60, 30.6, tolerance = 1e-12)
    expect_equal(on$v2, 15 * own$volume / kc$volume, tolerance = 1e-6)
    expect_equal(on$cl2 * 60, 30.6 * own$clearance / kc$clearance, tolerance = 1e-6)
    expect_equal(on$v1, off$v1)
    expect_equal(on$cl1, off$cl1)
  }
  pooled <- function(f) exp((59 * log(f("male")) + 41 * log(f("female"))) / 100)
  expect_equal(pooled(function(s) pk(s)$v2), 15, tolerance = 1e-6)
  expect_equal(pooled(function(s) pk(s)$cl2 * 60), 30.6, tolerance = 1e-6)
})
