# Melphalan (R/drugs_melphalan.R): Nath 2010.  Drafted by Claude Code,
# 2026-11-06.

# Creatinine giving a Cockcroft-Gault clearance of 88 mL/min per 70 kg at
# age 57 in a man
scr88 <- (140 - 57) * 70 / (72 * 88)

test_that("Table 5 at the reference: CL 28.1 L/h, 39.5% renal, Table 7's half-lives", {
  pk <- function(w, h, sex) melphalan(w, h, 57, sex, creatinine = scr88)$PK$default
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
  cl <- function(w, scr) melphalan(w, 170, 57, "male", adjustToFFM = FALSE,
                                   creatinine = scr)$PK$default$cl1 * 60
  ffm <- function(w) ffmAlSallami(w, 170, 57, "male")
  renal <- function(w, scr) cl(w, scr) - 17 * (ffm(w) / 50)^0.75
  expect_equal(renal(60, 1.2), renal(110, 1.2))
  expect_equal(renal(70, 2 * scr88), 11.1 / 2)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  m <- function(...) melphalan(70, 165, 30, "female", adjustToFFM = FALSE, ...)$PK$default$cl1
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
