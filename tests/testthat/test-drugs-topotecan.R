# Topotecan (R/drugs_topotecan.R): Mould 2002.  Drafted by Claude Code,
# 2026-11-06.

scr70 <- (140 - 60) * 70 / (72 * 70)   # Cockcroft-Gault 70 mL/min at 70 kg, a man of 60

test_that("Table V at the reference: CL 18.57 L/h, 69% renal, Vss 65.0 L, half-lives 5.6 min and 2.68 h", {
  set <- getDrugPK("topotecan", 70, 170, 60, "male", adjustToFFM = FALSE,
                   creatinine = scr70)$PK$default
  expect_equal(round(set$cl1 * 60, 2), 18.57)
  expect_equal(round(12.8 / (set$cl1 * 60), 2), 0.69)
  expect_equal_rounded(set$v1, 19.3)
  expect_equal_rounded(set$cl2 * 60, 90.4)
  expect_equal(round(set$v1 + set$v2, 1), 65.0)
  halfLives <- sort(log(2) / c(set$lambda_1, set$lambda_2) / 60)
  expect_equal(round(halfLives * c(60, 1), c(1, 2)), c(5.6, 2.68))
})

test_that("the printed RF: Cockcroft-Gault x 70/WT, so the renal term ignores weight", {
  cl <- function(w) topotecan(w, 170, 60, "male", adjustToFFM = FALSE,
                              creatinine = scr70)$PK$default$cl1 * 60
  renal <- function(w) cl(w) / (w / 70)^0.75 - 5.77
  expect_equal(renal(50), renal(100))
  expect_equal(renal(70), 12.8)
  # the cap: 150 mL/min on the raw Cockcroft-Gault
  big <- topotecan(120, 180, 30, "male", adjustToFFM = FALSE, creatinine = 0.6)$PK$default
  expect_equal(big$cl1 * 60, (5.77 + 12.8 * 150 / 120) * (120 / 70)^0.75)
})

test_that("pregnancy raises the creatinine clearance by 1.5 only with a blank creatinine", {
  m <- function(...) topotecan(70, 165, 30, "female", adjustToFFM = FALSE, ...)$PK$default$cl1 * 60
  expect_equal(m(pregnant = TRUE) - 5.77, 1.5 * (m() - 5.77))
  expect_equal(m(creatinine = 0.6, pregnant = TRUE), m(creatinine = 0.6))
})

test_that("topotecan is a chemotherapy drug, plasma only, with no band", {
  dd <- getDrugDefaultsGlobal()
  row <- dd[dd$Drug == "topotecan", ]
  expect_identical(row$Category, CHEMOTHERAPY_DRUG_CATEGORY)
  expect_identical(row$Concentration.Units, "ng")
  expect_equal(c(row$Lower, row$Upper, row$Typical, row$MEAC), c(0, 0, 0, 0))
  PK <- getDrugPK("topotecan", 70, 170, 60, "male")
  expect_equal(PK$tPeak, 0)
  expect_match(PK$reference, "Mould")
  expect_identical(therapeuticRange("topotecan")$band_status, "not_applicable")
})
