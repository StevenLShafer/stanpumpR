# empagliflozin: see R/drugs_empagliflozin.R (Baron 2016).
eGFRCreatinine <- function(egfr, age, sex)   # MDRD inverted
  (egfr / (175 * age^-0.203 * (if (sex == "female") 0.742 else 1)))^(1 / -1.154)
test_that("Baron's reference parameters at eGFR 100, with the fixed 0.5 h lag", {
  scr <- eGFRCreatinine(100, 50, "male")
  expect_equal(egfrMDRD(50, "male", scr), 100)
  pk <- empagliflozin(90, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(3.14, 70.6))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(10.6, 6.34))
  expect_equal(pk$ka_PO, 0.196 / 60)
  expect_equal(pk$tlag_PO, 30)
  size <- pkSizeFactors(90, 175, 50, "male", TRUE, legacyVolume = 1)
  on <- empagliflozin(90, 175, 50, "male", creatinine = scr)$PK$default
  expect_equal(on$v2, 70.6 * size$volume)
  expect_equal(on$cl1, 10.6 * size$clearance / 60)
})
test_that("the derived renal exponent reproduces the paper's AUC at eGFR 60, 30 and 15", {
  cl <- function(egfr) empagliflozin(70, 170, 50, "male", FALSE,
                                     creatinine = eGFRCreatinine(egfr, 50, "male"))$PK$default$cl1
  ref <- cl(100)
  # AUC 18.5%, 49.2% and 88.1% higher (Baron 2016, Results)
  expect_equal(ref / cl(60), 1.185, tolerance = 0.002)
  expect_equal(ref / cl(30), 1.492, tolerance = 0.002)
  expect_equal(ref / cl(15), 1.881, tolerance = 0.002)
})
