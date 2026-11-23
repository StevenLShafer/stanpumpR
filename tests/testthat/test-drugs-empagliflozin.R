# empagliflozin: see R/drugs_empagliflozin.R (Baron 2016, Table S1).
eGFRCreatinine <- function(egfr, age, sex)   # MDRD inverted
  (egfr / (175 * age^-0.203 * (if (sex == "female") 0.742 else 1)))^(1 / -1.154)
bmi25 <- 25 * 1.75^2                         # kg at 175 cm for a BMI of 25
test_that("Table S1's reference patient, with the fixed 0.5 h lag", {
  scr <- eGFRCreatinine(100, 50, "male")
  expect_equal(egfrMDRD(50, "male", scr), 100)
  pk <- empagliflozin(bmi25, 175, 50, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(c(pk$v1, pk$v2), c(3.14, 70.6))
  expect_equal(c(pk$cl1, pk$cl2) * 60, c(10.6, 6.34))
  expect_equal(pk$ka_PO, 0.196 / 60)
  expect_equal(pk$tlag_PO, 30)
})
test_that("age, sex and BMI as Table S1 gives them", {
  scr <- function(age, sex) eGFRCreatinine(100, age, sex)
  w <- 35 * 1.65^2                            # BMI 35 at 165 cm
  f <- empagliflozin(w, 165, 75, "female", FALSE, creatinine = scr(75, "female"))$PK$default
  a <- 75 / 50; b <- 35 / 25
  expect_equal(f$cl1 * 60, 10.6 * a^-0.241 * b^0.320 * 0.886)
  expect_equal(f$v1, 3.14 * a^0.795 * b^1.04 * 1.25)
  expect_equal(f$v2, 70.6 * a^0.135 * b^0.672 * 0.831)
  expect_equal(f$ka_PO * 60, 0.196 * a^0.108 * 1.17)
  expect_equal(f$cl2 * 60, 6.34)
  # BMI held within 20.8 to 42.4, age at 18 and above
  big <- empagliflozin(200, 165, 50, "male", FALSE, creatinine = scr(50, "male"))$PK$default
  expect_equal(big$v2, 70.6 * (42.4 / 25)^0.672)
  young <- empagliflozin(bmi25, 175, 12, "male", FALSE)$PK$default
  expect_equal(young$v2, 70.6 * (18 / 50)^0.135)
  # switch on: BMI from the pharmacokinetic weight
  size <- pkSizeFactors(100, 175, 50, "male", TRUE)
  on <- empagliflozin(100, 175, 50, "male", creatinine = scr(50, "male"))$PK$default
  expect_equal(on$v2, 70.6 * (min(size$pkWeight / 1.75^2, 42.4) / 25)^0.672)
  expect_equal_rounded(empagliflozin(70, 170, 40, "male")$PK$default$cl1 * 60, 10.8263560)
})
test_that("the renal exponent reproduces the paper's AUC at eGFR 60, 30 and 15", {
  cl <- function(egfr) empagliflozin(bmi25, 175, 50, "male", FALSE,
                                     creatinine = eGFRCreatinine(egfr, 50, "male"))$PK$default$cl1
  ref <- cl(100)
  # AUC 18.5%, 49.2% and 88.1% higher (Baron 2016, Results)
  expect_equal(ref / cl(60), 1.185, tolerance = 0.002)
  expect_equal(ref / cl(30), 1.492, tolerance = 0.002)
  expect_equal(ref / cl(15), 1.881, tolerance = 0.002)
})

test_that("body size: Q/F scaled to fat-free mass from the reference man, the BMI terms as fitted", {
  # Steven L. Shafer's first-principles rule, 2026-11-19 (R/drugs_empagliflozin.R,
  # BODY SIZE): Q/F has no covariate in Baron 2016 and takes the fat-free-mass
  # clearance factor, anchored at the reference man (no weight printed).
  scr <- eGFRCreatinine(100, 35, "male")
  b <- (70 / 1.70^2) / 25; a <- 35 / 50
  # (a) the reference man, the anchor, receives the published Q/F, and the
  # other parameters their own covariate values
  ref <- empagliflozin(70, 170, 35, "male", creatinine = scr)$PK$default
  expect_equal(ref$cl2 * 60, 6.34)
  expect_equal(ref$cl1 * 60, 10.6 * a^-0.241 * b^0.320)
  expect_equal(ref$v2, 70.6 * a^0.135 * b^0.672)
  # (b) a man of twice the reference fat-free mass: Q/F x 2^0.75; CL/F, Vc/F
  # and Vp/F by their own BMI terms at the pharmacokinetic weight, 140 kg
  w <- uniroot(function(w) ffmAlSallami(w, 200, 35, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- empagliflozin(w, 200, 35, "male", creatinine = scr)$PK$default
  expect_equal(big$cl2 * 60, 6.34 * 2^0.75)
  b2 <- (140 / 2^2) / 25
  expect_equal(big$cl1 * 60, 10.6 * a^-0.241 * b2^0.320)
  expect_equal(big$v1, 3.14 * a^0.795 * b2^1.04)
  expect_equal(big$v2, 70.6 * a^0.135 * b2^0.672)
  expect_equal(big$ka_PO, ref$ka_PO)
  # (c) the switch off: Q/F as published, whatever the size
  off <- empagliflozin(w, 200, 35, "male", adjustToFFM = FALSE, creatinine = scr)$PK$default
  expect_equal(off$cl2 * 60, 6.34)
})
