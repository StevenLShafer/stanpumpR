# vancomycinKidneyTransplant: see the header of
# R/drugs_vancomycinKidneyTransplant.R (Yu 2026).  Pins were worked out from
# the published numbers by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  # 70 kg, 171 cm, 50 y man, creatinine blank (assumed 0.9 mg/dL).  Devine IBW
  # 66.84 kg, so 70 kg is under 120% of it and actual weight is used:
  # CLcr = 90 x 70 / (72 x 0.9) = 97.22 mL/min;
  # CL = 4.32 x 0.9722 x exp(-0.28) = 3.1743 L/h.
  actual <- vancomycinKidneyTransplant(70, 171, 50, "male", adjustToFFM = FALSE)
  expected <- list(
    PK = list(
      default = list(
        v1 = 80.8,
        v2 = 1,
        v3 = 1,
        cl1 = 0.05290486190,
        cl2 = 0,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 11, upperTypical = 11, lowerTypical = 2.9,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("Cockcroft-Gault reads the adjusted body weight above 120% of IBW", {
  # 120 kg, 170 cm, 50 y man, creatinine 1.8: IBW 65.937, AjBW 87.562,
  # CLcr 60.807 mL/min, CL = 4.32 x 0.60807 x exp(-0.28) / 60 L/min.
  actual <- vancomycinKidneyTransplant(120, 170, 50, "male", adjustToFFM = FALSE,
                                       creatinine = 1.8)$PK$default
  expect_equal_rounded(actual$cl1, 0.03308904535)
  expect_equal(actual$v1, 80.8)
})

test_that("creatinine clearance is truncated at 150 mL/min", {
  # 70 kg, 180 cm, 30 y man, creatinine 0.5: CLcr 213.9 -> 150
  actual <- vancomycinKidneyTransplant(70, 180, 30, "male", adjustToFFM = FALSE,
                                       creatinine = 0.5)$PK$default
  expect_equal_rounded(actual$cl1, 0.08162464408)
})

test_that("the paper's typical clearances are reproduced", {
  # Yu 2026: 1.5 L/h in recipients at their median CLcr of 44.9 mL/min, and
  # 3.9 L/h in non-transplant patients at 91.0 (the transplant factor removed)
  kt <- VANCOMYCIN_KT_CL * 0.449 * exp(VANCOMYCIN_KT_THETA)
  expect_equal(round(kt, 1), 1.5)
  expect_equal(round(kt / exp(VANCOMYCIN_KT_THETA) * 0.910 / 0.449, 1), 3.9)
  # Clearance is proportional to CLcr: halving it (doubling the creatinine)
  # halves clearance
  a <- vancomycinKidneyTransplant(70, 171, 50, "male", creatinine = 1)$PK$default$cl1
  b <- vancomycinKidneyTransplant(70, 171, 50, "male", creatinine = 2)$PK$default$cl1
  expect_equal(b / a, 0.5)
})

test_that("fat-free-mass scaling: library volume factor, pharmacokinetic weight in CG", {
  # V anchored at Yu's cohort (2026-11-20): the old pin over its factor; the
  # renal term reads the unanchored pharmacokinetic weight
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  kc <- cohortFactorsIndependent(VANCOMYCIN_KT_COHORT)
  x <- vancomycinKidneyTransplant(120, 170, 50, "male", creatinine = 1.8)$PK$default
  expect_equal(x$v1, 80.8 * size$volume / kc$volume)
  crcl <- creatinineClearanceCG(70 * size$volume, 50, "male", 1.8)
  expect_equal(x$cl1, 4.32 * crcl / 100 * exp(-0.28) / 60)
  # the reference man receives 80.8 L over the cohort's factor, 77.7 L
  expect_equal(vancomycinKidneyTransplant(70, 170, 35, "male")$PK$default$v1,
               80.8 / kc$volume, tolerance = 1e-6)
})

test_that("anchored at Yu's cohort: its typical patients get 80.8 L, FFM scales V", {
  # (a) Table I (p5): recipients 304 men and 165 women, 60.7 y, 172.7 cm,
  # 84.5 kg; non-transplant 676 men and 394 women, 59.2 y, 171.3 cm, 84.3 kg.
  # V has no sex term: the cohort's four typical patients, pooled
  # geometrically by their numbers, receive the published 80.8 L
  kc <- cohortFactorsIndependent(VANCOMYCIN_KT_COHORT)
  expect_equal(round(kc$volume, 4), 1.0405)
  v <- function(w, h, a, s) vancomycinKidneyTransplant(w, h, a, s)$PK$default$v1
  pooled <- exp((304 * log(v(84.5, 172.7, 60.7, "male")) +
                   165 * log(v(84.5, 172.7, 60.7, "female")) +
                   676 * log(v(84.3, 171.3, 59.2, "male")) +
                   394 * log(v(84.3, 171.3, 59.2, "female"))) / 1539)
  expect_equal(pooled, 80.8)
  # (b) twice the reference fat-free mass: V x 2 over the cohort's factor;
  # CL is the renal term at the pharmacokinetic weight (140 kg), unanchored
  w <- uniroot(function(w) ffmAlSallami(w, 200, 50, "male") - 2 * FFM_REFERENCE,
               c(70, 400), tol = 1e-12)$root
  big <- vancomycinKidneyTransplant(w, 200, 50, "male", creatinine = 1.8)$PK$default
  expect_equal(big$v1, 80.8 * 2 / kc$volume)
  crcl <- min(creatinineClearanceCG(140, 50, "male", 1.8), 150)
  expect_equal(big$cl1, 4.32 * crcl / 100 * exp(-0.28) / 60)
  # (c) the switch off: 80.8 L at any size
  expect_equal(vancomycinKidneyTransplant(w, 200, 50, "male", adjustToFFM = FALSE)$PK$default$v1,
               80.8)
})

test_that("the transplant factor is the only difference from the source's non-transplant clearance", {
  # and that non-transplant clearance agrees with Thomson's (the library's
  # vancomycin) to within 6% where the cohorts overlap
  for (scr in c(0.9, 1.8, 3)) {
    crcl <- creatinineClearanceCG(70, 60, "male", scr)
    yu <- vancomycinKidneyTransplant(70, 171, 60, "male", adjustToFFM = FALSE,
                                     creatinine = scr)$PK$default$cl1
    expect_equal(yu, 4.32 * crcl / 100 * exp(-0.28) / 60)
    thomson <- vancomycin(70, 171, 60, "male", adjustToFFM = FALSE,
                          creatinine = scr)$PK$default$cl1
    expect_lt(abs(yu / exp(-0.28) / thomson - 1), 0.06)
  }
})

test_that("stays finite at the edges of the Patient Profile", {
  for (h in c(50, 100, 220)) for (w in c(3, 300)) for (a in c(0, 90)) for (s in c("male", "female"))
    for (ffm in c(TRUE, FALSE)) {
      x <- vancomycinKidneyTransplant(w, h, a, s, adjustToFFM = ffm)$PK$default
      expect_true(is.finite(x$cl1) && x$cl1 > 0 && is.finite(x$v1) && x$v1 > 0,
                  info = paste(h, w, a, s, ffm))
    }
})

test_that("pregnancy: x 1.5 on the creatinine clearance only with the creatinine blank", {
  p1 <- vancomycinKidneyTransplant(70, 165, 30, "female", pregnant = TRUE)
  p0 <- vancomycinKidneyTransplant(70, 165, 30, "female")
  expect_equal(p1$PK$default$cl1 / p0$PK$default$cl1, PREGNANCY_GFR_FACTOR)
  expect_equal(p1$PK$default$v1, p0$PK$default$v1)
  expect_match(p1$reference, "Ansari", fixed = TRUE)
  expect_identical(
    vancomycinKidneyTransplant(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE),
    vancomycinKidneyTransplant(70, 165, 30, "female", creatinine = 0.6))
})
