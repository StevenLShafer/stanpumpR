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
  size <- pkSizeFactors(120, 170, 50, "male", TRUE)
  x <- vancomycinKidneyTransplant(120, 170, 50, "male", creatinine = 1.8)$PK$default
  expect_equal(x$v1, 80.8 * size$volume)
  crcl <- creatinineClearanceCG(70 * size$volume, 50, "male", 1.8)
  expect_equal(x$cl1, 4.32 * crcl / 100 * exp(-0.28) / 60)
  # the reference man receives the published volume
  expect_equal(vancomycinKidneyTransplant(70, 170, 35, "male")$PK$default$v1, 80.8,
               tolerance = 1e-6)
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
