# sugammadex: see the header of R/drugs_sugammadex.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- sugammadex(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.4904106,
        v2 = 6.7861043,
        v3 = 1,
        cl1 = 0.079718088,
        cl2 = 0.206,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 10, upperTypical = 30, lowerTypical = 5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- sugammadex(120, 170, 50, "male")
  expected <- list(
    v1 = 5.423784,
    v2 = 8.0897291,
    v3 = 1,
    cl1 = 0.10302249,
    cl2 = 0.251508,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the source's derived covariate scenario is reproduced", {
  # W = 74.5 kg and CR = 119 mL/min: Cockcroft-Gault at the assumed creatinine
  # of 0.9 mg/dL gives 119 for a 74.5 kg man of (140 - 119 x 72 x 0.9 / 74.5)
  # = 36.5 years.
  x <- sugammadex(74.5, 175, 140 - 119 * 72 * SCR_ASSUMED_MALE / 74.5, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 5.58, tolerance = 1e-9)
  expect_equal(x$v1, 4.704143, tolerance = 1e-6)
  expect_equal(x$cl2 * 60, 12.951264, tolerance = 1e-6)
  expect_equal(x$v2, 6.758214, tolerance = 1e-6)
  expect_equal(x$v1 + x$v2, 11.462357, tolerance = 1e-6)
})

test_that("pregnancy: creatinine clearance x 1.5 in the clearance term only, with a blank creatinine", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on, creatinine
  # blank (the assumed nonpregnant normal).
  on  <- getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_equal(off, getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default
  b <- off$PK$default
  # Kleijn's renal term at 1.5 x CR; the peripheral volume keeps CR
  size <- pkSizeFactors(70, 165, 30, SEX_FEMALE, TRUE)
  W  <- 70 * size$volume
  CR <- creatinineClearanceCG(W, 30, SEX_FEMALE, adultEquivalentCreatinine(NULL, 30, SEX_FEMALE))
  term <- function(cr) (2 * cr / (cr + 119))^1.29
  expect_equal(a$cl1 / b$cl1, term(PREGNANCY_GFR_FACTOR * CR) / term(CR), tolerance = 1e-6)
  expect_equal(a$cl1, 5.58 * (1 + 0.00378 * (W - 74.5)) * term(1.5 * CR) / 60, tolerance = 1e-12)
  expect_equal(a$v2, 6.35 * exp(-0.00305 * (CR - 119)) * (W / 70), tolerance = 1e-12)
  for (p in c("v1", "v2", "v3", "cl2", "cl3")) expect_identical(a[[p]], b[[p]], info = p)
  expect_equal_rounded(0.09474692731, a$cl1)
  expect_equal_rounded(1.3089337, a$cl1 / b$cl1)
  # Hodge 2026's worked comparison: 78 kg at term, switch off, assumed
  # creatinine: 5.88 L/h without the factor, 7.38 L/h with it (Hodge 8.0 L/h)
  h0 <- sugammadex(78, 165, 30, SEX_FEMALE, adjustToFFM = FALSE)$PK$default
  h1 <- sugammadex(78, 165, 30, SEX_FEMALE, adjustToFFM = FALSE, pregnant = TRUE)$PK$default
  expect_equal(h0$cl1 * 60, 5.881, tolerance = 1e-3)
  expect_equal(h1$cl1 * 60, 7.381, tolerance = 1e-3)
  expect_lt(abs(h1$cl1 * 60 / 8.0 - 1), 0.08)
  expect_identical(h1$v2, h0$v2)
  # A measured creatinine already carries the rise in filtration: no change
  expect_equal(getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE, creatinine = 0.6, pregnant = TRUE)$PK,
               getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE, creatinine = 0.6)$PK)
  expect_no_match(getDrugPK("sugammadex", 70, 165, 30, SEX_FEMALE, creatinine = 0.6,
                            pregnant = TRUE)$reference, "pregnancy:")
  expect_match(on$reference, "pregnancy:", fixed = TRUE)
  expect_match(on$reference, "Hodge", fixed = TRUE)
  expect_no_match(off$reference, "Hodge")
})

test_that("the peripheral volume sees creatinine clearance held within Kleijn's 4.3-239 mL/min", {
  # Shafer, 2026-11-24.  A 25-year-old 150 kg man at 0.5 mg/dL: Cockcroft-Gault
  # 115 x 150 / (72 x 0.5) = 479 mL/min, beyond the studied 239.
  hi <- sugammadex(150, 180, 25, "male", adjustToFFM = FALSE, creatinine = 0.5)$PK$default
  expect_equal(hi$v2, 6.35 * exp(-0.00305 * (239 - 119)) * (150 / 70), tolerance = 1e-12)
  # the clearance term is not held: it reads the full 479 mL/min
  cr <- 115 * 150 / (72 * 0.5)
  expect_equal(hi$cl1, 5.58 * (1 + 0.00378 * (150 - 74.5)) * (2 * cr / (cr + 119))^1.29 / 60,
               tolerance = 1e-12)
  # an 85-year-old 40 kg man at 15 mg/dL: 55 x 40 / (72 x 15) = 2.04, below 4.3
  lo <- sugammadex(40, 160, 85, "male", adjustToFFM = FALSE, creatinine = 15)$PK$default
  expect_equal(lo$v2, 6.35 * exp(-0.00305 * (4.30 - 119)) * (40 / 70), tolerance = 1e-12)
  # inside the range nothing changes
  mid <- sugammadex(70, 171, 50, "male", adjustToFFM = FALSE, creatinine = 1.2)$PK$default
  cr <- 90 * 70 / (72 * 1.2)
  expect_equal(mid$v2, 6.35 * exp(-0.00305 * (cr - 119)) * (70 / 70), tolerance = 1e-12)
})
