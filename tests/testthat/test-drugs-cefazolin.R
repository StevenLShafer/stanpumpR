# cefazolin: see the header of R/drugs_cefazolin.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- cefazolin(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 36.6,
        v2 = 42.2,
        v3 = 1,
        cl1 = 0.59199703,
        cl2 = 0.90666667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 4, upperTypical = 8, lowerTypical = 0.5,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- cefazolin(120, 170, 50, "male")
  expected <- list(
    v1 = 47.759583,
    v2 = 55.06706,
    v3 = 1,
    cl1 = 0.69190916,
    cl2 = 1.1069608,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the unbound model is Komatsu's, with creatinine clearance from the covariates", {
  x <- cefazolin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Komatsu 2024: Vcu 36.6, Vpu 42.2, Qu 54.4 L/h; CLu 29.3 (CrCL/70)^0.586 L/h
  expect_equal(x$v1, 36.6); expect_equal(x$v2, 42.2); expect_equal(x$cl2 * 60, 54.4)
  crcl <- creatinineClearanceCG(70, 50, "male")           # 97.22 mL/min at the assumed 0.9 mg/dL
  expect_equal(x$cl1 * 60, 29.3 * (crcl / 70)^0.586)
  # An older patient has a lower creatinine clearance and a lower CLu
  expect_lt(cefazolin(70, 171, 80, "male", adjustToFFM = FALSE)$PK$default$cl1, x$cl1)
  # No effect site: the plotted row is plasma only
  PK <- getDrugPK("cefazolin", 70, 171, 50, "male")
  expect_equal(PK$PK$default$ke0, 0)
})

test_that("the unbound AUC after a dose is dose / CLu", {
  PK <- getDrugPK("cefazolin", 70, 171, 50, "male")$PK$default
  auc <- PK$p_coef_bolus_l1 / PK$lambda_1 + PK$p_coef_bolus_l2 / PK$lambda_2
  expect_equal(auc, 1 / PK$cl1)
})

test_that("the Pregnant field applies the review's factor (R/pregnancy.R)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  Creatinine clearance x 1.5, so CLu x 1.5^0.586.
  # The values are those the model gives with the flag on; test-pregnancy.R
  # checks the ratio against the flag off and that nothing else moves.
  actual <- cefazolin(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal_rounded(29.50307895, actual$v1)
  expect_equal_rounded(34.01721124, actual$v2)
  expect_equal_rounded(1, actual$v3)
  expect_equal_rounded(0.7249970217, actual$cl1)
  expect_equal_rounded(0.7713248818, actual$cl2)
  expect_equal_rounded(0, actual$cl3)
  expect_equal(cefazolin(70, 165, 30, "female")$PK$default,
               cefazolin(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(cefazolin(70, 165, 30, "female", pregnant = TRUE)$reference, "Ansari", fixed = TRUE)
  expect_no_match(cefazolin(70, 165, 30, "female")$reference, "Ansari")
})

test_that("pregnancy: a measured creatinine gives no pregnancy change", {
  # The rise in filtration is applied only when the creatinine is blank
  # (pregnancyGfrFactor()): a pregnant woman's measured creatinine is already
  # low, and the renal estimate made from it already carries the rise.
  m1 <- cefazolin(70, 165, 30, "female", creatinine = 0.6, pregnant = TRUE)
  m0 <- cefazolin(70, 165, 30, "female", creatinine = 0.6, pregnant = FALSE)
  expect_identical(m1, m0)
  expect_no_match(m1$reference, "Ansari")
  # and through getDrugPK(), as the app calls it
  expect_identical(
    getDrugPK("cefazolin", 70, 165, 30, SEX_FEMALE, creatinine = 0.6, pregnant = TRUE),
    getDrugPK("cefazolin", 70, 165, 30, SEX_FEMALE, creatinine = 0.6))
  # with the creatinine blank the factor is applied
  expect_gt(cefazolin(70, 165, 30, "female", pregnant = TRUE)$PK$default$cl1,
            cefazolin(70, 165, 30, "female")$PK$default$cl1)
})
