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
        cl1 = 0.55655186,
        cl2 = 0.90666667,
        cl3 = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 1, upperTypical = 2, lowerTypical = 0.5,
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
    cl1 = 0.65048186,
    cl2 = 1.1069608,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the unbound model is Komatsu's, with creatinine clearance from the covariates", {
  x <- cefazolin(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  # Komatsu 2024: Vcu 36.6, Vpu 42.2, Qu 54.4 L/h; CLu 29.3 (CrCL/70)^0.586 L/h
  expect_equal(x$v1, 36.6); expect_equal(x$v2, 42.2); expect_equal(x$cl2 * 60, 54.4)
  crcl <- creatinineClearanceCG(70, 50, "male")           # 87.5 mL/min
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
