# dexamethasone: see the header of R/drugs_dexamethasone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- dexamethasone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 41.6,
        v2 = 44.454902,
        v3 = 1,
        cl1 = 0.30166667,
        cl2 = 0.75573333,
        cl3 = 0,
        ka_PO = 0.0156,
        bioavailability_PO = 0.81,
        tlag_PO = 0,
        ka_IM = 0.0076666667,
        bioavailability_IM = 0.77884615,
        tlag_IM = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 50, upperTypical = 100, lowerTypical = 20,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- dexamethasone(120, 170, 50, "male")
  expected <- list(
    v1 = 54.284116,
    v2 = 58.009497,
    v3 = 1,
    cl1 = 0.36830864,
    cl2 = 0.92268436,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("Hong's microconstants give the published macroparameters", {
  x <- dexamethasone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(x$cl1 * 60, 18.1); expect_equal(x$v1, 41.6)
  expect_equal(x$cl2 * 60, 45.344, tolerance = 1e-9)        # Q = k12 Vc
  expect_equal(x$v2, 44.454902, tolerance = 1e-7)            # Vp = Q / k21
  expect_equal(x$cl2 / x$v1 * 60, 1.09); expect_equal(x$cl2 / x$v2 * 60, 1.02)
  # Eigenvalue half-times 0.294 and 3.68 h
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0)
  expect_equal(sort(log(2) / r[r > 0] / 60), c(0.29411, 3.68096), tolerance = 1e-4)
  # Routes: oral F 0.81 with ka 0.936 /h; intramuscular F 0.81/1.04 with ka 0.460 /h
  expect_equal(x$bioavailability_PO, 0.81); expect_equal(x$ka_PO * 60, 0.936)
  expect_equal(x$bioavailability_IM, 0.81 / 1.04); expect_equal(x$ka_IM * 60, 0.460)
})

test_that("pregnancy: clearance x 2 (Ansari 2016), volumes and Q x 2 (Tsuei 1980)", {
  # A woman of 30 years, 70 kg, 165 cm, fat-free-mass scaling on.  CL1, V1,
  # V2 and Q all double, so every rate constant and both half-lives are
  # Hong's, and a dose gives exactly half the concentration.
  on  <- getDrugPK("dexamethasone", 70, 165, 30, SEX_FEMALE, pregnant = TRUE)
  off <- getDrugPK("dexamethasone", 70, 165, 30, SEX_FEMALE, pregnant = FALSE)
  expect_equal(off, getDrugPK("dexamethasone", 70, 165, 30, SEX_FEMALE))
  a <- on$PK$default
  b <- off$PK$default
  expect_equal(DEXAMETHASONE_PREGNANCY_CL_FACTOR, 2)
  expect_equal(DEXAMETHASONE_PREGNANCY_V_FACTOR, 2)
  expect_equal(a$cl1 / b$cl1, DEXAMETHASONE_PREGNANCY_CL_FACTOR, tolerance = 1e-6)
  expect_equal(a$v1 / b$v1, DEXAMETHASONE_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  expect_equal(a$v2 / b$v2, DEXAMETHASONE_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  expect_equal(a$cl2 / b$cl2, DEXAMETHASONE_PREGNANCY_V_FACTOR, tolerance = 1e-6)
  for (p in c("v3", "cl3", "ka_PO", "bioavailability_PO", "tlag_PO",
              "ka_IM", "bioavailability_IM", "tlag_IM")) {
    expect_identical(a[[p]], b[[p]], info = p)
  }
  # The microconstants, and so both half-lives, are unchanged (Hong's 1.09
  # and 1.02 /h with the switch off)
  expect_equal(a$cl2 / a$v1, b$cl2 / b$v1, tolerance = 1e-12)
  expect_equal(a$cl2 / a$v2, b$cl2 / b$v2, tolerance = 1e-12)
  h <- dexamethasone(70, 171, 50, "female", adjustToFFM = FALSE, pregnant = TRUE)$PK$default
  expect_equal(h$cl2 / h$v1 * 60, 1.09, tolerance = 1e-9)
  expect_equal(h$cl2 / h$v2 * 60, 1.02, tolerance = 1e-9)
  r <- cube(h$cl1 / h$v1, h$cl2 / h$v1, 0, h$cl2 / h$v2, 0)
  expect_equal(sort(log(2) / r[r > 0] / 60), c(0.29411, 3.68096), tolerance = 1e-4)
  for (p in c("k10", "k12", "k21", "lambda_1", "lambda_2")) {
    expect_equal(a[[p]], b[[p]], tolerance = 1e-9, info = p)
  }
  # a bolus gives half the concentration at every time
  expect_equal(a$p_coef_bolus_l1 / b$p_coef_bolus_l1, 0.5, tolerance = 1e-9)
  expect_equal(a$p_coef_bolus_l2 / b$p_coef_bolus_l2, 0.5, tolerance = 1e-9)
  expect_equal_rounded(67.06710842, a$v1)
  expect_equal_rounded(71.66975312, a$v2)
  expect_equal_rounded(0.5132713368, a$cl1)
  expect_equal_rounded(1.28584395, a$cl2)
  # the direct call
  x <- dexamethasone(70, 165, 30, "female", pregnant = TRUE)$PK$default
  expect_equal(x[c("v1", "v2", "cl1", "cl2")], a[c("v1", "v2", "cl1", "cl2")])
  expect_equal(dexamethasone(70, 165, 30, "female")$PK$default,
               dexamethasone(70, 165, 30, "female", pregnant = FALSE)$PK$default)
  expect_match(on$reference, "Ansari", fixed = TRUE)
  expect_match(on$reference, "Tsuei", fixed = TRUE)
  expect_no_match(off$reference, "Ansari")
  expect_no_match(off$reference, "Tsuei")
})
