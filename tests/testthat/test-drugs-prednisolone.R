# prednisolone: see the header of R/drugs_prednisolone.R for what the model is and what it
# leaves out.  Pins were worked out from the published numbers and the
# fat-free-mass factors by hand (Python), not from the code under test.

test_that("returns the published parameters with the fat-free-mass switch off", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- prednisolone(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 110.4,
        v2 = 34.103246,
        v3 = 1,
        cl1 = 0.9116683,
        cl2 = 0.27416503,
        cl3 = 0,
        ka_PO = 0.007,
        bioavailability_PO = 0.74544771,
        tlag_PO = 0
      )
    ),
    tPeak = 0, MEAC = 0, typical = 40, upperTypical = 100, lowerTypical = 10,
    reference = actual$reference
  )
  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # size-free volumes x 1.3049067 and clearances x 1.2209126; a covariate the
  # source wrote on weight sees the pharmacokinetic weight 91.34 kg.
  actual <- prednisolone(120, 170, 50, "male")
  expected <- list(
    v1 = 144.06169,
    v2 = 44.501552,
    v3 = 1,
    cl1 = 1.1130673,
    cl2 = 0.33473155,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("the mammillary equivalent reproduces the Xu system exactly", {
  sys <- prednisolonePairSystem()
  # Eigenvalues of the two-state system, per hour (half-times 0.82 and 2.45 h)
  expect_equal(sys$eigen, c(0.84349199, 0.28333855), tolerance = 1e-7)
  # Prednisolone-centred model
  expect_equal(sys$PL$v1, 110.4)
  expect_equal(sys$PL$cl1, 54.7000981, tolerance = 1e-8)
  expect_equal(sys$PL$cl2, 16.4499019, tolerance = 1e-8)
  expect_equal(sys$PL$v2, 34.1032458, tolerance = 1e-8)
  # Prednisone-centred model
  expect_equal(sys$PN$v1, 397.3)
  expect_equal(sys$PN$cl1, 147.332773, tolerance = 1e-8)
  expect_equal(sys$PN$cl2, 44.3072270, tolerance = 1e-8)
  expect_equal(sys$PN$v2, 68.7493726, tolerance = 1e-8)
  # The AUC identity: a prednisolone bolus gives AUC_L = 191.64 / 10482.7268
  expect_equal(1 / sys$PL$cl1, 191.64 / 10482.7268, tolerance = 1e-8)
  # Oral F that makes the free prednisolone AUC after oral prednisolone exact
  expect_equal(sys$oralF_PL, 0.74544771, tolerance = 1e-7)
  # Prednisone side: formation and the two effective oral coefficients
  expect_equal(sys$kFormation, 0.17539392, tolerance = 1e-7)
  expect_equal(sys$oralF_PN, 0.42029304, tolerance = 1e-7)
  expect_equal(sys$firstPass_PN, 0.49587580, tolerance = 1e-7)
})

test_that("the eigenvalues of the implemented model are the system's", {
  x <- prednisolone(70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  r <- cube(x$cl1 / x$v1, x$cl2 / x$v1, 0, x$cl2 / x$v2, 0) * 60
  expect_equal(sort(r[r > 0], decreasing = TRUE), c(0.84349199, 0.28333855), tolerance = 1e-6)
  # The bolus response starts at 1/VL, as the source's free concentration does
  PK <- getDrugPK("prednisolone", 70, 171, 50, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(PK$p_coef_bolus_l1 + PK$p_coef_bolus_l2, 1 / 110.4)
})
