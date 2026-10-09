test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly: Eleveld's
  # Fsize = weight/70 = 1, V3 x exp(0.00731 x (50 - 35)) for age.
  actual <- remimazolam(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    v1 = 4.31,
    v2 = 12.3,
    v3 = 20.75551,
    cl1 = 1.12,
    cl2 = 1.45,
    cl3 = 0.3235427
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  expect_equal(actual$tPeak, 2.5)
  expect_equal(actual$reference, "Eleveld DJ et al., Br J Anaesth 2025;135(1):206-217. https://pubmed.ncbi.nlm.nih.gov/40312166/")
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand).  cl2 and cl3 follow their
  # volumes with the 0.75 exponent, as in the published model.
  actual <- remimazolam(120, 170, 50, "male")
  expected <- list(
    v1 = 5.624148,
    v2 = 16.05035,
    v3 = 27.084,
    cl1 = 1.367422,
    cl2 = 1.770323,
    cl3 = 0.3950173
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("applies the published female clearance factor, exp(0.163)", {
  # Eleveld 2025 Table 1: KCLsex = 16.3 %, so a woman's CL is the man's
  # x exp(0.163) = 1.177037.  At the 70 kg, 170 cm, 35 y reference with the
  # switch off every size factor is 1, so CL = 1.12 x exp(0.163) L/min.  The
  # intercompartmental clearances depend on sex only through V3.
  male <- remimazolam(70, 170, 35, "male", adjustToFFM = FALSE)$PK$default
  female <- remimazolam(70, 170, 35, "female", adjustToFFM = FALSE)$PK$default
  expect_equal(female$cl1, 1.12 * exp(0.163))
  expect_equal_rounded(female$cl1, 1.318281)
  expect_equal(female$cl1 / male$cl1, exp(0.163))
  expect_equal(female$cl2, male$cl2)

  # The factor is applied on top of the size scaling in both switch positions.
  for (ffm in c(TRUE, FALSE)) {
    m <- remimazolam(60, 165, 40, "male", adjustToFFM = ffm)$PK$default
    f <- remimazolam(60, 165, 40, "female", adjustToFFM = ffm)$PK$default
    sizeRatio <-
      pkSizeFactors(60, 165, 40, "female", ffm, legacyClearance = (60 / 70)^0.75)$clearance /
      pkSizeFactors(60, 165, 40, "male", ffm, legacyClearance = (60 / 70)^0.75)$clearance
    expect_equal(f$cl1 / m$cl1, exp(0.163) * sizeRatio)
  }
})

test_that("a woman's parameters with total-body-weight scaling", {
  # 60 kg, 165 cm, 40 y woman, switch off: volumes x 60/70, clearances x
  # (60/70)^0.75; V3 x exp(0.00731 x 5) x exp(0.287); CL x exp(0.163).
  actual <- remimazolam(60, 165, 40, "female", adjustToFFM = FALSE)
  expected <- list(
    v1 = 3.694286,
    v2 = 10.54286,
    v3 = 22.03343,
    cl1 = 1.174351,
    cl2 = 1.291689,
    cl3 = 0.3383710
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})

test_that("a woman's parameters scaled to fat-free mass", {
  # 60 kg, 165 cm, 40 y woman, switch on: FFM 39.85 kg, volumes x 0.7314943,
  # clearances x 0.7909667; the sex factors as above.
  actual <- remimazolam(60, 165, 40, "female")
  expected <- list(
    v1 = 3.152740,
    v2 = 8.997380,
    v3 = 18.80355,
    cl1 = 1.042716,
    cl2 = 1.146902,
    cl3 = 0.3004426
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
