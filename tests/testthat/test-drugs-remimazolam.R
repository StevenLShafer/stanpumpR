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
