test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly: Dahan's
  # parameters, unscaled.
  actual <- oliceridine(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    v1 = 28,
    v2 = 29.1,
    v3 = 1,
    cl1 = 0.5283333,
    cl2 = 0.625,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
  expect_equal(actual$tPeak, 15)
  expect_equal(actual$reference, "Dahan A et al., Anesthesiology 2020;133(3):559-568. https://pubmed.ncbi.nlm.nih.gov/32788558/")
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  # (worked out from the Al-Sallami formula by hand).  v3 = 1 and cl3 = 0 are
  # placeholders for a missing compartment and are left alone.
  actual <- oliceridine(120, 170, 50, "male")
  expected <- list(
    v1 = 36.53739,
    v2 = 37.97278,
    v3 = 1,
    cl1 = 0.6450488,
    cl2 = 0.7630704,
    cl3 = 0
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
