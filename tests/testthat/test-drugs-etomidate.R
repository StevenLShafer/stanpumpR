test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- etomidate(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.3,
        v2 = 13.65802,
        v3 = 274.8852,
        cl1 = 1.2915,
        cl2 = 1.7892,
        cl3 = 1.3167
      )
    ),
    tPeak = 1.6,
    MEAC = 0,
    typical = 0.5,
    upperTypical = 0.4,
    lowerTypical = 0.8,
    reference = "Arden JR et al., Anesthesiology 1986;65(1):19-27. https://pubmed.ncbi.nlm.nih.gov/3729056/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- etomidate(120, 170, 50, "male")
  expected <- list(
        v1 = 8.2209119,
        v2 = 17.822441,
        v3 = 358.69953,
        cl1 = 1.5768086,
        cl2 = 2.1844569,
        cl3 = 1.6075756
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
