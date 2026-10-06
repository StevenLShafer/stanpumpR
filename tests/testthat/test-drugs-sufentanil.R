test_that("returns the published parameters with total-body-weight scaling", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  # The switch off reproduces the pre-fat-free-mass output exactly.
  actual <- sufentanil(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(
      default = list(
        v1 = 14.3,
        v2 = 63.38694,
        v3 = 251.9,
        cl1 = 0.92235,
        cl2 = 1.55298,
        cl3 = 0.32747
      )
    ),
    tPeak = 5.8,
    MEAC = 0.056,
    typical = 0.0672,
    upperTypical = 0.0448,
    lowerTypical = 0.112,
    reference = "Gepts E et al., Anesthesiology 1995;83(6):1194-1204. https://pubmed.ncbi.nlm.nih.gov/8533912/"
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: FFM 71.09 kg against the 54.48 kg reference, so
  # volumes x 1.3049067 and clearances x 1.3049067^0.75 = 1.2209126 (worked out
  # from the Al-Sallami formula by hand, not from the code under test).
  actual <- sufentanil(120, 170, 50, "male")
  expected <- list(
        v1 = 18.660165,
        v2 = 82.71404,
        v3 = 328.70599,
        cl1 = 1.1261088,
        cl2 = 1.8960529,
        cl3 = 0.39981226
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
