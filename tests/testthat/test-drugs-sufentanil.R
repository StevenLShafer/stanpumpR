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
        v2 = 63.1,
        v3 = 261.6,
        cl1 = 0.92,
        cl2 = 1.55,
        cl3 = 0.33
      )
    ),
    tPeak = 5.6,
    MEAC = 0.056,
    typical = 0.0672,
    upperTypical = 0.112,
    lowerTypical = 0.0448,
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
        v2 = 82.339613,
        v3 = 341.36359,
        cl1 = 1.1232396,
        cl2 = 1.8924145,
        cl3 = 0.4029012
  )
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
