test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- sufentanil(weight, height, age, sex)

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
    reference = "Anesthesiology 1995 83:1194-1204"
  )

  expect_equal_rounded(actual, expected)
})
