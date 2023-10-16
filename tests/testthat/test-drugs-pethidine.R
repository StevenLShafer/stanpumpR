test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- pethidine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 18.1,
        v2 = 60.87231,
        v3 = 165.4081,
        cl1 = 0.7625801,
        cl2 = 5.430411,
        cl3 = 1.782144
      )
    ),
    tPeak = 10,
    MEAC = 0.25,
    typical = 0.3,
    upperTypical = 0.2,
    lowerTypical = 0.5,
    reference = "Fit of Sven Bjorkman data"
  )
  expect_equal_rounded(actual, expected)
})
