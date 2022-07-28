context("morphine")
library(here)

test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- morphine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 17.5,
        v2 = 85.82865,
        v3 = 195.646,
        cl1 = 1.233848,
        cl2 = 2.228464,
        cl3 = 0.3195225
      )
    ),
    tPeak = 93.8,
    MEAC = 0.008,
    typical = 0.0096,
    upperTypical = 0.0064,
    lowerTypical = 0.016,
    reference = "Lotsch PK"
  )
  expect_equal_rounded(actual, expected)
})
