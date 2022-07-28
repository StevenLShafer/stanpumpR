context("methadone")
library(here)

test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- methadone(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 12.6,
        v2 = 85.68,
        v3 = 1237.895,
        cl1 = 0.101556,
        cl2 = 5.1408,
        cl3 = 1.4112
      )
    ),
    tPeak = 11.3,
    MEAC = 0.06,
    typical = 0.072,
    upperTypical = 0.048,
    lowerTypical = 0.12,
    reference = "Inturissi?"
  )
  expect_equal_rounded(actual, expected)
})
