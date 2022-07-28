context("fentanyl")
library(here)

test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- fentanyl(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 12.1,
        v2 = 35.7,
        v3 = 224,
        cl1 = 0.632,
        cl2 = 2.8,
        cl3 = 1.55
      )
    ),
    tPeak = 3.694,
    MEAC = 0.6,
    typical = 0.72,
    upperTypical = 0.48,
    lowerTypical = 1.2,
    reference = "JPET 1987,240:159-166"
  )
  expect_equal_rounded(actual, expected)
})
