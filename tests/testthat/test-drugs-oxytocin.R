context("oxytocin")
library(here)

source(here('data', 'drugs', 'oxytocin.R'))

test_that("returns the correct calculations if weight is > 1", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- oxytocin(weight, height, age, sex)

  expected <- list(
    PK = list(default = list(
      v1 = 10.1,
      v2 = 7.03,
      v3 = 1,
      cl1 = 0.974,
      cl2 = 0.204,
      cl3 = 0
    )),
    tPeak = 5,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.05,
    lowerTypical = 0.2,
    reference = "190912-135448"
  )
  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations if weight is <= 1", {
  weight <- 70
  height <- 0.9
  age <- 50
  sex <- "male"
  actual <- oxytocin(weight, height, age, sex)

  expected <- list(
    PK = list(default = list(
      v1 = 10.1,
      v2 = 7.03,
      v3 = 1,
      cl1 = 0.974,
      cl2 = 0.204,
      cl3 = 0
    )),
    tPeak = 5,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.05,
    lowerTypical = 0.2,
    reference = "190912-135448"
  )
  expect_equal_rounded(actual, expected)
})
