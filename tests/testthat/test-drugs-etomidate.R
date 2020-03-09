context("etomidate")
library(here)

source(here('data', 'drugs', 'etomidate.R'))

test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- etomidate(weight, height, age, sex)

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
    reference = "Anesthesiology 1986 65:19-27"
  )

  expect_equal_rounded(actual, expected)
})
