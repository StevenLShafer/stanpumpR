context("midazolam")
library(here)

source(here('data', 'drugs', 'midazolam.R'))

test_that("returns the correct calculations", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- midazolam(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 3.3,
        v2 = 17.56348,
        v3 = 96.75715,
        cl1 = 0.5351973,
        cl2 = 2.014531,
        cl3 = 0.8321115
      )
    ),
    tPeak = 4,
    MEAC = 0,
    typical = 0.1,
    upperTypical = 0.04,
    lowerTypical = 0.12,
    reference = "Clin Pharmacol Ther. 1995 Jul;58(1):35-43, and Barr/Zomorodi 2001"
  )
  expect_equal_rounded(actual, expected)
})
