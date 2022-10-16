context("propofol")
library(here)

#
test_that("returns the correct calculations for male", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"
  actual <- propofol(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 6.283078,
        v2 = 20.17078,
        v3 = 137.4287,
        cl1 = 1.551355,
        cl2 = 1.516118,
        cl3 = 0.6698454
      )
    ),
    tPeak = 1.6,
    reference = "Anesthesiology 1998"
  )

  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations for female", {
  weight <- 55
  height <- 160
  age <- 50
  sex <- "female"
  actual <- propofol(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.772932,
        v2 = 15.84847,
        v3 = 92.30994,
        cl1 = 1.519402,
        cl2 = 1.265265,
        cl3 = 0.4969959
      )
    ),
    tPeak = 1.6,
    reference = "Anesthesiology 1998"
  )

  expect_equal_rounded(actual, expected)
})
