context("dexmedetomidine")
library(here)

test_that("returns the correct calculations for age less than or equal to 1", {
  weight <- 70
  height <- 171
  age <- 1
  sex <- "male"
  actual <- dexmedetomidine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 132,
        v2 = 78.9,
        v3 = 1,
        cl1 = 1.24,
        cl2 = 2.3,
        cl3 = 0
      ),
      CPBStart = list(
        v1 = 115,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB36 = list(
        v1 = 120.1535,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB35 = list(
        v1 = 125.6932,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB34 = list(
        v1 = 131.6601,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB33 = list(
        v1 = 138.1015,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB32 = list(
        v1 = 145.071,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPB31 = list(
        v1 = 152.6307,
        v2 = 144,
        v3 = 1,
        cl1 = 0.0741,
        cl2 = 2.98,
        cl3 = 0
      ),
      CPBEnd = list(
        v1 = 155,
        v2 = 105,
        v3 = 1,
        cl1 = 0.6199935,
        cl2 = 0.209,
        cl3 = 0
      )
    ),
    tPeak = 2,
    MEAC = 0,
    typical = 10,
    upperTypical = 0.4,
    lowerTypical = 0.8,
    reference = "Zuppa BJA 2019"
  )

  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations for age greater than 1", {
  weight <- 70
  height <- 171
  age <- 2
  sex <- "male"
  actual <- dexmedetomidine(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 8.0574,
        v2 = 12.75343,
        v3 = 177.6944,
        cl1 = 0.4447685,
        cl2 = 2.078809,
        cl3 = 1.990178
      )
    ),
    tPeak = 10,
    MEAC = 0,
    typical = 10,
    upperTypical = 0.4,
    lowerTypical = 0.8,
    reference = "Barry Dyck, Check reference and numbers"
  )

  expect_equal_rounded(actual, expected)
})
