calculateBMI <- function(weight, height) {
  weight / (height / 100) ^ 2
}

test_that("returns the correct calculations if BMI is > 30 and male", {
  weight <- 70
  height <- 150
  age <- 50
  sex <- "male"

  expect_gt(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.568805,
        v2 = 4.864832,
        v3 = 3.3799,
        cl1 = 2.518915,
        cl2 = 1.576,
        cl3 = 0.197
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 0.8,
    lowerTypical = 2,
    reference = "Minto/Schnider"
  )
  expect_equal_rounded(actual, expected)
})

test_that("returns the correct calculations if BMI is <= 30 and male", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "male"

  expect_lte(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 5.371314,
        v2 = 8.442319,
        v3 = 3.148362,
        cl1 = 2.462212,
        cl2 = 1.52838,
        cl3 = 0.08064262
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 0.8,
    lowerTypical = 2,
    reference = "Minto/Schnider"
  )
  expect_equal_rounded(actual, expected)
})


test_that("returns the correct calculations if BMI is > 30 and female", {
  weight <- 70
  height <- 150
  age <- 50
  sex <- "female"

  expect_gt(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.568805,
        v2 = 4.29787,
        v3 = 3.3799,
        cl1 = 2.518915,
        cl2 = 1.576,
        cl3 = 0.197
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 0.8,
    lowerTypical = 2,
    reference = "Minto/Schnider"
  )
  expect_equal_rounded(actual, expected)
})


test_that("returns the correct calculations if BMI is <= 30 and female", {
  weight <- 70
  height <- 171
  age <- 50
  sex <- "female"

  expect_lte(calculateBMI(weight, height), 30)

  actual <- remifentanil(weight, height, age, sex)

  expected <- list(
    PK = list(
      default = list(
        v1 = 4.403734,
        v2 = 8.051189,
        v3 = 2.581221,
        cl1 = 2.467679,
        cl2 = 1.715685,
        cl3 = 0.06948164
      )
    ),
    tPeak = 1.6,
    MEAC = 1,
    typical = 1.2,
    upperTypical = 0.8,
    lowerTypical = 2,
    reference = "Minto/Schnider"
  )
  expect_equal_rounded(actual, expected)
})
