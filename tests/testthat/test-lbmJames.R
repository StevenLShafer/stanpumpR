test_that("it returns the correct value for male", {
  weight <- 70
  height <- 171
  sex <- "male"

  actual <- lbmJames(weight, height, sex)
  expected <- 55.55067
  expect_equal_rounded(actual, expected)
})

test_that("it returns the correct value for female", {
  weight <- 55
  height <- 160
  sex <- "female"

  actual <- lbmJames(weight, height, sex)
  expected <- 41.36172
  expect_equal_rounded(actual, expected)
})
