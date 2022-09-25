context("server helpers")

test_that("checkNumericCovariates correctly identifies out of bounds input", {
  expect_true(checkNumericCovariates(21, 70, 170))
  expect_false(checkNumericCovariates(0, 0, 0))
  expect_false(checkNumericCovariates(MIN_AGE - 1, 70, 170))
  expect_false(checkNumericCovariates(MAX_AGE + 1, 70, 170))
  expect_false(checkNumericCovariates(5, MIN_WEIGHT - 1, 170))
  expect_false(checkNumericCovariates(5, MAX_WEIGHT + 1, 170))
  expect_false(checkNumericCovariates(5, 70, MIN_HEIGHT - 1))
  expect_false(checkNumericCovariates(5, 70, MAX_HEIGHT + 1))
})
