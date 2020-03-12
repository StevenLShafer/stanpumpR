
context("expect_equal_rounded")

test_that("works for whole numbers", {
  expect_success(expect_equal_rounded(1, 1))
})

test_that("can have a smaller tolerance", {
  expect_success(expect_equal_rounded(1.111, 1.112, 1e-3))
})

test_that("has a tolerance to 6 digits", {
  expect_success(expect_equal_rounded(1.1111111, 1.1111119))
})

test_that("fails if rounding does not produce the same values", {
  expect_failure(expect_equal_rounded(1.11, 1.12, 1e-3))
})
