
context("expect_equal_rounded")

test_that("works for whole numbers", {
  expect_success(expect_equal_rounded(1, 1))
})

test_that("rounds with set digits", {
  expect_success(expect_equal_rounded(1.111, 1.112, 2))
})

test_that("rounds by default to 5 digits", {
  expect_success(expect_equal_rounded(1.1111111, 1.1111119))
})

test_that("fails if rounding does not produce the same values", {
  expect_failure(expect_equal_rounded(1.11, 1.12, 2))
})
