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

test_that("is_valid_number correctly validates numbers and boundaries", {
  expect_true(is_valid_number(10, 0, 100))
  expect_true(is_valid_number(0, 0, 100))
  expect_true(is_valid_number(100, 0, 100))
  expect_true(is_valid_number(42))

  # Out of bounds
  expect_false(is_valid_number(-1, 0, 100))
  expect_false(is_valid_number(101, 0, 100))

  # Non-finite values
  expect_false(is_valid_number(Inf))
  expect_false(is_valid_number(-Inf))
  expect_false(is_valid_number(NaN))
  expect_false(is_valid_number(NA_real_))
  expect_false(is_valid_number(NA))

  # Wrong types or lengths
  expect_false(is_valid_number("10"))
  expect_false(is_valid_number(NULL))
  expect_false(is_valid_number(TRUE))
  expect_false(is_valid_number(c(1, 2)))
  expect_false(is_valid_number(numeric(0)))
})
