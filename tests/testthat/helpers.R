expect_equal_rounded <- function (expected, actual, digits = 5) {
  expect_equal(round(actual, digits), round(expected, digits))
}
