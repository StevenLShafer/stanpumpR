# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/all.equal
# Default tolerance is 1.5e-8
expect_equal_rounded <- function (expected, actual, tolerance = 1.5e-6) {
  expect_equal(actual, expected, tolerance = tolerance)
}
