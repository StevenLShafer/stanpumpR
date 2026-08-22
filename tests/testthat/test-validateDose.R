test_that("plain numeric strings pass through unchanged", {
  expect_equal(validateDose("5"), "5")
  expect_equal(validateDose("3.14"), "3.14")
  expect_equal(validateDose("0"), "0")
  expect_equal(validateDose("-15"), "15")
})

test_that("numeric and factor inputs are coerced to a character string", {
  expect_equal(validateDose(5), "5")
  expect_equal(validateDose(2.5), "2.5")
  expect_equal(validateDose(factor("7")), "7")
  expect_equal(validateDose(12345), "12345")
})

test_that("empty / missing inputs return \"0\"", {
  expect_equal(validateDose(""), "0")
  expect_equal(validateDose(NA), "0")
  expect_equal(validateDose(NULL), "0")
  expect_equal(validateDose(NaN), "0")
  expect_equal(validateDose(Inf), "0")
  expect_equal(validateDose(TRUE), "0")
  expect_equal(validateDose(FALSE), "0")
})

test_that("non-numeric characters are stripped", {
  expect_equal(validateDose("1.2mg"), "1.2")
  expect_equal(validateDose("abc"), "0")
  expect_equal(validateDose("1,000"), "1000")
  expect_equal(validateDose("!@#$%"), "0")
  expect_equal(validateDose("\n\t  4.2  "), "4.2")
  expect_equal(validateDose(" 4  .  2"), "4.2")
})

test_that("multiple decimal points collapse to a single one", {
  expect_equal(validateDose("1.2.3"), "1.23")
  expect_equal(validateDose("1..2"), "1.2")
})

test_that("a lone decimal point returns \"0\"", {
  expect_equal(validateDose("."), "0")
})

test_that("the sign is stripped (documents current behavior: no negatives)", {
  expect_equal(validateDose("-5"), "5")
})

test_that("large numbers don't get converted to scientific notation", {
  expect_equal(validateDose(1000000), "1000000")
  expect_equal(validateDose(2e3), "2000")
  expect_equal(validateDose(2e8), "200000000")
})

test_that("error work", {
  expect_error(validateDose(c("1", "2")), "single items")
  expect_error(validateDose(list(20)), "single items")
})
