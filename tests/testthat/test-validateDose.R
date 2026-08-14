# Tests for validateDose() (R/validateDose.R).
#
# validateDose is a permissive sanitizer: it is documented to "accept pretty
# much anything and not return an error", coercing junk into a numeric-looking
# character string (or "0"). These tests pin that contract, including the
# edge/adversarial inputs the function is specifically built to survive.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, as part of the
# pre-deployment test plan (see GitHub issue "Test Plan: Input Validation &
# Robustness"). Verified against R/validateDose.R on master with devtools::test().

test_that("plain numeric strings pass through unchanged", {
  expect_equal(validateDose("5"), "5")
  expect_equal(validateDose("3.14"), "3.14")
  expect_equal(validateDose("0"), "0")
})

test_that("numeric and factor inputs are coerced to a character string", {
  expect_equal(validateDose(5), "5")
  expect_equal(validateDose(2.5), "2.5")
  expect_equal(validateDose(factor("7")), "7")
  expect_type(validateDose(5), "character")
})

test_that("empty / missing inputs return \"0\"", {
  expect_equal(validateDose(""), "0")
  expect_equal(validateDose(NA), "0")
  expect_equal(validateDose(NULL), "0")
  expect_equal(validateDose(NaN), "0")
})

test_that("non-numeric characters are stripped", {
  expect_equal(validateDose("1.2mg"), "1.2")   # trailing unit removed
  expect_equal(validateDose("abc"), "0")        # nothing numeric left -> "0"
  expect_equal(validateDose(" 5 "), "5")        # surrounding whitespace removed
  expect_equal(validateDose("1,000"), "1000")   # thousands separator removed
})

test_that("multiple decimal points collapse to a single one", {
  expect_equal(validateDose("1.2.3"), "1.23")
  expect_equal(validateDose("1..2"), "1.2")
})

test_that("a lone decimal point returns \"0\"", {
  expect_equal(validateDose("."), "0")
})

test_that("the sign is stripped (documents current behavior: no negatives)", {
  # validateDose keeps only digits and '.', so a leading '-' disappears.
  # This is intentional for a dose field; pinned here so a refactor is a
  # conscious choice rather than an accident.
  expect_equal(validateDose("-5"), "5")
})

test_that("adversarial inputs do not error and stay bounded", {
  expect_silent(out <- validateDose(paste(rep("9", 1000), collapse = "")))
  expect_type(out, "character")
  expect_equal(validateDose("1e3"), "13")       # 'e' stripped, not exponent
  expect_equal(validateDose("\n\t 4.2 "), "4.2") # control chars removed
})

test_that("vector input raises the documented error", {
  expect_error(validateDose(c("1", "2")), "single items")
})
