test_that("%btwn% is inclusive and vectorized", {
  expect_true(5 %btwn% c(1, 10))
  expect_true(1 %btwn% c(1, 10))
  expect_true(10 %btwn% c(1, 10))
  expect_false(0 %btwn% c(1, 10))
  expect_false(11 %btwn% c(1, 10))
  expect_equal(c(0, 5, 11) %btwn% c(1, 10), c(FALSE, TRUE, FALSE))
})

test_that("isEmailValid accepts well-formed single addresses", {
  expect_true(isEmailValid("a@b.com"))
  expect_true(isEmailValid("steve.shafer@stanford.edu"))
  expect_true(isEmailValid("user+tag@example.co.uk"))
  expect_true(isEmailValid("first.last-name@sub.domain.org"))
})

test_that("isEmailValid rejects malformed addresses", {
  expect_false(isEmailValid(""))
  expect_false(isEmailValid(" a@b.com"))
  expect_false(isEmailValid("notanemail"))
  expect_false(isEmailValid("a@b"))
  expect_false(isEmailValid("@b.com"))
  expect_false(isEmailValid("a@.com"))
  expect_false(isEmailValid("a b@c.com"))
})

test_that("isEmailValid rejects multi-recipient and injection payloads", {
  expect_false(isEmailValid("a@b.com, c@d.com"))
  expect_false(isEmailValid("a@b.com;c@d.com"))
  expect_false(isEmailValid("a@b.com victim@evil.com"))
  expect_false(isEmailValid("a@b.com\r\nBcc: victim@evil.com"))
  expect_false(isEmailValid("a@b.com\nBcc: victim@evil.com"))
})

test_that("drugHasNonZeroDoses detects any non-zero dose for a drug", {
  dt <- data.frame(
    Drug = c("propofol", "propofol", "fentanyl", "fentanyl", "fentanyl", "ketamine"),
    Dose = c("100", "0", "0", "", "abc", "0")
  )
  expect_true(drugHasNonZeroDoses(dt, "propofol"))
  expect_false(drugHasNonZeroDoses(dt, "fentanyl"))
  expect_false(drugHasNonZeroDoses(dt, "ketamine"))
  expect_false(drugHasNonZeroDoses(dt, "midazolam"))
})

test_that("formatMinutes labels sub-hour durations in minutes", {
  expect_equal(formatMinutes(10), "10 minutes")
  expect_equal(formatMinutes(30), "30 minutes")
  expect_equal(formatMinutes(59), "59 minutes")
  expect_equal(formatMinutes(1), "1 minute")
})

test_that("formatMinutes labels sub-day durations in hours", {
  expect_equal(formatMinutes(60), "1 hour")
  expect_equal(formatMinutes(90), "1.5 hours")
  expect_equal(formatMinutes(60*2), "2 hours")
  expect_equal(formatMinutes(60*12), "12 hours")
})

test_that("formatMinutes labels whole days as days", {
  expect_equal(formatMinutes(60*24), "1 day")
  expect_equal(formatMinutes(60*24*2), "2 days")
  expect_equal(formatMinutes(60*24*5), "5 days")
  expect_equal(formatMinutes(60*24*7), "7 days")
})

test_that("formatMinutes adds leftover hours to whole days", {
  expect_equal(formatMinutes(60*24+60*4), "1 day 4 hours")
  expect_equal(formatMinutes(60*24+60*8), "1 day 8 hours")
  expect_equal(formatMinutes(60*24+60*1), "1 day 1 hour")
})

test_that("formatMinutes rounds untidy values instead of exposing fractions", {
  expect_equal(formatMinutes(61), "1 hour")
  expect_equal(formatMinutes(60*24 + 1), "1 day")
  expect_equal(formatMinutes(100), "1.7 hours")
})

test_that("formatMinutes gives the no-limit sentinel its own label", {
  expect_equal(formatMinutes(MAX_TIME_NO_LIMIT), "No limit")
})

test_that("formatMinutes is vectorized and total on bad input", {
  expect_equal(formatMinutes(c(10, 60, 60*24)), c("10 minutes", "1 hour", "1 day"))
})

test_that("formatMinutes deals with bad inputs", {
  expect_equal(formatMinutes(numeric(0)), character(0))
  expect_equal(formatMinutes(c(NA, -5, Inf)), c(NA_character_, NA_character_, NA_character_))
})
