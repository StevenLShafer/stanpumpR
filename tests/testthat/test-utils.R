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

