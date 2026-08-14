# Tests for the small helpers in R/utils.R: %btwn%, isEmailValid,
# drugHasNonZeroDoses. These had no dedicated coverage. isEmailValid in
# particular is a security-relevant guard: it is the server-side check that
# stops the shared mailer from being pointed at multiple / malformed
# recipients, so the negative cases below double as security assertions.
#
# Provenance: drafted by Claude Code (Opus 4.8), 2026-08-13, for the
# pre-deployment test plan (input validation + security). Verified against
# R/utils.R on master with devtools::test().

test_that("%btwn% is inclusive and vectorized", {
  expect_true(5 %btwn% c(1, 10))
  expect_true(1 %btwn% c(1, 10))    # lower bound inclusive
  expect_true(10 %btwn% c(1, 10))   # upper bound inclusive
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
  expect_false(isEmailValid("notanemail"))
  expect_false(isEmailValid("a@b"))          # no TLD
  expect_false(isEmailValid("@b.com"))       # no local part
  expect_false(isEmailValid("a@.com"))       # no domain label
  expect_false(isEmailValid("a b@c.com"))    # embedded space
})

test_that("isEmailValid rejects multi-recipient and injection payloads", {
  # These are the security-relevant cases: the shared account must not be
  # coerced into sending to extra recipients or into header injection.
  expect_false(isEmailValid("a@b.com, c@d.com"))          # comma list
  expect_false(isEmailValid("a@b.com;c@d.com"))           # semicolon list
  expect_false(isEmailValid("a@b.com victim@evil.com"))   # space-separated
  expect_false(isEmailValid("a@b.com\r\nBcc: victim@evil.com"))  # CRLF header injection
  expect_false(isEmailValid("a@b.com\nBcc: victim@evil.com"))
})

test_that("drugHasNonZeroDoses detects any non-zero dose for a drug", {
  dt <- data.frame(
    Drug = c("propofol", "propofol", "fentanyl", "ketamine"),
    Dose = c("100", "0", "0", ""),
    stringsAsFactors = FALSE
  )
  expect_true(drugHasNonZeroDoses(dt, "propofol"))   # 100 is non-zero
  expect_false(drugHasNonZeroDoses(dt, "fentanyl"))  # only a zero dose
  expect_false(drugHasNonZeroDoses(dt, "ketamine"))  # only a blank dose
  expect_false(drugHasNonZeroDoses(dt, "midazolam")) # drug absent from table
})

test_that("drugHasNonZeroDoses ignores blanks and non-numeric doses", {
  dt <- data.frame(
    Drug = c("propofol", "propofol", "propofol"),
    Dose = c("", "abc", "0"),
    stringsAsFactors = FALSE
  )
  expect_false(drugHasNonZeroDoses(dt, "propofol"))
})
