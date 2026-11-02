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
