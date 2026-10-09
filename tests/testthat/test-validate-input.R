test_that("validateTime: 'MM' gets parsed correctly", {
  expect_equal(validateTime("80"),"80")
  expect_equal(validateTime(80),"80")
  expect_equal(validateTime(6.7), "6.7")
  expect_equal(validateTime("-80"),"80")
  expect_equal(validateTime("8;30"),"830")
  expect_equal(validateTime("8-30"),"830")
  expect_equal(validateTime("8 30"),"830")
})

test_that("validateTime: 'HH:MM' gets parsed correctly", {
  expect_equal(validateTime("08:44"),"08:44")
  expect_equal(validateTime("08:80"),"09:20")
  expect_equal(validateTime("99:99"),"100:39")
  expect_equal(validateTime("\t 08:44  \n"),"08:44")
  expect_equal(validateTime("-08:44"),"08:44")
  expect_equal(validateTime("+08:44"),"08:44")
  expect_equal(validateTime("08:44 pm"),"08:44")
  expect_equal(validateTime("8:3"),"08:03")
  expect_equal(validateTime("0008:0003"),"08:03")
  expect_equal(validateTime("'08:44'"),"08:44")
  expect_equal(validateTime("00:2345"),"39:05")   # 2345 is minutes
  expect_equal(validateTime("1:2:3:4"),"04:54")   # 234 is minutes
})

test_that("validateTime: bad inputs return 0", {
  expect_equal(validateTime("."), "0")
  expect_equal(validateTime("abcde"), "0")
  expect_equal(validateTime(" "), "0")
  expect_equal(validateTime("!@#$%"), "0")
  expect_equal(validateTime(".."), "0")
  expect_equal(validateTime(".:"), "0")
  expect_equal(validateTime(":."), "0")
  expect_equal(validateTime(NA), "0")
  expect_equal(validateTime(NaN), "0")
  expect_equal(validateTime(NULL), "0")
  expect_equal(validateTime(TRUE), "0")
})

test_that("validateTime: colon for empty hours/minutes", {
  expect_equal(validateTime(":"),"00:00")
  expect_equal(validateTime("::"),"00:00")
  expect_equal(validateTime(":30"),"00:30")
  expect_equal(validateTime("30:"),"30:00")
})

test_that("validateTime: large numbers are written out, not in scientific notation", {
  # as.character(1e5) is "1e+05", which validateTime() used to clean to "105"
  expect_equal(validateTime(1e5), "100000")
  expect_equal(validateTime(524160), "524160")
  expect_equal(validateTime(0.1 + 0.2), "0.3")
  expect_equal(validateTime(1/7), "0.142857142857143")
})

test_that("validateTime: decimals and long elapsed times are fixed points", {
  for (x in c("1.5", ".5", "0.25", "10080", "100000", "0.1428571429", "36:00", "100:30", "007")) {
    expect_identical(validateTime(x), x, info = x)
  }
  # an hour count too large for an integer is written out, not an error
  expect_equal(validateTime("12345678901:00"), "12345678901:00")
})

test_that("validateTime: errors on vectors", {
  expect_error(validateTime(c("1", "2")))
  expect_error(validateTime(c(1, 2)))
})

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
