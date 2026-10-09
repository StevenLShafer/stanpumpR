test_that("validateTime: 'MM' gets parsed correctly", {
  expect_equal(validateTime("80"),"80")
  expect_equal(validateTime(80),"80")
  expect_equal(validateTime(6.7), "6.7")
  expect_equal(validateTime("+80"),"80")
})

test_that("validateTime: 'HH:MM' gets parsed correctly", {
  expect_equal(validateTime("08:44"),"08:44")
  expect_equal(validateTime("08:80"),"09:20")
  expect_equal(validateTime("99:99"),"100:39")
  expect_equal(validateTime("\t 08:44  \n"),"08:44")
  expect_equal(validateTime("+08:44"),"08:44")
  expect_equal(validateTime("8:3"),"08:03")
  expect_equal(validateTime("0008:0003"),"08:03")
  expect_equal(validateTime("'08:44'"),"08:44")
  expect_equal(validateTime("00:2345"),"39:05")   # 2345 is minutes
})

test_that("validateTime: a blank entry is 0", {
  expect_equal(validateTime(""), "0")
  expect_equal(validateTime(" "), "0")
  expect_equal(validateTime(NA), "0")
  expect_equal(validateTime(NaN), "0")
  expect_equal(validateTime(NULL), "0")
})

test_that("validateTime: an entry that is not a time is blank, not 0", {
  # "", the unfinished cell: cleanDoseTable() drops the row, and the dialogs
  # that read a time refuse it
  for (x in c(".", "..", ".:", ":.", "::", "abcde", "!@#$%")) {
    expect_identical(validateTime(x), "", info = x)
  }
  expect_identical(validateTime(TRUE), "")
  expect_identical(validateTime(Inf), "")
})

test_that("validateTime: colon for empty hours/minutes", {
  expect_equal(validateTime(":"),"00:00")
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
  for (x in c("1.5", ".5", "0.25", "10080", "100000", "0.1428571429", "36:00", "100:30", "007", "1.")) {
    expect_identical(validateTime(x), x, info = x)
  }
  # an hour count too large for an integer is written out, not an error
  expect_equal(validateTime("12345678901:00"), "12345678901:00")
})

test_that("validateTime: errors on vectors", {
  expect_error(validateTime(c("1", "2")))
  expect_error(validateTime(c(1, 2)))
})

# Audit finding F01 (October 2026): every character that was not a digit, a
# decimal point or a colon used to be deleted, so one number silently became
# another.  Each of these is now read as the number it is, or refused.
test_that("validateTime: a number is never silently turned into a different number", {
  # negative times are not supported: refused, not made positive
  expect_identical(validateTime("-10"), "")
  expect_identical(validateTime("-80"), "")
  expect_identical(validateTime("-08:44"), "")
  expect_identical(validateTime(-10), "")
  # scientific notation is read, not stripped to its digits ("1e3" was 13)
  expect_identical(validateTime("1e3"), "1000")
  expect_identical(validateTime("1.5E2"), "150")
  expect_identical(validateTime("2.5e-1"), "0.25")
  expect_identical(validateTime("1e400"), "")
  # thousands are separated by commas only in groups of three
  expect_identical(validateTime("1,440"), "1440")
  expect_identical(validateTime("1,5"), "")
  # marks inside the number, letters, and a second colon or decimal point
  for (x in c("8;30", "8-30", "8 30", "08:44 pm", "2h", "1:2:3:4", "1:2:30",
              "1.5:30", "1:30.5", "1.2.3", "08: 44")) {
    expect_identical(validateTime(x), "", info = x)
  }
})

test_that("plain numeric strings pass through unchanged", {
  expect_equal(validateDose("5"), "5")
  expect_equal(validateDose("3.14"), "3.14")
  expect_equal(validateDose("0"), "0")
  expect_equal(validateDose(".5"), ".5")
  expect_equal(validateDose("+5"), "5")
  expect_equal(validateDose("\n\t  4.2  "), "4.2")
  expect_equal(validateDose("1,000"), "1000")
  expect_equal(validateDose("12,345.6"), "12345.6")
})

test_that("numeric and factor inputs are coerced to a character string", {
  expect_equal(validateDose(5), "5")
  expect_equal(validateDose(2.5), "2.5")
  expect_equal(validateDose(factor("7")), "7")
  expect_equal(validateDose(12345), "12345")
  # all the digits a double carries, not format()'s default seven
  expect_equal(validateDose(1234567.89), "1234567.89")
})

test_that("empty / missing inputs return \"0\"", {
  expect_equal(validateDose(""), "0")
  expect_equal(validateDose("  "), "0")
  expect_equal(validateDose(NA), "0")
  expect_equal(validateDose(NULL), "0")
  expect_equal(validateDose(NaN), "0")
})

test_that("an entry that is not a number is blank, not 0", {
  for (x in c("abc", "!@#$%", ".", "e5", "1e")) {
    expect_identical(validateDose(x), "", info = x)
  }
  expect_identical(validateDose(Inf), "")
  expect_identical(validateDose(TRUE), "")
  expect_identical(validateDose(FALSE), "")
})

test_that("large numbers don't get converted to scientific notation", {
  expect_equal(validateDose(1000000), "1000000")
  expect_equal(validateDose(2e3), "2000")
  expect_equal(validateDose(2e8), "200000000")
})

# Audit finding F01, as for validateTime() above
test_that("validateDose: a number is never silently turned into a different number", {
  # negative doses: refused, not made positive ("-5" was 5)
  expect_identical(validateDose("-5"), "")
  expect_identical(validateDose("-15"), "")
  expect_identical(validateDose(-5), "")
  # scientific notation is read, not stripped to its digits ("1e3" was 13)
  expect_identical(validateDose("1e3"), "1000")
  expect_identical(validateDose("1E3"), "1000")
  expect_identical(validateDose("2.5e-1"), "0.25")
  expect_identical(validateDose("1e-7"), "0.0000001")
  # a decimal comma is not a thousands separator ("1,5" was 15)
  expect_identical(validateDose("1,5"), "")
  expect_identical(validateDose("1,0000"), "")
  # a second decimal point ("1.2.3" was 1.23), marks inside the number, and a
  # unit, which would read "500 mcg" in a mg row as 500 mg
  for (x in c("1.2.3", "1..2", " 4  .  2", "1 000", "1.2mg", "5 mg", "500 mcg", "1/2", "0x10")) {
    expect_identical(validateDose(x), "", info = x)
  }
})

test_that("a refused dose or time leaves the dose table row incomplete, so it is ignored", {
  DT <- data.frame(
    Drug  = c("propofol", "propofol", "propofol"),
    Time  = c("0", validateTime("-10"), "20"),
    Dose  = c("10", "20", validateDose("-5")),
    Units = c("mg", "mg", "mg")
  )
  clean <- cleanDoseTable(DT)
  expect_equal(clean$Time, "0")
  expect_equal(clean$Dose, 10)
  expect_true(validateDoseTableInput(DT))
})

test_that("every accepted time is one validateTime() leaves unchanged", {
  # validateDoseTableInput() checks the stored times this way
  for (x in c("-10", "1e3", "1,440", "+08:44", "0:80", " 12 ", "'1.5'", ":")) {
    out <- validateTime(x)
    if (nzchar(out)) expect_identical(validateTime(out), out, info = x)
  }
})

test_that("isBlankEntry", {
  for (x in list(NULL, NA, NA_character_, "", "  ", "\t", "''", factor(""))) {
    expect_true(isBlankEntry(x), info = deparse(x))
  }
  for (x in list("0", 0, "-5", "abc", factor("7"), 5)) {
    expect_false(isBlankEntry(x), info = deparse(x))
  }
})

test_that("error work", {
  expect_error(validateDose(c("1", "2")), "single items")
  expect_error(validateDose(list(20)), "single items")
})

# The grid's Dose column must be text.  A numeric Handsontable column parses
# a pasted entry itself before hookSanitize() (inst/www/hot_funs.js) sees it:
# "1,000" became 1 and "1,5" 1.5, in the browser, whatever validateDose() says.
test_that("the dose grid passes pasted doses to the sanitiser as typed", {
  hot <- createHOT(doseTableInit, getDrugDefaultsGlobal())
  headers <- hot$x$colHeaders
  types <- vapply(hot$x$columns, function(col) col$type, character(1))
  expect_equal(types[headers == "Dose"], "text")
  expect_equal(types[headers == "Time"], "text")
})
