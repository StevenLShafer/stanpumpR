test_that("clockTimeToDelta tests", {
  expect_equal(clockTimeToDelta("08:00", c("7", "09:00", "10:15", "06:00")), c(7,60,135,1320))
  expect_equal(clockTimeToDelta("none", c("7", "09:00"," 10:15", "06:00")), c(7,540,615,360))
  expect_equal(clockTimeToDelta("none", c("7", "360")), c(7,360))
  expect_equal(clockTimeToDelta("08:00", "07:59"), MINS_PER_DAY - 1)
  expect_equal(clockTimeToDelta("23:45", "00:15"), 30)
  expect_equal(clockTimeToDelta("08:00", "08:00"), 0)
})

test_that("deltaToClockTime tests", {
  expect_equal(deltaToClockTime("08:00", c(7,60,135,1320)), c("08:07", "09:00", "10:15", "06:00"))
  expect_equal(deltaToClockTime("none", c(7,540,615,360)), c(7,540,615,360))
  expect_equal(deltaToClockTime("08:00", -30), "07:30")
  expect_equal(deltaToClockTime("00:10", -30), "23:40")
  expect_equal(deltaToClockTime("22:00", 180), "01:00")
  expect_equal(deltaToClockTime("01:23", MINS_PER_DAY), "01:23")
  expect_equal(deltaToClockTime("none", c("15", "930")), c(15, 930))
})

test_that("hourMinute tests", {
  expect_equal(hourMinute("12:34"), 754)
  expect_equal(hourMinute("1234"), 754)
  expect_equal(hourMinute("00:00"), 0)
  expect_equal(hourMinute("1:0"), 60)
  expect_equal(hourMinute("8:30"), 510)
  expect_equal(hourMinute("12:3"), 723)
  expect_equal(hourMinute("23:59"), MINS_PER_DAY - 1)
  expect_equal(hourMinute("24:04"), 4)
  expect_true(is.na(hourMinute("A2:34")))
})

test_that("getReferenceTime: 'HH:MM:SS AM' gets parsed correctly", {
  expect_equal(getReferenceTime("08:30:00 AM"),"08:30")
  expect_equal(getReferenceTime("08:30:00 am"),"08:30")
})

test_that("getReferenceTime: 'HH:MM:SS PM' gets parsed correctly", {
  expect_equal(getReferenceTime("08:44:55 PM"),"20:30")
  expect_equal(getReferenceTime("08:44:55 pm"),"20:30")
})

test_that("getReferenceTime: 'HH:MM AM' gets parsed correctly", {
  expect_equal(getReferenceTime("08:30 AM"),"08:30")
  expect_equal(getReferenceTime("08:30 am"),"08:30")
})

test_that("getReferenceTime: 'HH:MM PM' gets parsed correctly", {
  expect_equal(getReferenceTime("08:44 PM"),"20:30")
  expect_equal(getReferenceTime("08:44 pm"),"20:30")
  expect_equal(getReferenceTime("  08:44   pm  "),"20:30")
})

test_that("getReferenceTime: 'HH:MM' gets parsed correctly", {
  expect_equal(getReferenceTime("08:44"),"08:30")
  expect_equal(getReferenceTime("08:14"),"08:00")
  expect_equal(getReferenceTime("08:15"),"08:15")
})

test_that("getReferenceTime: noon and midnight land on the right side of the clock", {
  expect_equal(getReferenceTime("12:00 am"), "00:00")
  expect_equal(getReferenceTime("12:30 am"), "00:30")
  expect_equal(getReferenceTime("12:00 pm"), "12:00")
  expect_equal(getReferenceTime("12:30 pm"), "12:30")
})

test_that("getReferenceTime: a colon-less time is accepted", {
  expect_equal(getReferenceTime("0830"), "08:30")
  expect_equal(getReferenceTime("0830pm"), "20:30")
  expect_equal(getReferenceTime("2345"), "23:45")
})

test_that("getReferenceTime: unparseable input returns NA", {
  expect_true(is.na(getReferenceTime("")))
  expect_true(is.na(getReferenceTime("noon")))
  expect_true(is.na(getReferenceTime("abc")))
  expect_true(is.na(getReferenceTime("25:00")))
  expect_true(is.na(getReferenceTime("8:75")))
})

test_that("formatMinutes: labels sub-hour durations in minutes", {
  expect_equal(formatMinutes(10), "10 minutes")
  expect_equal(formatMinutes(30), "30 minutes")
  expect_equal(formatMinutes(59), "59 minutes")
  expect_equal(formatMinutes(1), "1 minute")
})

test_that("formatMinutes: labels sub-day durations in hours", {
  expect_equal(formatMinutes(60), "1 hour")
  expect_equal(formatMinutes(90), "1.5 hours")
  expect_equal(formatMinutes(60*2), "2 hours")
  expect_equal(formatMinutes(60*12), "12 hours")
})

test_that("formatMinutes: labels whole days as days", {
  expect_equal(formatMinutes(60*24), "1 day")
  expect_equal(formatMinutes(60*24*2), "2 days")
  expect_equal(formatMinutes(60*24*5), "5 days")
  expect_equal(formatMinutes(60*24*6), "6 days")
})

test_that("formatMinutes: labels a week or more in weeks, not days", {
  expect_equal(formatMinutes(MINS_PER_WEEK), "1 week")
  expect_equal(formatMinutes(MINS_PER_WEEK*2), "2 weeks")
  expect_equal(formatMinutes(MINS_PER_WEEK*51), "51 weeks")
})

test_that("formatMinutes: adds leftover days to whole weeks", {
  expect_equal(formatMinutes(MINS_PER_WEEK + MINS_PER_DAY), "1 week 1 day")
  expect_equal(formatMinutes(MINS_PER_WEEK + MINS_PER_DAY*3), "1 week 3 days")
  expect_equal(formatMinutes(MINS_PER_WEEK*2 + MINS_PER_DAY*6), "2 weeks 6 days")
})

test_that("formatMinutes: labels a year or more in years, with leftover weeks", {
  expect_equal(formatMinutes(MINS_PER_YEAR), "1 year")
  expect_equal(formatMinutes(MINS_PER_YEAR + MINS_PER_WEEK), "1 year 1 week")
  expect_equal(formatMinutes(MINS_PER_YEAR + MINS_PER_WEEK*4), "1 year 4 weeks")
})

test_that("formatMinutes: never labels months", {
  expect_false(any(grepl("month", formatMinutes(
    c(MINS_PER_WEEK*5, MINS_PER_WEEK*9, MINS_PER_YEAR, MINS_PER_YEAR + MINS_PER_WEEK*30)
  ))))
})

test_that("formatMinutes: carries a remainder that rounds up to a whole unit", {
  expect_equal(formatMinutes(60*24*2 - 1), "2 days")
  expect_equal(formatMinutes(60*24*3 - 1), "3 days")
  expect_equal(formatMinutes(MINS_PER_WEEK*2 - 1), "2 weeks")
})

test_that("formatMinutes: adds leftover hours to whole days", {
  expect_equal(formatMinutes(60*24+60*4), "1 day 4 hours")
  expect_equal(formatMinutes(60*24+60*8), "1 day 8 hours")
  expect_equal(formatMinutes(60*24+60*1), "1 day 1 hour")
})

test_that("formatMinutes: rounds untidy values instead of exposing fractions", {
  expect_equal(formatMinutes(61), "1 hour")
  expect_equal(formatMinutes(60*24 + 1), "1 day")
  expect_equal(formatMinutes(100), "1.7 hours")
})

test_that("formatMinutes: is vectorized and total on bad input", {
  expect_equal(formatMinutes(c(10, 60, 60*24)), c("10 minutes", "1 hour", "1 day"))
})

test_that("formatMinutes: deals with bad inputs", {
  expect_equal(formatMinutes(numeric(0)), character(0))
  expect_equal(formatMinutes(c(NA, -5, Inf)), c(NA_character_, NA_character_, NA_character_))
})
