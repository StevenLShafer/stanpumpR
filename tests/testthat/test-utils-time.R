test_that("clockTimeToDelta: valid delta times are returned", {
  expect_equal(clockTimeToDelta("08:00", c("7", "09:00", "10:15", "06:00")), c(7,60,135,1320))
})

test_that("clockTimeToDelta: valid delta times are returned", {
  expect_equal(clockTimeToDelta("none", c("7", "09:00"," 10:15", "06:00")), c(7,540,615,360))
})

test_that("clockTimeToDelta: valid delta times are returned", {
  expect_equal(clockTimeToDelta("none", c("7", "360")), c(7,360))
})

test_that("deltaToClockTime: valid clock times are returned", {
  expect_equal(deltaToClockTime("08:00", c(7,60,135,1320)), c("08:07", "09:00", "10:15", "06:00"))
})

test_that("deltaToClockTime: valid clock times are returned", {
  expect_equal(deltaToClockTime("none", c(7,540,615,360)), c(7,540,615,360))
})

test_that("hourMinute: HH:MM gets parsed correctly", {
    expect_equal(hourMinute("12:34"),754)
})

test_that("hourMinute: HHMM gets parsed correctly", {
    expect_equal(hourMinute("1234"),754)
})

test_that("hourMinute: AH:MM gives NA", {
    expect_true(is.na(hourMinute("A2:34")))
})

test_that("getReferenceTime: 'HH:MM:SS AM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:30:00 AM"),"08:30")
})

test_that("getReferenceTime: 'HH:MM:SS PM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44:55 PM"),"20:30")
})

test_that("getReferenceTime: 'HH:MM AM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:30 AM"),"08:30")
})

test_that("getReferenceTime: 'HH:MM PM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44 PM"),"20:30")
})

test_that("getReferenceTime: 'HH:MM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44"),"08:30")
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
