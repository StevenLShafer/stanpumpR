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

# --- Time units (R/utils-time.R, "Time units") -------------------------------

test_that("deltaToClockTime rounds before splitting, so never 'HH:60'", {
  # 59.6 minutes past 08:00 is 08:59.6, i.e. 09:00 to the minute; rounding
  # the minutes after splitting off the hours gave "08:60"
  expect_equal(deltaToClockTime("08:00", 59.6), "09:00")
  expect_equal(deltaToClockTime("08:00", 59.4), "08:59")
  expect_equal(deltaToClockTime("23:00", 59.7), "00:00")
  expect_false(any(grepl(":60$", deltaToClockTime("08:00", seq(0, 240, by = 0.1)))))
})

test_that("the time-unit constants", {
  expect_equal(TIME_UNITS, c(minutes = 1, hours = 60, days = 1440, weeks = 10080))
  expect_true(all(CLOCK_TIME_UNITS %in% names(TIME_UNITS)))
  expect_equal(ACUTE_MAX_PLOT_MINUTES, 7 * 24 * 60)
  expect_setequal(names(MAX_TIMES), names(TIME_UNITS))
  for (unit in names(MAX_TIMES)) {
    t <- MAX_TIMES[[unit]]
    expect_false(is.unsorted(t$times), info = unit)
    expect_true(all(t$steps > 0 & t$steps <= t$times), info = unit)
    # every tick spacing is a whole number of quarter units
    expect_true(all(abs(t$steps / TIME_UNITS[[unit]] * 4 - round(t$steps / TIME_UNITS[[unit]] * 4)) < 1e-9),
                info = unit)
  }
  # minutes and hours offer the same durations, so switching between them
  # never changes Max time
  expect_equal(MAX_TIMES$minutes$times, MAX_TIMES$hours$times)
  expect_equal(max(MAX_TIMES$minutes$times), 24 * 60)
  expect_equal(max(MAX_TIMES$days$times), 365 * 24 * 60)
  expect_equal(max(MAX_TIMES$weeks$times), 52 * 7 * 24 * 60)
  expect_true(all(unlist(lapply(MAX_TIMES, `[[`, "times")) %in% MAX_TIME_VALUES))
  expect_true(all(names(TIME_EXTEND_MARGIN) == names(TIME_UNITS)))
})

test_that("maxTimeChoices: values in minutes, labels in the unit", {
  m <- maxTimeChoices("minutes")
  expect_equal(unname(m), c("60", "120", "240", "360", "480", "720", "1080", "1440"))
  expect_equal(names(m), c("1 hour", "2 hours", "4 hours", "6 hours", "8 hours",
                           "12 hours", "18 hours", "24 hours"))
  expect_identical(maxTimeChoices("hours"), m)
  d <- maxTimeChoices("days")
  expect_equal(names(d)[c(1, 10)], c("2 days", "365 days"))
  expect_equal(unname(d)[10], "525600")   # not "525600" via 5.256e+05
  w <- maxTimeChoices("weeks")
  expect_equal(names(w), paste(c(4, 8, 13, 26, 39, 52), "weeks"))
  expect_equal(unname(w)[6], "524160")
  # every value is written without scientific notation
  expect_false(any(grepl("e", unlist(lapply(names(TIME_UNITS), maxTimeChoices)))))
  # an unknown unit is minutes
  expect_identical(maxTimeChoices("fortnights"), m)
  expect_equal(maxTimeLabel(1440, "minutes"), "24 hours")
  expect_equal(maxTimeLabel(MINS_PER_YEAR, "days"), "365 days")
  expect_true(is.na(maxTimeLabel(MINS_PER_YEAR, "weeks")))
})

test_that("snapMaximum keeps a choice, else takes the next longer, else the longest", {
  expect_equal(snapMaximum(60, "minutes"), 60)
  expect_equal(snapMaximum("1440", "minutes"), 1440)
  expect_equal(snapMaximum(100, "minutes"), 120)
  expect_equal(snapMaximum(10080, "minutes"), 1440)
  expect_equal(snapMaximum(60, "days"), 2 * MINS_PER_DAY)
  expect_equal(snapMaximum(16 * MINS_PER_WEEK, "days"), 182 * MINS_PER_DAY)
  expect_equal(snapMaximum(MINS_PER_YEAR, "days"), MINS_PER_YEAR)
  expect_equal(snapMaximum(MINS_PER_YEAR, "weeks"), 52 * MINS_PER_WEEK)
  expect_equal(snapMaximum(1440, "weeks"), 4 * MINS_PER_WEEK)
  # missing or unreadable: the unit's first choice
  expect_equal(snapMaximum(NA, "minutes"), 60)
  expect_equal(snapMaximum(NULL, "days"), 2 * MINS_PER_DAY)
  expect_equal(snapMaximum("abc", "weeks"), 4 * MINS_PER_WEEK)
})

test_that("legacyTimeUnit: old bookmarks longer than a day open in days", {
  expect_equal(legacyTimeUnit(60), "minutes")
  expect_equal(legacyTimeUnit("1440"), "minutes")
  expect_equal(legacyTimeUnit(2880), "days")
  expect_equal(legacyTimeUnit(MINS_PER_YEAR), "days")
  expect_equal(legacyTimeUnit(NA), "minutes")
  expect_equal(legacyTimeUnit(NULL), "minutes")
})

test_that("timeFormat, its string form, and the Time Display choices", {
  expect_equal(timeFormat("hours", "clock"), c(unit = "hours", mode = "clock"))
  expect_equal(timeFormat("days", "clock"), c(unit = "days", mode = "relative"))
  expect_equal(timeFormat("weeks", "relative"), c(unit = "weeks", mode = "relative"))
  expect_equal(timeFormat(NULL, NULL), c(unit = "minutes", mode = "clock"))
  expect_equal(timeFormat("fortnights", "sideways"), c(unit = "minutes", mode = "clock"))
  expect_equal(timeFormatString(timeFormat("days", "relative")), "days/relative")
  for (unit in names(TIME_UNITS)) for (mode in TIME_MODES) {
    f <- timeFormat(unit, mode)
    expect_identical(parseTimeFormat(timeFormatString(f)), f)
  }
  expect_equal(parseTimeFormat("weeks/clock"), c(unit = "weeks", mode = "relative"))
  expect_null(parseTimeFormat(NULL))
  expect_null(parseTimeFormat(NA_character_))
  expect_null(parseTimeFormat("days"))
  expect_null(parseTimeFormat("days/relative/x"))
  expect_null(parseTimeFormat("eons/relative"))
  expect_null(parseTimeFormat(c("days/relative", "days/relative")))
  expect_equal(timeModeChoices("minutes"), c("Actual time" = "clock", "Elapsed time" = "relative"))
  expect_equal(timeModeChoices("hours"), timeModeChoices("minutes"))
  expect_equal(timeModeChoices("days"), c("Elapsed time" = "relative"))
  expect_equal(timeModeChoices("weeks"), c("Elapsed time" = "relative"))
  expect_equal(timeEntryLabel(timeFormat("days", "relative")), "Time (days)")
  expect_match(timeEntryLabel(timeFormat("minutes", "clock")), "HH:MM")
})

test_that("displayTimeToMinutes: bare numbers are in the unit", {
  expect_identical(displayTimeToMinutes(c("0", "90", "1.5", ".5"), unit = "minutes"),
                   c(0, 90, 1.5, 0.5))
  expect_identical(displayTimeToMinutes(c("1.5", "24"), unit = "hours"), c(90, 1440))
  expect_identical(displayTimeToMinutes(c("2", "0.25", "365"), unit = "days"),
                   c(2880, 360, MINS_PER_YEAR))
  expect_identical(displayTimeToMinutes("52", unit = "weeks"), 524160)
  # minutes are not rounded (the minute plots stay bit for bit as they were);
  # other units are, to 0.001 minute
  expect_identical(displayTimeToMinutes("0.00001", unit = "minutes"), 0.00001)
  expect_identical(displayTimeToMinutes("0.1428571429", unit = "weeks"), 1440)
  # in clock mode a bare number is still an offset from the start
  expect_identical(displayTimeToMinutes("1.5", "08:00", "hours"), 90)
  expect_identical(displayTimeToMinutes("1.5", "not a time", "hours"), 90)
})

test_that("displayTimeToMinutes: colon entries are never scaled", {
  # clock mode: minutes after the start, wrapping past midnight
  expect_identical(displayTimeToMinutes(c("09:30", "07:59", "08:00"), "08:00", "hours"),
                   c(90, MINS_PER_DAY - 1, 0))
  expect_identical(displayTimeToMinutes("00:15", "23:45", "minutes"), 30)
  # elapsed: hours and minutes, read by arithmetic, so 24 hours and more work
  # (lubridate, which reads clock times, wraps "24:04" to 4 and cannot read
  # "36:00")
  expect_identical(displayTimeToMinutes(c("01:30", "24:04", "36:00", "100:30"), unit = "days"),
                   c(90, 1444, 2160, 6030))
  expect_identical(elapsedHourMinute(c("00:30", "1:", ":5", "a:b")), c(30, 60, 5, NA))
})

test_that("displayTimeToMinutes: blank and unreadable are NA", {
  expect_identical(displayTimeToMinutes(c("", NA, " ", "abc", "1e5", "-5", "."), unit = "hours"),
                   rep(NA_real_, 7))
  # a clock time needs a readable procedure start
  expect_identical(displayTimeToMinutes("09:30", "", "minutes"), NA_real_)
  expect_identical(displayTimeToMinutes("09:30", NULL, "minutes"), NA_real_)
  expect_identical(displayTimeToMinutes("25:00", "08:00", "minutes"), NA_real_)
  expect_identical(displayTimeToMinutes(character(0)), numeric(0))
})

test_that("displayTimeToMinutes reads minute tables as clockTimeToDelta did", {
  # The function doseTableClean() used to call, for the times both can read
  x <- c("7", "09:00", "10:15", "06:00", "0", "1.5", "1439")
  expect_identical(displayTimeToMinutes(x, "08:00", "minutes"), clockTimeToDelta("08:00", x))
  expect_identical(displayTimeToMinutes(x, REFERENCE_TIME_NONE, "minutes"),
                   clockTimeToDelta(REFERENCE_TIME_NONE, x))
})

test_that("minutesToDisplayTime: plain numbers, never scientific, NA is blank", {
  expect_equal(minutesToDisplayTime(c(0, 90, 1.5, 100000, 524160), "minutes"),
               c("0", "90", "1.5", "100000", "524160"))
  expect_equal(minutesToDisplayTime(c(90, 1440, 0.001), "hours"), c("1.5", "24", "0.00001666666667"))
  expect_equal(minutesToDisplayTime(c(1440, 7200), "weeks"), c("0.1428571429", "0.7142857143"))
  expect_equal(minutesToDisplayTime(MINS_PER_YEAR, "days"), "365")
  expect_equal(minutesToDisplayTime(c(NA, Inf, 5), "minutes"), c("", "", "5"))
  expect_equal(minutesToDisplayTime(numeric(0), "days"), character(0))
  # rounded to 0.001 minute first
  expect_equal(minutesToDisplayTime(1/3, "minutes"), "0.333")
})

test_that("minutesToDisplayTime writes valid, short time strings", {
  set.seed(20261007)
  m <- c(0, 0.001, 1/7, 59.999, 1440, 7200, 100000, 524160, MINS_PER_YEAR,
         round(stats::runif(2000, 0, MINS_PER_YEAR), 3))
  for (unit in names(TIME_UNITS)) {
    s <- minutesToDisplayTime(m, unit)
    expect_true(all(vapply(s, function(x) identical(validateTime(x), x), logical(1))), info = unit)
    expect_true(all(nchar(s) <= MAX_TIME_STRING_LENGTH), info = unit)
  }
})

test_that("converting between units is exact, both ways and through any chain", {
  # On a 0.001-minute grid up to a year, a time written in any unit reads back
  # as the identical minutes, so a change of unit never changes the simulation
  # (and the simulation cache is reused).
  set.seed(1)
  k <- c(0:2000, sample.int(MINS_PER_YEAR * 1000, 20000))
  m <- round(k / 1000, 3)
  units <- names(TIME_UNITS)
  for (u in units) {
    back <- displayTimeToMinutes(minutesToDisplayTime(m, u), REFERENCE_TIME_NONE, u)
    expect_identical(back, m, info = u)
  }
  # a chain: minutes -> weeks -> days -> hours -> weeks -> minutes
  chain <- c("minutes", "weeks", "days", "hours", "weeks", "minutes")
  s <- minutesToDisplayTime(m, chain[1])
  for (i in 2:length(chain)) {
    s <- minutesToDisplayTime(displayTimeToMinutes(s, REFERENCE_TIME_NONE, chain[i - 1]), chain[i])
  }
  expect_identical(s, minutesToDisplayTime(m, "minutes"))
})

test_that("a stop at day 5 written in weeks still stops at day 5", {
  # Six significant digits wrote day 5 as 0.714286 weeks = 7200.00288 minutes,
  # which put a "0 mg qd" stop just after the day-5 repeat and added a dose.
  days <- data.frame(Drug = c("cefazolin", "cefazolin"), Time = c("0", "5"),
                     Dose = c("1000", "0"), Units = c("mg qd", "mg qd"))
  weeks <- rebaseDoseTimes(days, timeFormat("days", "relative"), timeFormat("weeks", "relative"))
  expect_true(weeks$ok)
  expect_equal(weeks$table$Time, c("0", "0.7142857143"))
  expect_identical(displayTimeToMinutes(weeks$table$Time, unit = "weeks"), c(0, 7200))
  doseOf <- function(dt, unit) {
    data.frame(Drug = dt$Drug, Time = displayTimeToMinutes(dt$Time, unit = unit),
               Dose = as.numeric(dt$Dose), Units = dt$Units)
  }
  fromDays <- expandScheduledDoses(doseOf(days, "days"), 14 * MINS_PER_DAY)
  fromWeeks <- expandScheduledDoses(doseOf(weeks$table, "weeks"), 14 * MINS_PER_DAY)
  expect_identical(fromWeeks$dose, fromDays$dose)
  # daily doses on days 0 to 4: five of 1000 mg (and the stop)
  expect_equal(sum(fromDays$dose$Dose == 1000), 5)
})

test_that("minutesToEntryTime: clock times in clock mode, numbers otherwise", {
  clockMin <- timeFormat("minutes", "clock")
  expect_equal(minutesToEntryTime(c(0, 90, 59.6), clockMin, "08:00"), c("08:00", "09:30", "09:00"))
  # a day or more after the start cannot be a clock time: an offset instead
  expect_equal(minutesToEntryTime(c(1439.4, 1439.7, 2000), clockMin, "08:00"),
               c("07:59", "1439.7", "2000"))
  expect_equal(minutesToEntryTime(90, timeFormat("hours", "clock"), "08:00"), "09:30")
  # no readable start: an offset, which clock mode reads from the start
  expect_equal(minutesToEntryTime(90, timeFormat("hours", "clock"), ""), "1.5")
  expect_equal(minutesToEntryTime(c(90, NA), timeFormat("days", "relative"), "08:00"),
               c("0.0625", ""))
})

test_that("rebaseDoseTimes converts the times of rows with a drug", {
  dt <- data.frame(Drug = c("propofol", "fentanyl", "propofol", ""),
                   Time = c("90", "09:30", "", "5"),
                   Dose = c("1", "2", "3", ""), Units = c("mg", "mcg", "mg", ""))
  minClock <- timeFormat("minutes", "clock")
  hoursClock <- timeFormat("hours", "clock")
  hoursRel <- timeFormat("hours", "relative")
  daysRel <- timeFormat("days", "relative")

  # clock to clock keeps the clock time as typed; the offset is rescaled
  r <- rebaseDoseTimes(dt, minClock, hoursClock, "08:00")
  expect_true(r$ok)
  expect_equal(r$table$Time, c("1.5", "09:30", "", "5"))
  # ... and needs no procedure start to do it
  expect_equal(rebaseDoseTimes(dt, minClock, hoursClock, "")$table$Time, r$table$Time)

  # clock to elapsed reads the clock time against the start
  r <- rebaseDoseTimes(r$table, hoursClock, hoursRel, "08:00")
  expect_equal(r$table$Time, c("1.5", "1.5", "", "5"))
  r <- rebaseDoseTimes(r$table, hoursRel, daysRel, "08:00")
  expect_equal(r$table$Time, c("0.0625", "0.0625", "", "5"))

  # elapsed H:MM becomes a number of the new unit
  e <- data.frame(Drug = "propofol", Time = "36:00", Dose = "1", Units = "mg")
  expect_equal(rebaseDoseTimes(e, timeFormat("minutes", "relative"), daysRel)$table$Time, "1.5")

  # the other columns, and rows without a drug, are left alone
  expect_identical(r$table[, c("Drug", "Dose", "Units")], dt[, c("Drug", "Dose", "Units")])
})

test_that("rebaseDoseTimes refuses, and changes nothing, when a time cannot be read", {
  dt <- data.frame(Drug = c("propofol", "fentanyl"), Time = c("90", "09:30"),
                   Dose = c("1", "2"), Units = c("mg", "mcg"))
  r <- rebaseDoseTimes(dt, timeFormat("minutes", "clock"), timeFormat("days", "relative"), "")
  expect_false(r$ok)
  expect_identical(r$table, dt)
})

test_that("rebaseDoseTimes returns the identical table when nothing changes", {
  dt <- data.frame(Drug = c("propofol", ""), Time = c("0", ""), Dose = c("1", ""), Units = c("mg", ""))
  f <- timeFormat("minutes", "clock")
  expect_identical(rebaseDoseTimes(dt, f, f)$table, dt)
  expect_identical(rebaseDoseTimes(dt, f, timeFormat("weeks", "relative"))$table, dt)
})

test_that("a bookmark made before time units opens converted to days", {
  # Clock times in minutes, with a Max time of a week: app_ui() picks days
  # (legacyTimeUnit()), and onRestored converts the saved table from
  # minutes/clock, the format such a bookmark was always in.
  saved <- data.frame(Drug = c("propofol", "propofol"), Time = c("09:30", "120"),
                      Dose = c("1", "2"), Units = c("mg", "mg"))
  unit <- legacyTimeUnit(10080)
  expect_equal(unit, "days")
  expect_equal(snapMaximum(10080, unit), 10080)
  r <- rebaseDoseTimes(saved, timeFormat("minutes", "clock"), timeFormat(unit, "clock"), "08:00")
  expect_true(r$ok)
  expect_equal(r$table$Time, c("0.0625", "0.08333333333"))
  expect_identical(displayTimeToMinutes(r$table$Time, REFERENCE_TIME_NONE, "days"), c(90, 120))
})

test_that("editedTimesToMinutes keeps the times the user did not edit", {
  f <- timeFormat("days", "relative")
  minutes <- c(1440 / 7, 100, 3000)
  shown <- minutesToEntryTime(minutes, f, "08:00")
  # untouched rows keep their exact minutes (1440/7 is not 0.1428571429 days)
  out <- editedTimesToMinutes(shown, shown, minutes, f, "08:00")
  expect_identical(out, minutes)
  # an edited row is read in the format; a blank one is NA
  out <- editedTimesToMinutes(c(shown[1], "1.5", ""), shown, minutes, f, "08:00")
  expect_identical(out, c(minutes[1], 2160, NA))
  # clock mode: shown to the minute, kept exactly
  fc <- timeFormat("minutes", "clock")
  shown <- minutesToEntryTime(c(30.4, 61), fc, "08:00")
  expect_equal(shown, c("08:30", "09:01"))
  expect_identical(editedTimesToMinutes(c(shown[1], "09:05"), shown, c(30.4, 61), fc, "08:00"),
                   c(30.4, 65))
})
