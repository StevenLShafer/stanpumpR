# Writing a time in the display unit: axis labels and title, the hover, the
# recovery labels and the export column (R/utils-time-display.R).  The engine
# stays in minutes; these only change how minutes are written, so every
# expectation below is a hand conversion (60 min an hour, 1440 a day, 10080 a
# week).  (Claude Code, 2026-10-07.)

local_mocked_bindings(outputComments = function(...) {})

test_that("the display table's minutes per unit are the usual ones", {
  expect_equal(TIME_UNIT_DISPLAY$unit, c("minutes", "hours", "days", "weeks"))
  expect_equal(TIME_UNIT_DISPLAY$minutes, c(1, 60, 1440, 10080))
  # ...and agree with the entry side's table, once both halves are present
  if (exists("TIME_UNITS")) {
    expect_equal(unname(TIME_UNITS[TIME_UNIT_DISPLAY$unit]), TIME_UNIT_DISPLAY$minutes)
  }
  expect_error(formatElapsed(1, "fortnights"), "Unknown time unit")
  expect_error(axisTimeLabels(0, c("days", "weeks")), "Unknown time unit")
})

test_that("formatElapsed writes a duration in the display unit", {
  # Minutes keep the hover's tenth of a minute
  expect_equal(formatElapsed(c(0, 1, 90, 12.34), "minutes"),
               c("0 minutes", "1 minute", "90 minutes", "12.3 minutes"))
  # 90 min = 1.5 h; 480 min = 8 h
  expect_equal(formatElapsed(c(60, 90, 480), "hours"),
               c("1 hour", "1.5 hours", "8 hours"))
  # 480 min = 1/3 day, to two decimals; 34992 min = 24.3 days
  expect_equal(formatElapsed(c(1440, 480, 34992), "days"),
               c("1 day", "0.33 days", "24.3 days"))
  # 52 weeks = 524160 min; 34977.6 min = 3.47 weeks
  expect_equal(formatElapsed(c(10080, 34977.6, 524160), "weeks"),
               c("1 week", "3.47 weeks", "52 weeks"))
  # Never scientific notation, which as.character(1e5) would give
  expect_equal(formatElapsed(1e5, "minutes"), "100000 minutes")
  # Missing stays missing; a hover a hair left of zero is zero, not "-0"
  expect_true(is.na(formatElapsed(NA, "days")))
  expect_equal(formatElapsed(-0.001, "minutes"), "0 minutes")
})

test_that("formatPlotTime gives clock time in clock mode and elapsed time otherwise", {
  expect_equal(formatPlotTime(90, "hours"), "1.5 hours")
  expect_equal(formatPlotTime(90, "minutes", REFERENCE_TIME_NONE), "90 minutes")
  # 08:00 + 90 min; the unit does not matter on the clock
  expect_equal(formatPlotTime(90, "hours", "08:00"), "09:30")
  expect_equal(formatPlotTime(90, "minutes", "08:00"), "09:30")
  # Whole minutes before the clock is read: 59.7 min after midnight is 01:00
  expect_equal(formatPlotTime(59.7, "minutes", "00:00"), "01:00")
})

test_that("axisTimeLabels writes the minute breaks in the display unit", {
  expect_equal(axisTimeLabels(seq(0, 60, by = 10), "minutes"),
               c("0", "10", "20", "30", "40", "50", "60"))
  # 15-minute ticks on a 1-hour plot in hours
  expect_equal(axisTimeLabels(seq(0, 60, by = 15), "hours"),
               c("0", "0.25", "0.5", "0.75", "1"))
  expect_equal(axisTimeLabels(seq(0, 1440, by = 240), "hours"),
               c("0", "4", "8", "12", "16", "20", "24"))
  # 365 days ticked every 30 days (43200 min): the last tick is day 360
  expect_equal(axisTimeLabels(seq(0, 525600, by = 43200), "days"),
               as.character(seq(0, 360, by = 30)))
  # 52 weeks ticked every 4 weeks (40320 min)
  expect_equal(axisTimeLabels(seq(0, 524160, by = 40320), "weeks"),
               as.character(seq(0, 52, by = 4)))
  # Large minute values stay plain numbers
  expect_equal(axisTimeLabels(c(0, 1e5, 5e5), "minutes"), c("0", "100000", "500000"))
  # Clock mode keeps HH:MM, starting at the procedure start
  expect_equal(axisTimeLabels(c(0, 30, 60, 90), "hours", "08:00"),
               c("08:00", "08:30", "09:00", "09:30"))
})

test_that("timeAxisTitle names the unit, or just Time under clock labels", {
  expect_equal(timeAxisTitle("minutes"), "Time (minutes)")
  expect_equal(timeAxisTitle("hours"), "Time (hours)")
  expect_equal(timeAxisTitle("days"), "Time (days)")
  expect_equal(timeAxisTitle("weeks"), "Time (weeks)")
  expect_equal(timeAxisTitle("minutes", "08:00"), "Time")
  expect_equal(timeAxisTitle("hours", "08:00"), "Time")
})

test_that("recoveryAxisUnit picks min, h, d or wk from the panel's longest time", {
  unitOf <- function(m) recoveryAxisUnit(m)$abbreviation
  # below 2 hours (120 min): minutes
  expect_equal(unitOf(0), "min")
  expect_equal(unitOf(119.9), "min")
  # 2 hours up to 2 days (2880 min): hours; a day (the effect-site cap) is hours
  expect_equal(unitOf(120), "h")
  expect_equal(unitOf(1440), "h")
  expect_equal(unitOf(2879), "h")
  # 2 days up to 3 weeks (30240 min): days; a week (the plasma cap) is days
  expect_equal(unitOf(2880), "d")
  expect_equal(unitOf(10080), "d")
  # 3 weeks and beyond: weeks
  expect_equal(unitOf(30240), "wk")
  expect_equal(unitOf(524160), "wk")
  # Nothing to time, or nothing known, stays in minutes
  expect_equal(unitOf(NA), "min")
  expect_equal(unitOf(NULL), "min")
  expect_equal(recoveryAxisUnit(1440),
               list(unit = "hours", factor = 60, abbreviation = "h"))
  expect_equal(recoveryAxisUnit(10080),
               list(unit = "days", factor = 1440, abbreviation = "d"))
})

test_that("recoveryHorizonFor is a day on the effect site and at least a week on the plasma", {
  expect_equal(recoveryHorizonFor(list(ke0 = 0.15), 60), MINS_PER_DAY)
  expect_equal(recoveryHorizonFor(list(ke0 = 0), 60), MINS_PER_WEEK)
  # Plasma-timed: the plot's length when that is longer than a week
  expect_equal(recoveryHorizonFor(list(ke0 = 0), 52 * MINS_PER_WEEK), 52 * MINS_PER_WEEK)
  expect_equal(recoveryHorizonFor(list(ke0 = 0.15), 52 * MINS_PER_WEEK), MINS_PER_DAY)
  # A drug entry carries one set per PK event; any effect site counts, as in
  # advanceClosedForm1()
  expect_equal(recoveryHorizonFor(list(PK = list(default = list(ke0 = 0),
                                                 later = list(ke0 = 0.1))), 60),
               MINS_PER_DAY)
  expect_equal(recoveryHorizonFor(list(PK = list(default = list(ke0 = 0))), 60),
               MINS_PER_WEEK)
  # The inhaled agents' washout is searched a day ahead (gasRecovery.R)
  expect_equal(recoveryHorizonFor(list(isGas = TRUE), 2 * MINS_PER_WEEK), MINS_PER_DAY)
})

test_that("recoveryHorizonFor agrees with where the engines stop", {
  # Doses large enough that neither drug falls to its threshold within its
  # horizon, so the engine reports the horizon itself throughout.  Fentanyl
  # is timed on its effect site; vancomycin has none and is timed on its
  # plasma.  A one-hour plot keeps the plasma horizon at a week either way.
  dd <- getDrugDefaultsGlobal(FALSE)
  noEvents <- data.frame(Time = double(), Event = character())
  DT <- data.frame(Drug = c("fentanyl", "vancomycin"), Time = 0,
                   Dose = c(5000, 100000), Units = c("mcg", "mg"))
  drugs <- processdoseTable(DT, noEvents,
                            recalculatePK(NULL, dd, DT, 50, 70, 170, "male"),
                            60, TRUE)
  for (d in c("fentanyl", "vancomycin")) {
    expect_equal(drugs[[d]]$max$Recovery, recoveryHorizonFor(drugs[[d]], 60))
  }
  expect_equal(recoveryHorizonFor(drugs$fentanyl, 60), MINS_PER_DAY)
  expect_equal(recoveryHorizonFor(drugs$vancomycin, 60), MINS_PER_WEEK)
})

test_that("formatRecovery writes the panel's unit and 'more than' at the horizon", {
  # A panel whose longest time is a day reads in hours: 700 min = 11.67 h
  expect_equal(formatRecovery(700, 1440, 1440), "11.67 hours")
  expect_equal(formatRecovery(1440, 1440, 1440), "more than 24 hours")
  # A plasma-timed panel capped at a week reads in days
  expect_equal(formatRecovery(10080, 10080, 10080), "more than 7 days")
  expect_equal(formatRecovery(2160, 10080, 10080), "1.5 days")
  expect_equal(formatRecovery(524160, 524160, 524160), "more than 52 weeks")
  # A short panel stays in minutes, as the hover always read
  expect_equal(formatRecovery(12.34, 30, 1440), "12.3 minutes")
  expect_equal(formatRecovery(0, 30, 1440), "0 minutes")
  # A pending dose is not a time
  expect_equal(formatRecovery(NA, 1440, 1440), "not yet, dose still being absorbed")
})

# A drug's results in the long form the engines return: Drug, Time, Site, Y.
longResults <- function(Time, ...) {
  sites <- list(...)
  do.call(rbind, lapply(names(sites), function(s)
    data.frame(Drug = "x", Time = Time, Site = s, Y = sites[[s]])))
}

test_that("hoverConcentration interpolates at the hovered time, not the nearest point", {
  r <- longResults(c(0, 10, 20),
                   Plasma = c(0, 10, 4), `Effect Site` = c(0, 2, 6),
                   CpNormCp = c(0, 100, 40), CeNormCp = c(0, 20, 60))
  # Halfway between 10 and 20: Ce (2 + 6) / 2 = 4
  expect_equal(hoverConcentration(r, 15), list(label = "Ce", value = 4))
  # A quarter of the way: 2 + (6 - 2) / 4 = 3
  expect_equal(hoverConcentration(r, 12.5)$value, 3)
  # Under normalization, the normalized series the panel shows: (20 + 60) / 2
  expect_equal(hoverConcentration(r, 15, "Peak plasma"), list(label = "Ce", value = 40))
  # Held at the ends for a hover just outside the data
  expect_equal(hoverConcentration(r, 25)$value, 6)
})

test_that("hoverConcentration reports Cp for a drug with no effect site", {
  r <- longResults(c(0, 10, 20),
                   Plasma = c(0, 10, 4), `Effect Site` = c(NA, NA, NA))
  # Plasma halfway between 10 and 20: (10 + 4) / 2 = 7, where "Ce: 0" was read
  expect_equal(hoverConcentration(r, 15), list(label = "Cp", value = 7))

  # And so for a real one: vancomycin has no effect site.  At a time on the
  # engine's own grid the reading is the simulated value exactly.
  dd <- getDrugDefaultsGlobal(FALSE)
  noEvents <- data.frame(Time = double(), Event = character())
  DT <- data.frame(Drug = "vancomycin", Time = 0, Dose = 1000, Units = "mg")
  v <- processdoseTable(DT, noEvents,
                        recalculatePK(NULL, dd, DT, 50, 70, 170, "male"),
                        60, FALSE)$vancomycin
  plasma <- v$results[v$results$Site == "Plasma", ]
  t <- plasma$Time[10]
  got <- hoverConcentration(v$results, t)
  expect_equal(got$label, "Cp")
  expect_equal(got$value, plasma$Y[10])
  expect_gt(got$value, 0)
})

test_that("hoverRecovery keeps a pending stretch missing and falls back to the equispaced grid", {
  r <- longResults(c(0, 10, 20, 30),
                   Plasma = c(1, 1, 1, 1),
                   Recovery = c(40, NA, NA, 10))
  expect_equal(hoverRecovery(list(results = r), 0), 40)
  # Next to a missing value is missing, not an interpolation across the gap
  expect_true(is.na(hoverRecovery(list(results = r), 5)))
  expect_true(is.na(hoverRecovery(list(results = r), 15)))

  # The inhaled agents carry recovery on the equispaced grid only
  gas <- list(results = longResults(c(0, 10), Plasma = c(1, 1)),
              equiSpace = data.frame(Time = c(0, 10, 20), Recovery = c(30, 20, 10)))
  expect_equal(hoverRecovery(gas, 15), 15)
  expect_true(is.na(hoverRecovery(list(results = r[r$Site == "Plasma", ],
                                       equiSpace = data.frame(Time = 0)), 5)))
})

test_that("addDisplayTimeColumn adds the unit beside the minutes and changes nothing in minutes", {
  df <- data.frame(Drug = "a", Time = c(0, 720, 1440), Dose = 1, Units = "mg")
  expect_identical(addDisplayTimeColumn(df, "minutes"), df)
  expect_identical(addDisplayTimeColumn(df, NULL), df)
  expect_null(addDisplayTimeColumn(NULL, "days"))
  expect_identical(addDisplayTimeColumn(df[, c("Drug", "Dose")], "days"),
                   df[, c("Drug", "Dose")])

  out <- addDisplayTimeColumn(df, "days")
  expect_equal(names(out), c("Drug", "Time", "Time (days)", "Dose", "Units"))
  expect_equal(out$Time, c(0, 720, 1440))
  expect_equal(out$`Time (days)`, c(0, 0.5, 1))
  expect_equal(out[, c("Drug", "Time", "Dose", "Units")], df)

  # Time as the last column, and in weeks: 5040 min = half a week
  out <- addDisplayTimeColumn(data.frame(Drug = "a", Time = c(5040, 10080)), "weeks")
  expect_equal(names(out), c("Drug", "Time", "Time (weeks)"))
  expect_equal(out$`Time (weeks)`, c(0.5, 1))
})
