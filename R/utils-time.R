# Convert clock times (x) to difference from the reference time
clockTimeToDelta <- function(reference, x) {
  if (reference == REFERENCE_TIME_NONE) {
    FIX <- grepl(":",x)
    x[FIX] <- as.numeric(unlist(lapply(x[FIX],FUN = hourMinute)))
    x <- as.numeric(x)
    return(x)
  }
  start <- hourMinute(reference)
  if (is.na(start)) return(NA)
  FIX <- grepl(":",x)
  x[FIX] <- as.numeric(unlist(lapply(x[FIX],FUN = hourMinute))) - start
  x <- as.numeric(x)
  x[x < 0] <- x[x < 0] + MINS_PER_DAY # Wrap around midnight
  x
}

# Convert delta time (x) from the reference time to an actual clock time
deltaToClockTime <- function(reference, x)
{
  if (reference == REFERENCE_TIME_NONE) {
    return(as.numeric(x))
  }
  start <- hourMinute(reference)
  # Round to whole minutes BEFORE splitting off the hours.  Rounding the
  # minutes afterwards turned 59.6 minutes past the hour into "HH:60".
  x <- round(as.numeric(x) + start)
  xHours <- floor(x/60)
  xMinutes <- x - xHours * 60
  xHours <- xHours %% 24
  return(sprintf("%02d:%02d",xHours,xMinutes))
}

# Separate hour from minute in hh:ss format. Return number of minutes
# Used for clock times (clockTimeToDelta, displayTimeToMinutes); it wraps 24:xx
# to 0:xx and cannot read 25:00 or more, so an elapsed H:MM is read with
# elapsedHourMinute() instead.
hourMinute <- function(x)
{
  px <- lubridate::parse_date_time(x, "HM", quiet=TRUE)
  if (!is.na(px)) px <- 60*lubridate::hour(px) + lubridate::minute(px)
  return(px)
}

getReferenceTime <- function(time) {
  time <- gsub("[^[:digit:]:. APMapm]","",time) # Get rid of strange formatting characters
  time <- lubridate::parse_date_time(time, c("HMSOp","HMOp","HMS","HM"), quiet=TRUE)
  if (is.na(time)) return(NA)
  time <- 60*lubridate::hour(time) + lubridate::minute(time)
  time <- floor(time / 15) * 15
  HH   <- floor(time / 60)
  MM   <- time %% 60
  start <- sprintf("%02d:%02d",HH,MM)
  start
}

# Format a duration given in minutes as a human-readable number of
# minutes/hours/days/weeks/years. Month is skipped because it's so irregular.
formatMinutes <- function(minutes) {
  vapply(minutes, function(mins) {
    if (!is_valid_number(mins) || mins < 0) return(NA_character_)
    if (mins < MINS_PER_HOUR) return(pluralNoun(mins, "minute"))
    if (mins < MINS_PER_DAY) return(pluralNoun(round(mins / MINS_PER_HOUR, 1), "hour"))
    if (mins < MINS_PER_WEEK) return(withRemainder(mins, MINS_PER_DAY, "day", MINS_PER_HOUR, "hour"))
    if (mins < MINS_PER_YEAR) return(withRemainder(mins, MINS_PER_WEEK, "week", MINS_PER_DAY, "day"))
    withRemainder(mins, MINS_PER_YEAR, "year", MINS_PER_WEEK, "week")
  }, character(1))
}

# Label a duration as whole "major" units plus any remainder in "minor" units,
# e.g. "2 days 4 hours"
withRemainder <- function(mins, majorSize, majorUnit, minorSize, minorUnit) {
  major <- mins %/% majorSize
  minor <- round((mins %% majorSize) / minorSize, 1)

  if (minor * minorSize >= majorSize) {
    major <- major + 1
    minor <- 0
  }

  if (minor == 0) {
    pluralNoun(major, majorUnit)
  } else {
    paste(pluralNoun(major, majorUnit), pluralNoun(minor, minorUnit))
  }
}

# Pluralize a noun if there is more than 1 of it
pluralNoun <- function(n, unit) {
  paste0(n, " ", unit, if (n == 1) "" else "s")
}

# -----------------------------------------------------------------------------
# Time units
# -----------------------------------------------------------------------------
# Provenance: drafted by Claude Code, 2026-10-07, at the request of Steven L.
# Shafer; verified by tests/testthat/test-utils-time.R and
# tests/testthat/test-time-units-server.R.
#
# The engine works in minutes.  The user picks a time unit (minutes, hours,
# days, weeks) and, for minutes and hours, whether times are clock times
# ("Actual time") or elapsed.  The dose table holds its times AS TYPED, so its
# Time strings only mean something together with the format they were typed
# in: a "time format", c(unit = , mode = ), which the server keeps beside the
# table (doseTableFormat() in R/app_server.R).  The rules for a Time string:
#
#   - a bare number ("90", "1.5") is an offset in the unit: from 0 when
#     elapsed, from the procedure start in clock mode;
#   - an entry with a colon is a clock time in clock mode ("09:30") and an
#     elapsed hours:minutes otherwise ("36:00" is 2160 minutes).  It is never
#     scaled by the unit.
#
# Changing the unit or the mode rewrites the table into the new format
# (rebaseDoseTimes()), so the minutes the engine sees do not change.

# A time unit name, or the default when it is missing or not a unit
validTimeUnit <- function(unit) {
  if (is.character(unit) && length(unit) == 1 && !is.na(unit) && unit %in% names(TIME_UNITS)) {
    unit
  } else {
    TIME_UNIT_DEFAULT
  }
}

# The format a set of Time strings is written in: c(unit = , mode = ).  Clock
# mode exists only for CLOCK_TIME_UNITS; any other unit is elapsed.  An
# unknown unit is the default; an unknown mode is "clock", as the UI starts.
timeFormat <- function(unit = TIME_UNIT_DEFAULT, mode = "clock") {
  unit <- validTimeUnit(unit)
  if (!(is.character(mode) && length(mode) == 1 && !is.na(mode) && mode %in% TIME_MODES)) {
    mode <- "clock"
  }
  if (!unit %in% CLOCK_TIME_UNITS) mode <- "relative"
  c(unit = unit, mode = mode)
}

# "days/relative": the form a time format travels in (bookmarks, the grid)
timeFormatString <- function(format) {
  paste(format[["unit"]], format[["mode"]], sep = "/")
}

# The inverse of timeFormatString(); NULL for anything that is not one
parseTimeFormat <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(NULL)
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2 || !parts[1] %in% names(TIME_UNITS) || !parts[2] %in% TIME_MODES) {
    return(NULL)
  }
  timeFormat(parts[1], parts[2])
}

isClockFormat <- function(format) {
  identical(format[["mode"]], "clock")
}

# The "Time Display" choices for a unit
timeModeChoices <- function(unit) {
  if (validTimeUnit(unit) %in% CLOCK_TIME_UNITS) {
    c("Actual time" = "clock", "Elapsed time" = "relative")
  } else {
    c("Elapsed time" = "relative")
  }
}

# What the times in a format are, for labels: "days", or for clock mode
# "HH:MM or minutes" (a bare number is still an offset, from the start)
timeEntryUnitText <- function(format) {
  if (isClockFormat(format)) {
    paste0("HH:MM or ", format[["unit"]])
  } else {
    format[["unit"]]
  }
}

# The label of a time field in a dialog: "Time (days)"
timeEntryLabel <- function(format) {
  paste0("Time (", timeEntryUnitText(format), ")")
}

# Minutes as the value of a Max time choice.  as.character(1e5) is "1e+05".
maxTimeValue <- function(x) {
  vapply(x, function(v) format(v, scientific = FALSE, trim = TRUE), character(1),
         USE.NAMES = FALSE)
}

# The Max time choices for a unit: labels ("365 days") naming the values
# (minutes, as character)
maxTimeChoices <- function(unit) {
  unit <- validTimeUnit(unit)
  times <- MAX_TIMES[[unit]]$times
  labelUnit <- MAX_TIME_LABEL_UNITS[[unit]]
  size <- c(hour = MINS_PER_HOUR, day = MINS_PER_DAY, week = MINS_PER_WEEK)[[labelUnit]]
  labels <- vapply(times / size, pluralNoun, character(1), unit = labelUnit)
  stats::setNames(maxTimeValue(times), labels)
}

# The label of one Max time in a unit ("24 hours"), or NA if it is not a choice
maxTimeLabel <- function(maximum, unit) {
  choices <- maxTimeChoices(unit)
  names(choices)[match(maxTimeValue(maximum), choices)]
}

# A Max time (minutes) that the unit offers: x itself when it is a choice,
# else the shortest choice at least as long, else the longest.  NA or a
# non-number is the unit's first (default) choice.
snapMaximum <- function(x, unit) {
  times <- MAX_TIMES[[validTimeUnit(unit)]]$times
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1 || !is.finite(x)) return(times[1])
  if (x %in% times) return(x)
  above <- times[times >= x]
  if (length(above) > 0) above[1] else max(times)
}

# The unit for a bookmark made before there were time units: minutes, unless
# its Max time was more than a day.
legacyTimeUnit <- function(maximum) {
  maximum <- suppressWarnings(as.numeric(maximum))
  if (length(maximum) == 1 && is.finite(maximum) && maximum > MINS_PER_DAY) "days" else "minutes"
}

# Minutes from midnight of a procedure start ("08:30"), or NA
referenceMinutes <- function(reference) {
  if (!is.character(reference) || length(reference) != 1 || is.na(reference) ||
      reference == REFERENCE_TIME_NONE) {
    return(NA_real_)
  }
  as.numeric(hourMinute(reference))
}

isValidReferenceTime <- function(reference) {
  !is.na(referenceMinutes(reference))
}

# The reference a format's times are read against: the procedure start in
# clock mode, else none
referenceForFormat <- function(format, reference) {
  if (isClockFormat(format)) reference else REFERENCE_TIME_NONE
}

# Elapsed "H:MM" as minutes, by arithmetic, so that "36:00" is 2160 (lubridate,
# in hourMinute(), wraps 24:xx to 0:xx and cannot read 25:00).  NA if x is not
# digits, a colon and digits.
elapsedHourMinute <- function(x) {
  x <- trimws(as.character(x))
  ok <- !is.na(x) & grepl("^[0-9]*:[0-9]*$", x)
  out <- rep(NA_real_, length(x))
  if (any(ok)) {
    hh <- suppressWarnings(as.numeric(sub(":.*$", "", x[ok])))
    mm <- suppressWarnings(as.numeric(sub("^.*:", "", x[ok])))
    hh[is.na(hh)] <- 0   # ":30" and "1:" read as validateTime() writes them
    mm[is.na(mm)] <- 0
    out[ok] <- hh * MINS_PER_HOUR + mm
  }
  out
}

# Time strings (as typed, in `unit`) to minutes.  `reference` is the procedure
# start in clock mode, or REFERENCE_TIME_NONE.  Blank, NA or unreadable
# entries are NA, as is a clock time when the procedure start is unreadable.
#   bare number: x * minutes per unit, rounded to 0.001 minute unless the unit
#                is minutes (so the minute plots stay exactly as they were)
#   colon, clock:   minutes after the reference, wrapping past midnight
#   colon, elapsed: H * 60 + MM
displayTimeToMinutes <- function(x, reference = REFERENCE_TIME_NONE, unit = TIME_UNIT_DEFAULT) {
  unit <- validTimeUnit(unit)
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  present <- !is.na(x) & nzchar(x)
  colon <- present & grepl(":", x, fixed = TRUE)
  bare <- present & grepl("^([0-9]+\\.?[0-9]*|\\.[0-9]+)$", x)

  if (any(bare)) {
    m <- as.numeric(x[bare]) * TIME_UNITS[[unit]]
    if (unit != "minutes") m <- round(m, TIME_SNAP_DIGITS)
    out[bare] <- m
  }

  if (any(colon)) {
    if (identical(reference, REFERENCE_TIME_NONE)) {
      out[colon] <- elapsedHourMinute(x[colon])
    } else {
      start <- referenceMinutes(reference)
      if (!is.na(start)) {
        m <- as.numeric(unlist(lapply(x[colon], hourMinute))) - start
        m[!is.na(m) & m < 0] <- m[!is.na(m) & m < 0] + MINS_PER_DAY # Wrap around midnight
        out[colon] <- m
      }
    }
  }
  out
}

# Minutes as Time strings in `unit`: a number with TIME_STRING_DIGITS
# significant digits after rounding to 0.001 minute, never in scientific
# notation, and always a fixed point of validateTime().  NA is "".
minutesToDisplayTime <- function(m, unit = TIME_UNIT_DEFAULT) {
  unit <- validTimeUnit(unit)
  m <- suppressWarnings(as.numeric(m))
  out <- rep("", length(m))
  ok <- is.finite(m)
  if (any(ok)) {
    value <- signif(round(m[ok], TIME_SNAP_DIGITS) / TIME_UNITS[[unit]], TIME_STRING_DIGITS)
    out[ok] <- trimws(formatC(value, digits = TIME_STRING_DIGITS, format = "fg", decimal.mark = "."))
  }
  out
}

# Minutes as a time to put in front of the user in an entry field, in the
# format the dose table is in: "HH:MM" in clock mode for times in the 24 hours
# after the procedure start, else (and if the start is unreadable) a number in
# the unit, which clock mode reads as an offset from the start.
minutesToEntryTime <- function(m, format, reference) {
  m <- suppressWarnings(as.numeric(m))
  out <- minutesToDisplayTime(m, format[["unit"]])
  if (isClockFormat(format) && isValidReferenceTime(reference)) {
    # round(): deltaToClockTime() rounds to the minute, and 1439.7 minutes
    # would come back as the start itself
    clock <- is.finite(m) & m >= 0 & round(m) < MINS_PER_DAY
    if (any(clock)) out[clock] <- deltaToClockTime(reference, m[clock])
  }
  out
}

# Rewrite a dose table's Time strings from one format to another.
#
# Only rows with a drug and a time are touched.  Each time is read in `from`
# and written in `to`: as a number in the new unit, except that a clock time
# stays as typed when both formats are clock.  `reference` is the procedure
# start (input$referenceTime); it is used only for a clock-mode `from`.
#
# Returns list(table, ok).  ok is FALSE, and the table is returned unchanged,
# when any time cannot be read (a clock time with no readable procedure
# start): a time is never replaced by a blank or NA.  When nothing changes the
# table comes back identical(), so callers can tell.
rebaseDoseTimes <- function(dt, from, to, reference = REFERENCE_TIME_NONE) {
  if (identical(from, to)) return(list(table = dt, ok = TRUE))
  drug <- as.character(dt$Drug)
  time <- as.character(dt$Time)
  rows <- which(!is.na(drug) & nzchar(drug) & !is.na(time) & nzchar(trimws(time)))
  if (length(rows) == 0) return(list(table = dt, ok = TRUE))

  old <- time[rows]
  keep <- isClockFormat(from) & isClockFormat(to) & grepl(":", old, fixed = TRUE)
  new <- old
  if (any(!keep)) {
    m <- displayTimeToMinutes(old[!keep], referenceForFormat(from, reference), from[["unit"]])
    if (anyNA(m)) return(list(table = dt, ok = FALSE))
    new[!keep] <- minutesToDisplayTime(m, to[["unit"]])
  }
  if (identical(new, old)) return(list(table = dt, ok = TRUE))

  dt$Time <- time
  dt$Time[rows] <- new
  list(table = dt, ok = TRUE)
}

# The minutes of a column of times after the user has edited it in a dialog
# that showed `shownText` for times of `shownMinutes`.  A row whose text is as
# shown keeps its minutes exactly: the text may be rounded (a clock time to
# the minute, 1/7 day as "0.1428571429"), and reading it back would move
# every time the user did not touch.  An edited row is cleaned like any typed
# time and read in `format`.  Blank or unreadable is NA.
editedTimesToMinutes <- function(text, shownText, shownMinutes, format, reference) {
  text <- as.character(text)
  blank <- is.na(text) | !nzchar(trimws(text))
  cleaned <- text
  cleaned[!blank] <- vapply(text[!blank], validateTime, character(1), USE.NAMES = FALSE)
  minutes <- displayTimeToMinutes(cleaned, referenceForFormat(format, reference), format[["unit"]])
  minutes[blank] <- NA
  if (length(shownText) == length(text) && length(shownMinutes) == length(text)) {
    same <- !blank & !is.na(shownText) & text == shownText
    minutes[same] <- shownMinutes[same]
  }
  minutes
}
