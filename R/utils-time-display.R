# Showing a time in the display unit: the plot's x axis, the hover readout,
# the "time until threshold" labels and the exported workbook.
#
# Everything the engines produce is in MINUTES, and stays in minutes all the
# way to the plot: the curves, the dose and event times, Max time, the TCI
# rates and the recovery line are all drawn on a minute axis.  The Time units
# selector (Time card) changes only how those minutes are WRITTEN -- the tick
# labels and the axis title, the hover, the recovery labels and an extra
# column in the export.  Shiny reports a hover or a click in data coordinates,
# so e$x is minutes as well, and nothing that reads it needs converting.
# Doing it this way rather than rescaling the plotted data leaves every
# x = Time layer, the simulation cache and the exported minute columns as they
# were.
#
# Reading a time the user has typed, and writing one back into the dose table,
# is the other half of the feature and lives in R/utils-time.R.
#
# (Claude Code, 2026-10-07, at the request of Steven L.
# Shafer.)

# How each display unit is written.  `minutes` is the length of one unit;
# `digits` is the number of decimals a duration is rounded to in the hover:
# one for minutes, as the hover always has, and two for the longer units, so
# that a q8h dose on a days axis reads 0.33 rather than 0.3.  The minutes per
# unit must agree with TIME_UNITS in R/constants.R, which the dose table uses
# for entry; test-utils-time-display.R checks that they do.
TIME_UNIT_DISPLAY <- data.frame(
  unit         = c("minutes", "hours", "days", "weeks"),
  minutes      = c(1, MINS_PER_HOUR, MINS_PER_DAY, MINS_PER_WEEK),
  singular     = c("minute", "hour", "day", "week"),
  abbreviation = c("min", "h", "d", "wk"),
  digits       = c(1, 2, 2, 2),
  stringsAsFactors = FALSE
)

# The row of TIME_UNIT_DISPLAY for a unit, as a list, refusing anything else.
# The server validates the selector before it gets here, so an unknown unit
# is a programming error, and saying so beats silently drawing in minutes.
timeUnitDisplay <- function(unit)
{
  i <- match(unit, TIME_UNIT_DISPLAY$unit)
  if (length(i) != 1 || is.na(i))
    stop("Unknown time unit: ", paste(unit, collapse = ", "))
  as.list(TIME_UNIT_DISPLAY[i, ])
}

# A number for display: never in scientific notation (as.character(1e5) is
# "1e+05"), no trailing zeros, and element by element, because format() on a
# vector gives every element the same number of decimals.
plainNumber <- function(x)
{
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    format(v, scientific = FALSE, trim = TRUE, digits = 15)
  }, character(1), USE.NAMES = FALSE)
}

# A duration in the display unit, as words: "90 minutes", "1.5 hours",
# "24.3 days", "1 week".  Every elapsed time in the hover goes through this,
# and so does the time until threshold.  Minutes are rounded to a tenth, as
# the hover always has; hours, days and weeks to two decimals.  NA stays NA.
formatElapsed <- function(minutes, unit = "minutes")
{
  u <- timeUnitDisplay(unit)
  value <- round(as.numeric(minutes) / u$minutes, u$digits)
  # round() leaves -0 for a hover a hair left of the origin
  value[!is.na(value) & value == 0] <- 0
  out <- paste(plainNumber(value), ifelse(value == 1, u$singular, u$unit))
  out[is.na(value)] <- NA_character_
  out
}

# A plotted time for the hover.  In clock ("Actual time") mode the time of
# day after the procedure start, "HH:MM", exactly as the axis labels it;
# otherwise the elapsed time in the display unit.  `reference` is the
# procedure start, or REFERENCE_TIME_NONE for elapsed time -- what the
# server's referenceTime() returns.
formatPlotTime <- function(minutes, unit = "minutes",
                           reference = REFERENCE_TIME_NONE)
{
  if (identical(reference, REFERENCE_TIME_NONE))
    return(formatElapsed(minutes, unit))
  # Whole minutes first, so that 59.7 reads 01:00 after a 00:00 start rather
  # than 00:60.
  deltaToClockTime(reference, round(as.numeric(minutes)))
}

# The x-axis tick labels.  The breaks stay in minutes; the labels are those
# minutes in the display unit as plain numbers ("0", "0.25", "0.5" hours), or
# the time of day in clock mode.  Every tick step is a whole number or a
# simple fraction of its unit (constants.R), so rounding to six decimals only
# removes floating-point dust.
axisTimeLabels <- function(breaksMinutes, unit = "minutes",
                           reference = REFERENCE_TIME_NONE)
{
  if (!identical(reference, REFERENCE_TIME_NONE))
    return(deltaToClockTime(reference, breaksMinutes))
  plainNumber(round(breaksMinutes / timeUnitDisplay(unit)$minutes, 6))
}

# The x-axis title: "Time (hours)" and so on, or plain "Time" under clock
# labels, which say what they are.
timeAxisTitle <- function(unit = "minutes", reference = REFERENCE_TIME_NONE)
{
  if (!identical(reference, REFERENCE_TIME_NONE)) return("Time")
  paste0("Time (", timeUnitDisplay(unit)$unit, ")")
}

# The unit of one panel's "time until threshold" labels, as
# list(unit, factor = minutes per unit, abbreviation = "min"/"h"/"d"/"wk").
#
# Chosen per panel from the longest time on it rather than from the x axis: a
# remifentanil infusion on a 24-hour plot recovers in minutes, while an
# antibiotic on a 2-hour plot can stay above its MIC for days.  Minutes below
# 2 hours, hours below 2 days, days below 3 weeks, weeks beyond; the
# breakpoints keep the top label at two or more of the unit, so the labels
# below it are not all fractions.  A panel with nothing to time (zero, NA)
# stays in minutes.
recoveryAxisUnit <- function(maxRecoveryMinutes)
{
  m <- suppressWarnings(as.numeric(maxRecoveryMinutes))
  unit <- if (length(m) != 1 || is.na(m) || m < 2 * MINS_PER_HOUR) {
    "minutes"
  } else if (m < 2 * MINS_PER_DAY) {
    "hours"
  } else if (m < 3 * MINS_PER_WEEK) {
    "days"
  } else {
    "weeks"
  }
  u <- timeUnitDisplay(unit)
  list(unit = u$unit, factor = u$minutes, abbreviation = u$abbreviation)
}

# How far ahead, in minutes, a drug's time until threshold is searched.
#
# recoveryCalc() stops looking at a horizon and returns the horizon itself
# when the concentration is still above the threshold there, so a time equal
# to it means "at least this long" and the hover says "more than".  A drug
# timed on its effect site, and an inhaled agent, is searched a day ahead
# (RECOVERY_HORIZON_EFFECT).  A drug timed on its plasma because it has no
# effect site -- an antibiotic, a long-term oral drug -- is searched a week
# ahead or to the end of the plot, whichever is longer, so that a plot of
# months does not flatten every time at one week.  The rule must be kept in
# step with the horizon the engines pass to recoveryStateSet()
# (R/recoveryStates.R, "Which concentration is timed").
#
# `PK` is the drug's entry as recalculatePK() builds it (its `PK` sets carry
# ke0), or a gas entry (`isGas`); a single PK set also works.  `maximum` is
# the length of the plot in minutes.
recoveryHorizonFor <- function(PK, maximum)
{
  sets <- if (is.list(PK$PK)) PK$PK else list(PK)
  ke0 <- unlist(lapply(sets, function(s) s$ke0))
  timedOnEffectSite <- isTRUE(PK$isGas) || any(ke0 > 0, na.rm = TRUE)
  if (timedOnEffectSite) {
    RECOVERY_HORIZON_EFFECT
  } else {
    max(RECOVERY_HORIZON_PLASMA, maximum)
  }
}

# The time until threshold as the hover writes it: in the unit of that
# panel's recovery labels (recoveryAxisUnit() of the panel's longest time,
# `maxRecovery`), "more than ..." at the search horizon (recoveryHorizonFor()),
# and a sentence rather than a number where a dose has been given but has not
# begun to be absorbed (NA; see pendingDoseTimes()), because "0 minutes" and
# "not yet absorbed" are opposites.
formatRecovery <- function(recovery, maxRecovery, horizon)
{
  if (length(recovery) != 1 || is.na(recovery))
    return("not yet, dose still being absorbed")
  unit <- recoveryAxisUnit(maxRecovery)$unit
  # The engines return the horizon itself, exactly; the tolerance only allows
  # for interpolating between two points that are both at it.
  if (recovery >= horizon * (1 - 1e-9))
    return(paste("more than", formatElapsed(horizon, unit)))
  formatElapsed(recovery, unit)
}

# One series of a drug's `results` (long form: Drug, Time, Site, Y),
# interpolated at time x, in minutes.  Missing values are kept
# (na.rm = FALSE), so that a stretch with no value -- recovery while a dose is
# pending -- reads as missing rather than as an invented number, and the ends
# are held (rule = 2) for a hover a pixel outside the data.  NA when the
# series has fewer than two known points.
seriesAt <- function(results, site, x)
{
  r <- results[results$Site == site, c("Time", "Y")]
  if (sum(!is.na(r$Y)) < 2) return(NA_real_)
  stats::approx(r$Time, r$Y, xout = x, rule = 2, ties = mean,
                na.rm = FALSE)$y
}

# The concentration the hover reports on a drug's panel, as list(label,
# value), interpolated at the hovered time x (minutes) from the drug's full
# simulated series.  The hover used to take the nearest of the 100 equispaced
# points instead, which are 3.7 days apart on a 52-week plot.
#
# The effect site ("Ce") where the drug has one; the plasma ("Cp") where it
# has none -- an antibiotic, a prodrug, a long-term oral drug -- which used to
# read "Ce: 0".  Under normalization the normalized series, so that the value
# matches the panel's "% Peak" units.
hoverConcentration <- function(results, x, normalization = NORMALIZE_NONE)
{
  sites <- switch(
    normalization,
    "Peak plasma"      = c(Cp = "CpNormCp", Ce = "CeNormCp"),
    "Peak effect site" = c(Cp = "CpNormCe", Ce = "CeNormCe"),
    c(Cp = "Plasma", Ce = "Effect Site")
  )
  hasEffectSite <- any(results$Site == "Effect Site" & !is.na(results$Y))
  label <- if (hasEffectSite) "Ce" else "Cp"
  list(label = label, value = seriesAt(results, sites[[label]], x))
}

# The time until threshold at the hovered time x, in minutes, or NA.  From
# the drug's full series where it carries one, which every intravenous,
# extravascular and metabolite drug does while the line is switched on; the
# inhaled agents keep theirs only on the equispaced grid.
hoverRecovery <- function(entry, x)
{
  if (any(entry$results$Site == "Recovery"))
    return(seriesAt(entry$results, "Recovery", x))
  e <- entry$equiSpace
  if (is.null(e$Recovery) || sum(!is.na(e$Recovery)) < 2) return(NA_real_)
  stats::approx(e$Time, e$Recovery, xout = x, rule = 2, ties = mean,
                na.rm = FALSE)$y
}

# An exported sheet with the time in the display unit beside its minutes.
#
# The sheets keep their Time column in minutes whatever the display, which is
# what a script reading them expects and what the tests pin.  In any other
# unit a "Time (<unit>)" column follows it with the same times in that unit,
# so that the sheet can be read against the plot.  Minutes, or no unit,
# changes nothing.
addDisplayTimeColumn <- function(df, unit = "minutes")
{
  if (is.null(df) || is.null(unit) || identical(unit, "minutes")) return(df)
  if (!"Time" %in% names(df)) return(df)
  u <- timeUnitDisplay(unit)
  shown <- stats::setNames(data.frame(as.numeric(df$Time) / u$minutes),
                           paste0("Time (", u$unit, ")"))
  at <- match("Time", names(df))
  out <- cbind(df[seq_len(at)], shown)
  if (at < ncol(df)) out <- cbind(out, df[(at + 1):ncol(df)])
  out
}
