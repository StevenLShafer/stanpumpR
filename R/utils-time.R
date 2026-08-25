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
  x <- x + start
  xHours <- floor(x/60)
  xMinutes <- round(x-xHours * 60, 0)
  xHours <- xHours %% 24
  return(sprintf("%02d:%02d",xHours,xMinutes))
}

# Separate hour from minute in hh:ss format. Return number of minutes
# Used only in clockTimeToDelta
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
