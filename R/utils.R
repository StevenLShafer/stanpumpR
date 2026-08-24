# Check if a number is between two numbers
`%btwn%` <- function(x, range) {
  x >= range[1] & x <= range[2]
}

#' Check if a value is a valid single number within bounds
#'
#' @param x Value to check
#' @param min Minimum allowable value (optional)
#' @param max Maximum allowable value (optional)
#' @return Logical value indicating whether the value is valid
#' @noRd
is_valid_number <- function(x, min = -Inf, max = Inf) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= min && x <= max
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

identicalTable <- function(x, y) {
  rownames(x) <- NULL
  rownames(y) <- NULL
  identical(x, y)
}

isEmailValid <- function(email) {
  regex_email <- "^\\w+([-+.']\\w+)*@\\w+([-.]\\w+)*\\.\\w{2,}([-.]\\w+)*$"
  nchar(email) == attr(regexpr(regex_email, email, perl = FALSE), "match.length")
}

# Get a snapshot of the R version and packages, to help with troubleshooting in production
getInstalledPackagesInfo <- function() {
  pkgs <- utils::installed.packages()[, c("Package", "Version"), drop = FALSE]
  pkgs <- pkgs[order(tolower(pkgs[, "Package"])), , drop = FALSE]
  paste0(
    R.version.string, "\n", nrow(pkgs), " packages:\n",
    paste0("  ", pkgs[, "Package"], " ", pkgs[, "Version"], collapse = "\n")
  )
}

drugHasNonZeroDoses <- function(dt, drug) {
  drugDoses <- dt[dt$Drug == drug & dt$Dose != "", ]
  any(suppressWarnings(as.numeric(drugDoses$Dose)) != 0, na.rm = TRUE)
}
