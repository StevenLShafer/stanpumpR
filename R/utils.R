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

# Format a duration given in minutes as a human-readable number of minutes/hours/days
formatMinutes <- function(minutes) {
  vapply(minutes, function(mins) {
    if (!is_valid_number(mins) || mins < 0) return(NA_character_)
    if (mins >= MAX_TIME_NO_LIMIT) return("No limit")
    if (mins < 60) return(pluralNoun(mins, "minute"))
    if (mins < 60*24) return(pluralNoun(round(mins / 60, 1), "hour"))

    days <- mins %/% 1440
    leftoverHours <- round((mins %% 1440) / 60, 1)
    if (leftoverHours == 0) {
      pluralNoun(days, "day")
    } else {
      paste(pluralNoun(days, "day"), pluralNoun(leftoverHours, "hour"))
    }
  }, character(1))
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
