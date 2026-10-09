# -----------------------------------------------------------------------------
# Reading a typed time or dose
# -----------------------------------------------------------------------------
# validateTime() and validateDose() turn what is typed or pasted into a Time
# or Dose cell, or into a dialog's time or dose field, into the string that is
# stored.  There are three outcomes:
#
#   - a blank entry (empty, white space, NA, NULL) is "0", the value a new row
#     is given;
#   - one number, written in one of the forms below, is that number;
#   - anything else is "", an unfinished cell.  A dose table row with a blank
#     time or dose is ignored by the simulation (cleanDoseTable()), the
#     dialogs that read a time refuse a blank one, and the grid shows the cell
#     empty, so an entry that could not be read is seen to have failed.
#
# Nothing is guessed at.  Until October 2026 every character that was not a
# digit, a decimal point or (for a time) a colon was deleted and what was left
# was read as a number, which turned one number into another without a word:
# "-5" became 5, "1e3" 13, "1,5" (a decimal comma) 15, "8;30" 830 minutes and
# "8:44 pm" 08:44 (audit finding F01).  Now a minus sign, a letter or unit, a
# second decimal point or colon, or a space or other mark inside the number
# makes the entry unreadable.  Times and doses are never negative.
#
# What is still tidied, because it cannot change the number:
#   - white space and quotation marks around the entry, and a leading "+";
#   - commas between groups of three digits, "1,000" (a comma anywhere else,
#     as in "1,5", makes the entry unreadable);
#   - scientific notation, "1e3" or "2.5E-1", which is written out in full
#     ("1000", "0.25"): the rest of the app reads only plain decimals.
# Otherwise a number is kept as typed ("007", "1.").
#
# A time with a colon is hours and minutes, "H:MM", with digits only on either
# side.  It is written HH:MM, minutes of 60 or more rolling into the hours
# ("0:80" is "01:20").  Whether it is a clock time or an elapsed time depends
# on the time display; see R/utils-time.R.
#
# inst/www/hot_funs.js has the same two functions for the grid, and the two
# must agree: the server checks that every stored time is one validateTime()
# leaves unchanged (validateDoseTableInput()).
# -----------------------------------------------------------------------------

# A number in plain decimal, as validateTime() and validateDose() write one
# that was typed in scientific notation or arrived as a number.  The digits
# and trim are validateTime()'s from before: as.character(1e5) is "1e+05".
plainDecimal <- function(x) {
  format(x, scientific = FALSE, trim = TRUE, digits = 15)
}

# The entry with the white space and quotation marks around it removed.  The
# white space is JavaScript's \s, which takes in the no-break space a
# spreadsheet paste can carry, so that the grid's trimEntry() agrees.
# Written with \x{} escapes, and (*UTF) to read them, so that the pattern is
# the same in any locale.
ENTRY_PADDING <- paste0(
  "[\\s\\x{00A0}\\x{1680}\\x{2000}-\\x{200A}\\x{2028}\\x{2029}\\x{202F}",
  "\\x{205F}\\x{3000}\\x{FEFF}'\"`]"
)
trimEntry <- function(x) {
  gsub(paste0("(*UTF)^", ENTRY_PADDING, "+|", ENTRY_PADDING, "+$"), "", enc2utf8(x), perl = TRUE)
}

# A trimmed entry that is one non-negative number in an accepted form (see
# the header), as the number to store; NA for anything else.
readEntryNumber <- function(x) {
  x <- sub("^\\+", "", x)
  ok <- grepl(
    "^(([0-9]+|[0-9]{1,3}(,[0-9]{3})+)(\\.[0-9]*)?|\\.[0-9]+)([eE][+-]?[0-9]+)?$",
    x, perl = TRUE
  )
  if (!ok) return(NA_character_)
  x <- gsub(",", "", x, fixed = TRUE)
  if (!grepl("[eE]", x)) return(x)
  value <- as.numeric(x)
  if (!is.finite(value)) return(NA_character_)
  plainDecimal(value)
}

# The entry as list(text = ) a single trimmed string ("" when blank), or as
# list(done = ) the result itself when the entry settles it: TRUE or FALSE,
# and a negative or infinite number, are unreadable.  A number arriving as a
# number (a numeric grid column) is written out in plain decimal.
entryString <- function(x) {
  if (is.null(x) || is.na(x) || is.nan(x)) return(list(text = ""))
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) return(list(done = ""))  # TRUE or FALSE is not a number
  if (is.numeric(x)) {
    if (!is.finite(x) || x < 0) return(list(done = ""))
    return(list(text = plainDecimal(x)))
  }
  list(text = trimEntry(as.character(x)))
}

# TRUE for an entry that is blank: NULL, NA, or nothing but white space and
# quotation marks.  validateTime() and validateDose() make it "0"; a dialog
# that must not take a cell the grid has cleared for 0 tests for it first.
isBlankEntry <- function(x) {
  if (is.null(x) || length(x) == 0) return(TRUE)
  if (is.na(x)) return(TRUE)
  is.character(x) && !nzchar(trimEntry(x)) ||
    is.factor(x) && !nzchar(trimEntry(as.character(x)))
}

# Validate time: a number in the time unit, or H:MM.  See the header.
validateTime <- function(x)
{
  if (length(x) > 1) {
    stop("validateTime can only accept single items, not vectors.")
  }
  entry <- entryString(x)
  if (!is.null(entry$done)) return(entry$done)
  x <- entry$text
  if (x == "") return("0")

  if (grepl(":", x, fixed = TRUE)) {
    x <- sub("^\\+", "", x)
    if (!grepl("^[0-9]*:[0-9]*$", x)) return("")
    colonPosition <- regexpr(":", x, fixed = TRUE)
    HH <- as.numeric(substr(x, 1, colonPosition - 1))
    if (is.na(HH)) HH <- 0
    MM <- as.numeric(substr(x, colonPosition + 1, nchar(x)))
    if (is.na(MM)) MM <- 0
    # force 80 minutes into 1 hour and 20 minutes
    HH <- HH + floor(MM/60)
    MM <- MM %% 60
    # %.0f rather than %d, which stops on an hour count too large for an
    # integer.  inst/www/hot_funs.js validateTime() must give the same string.
    return(sprintf("%02.0f:%02.0f", HH, MM))
  }

  out <- readEntryNumber(x)
  if (is.na(out)) "" else out
}

# Validate dose: a non-negative number, returned as a character string.  See
# the header.
validateDose <- function(x)
{
  if (length(x) > 1 || is.list(x)) {
    stop("validateDose can only accept single items.")
  }
  entry <- entryString(x)
  if (!is.null(entry$done)) return(entry$done)
  x <- entry$text
  if (x == "") return("0")

  out <- readEntryNumber(x)
  if (is.na(out)) "" else out
}
