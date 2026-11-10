# The tables a bookmark link restores, read as untrusted input.
#
# A bookmark is the address bar's query string, so anyone can write one and
# send it.  Until 2026-11-02 (the external security audit of that day,
# re-checked in a browser) onRestored() put the link's dose and event tables
# straight into the session: validateDoseTableInput() reads only complete
# rows (cleanDoseTable() drops the rest), so an incomplete row of any text, of
# any length, reached the dose grid unchecked; an unknown event replaced the
# plot with "Event table contains an unknown event"; an event table without
# an Event column stopped the restore part way; and a dose table that was a
# string rather than a table ended the session at startup.  Handsontable
# draws every cell as text, so none of it ran as script (that was tested:
# HTML-shaped cells, the editors open), but the grid is a frozen 6.2.2 with
# known advisories, and what it is handed should be what the user could have
# typed.
#
# So each table is rebuilt from what can be read: a cell that is not what the
# grid would accept is blanked (never replaced: a unit swapped for the drug's
# default could be a dose a thousand times off), the rows are capped, and the
# caller says so when anything was dropped.

#' The dose table of a bookmark link, as the grid would accept it
#'
#' @param x the link's `DT` value, as Shiny parsed it (anything)
#' @param drugDefaults the session's drug table
#' @returns list(table = a dose table of character columns Drug, Time, Dose,
#'   Units with at least one row, changed = TRUE when anything was blanked or
#'   dropped)
#' @noRd
restoredDoseTable <- function(x, drugDefaults)
{
  columns <- c("Drug", "Time", "Dose", "Units")
  blank <- data.frame(Drug = "", Time = "", Dose = "", Units = "", stringsAsFactors = FALSE)
  table <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.list(x) || is.null(table) || !all(columns %in% names(table))) {
    return(list(table = blank, changed = !is.null(x)))
  }
  table <- table[, columns, drop = FALSE]
  changed <- FALSE
  text <- function(v) {
    v <- if (is.list(v)) vapply(v, function(e) if (length(e) == 1 && !is.na(e)) as.character(e) else "", "")
         else as.character(v)
    v[is.na(v)] <- ""
    v
  }
  # The grid holds at most MAX_DOSE_ROWS - 1 rows.  Blank rows past them (the
  # one the new grid keeps below 499 doses) are dropped without a word; only
  # a row that held something is said (the external review of 2026-11-06,
  # Gemini's final pass: every table of 499 doses was said to be cut).
  keep <- MAX_DOSE_ROWS - 1L
  if (nrow(table) > keep) {
    extra <- table[-seq_len(keep), , drop = FALSE]
    if (any(vapply(columns, function(cl) any(nzchar(trimws(text(extra[[cl]])))), logical(1)))) changed <- TRUE
    table <- table[seq_len(keep), , drop = FALSE]
  }
  for (col in columns) table[[col]] <- text(table[[col]])

  for (i in seq_len(nrow(table))) {
    row <- table[i, ]
    drug <- row$Drug
    if (!nzchar(drug) || !drug %in% drugDefaults$Drug) {
      # No drug, or one the library does not have: the row is empty
      if (any(nzchar(unlist(row)))) changed <- TRUE
      table[i, ] <- ""
      next
    }
    if (nzchar(row$Time)) {
      time <- if (nchar(row$Time) > MAX_TIME_STRING_LENGTH) "" else validateTime(row$Time)
      if (!nzchar(time)) changed <- TRUE
      table$Time[i] <- time
    }
    if (nzchar(row$Dose)) {
      dose <- if (nchar(row$Dose) > MAX_TIME_STRING_LENGTH) "" else validateDose(row$Dose)
      if (!nzchar(dose)) changed <- TRUE
      table$Dose[i] <- dose
    }
    if (nzchar(row$Units)) {
      offered <- unlist(drugDefaults$Units[match(drug, drugDefaults$Drug)])
      if (!row$Units %in% offered) {
        table$Units[i] <- ""
        changed <- TRUE
      }
    }
  }
  if (nrow(table) == 0) table <- blank
  rownames(table) <- NULL
  list(table = table, changed = changed)
}

#' The event table of a bookmark link: the rows with a known event and a time
#'
#' @param x the link's `ET` value, as Shiny parsed it (anything)
#' @param eventDefaults the event table
#' @param empty the table to return when no row survives (eventTableInit)
#' @returns list(table, changed)
#' @noRd
restoredEventTable <- function(x, eventDefaults, empty)
{
  table <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (!is.list(x) || is.null(table) || nrow(table) == 0) {
    return(list(table = empty, changed = !is.null(x) && !is.null(table) && nrow(table) > 0))
  }
  if (!all(c("Time", "Event") %in% names(table))) return(list(table = empty, changed = TRUE))
  event <- if (is.list(table$Event)) vapply(table$Event, function(e) if (length(e) == 1) as.character(e) else "", "")
           else as.character(table$Event)
  time <- suppressWarnings(as.numeric(if (is.list(table$Time)) vapply(table$Time, function(e)
    if (length(e) == 1) as.character(e) else "", "") else as.character(table$Time)))
  ok <- !is.na(event) & event %in% eventDefaults$Event & is.finite(time) & time >= 0
  ok[seq_along(ok) > MAX_EVENT_ROWS] <- FALSE
  if (!any(ok)) return(list(table = empty, changed = TRUE))
  list(table = data.frame(Time = time[ok], Event = event[ok], stringsAsFactors = FALSE),
       changed = !all(ok))
}
