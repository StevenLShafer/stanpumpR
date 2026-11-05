# The dose grid: which one draws the main dose table, and the server half of
# the prototype that may replace rhandsontable there (PROTOTYPE,
# docs/grid-replacement.md; the browser half is inst/www/dose-grid.js).
#
# rhandsontable 0.3.8 bundles Handsontable 6.2.2, the last MIT-licensed
# release, frozen with published advisories; and the grid it draws is redrawn
# by the server after every edit, which lost the first key typed meanwhile
# (tests/playwright).  The prototype is a plain HTML table of form controls
# with its own Shiny input binding: the browser checks each entry as it is
# committed, the server receives the whole table once per committed edit and
# keeps the draft and its undo history exactly as before, and it sends the
# draft back only when it differs from what the grid holds.
#
# The switch, until Steven Shafer decides: options(stanpumpR.doseGrid =
# "native"), or the environment variable STANPUMPR_DOSE_GRID; rhandsontable
# otherwise.  Only the main dose table has a prototype; every other grid is
# still rhandsontable.

DOSE_GRID_MODES <- c("handsontable", "native")

doseGridMode <- function() {
  mode <- getOption("stanpumpR.doseGrid", Sys.getenv("STANPUMPR_DOSE_GRID", ""))
  if (is.character(mode) && length(mode) == 1 && mode %in% DOSE_GRID_MODES) mode else "handsontable"
}

# The grid's place in the page.  Everything inside it is drawn by
# dose-grid.js; the attributes tell it the Dose column's header and how many
# rows the table may hold (one fewer than MAX_DOSE_ROWS, which leaves room for
# the blank row below the last drug).
doseGridInput <- function(inputId) {
  htmltools::tags$div(
    id = inputId,
    class = "stanpumpr-dose-grid",
    `data-dose-header` = DOSE_COLUMN_HEADER,
    `data-max-rows` = MAX_DOSE_ROWS - 1L
  )
}

# What the grid needs to know of the drugs: every drug's units and default
# unit (a drug already in the table keeps its units even when the illicit-drug
# opt-in would hide it), which are illicit (drawn red), and the names the
# autocomplete offers.
doseGridDrugsMessage <- function(drugDefaults, choices) {
  drugs <- lapply(seq_len(nrow(drugDefaults)), function(i) {
    list(
      Drug = drugDefaults$Drug[i],
      Units = I(as.character(unlist(drugDefaults$Units[i]))),
      DefaultUnits = as.character(drugDefaults$Default.Units[i]),
      Illicit = isTRUE(drugDefaults$Category[i] == ILLICIT_DRUG_CATEGORY)
    )
  })
  list(drugs = drugs, choices = I(as.character(choices)))
}

# The draft as the grid draws it: one array of four strings a row
doseGridRowsMessage <- function(table, format, ack) {
  table <- blankMissingCells(table)
  rows <- lapply(seq_len(nrow(table)), function(i) {
    I(vapply(c("Drug", "Time", "Dose", "Units"), function(col) as.character(table[[col]][i]), character(1),
             USE.NAMES = FALSE))
  })
  list(rows = rows, timeFormat = timeFormatString(format), ack = ack)
}

# One cell of what the browser sent, as a string.  The value is untrusted
# (anyone can send their own session anything): whatever is not one string or
# one number is a blank cell, and the server's own checks
# (validateDoseTableInput(), cleanDoseTable()) judge the rest.
doseGridCell <- function(x) {
  if (is.list(x)) x <- if (length(x) == 1 && is.null(names(x))) x[[1]] else NULL
  if (length(x) != 1 || is.na(x)) return("")
  if (is.numeric(x) && is.finite(x)) return(format(x, scientific = FALSE, digits = 15, trim = TRUE))
  if (!is.character(x)) return("")
  x
}

# input$doseGrid as list(table, format, seq); NULL for anything that is not
# a grid's value.  Never an error: an error in the observer that reads it
# would end the session.  More rows than the grid may hold are cut to its
# limit, as the grid itself does.
doseGridValue <- function(value) {
  if (!is.list(value) || is.null(value$rows)) return(NULL)
  rows <- value$rows
  if (is.matrix(rows)) rows <- lapply(seq_len(nrow(rows)), function(i) rows[i, ])
  if (!is.list(rows) && !is.character(rows)) return(NULL)
  rows <- as.list(rows)
  if (length(rows) > MAX_DOSE_ROWS - 1L) rows <- rows[seq_len(MAX_DOSE_ROWS - 1L)]
  cells <- lapply(rows, function(r) {
    r <- as.list(r)
    vapply(1:4, function(j) if (j <= length(r)) doseGridCell(r[[j]]) else "", character(1))
  })
  table <- data.frame(
    Drug = vapply(cells, `[`, "", 1),
    Time = vapply(cells, `[`, "", 2),
    Dose = vapply(cells, `[`, "", 3),
    Units = vapply(cells, `[`, "", 4),
    stringsAsFactors = FALSE
  )
  # The number of edits the grid has sent: a count, so never negative.  A
  # negative one made every draft the server sent back look older than the
  # grid's latest edit, so the grid kept drawing its old rows while Apply
  # committed the new ones (the external review of 2026-11-05).
  seq <- suppressWarnings(as.numeric(doseGridCell(value$seq)))
  list(
    table = table,
    format = parseTimeFormat(doseGridCell(value$timeFormat)),
    seq = if (length(seq) == 1 && is.finite(seq) && seq >= 0) seq else 0
  )
}

# What the grid sent, as the grid would accept it: restoredDoseTable() (the
# rule a bookmark link is read by) blanks an unknown drug's row, a unit the
# drug does not offer, a time or dose that cannot be read or is too long, and
# rows past the limit.  The grid itself never sends such a table; anyone can
# send their own session one, and until 2026-11-05 it was stored, drawn and
# applied: fentanyl in mg/kg, a dose over the limit, a 1 MB string (the
# external review of that day).  A unit is blanked, never swapped for the
# drug's default, which could be a thousandfold error.
doseGridAccepted <- function(table, drugDefaults) {
  restoredDoseTable(table, drugDefaults)
}
