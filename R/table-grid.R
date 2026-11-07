# The table grid: the dialogs' grids (inst/www/table-grid.js), in place of
# rhandsontable (docs/grid-replacement.md).  A grid is drawn from a column
# specification and the rows to show, and is read when its dialog's Apply or
# OK is pressed.

# The grids' scripts and styles, as one dependency that every grid carries,
# so a page loads them once with the first grid it draws: the app with its dose
# grid, the Single Paper Review app (run_paper_review_app()) with its doses.
# grid-funs.js holds what the grids share with R (validateTime(),
# validateDose(), the messages); dose-grid.js the dose grid; table-grid.js the
# dialogs' grid.
gridDependency <- function() {
  htmltools::htmlDependency(
    name = "stanpumpr-grids",
    version = as.character(utils::packageVersion("stanpumpR")),
    src = system.file("www", package = "stanpumpR"),
    script = c("grid-funs.js", "dose-grid.js", "table-grid.js"),
    stylesheet = "grids.css",
    all_files = FALSE
  )
}

# One column of a table grid.
#   type: "text", "time" (read as validateTime() reads it), "dose" and
#     "number" (as validateDose() reads them: 0 to MAX_DOSE_VALUE), "checkbox",
#     "select" (one of `options`, or of rowOptions[[i]] in row i) or
#     "readonly".
#   readonlyRows: the rows (1-based) whose cell cannot be changed.
#   wrapUnits: a select of dose units, shown wrapped after the route
#     (unitWrapText()).
tableColumn <- function(name, label = name,
                        type = c("text", "time", "dose", "number", "checkbox", "select", "readonly"),
                        options = NULL, rowOptions = NULL, readonlyRows = NULL,
                        width = NULL, wrapUnits = FALSE, tip = NULL) {
  type <- match.arg(type)
  col <- list(name = name, label = label, type = type)
  if (!is.null(options)) col$options <- I(as.character(options))
  if (!is.null(rowOptions)) col$rowOptions <- lapply(rowOptions, function(o) I(as.character(o)))
  if (length(readonlyRows) > 0) col$readonlyRows <- I(as.integer(readonlyRows) - 1L)
  if (!is.null(width)) col$width <- width
  if (isTRUE(wrapUnits)) col$wrapUnits <- TRUE
  if (!is.null(tip)) col$tip <- tip
  col
}

# The grid, with `data` (a data frame, one column per entry of `columns`, in
# order) as its rows.  `tag` is returned with every value (the Edit doses
# dialog's drug, so that Apply cannot write one drug's rows as another's from
# a grid left over from an earlier dialog).  `rowMenu`: rows can be inserted
# and removed (Shift+F10, the context-menu key or a right click), up to
# `maxRows`.  `label` names the grid to a screen reader (its caption), which
# is also told the keys.
tableGridInput <- function(inputId, columns, data, tag = NULL, rowMenu = FALSE,
                           maxRows = MAX_DOSE_ROWS - 1L, height = NULL, label = "Table") {
  rows <- lapply(seq_len(NROW(data)), function(i) {
    lapply(seq_along(columns), function(j) {
      v <- if (j <= NCOL(data)) data[[j]][[i]] else NA
      if (identical(columns[[j]]$type, "checkbox")) return(isTRUE(as.logical(v)))
      if (length(v) != 1 || is.na(v)) return("")
      if (is.numeric(v)) return(format(v, scientific = FALSE, digits = 15, trim = TRUE))
      as.character(v)
    })
  })
  spec <- list(columns = columns, rows = rows, rowMenu = isTRUE(rowMenu), maxRows = maxRows,
               label = label)
  if (!is.null(tag)) spec$tag <- tag
  htmltools::tagList(gridDependency(), htmltools::tags$div(
    id = inputId,
    class = "stanpumpr-table-grid",
    style = if (!is.null(height)) sprintf("max-height:%dpx;overflow-y:auto", as.integer(height)),
    `data-spec` = as.character(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null"))
  ))
}

# What a table grid sent, as a data frame named by `columns` (text, and
# logical for a checkbox), or NULL for anything that is not a grid's value.
# Never an error, whatever the browser sent: an error in the observer that
# reads it would end the session.  A cell that is not one value is blank (or
# unticked); rows past `maxRows` are cut, and the table returned says so
# (tableGridTruncated()): a grid never sends more rows than it was drawn to
# hold, so a handler refuses such a table rather than store the cut one.
tableGridRead <- function(value, columns, maxRows = MAX_DOSE_ROWS - 1L) {
  if (!is.list(value) || is.null(value$rows)) return(NULL)
  rows <- value$rows
  if (is.matrix(rows)) rows <- lapply(seq_len(nrow(rows)), function(i) rows[i, ])
  if (!is.list(rows) && !is.character(rows)) return(NULL)
  rows <- as.list(rows)
  truncated <- length(rows) > maxRows
  if (truncated) rows <- rows[seq_len(maxRows)]
  out <- lapply(seq_along(columns), function(j) {
    checkbox <- identical(columns[[j]]$type, "checkbox")
    vals <- lapply(rows, function(r) {
      r <- if (is.list(r) || is.atomic(r)) as.list(r) else list()
      cell <- if (j <= length(r)) r[[j]] else NULL
      if (checkbox) {
        if (is.list(cell) && length(cell) == 1) cell <- cell[[1]]
        return(isTRUE(cell))
      }
      doseGridCell(cell)
    })
    if (checkbox) vapply(vals, isTRUE, logical(1)) else vapply(vals, identity, "")
  })
  names(out) <- vapply(columns, `[[`, "", "name")
  out <- as.data.frame(out, stringsAsFactors = FALSE, optional = TRUE)
  if (truncated) attr(out, "truncated") <- TRUE
  out
}

# Did tableGridRead() cut the rows the browser sent?
tableGridTruncated <- function(table) isTRUE(attr(table, "truncated"))

# The tag the grid was drawn with (tableGridInput()), or ""
tableGridTag <- function(value) {
  if (!is.list(value)) return("")
  doseGridCell(value$tag)
}

# --- The dialogs' grids -------------------------------------------------------------

# Edit doses: Delete, Time, Dose (headed `doseLabel`, "Dose or Target" for a
# drug with TCI units, as the Add a dose dialog: app_server()'s
# doseFieldLabel()), and the drug's units
editDosesGridColumns <- function(units, doseLabel = "Dose") {
  list(
    tableColumn("Delete", type = "checkbox", width = 55),
    tableColumn("Time", type = "time", width = 70),
    tableColumn("Dose", label = doseLabel, type = "dose", width = 100),
    tableColumn("Units", type = "select", options = units, wrapUnits = TRUE)
  )
}

# Edit events: Delete, Time, and an event from the list
editEventsGridColumns <- function(events) {
  list(
    tableColumn("Delete", type = "checkbox", width = 55),
    tableColumn("Time", type = "time", width = 70),
    tableColumn("Event", type = "select", options = events)
  )
}

# Suggest Dosing's targets: a time and a target concentration
targetGridColumns <- function() {
  list(
    tableColumn("Time", type = "time"),
    tableColumn("Target", type = "number")
  )
}

# Drug thresholds: the drug, which cannot be changed, and its threshold
thresholdGridColumns <- function() {
  list(
    tableColumn("Drug", type = "readonly"),
    tableColumn("Threshold", type = "number", width = 90)
  )
}

# The Drug Library (drugLibraryGridData()).  mcg and ng differ only by the
# factor shown, so the concentration list offers those two; a drug in %,
# mmol or mOsm (an inhaled agent, calcium, mannitol) is simulated in those
# units by its own engine or conversion, and switching it to mcg or ng would
# break the dose conversion in simCpCe(), so its cell cannot be changed
# (Claude Code, 2026-10-17, from an external review).  The numbers are text,
# which drugLibraryProblems() reads as typed.
drugLibraryGridColumns <- function(x) {
  list(
    tableColumn("Drug", type = "readonly", width = 130),
    tableColumn("Concentration.Units", type = "select", options = c("mcg", "ng"),
                readonlyRows = which(!x$Concentration.Units %in% c("mcg", "ng")), width = 90),
    tableColumn("Bolus.Units", type = "select", options = bolusUnits, width = 90),
    tableColumn("Infusion.Units", type = "select", options = infusionUnits, width = 110),
    tableColumn("Default.Units", type = "select", options = allUnits, wrapUnits = TRUE, width = 120),
    tableColumn("Units", width = 260),
    tableColumn("Color", width = 80),
    tableColumn("Lower", width = 60),
    tableColumn("Upper", width = 60),
    tableColumn("Typical", width = 60),
    tableColumn("MEAC", width = 60),
    tableColumn("Class", width = 60)
  )
}

# Single Paper Review's doses: a time in the paper's unit, a dose and one of
# the drug's units
paperReviewDoseColumns <- function(unit, units) {
  list(
    tableColumn("Time", label = paste0("Time (", unit, ")"), type = "number"),
    tableColumn("Dose", type = "dose"),
    tableColumn("Units", type = "select", options = units)
  )
}
