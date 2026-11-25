# The sidebar's Bayesian Updating panel (app_ui()), and what app_server()
# reads from it: the measured concentrations, in minutes, and the two
# variabilities.  The estimate itself is R/bayesian.R.
#
# The table is a table grid (R/table-grid.R): Drug (the drugs of the dose
# table), Time (typed in the dose table's format, as the dose table is) and
# Concentration (in the units of the drug's plot).  The grid sends its rows on
# every committed edit, and a complete row updates the drug: there is no Apply.
# What the user typed is kept as text, with the format it was typed in, so an
# incomplete row survives the grid being redrawn (a new drug in the dose
# table, a change of time units, which converts it); the measurements are read
# from it in minutes, as the dose table's times are.

# The grid's columns
bayesGridColumns <- function(drugs) {
  list(
    tableColumn("Drug", type = "select", options = drugs, width = 105),
    tableColumn("Time", type = "time", width = 70),
    tableColumn("Concentration", label = "Conc.", type = "number", width = 60)
  )
}

# Blank text rows
bayesBlankRows <- function(n = BAYES_TABLE_BLANK_ROWS) {
  data.frame(Drug = rep("", n), Time = rep("", n), Concentration = rep("", n),
             stringsAsFactors = FALSE)
}

# Text rows to measurements: Drug, Time in minutes, Concentration, as
# bayesianObservations() keeps them (complete rows only).  `format` is the
# format the times were typed in, `reference` the procedure start in clock
# mode.
bayesRowsToObservations <- function(rows, format, reference) {
  if (is.null(rows) || nrow(rows) == 0 || is.null(format))
    return(bayesianObservations(NULL))
  time <- trimws(as.character(rows$Time))
  minutes <- rep(NA_real_, length(time))
  typed <- !is.na(time) & nzchar(time)
  if (any(typed))
    minutes[typed] <- displayTimeToMinutes(vapply(time[typed], validateTime, ""),
                                           referenceForFormat(format, reference),
                                           format[["unit"]])
  bayesianObservations(data.frame(Drug = rows$Drug, Time = minutes,
                                  Concentration = rows$Concentration,
                                  stringsAsFactors = FALSE))
}

# Text rows typed in one format, shown in another: each readable time is
# converted through minutes; one that cannot be read is kept as typed.
bayesRowsInFormat <- function(rows, from, to, reference) {
  if (is.null(rows) || nrow(rows) == 0 || is.null(from) || identical(from, to)) return(rows)
  time <- trimws(as.character(rows$Time))
  typed <- !is.na(time) & nzchar(time)
  if (!any(typed)) return(rows)
  minutes <- displayTimeToMinutes(vapply(time[typed], validateTime, ""),
                                  referenceForFormat(from, reference), from[["unit"]])
  ok <- !is.na(minutes)
  converted <- time[typed]
  converted[ok] <- minutesToEntryTime(minutes[ok], to, reference)
  rows$Time[typed] <- converted
  rows
}

# Measurements (minutes) as text rows in a format, for a bookmark restored
bayesObservationsToRows <- function(observations, format, reference) {
  if (is.null(observations) || nrow(observations) == 0) return(bayesBlankRows(0))
  data.frame(
    Drug = observations$Drug,
    Time = minutesToEntryTime(observations$Time, format, reference),
    # base::format(): the argument `format` is the time format
    Concentration = vapply(observations$Concentration, base::format, "",
                           scientific = FALSE, digits = 15, trim = TRUE),
    stringsAsFactors = FALSE
  )
}

# A bookmark's measurements, read as untrusted input (R/bookmark-restore.R):
# only the drugs of the library, complete rows, at most
# MAX_BAYES_OBSERVATIONS of them.  `changed` says whether anything was left
# out.
restoredBayesObservations <- function(x, drugNames) {
  if (is.null(x)) return(list(table = bayesianObservations(NULL), changed = FALSE))
  table <- tryCatch({
    if (is.list(x) && !is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (!is.data.frame(x)) stop("not a table")
    bayesianObservations(x)
  }, error = function(e) NULL)
  if (is.null(table)) return(list(table = bayesianObservations(NULL), changed = TRUE))
  n <- if (is.data.frame(x)) nrow(x) else 0
  # No later than the longest plot (BAYES_MAX_TIME): a link opened with a
  # level at 1e12 minutes, and no click, held the worker in the fit (Carbon's
  # UI audit, 2026-11-14, F1)
  table <- table[table$Drug %in% drugNames & table$Time <= BAYES_MAX_TIME, , drop = FALSE]
  if (nrow(table) > MAX_BAYES_OBSERVATIONS) table <- table[seq_len(MAX_BAYES_OBSERVATIONS), ]
  rownames(table) <- NULL
  list(table = table, changed = nrow(table) != n)
}

# A measurement's time for the estimate's table: the clock time in Actual
# time, else the time in the unit to four significant digits (the table's
# own entry keeps every digit)
bayesTimeLabel <- function(minutes, format, reference) {
  if (isClockFormat(format)) return(minutesToEntryTime(minutes, format, reference))
  trimws(formatC(signif(minutes / TIME_UNITS[[format[["unit"]]]], 4), digits = 4, format = "fg"))
}

# A slider's value as a number within its range, or its default
bayesSliderValue <- function(x, default, min, max) {
  x <- asScalarNumeric(x)
  if (length(x) != 1 || !is.finite(x)) return(default)
  min(max(x, min), max)
}

# The unit of each drug's plot, as the panel names it: "amiodarone: mcg/mL"
bayesUnitsText <- function(drugs, drugDefaults) {
  units <- drugDefaults$Concentration.Units[match(drugs, drugDefaults$Drug)]
  paste0(drugs, ": ", plottedUnitText(units, drugs))
}

#' The Bayesian Updating panel's server
#'
#' @param input,output,session the app's
#' @param doseTableClean,doseTableFormat,drugDefaults the app's reactives
#' @param result a function returning the update (bayesianUpdateDrugs()), for
#'   the panel's summary; called lazily, since app_server() defines it after
#'   this
#' @returns a list: `observations()` (debounced, minutes), `pkVariability()`,
#'   `residualVariability()`, `rows` (the table as typed, for the bookmark)
#'   and `restore(observations, format)`, which puts a bookmark's or a
#'   scenario's measurements in the table, written in `format` (by default
#'   the dose table's); an empty table clears it
#' @noRd
bayesianServer <- function(input, output, session, doseTableClean, doseTableFormat,
                           drugDefaults, result) {
  # The table as typed: text rows, and the format their times are in
  rows <- reactiveVal(list(rows = bayesBlankRows(0), format = NULL))
  redraw <- reactiveVal(0)

  # The drugs that can be measured: those of the committed dose table, but
  # not the inhaled agents
  choices <- reactive({
    DT <- tryCatch(doseTableClean(), error = function(e) NULL)
    if (is.null(DT)) return(character(0))
    # A combination product's levels are measured of its components
    # (R/combination-products.R): artemether and lumefantrine for Coartem.
    sortDrugNames(combinationExpandDrugNames(unique(DT$Drug[!isGasDrug(DT$Drug)])))
  })

  output$bayesTableUI <- renderUI({
    redraw()
    format <- doseTableFormat()
    drugs <- choices()
    if (length(drugs) == 0)
      return(tags$p(class = "small text-muted", "Enter doses in the dose table first."))
    stored <- isolate(rows())
    reference <- isolate(asScalarString(input$referenceTime))
    # Clock times are converted through the procedure start.  Without a
    # readable one the rows stay in the format they were typed in, and so
    # does the grid, which reads what is typed in its own: converted
    # without it, "08:30" was kept as typed and then read as eight and a
    # half hours elapsed (Carbon's third UI audit, 2026-11-15, M2).  It is
    # drawn again when there is one.
    if (!is.null(stored$format) && !identical(stored$format, format) &&
        (isClockFormat(stored$format) || isClockFormat(format)) &&
        !isValidReferenceTime(reference)) {
      input$referenceTime
      format <- stored$format
    }
    shown <- bayesRowsInFormat(stored$rows, stored$format, format, reference)
    n <- min(MAX_BAYES_OBSERVATIONS, max(BAYES_TABLE_BLANK_ROWS, nrow(shown) + 2L))
    shown <- rbind(shown, bayesBlankRows(n - nrow(shown)))
    tagList(
      tags$p(class = "small text-muted mb-1", timeEntryLabel(format)),
      tableGridInput("bayesTable", bayesGridColumns(drugs), shown,
                     tag = timeFormatString(format), rowMenu = TRUE,
                     maxRows = MAX_BAYES_OBSERVATIONS, label = "Measured concentrations"),
      tags$p(class = "small text-muted mt-1 mb-0",
             paste0("Concentrations in the units of each drug's plot: ",
                    paste(bayesUnitsText(drugs, drugDefaults()), collapse = "; "), "."))
    )
  })

  # Every committed edit.  Never an error, whatever the browser sent
  # (tableGridRead()); a table cut short was never one the grid sent.
  observeEvent(input$bayesTable, {
    value <- input$bayesTable
    table <- tableGridRead(value, bayesGridColumns(character(0)),
                           maxRows = MAX_BAYES_OBSERVATIONS)
    if (is.null(table)) return()
    if (tableGridTruncated(table)) {
      showNotification(sprintf("The table holds at most %d measurements.",
                               MAX_BAYES_OBSERVATIONS), type = "error")
      return()
    }
    format <- parseTimeFormat(tableGridTag(value))
    if (is.null(format)) return()
    immediate(FALSE)
    typed <- nzchar(trimws(table$Drug)) | nzchar(trimws(table$Time)) |
      nzchar(trimws(table$Concentration))
    table <- table[typed, , drop = FALSE]
    rownames(table) <- NULL
    rows(list(rows = table, format = format))
  })

  # The stored rows follow the dose table's format, as the dose table's own
  # times do, once they can be converted (a clock time needs a readable
  # procedure start).  Converted only where they were drawn, they stayed in
  # the old format, and a Procedure start emptied afterwards in elapsed time
  # dropped a level typed as 08:30 (Carbon's fourth UI audit, 2026-11-15,
  # M3).  A change of the start in clock time moves the levels as it moves
  # the doses, so they take effect together, not 750 ms apart.
  observeEvent(list(doseTableFormat(), input$referenceTime), {
    stored <- rows()
    if (is.null(stored$format) || nrow(stored$rows) == 0) return()
    format <- doseTableFormat()
    reference <- asScalarString(input$referenceTime)
    if (identical(stored$format, format)) {
      if (isClockFormat(format)) immediate(TRUE)
      return()
    }
    if ((isClockFormat(stored$format) || isClockFormat(format)) &&
        !isValidReferenceTime(reference)) return()
    immediate(TRUE)
    rows(list(rows = bayesRowsInFormat(stored$rows, stored$format, format, reference),
              format = format))
  }, ignoreInit = TRUE)

  observeEvent(input$bayesClear, {
    immediate(TRUE)
    rows(list(rows = bayesBlankRows(0), format = NULL))
    redraw(redraw() + 1)
  })

  # Each value numbered, so that the debounced one is never older than a
  # change the app made itself (`lastImmediate`, below)
  observationsSeq <- 0
  observations <- reactive({
    stored <- rows()
    observationsSeq <<- observationsSeq + 1
    out <- bayesRowsToObservations(stored$rows, stored$format, asScalarString(input$referenceTime))
    attr(out, "seq") <- observationsSeq
    out
  })
  # Typing a row commits two or three cells in quick succession; the estimate
  # waits for the row.  A change the app makes itself (a link restored, a
  # unit switch converting the levels, Clear) takes effect at once: through
  # the wait, a fit of the old levels against the new units ran first,
  # reported the search's limit and held the worker (Carbon's third UI
  # audit, 2026-11-15, C4).
  observationsDebounced <- debounce(observations, 750)
  immediate <- reactiveVal(FALSE)
  # An edit typed within the wait after such a change (a unit switch's
  # converted levels) would otherwise fall back to the debounced value from
  # before it, the levels in the old units against the new model, until the
  # timer fired (CodeRabbit on PR #730)
  lastImmediate <- NULL
  observationsUsed <- reactive({
    if (immediate()) {
      out <- observations()
      lastImmediate <<- out
      return(out)
    }
    out <- observationsDebounced()
    if (!is.null(lastImmediate) && isTRUE(attr(out, "seq") < attr(lastImmediate, "seq")))
      lastImmediate else out
  })

  pkVariability <- debounce(reactive(bayesSliderValue(
    input$bayesPKVariability, BAYES_PK_VARIABILITY_DEFAULT,
    BAYES_PK_VARIABILITY_MIN, BAYES_PK_VARIABILITY_MAX)), 500)
  residualVariability <- debounce(reactive(bayesSliderValue(
    input$bayesResidualVariability, BAYES_RESIDUAL_VARIABILITY_DEFAULT,
    BAYES_RESIDUAL_VARIABILITY_MIN, BAYES_RESIDUAL_VARIABILITY_MAX)), 500)

  # What was estimated, and what could not be used
  output$bayesSummary <- renderUI({
    obs <- observations()
    if (nrow(obs) == 0) return(NULL)
    res <- tryCatch(result(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    format <- doseTableFormat()
    reference <- asScalarString(input$referenceTime)
    fitBlocks <- lapply(names(res$fits), function(drug) {
      fit <- res$fits[[drug]]
      o <- fit$observations
      shown <- function(x) formatC(x, digits = 3, format = "fg")
      tags$div(
        class = "bayes-fit mb-2",
        tags$p(class = "small mb-1", bayesianFitSummary(drug, fit)),
        tags$table(
          class = "table table-sm small mb-0 bayes-fit-table",
          tags$thead(tags$tr(tags$th("Time"), tags$th("Measured"),
                             tags$th("Model"), tags$th("Updated"))),
          tags$tbody(lapply(seq_len(nrow(o)), function(i) tags$tr(
            tags$td(bayesTimeLabel(o$Time[i], format, reference)),
            tags$td(shown(o$Measured[i])),
            tags$td(shown(o$Population[i])),
            tags$td(shown(o$Individual[i]))
          )))
        )
      )
    })
    tagList(
      fitBlocks,
      if (length(res$problems) > 0)
        tags$div(class = "small text-danger", lapply(res$problems, tags$p, class = "mb-1"))
    )
  })

  # The converted value of each measured concentration of the drugs in
  # `factors` (named by drug), or NULL when one would be past the table's
  # ceiling, which the reading of the rows drops (validateDose())
  rescaledRows <- function(factors) {
    stored <- isolate(rows())
    table <- stored$rows
    for (i in which(as.character(table$Drug) %in% names(factors))) {
      value <- suppressWarnings(as.numeric(as.character(table$Concentration[i])))
      if (is.na(value)) next
      converted <- validateDose(plainDecimal(value * factors[[as.character(table$Drug[i])]]))
      if (!nzchar(converted)) return(NULL)
      table$Concentration[i] <- converted
    }
    list(rows = table, format = stored$format)
  }

  list(
    observations = observationsUsed,
    pkVariability = pkVariability,
    residualVariability = residualVariability,
    rows = rows,
    # Measured concentrations of drugs moved between mcg and ng in the Drug
    # Library, converted as their thresholds are (`factors` named by drug;
    # Carbon's second UI audit, 2026-11-15, F6)
    canRescale = function(factors) !is.null(rescaledRows(factors)),
    # A row with a concentration typed and no drug chosen yet
    hasValueWithoutDrug = function() {
      table <- isolate(rows())$rows
      if (is.null(table) || nrow(table) == 0) return(FALSE)
      drug <- trimws(as.character(table$Drug)); drug[is.na(drug)] <- ""
      value <- trimws(as.character(table$Concentration)); value[is.na(value)] <- ""
      any(!nzchar(drug) & nzchar(value))
    },
    rescale = function(factors) {
      if (!any(as.character(isolate(rows())$rows$Drug) %in% names(factors))) return(invisible())
      converted <- rescaledRows(factors)
      if (is.null(converted)) return(invisible())
      immediate(TRUE)
      rows(converted)
      redraw(isolate(redraw()) + 1)
    },
    # A bookmark's or a teaching scenario's measurements (minutes), written
    # in `format` (by default the dose table's); an empty table clears it
    restore = function(observations, format = NULL) {
      immediate(TRUE)
      if (is.null(format)) format <- isolate(doseTableFormat())
      rows(list(rows = bayesObservationsToRows(observations, format,
                                               isolate(asScalarString(input$referenceTime))),
                format = format))
      redraw(isolate(redraw()) + 1)
    },
    # A new patient (a teaching scenario): no measurements, and the
    # variabilities at their defaults (Carbon's third UI audit, 2026-11-15,
    # M3: a propofol level stayed on a scenario's propofol)
    clear = function() {
      immediate(TRUE)
      rows(list(rows = bayesBlankRows(0), format = NULL))
      redraw(isolate(redraw()) + 1)
      updateSliderInput(session, "bayesPKVariability", value = BAYES_PK_VARIABILITY_DEFAULT)
      updateSliderInput(session, "bayesResidualVariability",
                        value = BAYES_RESIDUAL_VARIABILITY_DEFAULT)
    }
  )
}
