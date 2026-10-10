# The drugs the simulator opens with
#
# Drafted by Claude Code, 2026-10-07, at the request of Steven L. Shafer;
# tests/testthat/test-startup-drugs.R.
#
# The drug library has grown well beyond anaesthesia, so the app no longer
# presumes which drugs to show.  A fresh session opens with an empty dose table
# and a menu of the drugs, one group of checkboxes per category (the Category
# column of drugDefaults_global.csv, in DRUG_CATEGORIES order).  The four drugs
# the app used to open with are ticked, and can be unticked.  Each chosen drug
# gets a zero-dose row at time 0, so pressing Start with the defaults gives the
# same dose table the app used to open with (doseTableInit).
#
# The menu is not shown when the URL restores a bookmark whose dose table has
# drugs in it.  It is shown from the server as the session starts, without
# waiting on the browser, so it does not depend on app.js being current.  When
# the welcome dialog is due (app.js decides, from a cookie), its text is added
# to the top of the open menu rather than replacing it.

#' Does a restore context carry a stanpumpR bookmark?
#'
#' Shiny restores from any query string, so a bare `?debug=1` "restores" too,
#' with nothing in it.  Only a URL holding a dose table is a bookmark.
#'
#' @param values the restore context's values: `session$restoreContext$values`
#'   (an environment) or the `state$values` list passed to `onRestored()`
#' @noRd
isBookmarkRestore <- function(values) {
  !is.null(values) && !is.null(values$DT)
}

#' Does the session open on the startup menu?
#'
#' Yes, unless it restores a bookmark with a drug in its dose table.  The URL
#' is rewritten on every change, so reloading the page while the menu is open,
#' or after deleting every dose, gives a bookmark with an empty dose table, and
#' that is a fresh start too.
#' @noRd
opensOnStartupMenu <- function(values) {
  if (!isBookmarkRestore(values)) return(TRUE)
  !any(nzchar(as.character(unlist(values$DT$Drug))))
}

#' Input id of one category's checkbox group in the startup menu
#' @noRd
startupDrugInputId <- function(category) {
  paste0("startupDrugs_", match(category, DRUG_CATEGORIES))
}

#' The drugs the startup menu offers, by category
#'
#' @param drugDefaults the drug library
#' @returns a named list, one character vector of drug names for each category
#'   that has any, in DRUG_CATEGORIES order, each sorted by displayed name.  A
#'   drug with no category, or one not in DRUG_CATEGORIES, is not offered.  The
#'   illicit-drug category (ILLICIT_DRUG_CATEGORY) is left out: those are opt-in
#'   research models (R/illicit-drugs.R), never ticked at startup.
#' @noRd
startupDrugChoices <- function(drugDefaults = getDrugDefaultsGlobal()) {
  if (is.null(drugDefaults$Category)) return(list())
  categories <- setdiff(DRUG_CATEGORIES, ILLICIT_DRUG_CATEGORY)
  category <- as.character(drugDefaults$Category)
  choices <- lapply(categories, function(cat) {
    drugs <- drugDefaults$Drug[!is.na(category) & category == cat]
    drugs[order(tolower(helpDrugTitle(drugs)))]
  })
  names(choices) <- categories
  choices[lengths(choices) > 0]
}

#' The dose table for the drugs chosen in the startup menu
#'
#' One row per chosen drug, at time 0 with a dose of 0, in its Default.Units
#' (two rows for the drugs in STARTUP_UNITS), in the menu's order, then
#' STARTUP_BLANK_ROWS blank rows to type into.
#'
#' An inhaled agent also brings an oxygen row, with its flow left blank: an
#' agent needs fresh gas to carry it, and a blank flow is the one nitrous oxide
#' fills in at 21% once its own flow is entered (ensureGasOxygen()), where an
#' explicit 0 would be kept.  The ventilation row is added by the dose table's
#' gas rules, as for any gas entered by hand.
#'
#' @param drugs the chosen drug names.  They come from the browser, so anything
#'   the menu does not offer is dropped.
#' @param drugDefaults the drug library
#' @returns a dose table with character columns, as the app holds it
#' @noRd
startupDoseTable <- function(drugs, drugDefaults = getDrugDefaultsGlobal()) {
  offered <- unlist(startupDrugChoices(drugDefaults), use.names = FALSE)
  drugs <- offered[offered %in% drugs]

  rows <- lapply(drugs, function(drug) {
    units <- STARTUP_UNITS[[drug]]
    if (is.null(units)) {
      units <- as.character(drugDefaults$Default.Units[drugDefaults$Drug == drug])
    }
    data.frame(Drug = drug, Time = "0", Dose = "0", Units = units)
  })
  gas <- which(isGasDrug(drugs))
  if (length(gas) > 0) {
    oxygen <- data.frame(Drug = "oxygen", Time = "0", Dose = "", Units = "L/min")
    rows[[gas[1]]] <- rbind(oxygen, rows[[gas[1]]])
  }

  blanks <- doseTableNewRow[rep(1, STARTUP_BLANK_ROWS), ]
  dt <- do.call(rbind, c(rows, list(blanks)))
  rownames(dt) <- NULL
  dt
}

#' What stanpumpR is, and is not: the text of the welcome dialog
#' @noRd
welcomeText <- function() {
  tagList(
    p(
      "stanpumpR, derived from the original STANPUMP program developed at
      Stanford University,  performs pharmacokinetic simulations
      based on mathematical models published in the peer-reviewed
      literature. stanpumpR is intended to help clinicians and investigators
      better understand the mathematical implications of published models.
      stanpumpR is only an advisory program. How these models are applied to
      individual patients is a matter of clinical judgment by the health care
      provider."
    ),
    p("stanpumpR does not collect any protected healthcare information.")
  )
}

#' The startup menu
#'
#' @param choices startupDrugChoices()
#' @param selected the drugs ticked when it opens
#' @returns a modalDialog().  It has no close button and ignores Escape and
#'   clicks outside: Start is how it closes.  `output$startup_welcome` is a
#'   placeholder, filled with the welcome text when that is due.
#' @noRd
startupDrugModal <- function(choices, selected = STARTUP_DRUGS_DEFAULT) {
  groups <- lapply(names(choices), function(category) {
    drugs <- choices[[category]]
    div(
      class = "startup-drug-group",
      checkboxGroupInput(
        inputId = startupDrugInputId(category),
        label = category,
        choices = stats::setNames(drugs, helpDrugTitle(drugs)),
        selected = intersect(selected, drugs)
      )
    )
  })

  modalDialog(
    title = "Welcome to stanpumpR",
    uiOutput("startup_welcome"),
    p(
      class = "startup-drug-lead",
      tags$strong("Choose the drugs to display."),
      "Each starts with a dose of 0 at time 0, ready to edit. Any drug can be
      added to the dose table later."
    ),
    div(class = "startup-drug-groups", groups),
    footer = actionButton("startup_ok", "Start", class = "btn-primary"),
    easyClose = FALSE,
    size = "xl"
  )
}

#' The welcome text as it appears inside the startup menu, with the tour
#' @noRd
startupWelcomeUI <- function() {
  div(
    class = "startup-welcome",
    welcomeText(),
    actionButton(
      "startup_tour", "Take the tour",
      icon = icon("circle-question"),
      class = "btn-outline-primary btn-sm"
    )
  )
}
