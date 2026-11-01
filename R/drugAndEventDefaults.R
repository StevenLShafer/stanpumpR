# Split each drug's comma-separated Units into a vector, grouped by route (IV,
# then PO, IM, IN, RA) so the units dropdowns list each route's units together.
drugUnitsExpand <- function(units) {
  # trimws: a Drug Library edit typed as "mg, mcg/kg/min" must offer
  # "mcg/kg/min", not " mcg/kg/min", which the dose table would then refuse
  lapply(strsplit(units, ","), function(tokens) groupUnitsByRoute(trimws(tokens)))
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}

# Can ggplot draw this colour?  A name or "#RRGGBB" col2rgb() reads.
isValidColor <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) return(FALSE)
  !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

# What is wrong with an edited drug library (the Drug Library dialog), as
# sentences, or character(0).  Only entries that changed are judged, since a
# metabolite-only row has no colour or units of its own.  A Color ggplot cannot
# read reached the plot as a generic error in its place, and a Units entry the
# dose table does not accept (validateDoseTableInput()) was offered in the
# table and then refused as unknown units, with a blank plot (2026-10-31).
# Lower/Upper/Typical/MEAC had no check at all: a non-numeric entry (e.g.
# "bogus") was silently coerced to NA by the as.numeric() in the
# input$drugEditsOK observer, with no message, erasing the stored threshold
# (production audit, 2026-11-01) -- the same failure the separate Drug
# Thresholds dialog already guards against (thresholdTableToDefaults()).
drugLibraryProblems <- function(edited, current) {
  problems <- character(0)
  at <- match(edited$Drug, current$Drug)
  color <- as.character(edited$Color)
  colorBefore <- as.character(current$Color)[at]
  badColor <- !is.na(at) & !identical2(color, colorBefore) & !vapply(color, isValidColor, logical(1))
  if (any(badColor)) {
    problems <- c(problems, paste0(
      "Color must be a colour name or #RRGGBB (",
      paste(sprintf("%s: \"%s\"", edited$Drug[badColor], color[badColor]), collapse = ", "), ")."))
  }
  known <- c(allUnits, gasUnits, tciUnits, scheduledUnits)
  units <- drugUnitsSimplify(edited$Units)
  unitsBefore <- drugUnitsSimplify(current$Units)[at]
  unknown <- character(0)
  emptied <- character(0)
  for (i in seq_along(units)) {
    if (is.na(at[i]) || identical(units[i], unitsBefore[i])) next
    tokens <- trimws(strsplit(units[i], ",", fixed = TRUE)[[1]])
    # A changed list with no unit, or an empty entry between commas, would
    # offer a blank the dose table ignores: refused, as an unknown unit is.
    if (length(tokens) == 0 || any(!nzchar(tokens))) emptied <- c(emptied, edited$Drug[i])
    bad <- tokens[nzchar(tokens) & !tokens %in% known]
    if (length(bad)) unknown <- c(unknown, sprintf("%s: %s", edited$Drug[i], paste0("\"", bad, "\"", collapse = ", ")))
  }
  if (length(unknown)) {
    problems <- c(problems, paste0(
      "Units must be units the dose table accepts (", paste(unknown, collapse = "; "), ")."))
  }
  if (length(emptied)) {
    problems <- c(problems, paste0(
      "Units must list at least one unit, with none left blank (", paste(emptied, collapse = ", "), ")."))
  }
  badNumbers <- character(0)
  for (col in c("Lower", "Upper", "Typical", "MEAC")) {
    valueChr <- trimws(as.character(edited[[col]]))
    blank <- is.na(edited[[col]]) | !nzchar(valueChr)
    parsed <- suppressWarnings(as.numeric(valueChr))
    bad <- !is.na(at) & !blank & (is.na(parsed) | !is.finite(parsed) | parsed < 0)
    if (any(bad)) {
      badNumbers <- c(badNumbers, sprintf("%s %s: \"%s\"", edited$Drug[bad], col, valueChr[bad]))
    }
  }
  if (length(badNumbers)) {
    problems <- c(problems, paste0(
      "Lower, Upper, Typical and MEAC must be left blank or a number of 0 or more (",
      paste(badNumbers, collapse = ", "), ")."))
  }
  problems
}

# Elementwise identical() for two character vectors, NA equal to NA
identical2 <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

#' Return the stanpumpR drug defaults table
#'
#' @param expand If `TRUE`, the list in the Units column is expanded. Otherwise,
#' the Units column contains comma-separated strings.
#' @returns A data.frame containing concentrations and bolus units, suggested colors and plasma/effect levels for drug effect endpoints
#' @details The function is memoised so that the file is only read once per session.
#'
#' @export
getDrugDefaultsGlobal <- memoise::memoise(function(expand = TRUE)
{
  drugDefaultsDataset <- utils::read.csv(
    system.file("extdata", "drugDefaults_global.csv", package = "stanpumpR"),
    na.strings = ""
  )

  if (expand) {
    drugDefaultsDataset$Units <- drugUnitsExpand(drugDefaultsDataset$Units)
  }

  drugDefaultsDataset
})

#' Drug names in the order the app offers them
#'
#' Alphabetical, ignoring case, in the C collation so the order is the same
#' on every platform.  The dose table's Drug column, the Add a dose dialog and
#' Suggest Dosing list the drugs this way; the drug library itself, and so
#' everything indexed by its rows, keeps the order of the CSV.
#'
#' @param drugs drug names
#' @returns `drugs`, sorted
#' @keywords internal
sortDrugNames <- function(drugs) {
  drugs[order(tolower(drugs), method = "radix")]
}

getEventDefaults <- function() {
  utils::read.csv(
    system.file("extdata", "eventDefaults.csv", package = "stanpumpR")
  )
}

#' Returns the stanpumpR drug defaults table for a single drug
#'
#' @param drug The drug in question
#'
#' @returns A subsetted data.frame from getDrugDefaultsGlobal()
#'
#' @export
getDrugDefaults <- function(drug)
{
  drugDefaults_global <- getDrugDefaultsGlobal()
  drugDefaults_global[drugDefaults_global$Drug == drug, ]
}
