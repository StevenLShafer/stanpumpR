# Split each drug's comma-separated Units into a vector, grouped by route (IV,
# then PO, IM, IN, RA) so the units dropdowns list each route's units together.
drugUnitsExpand <- function(units) {
  lapply(strsplit(units, ","), groupUnitsByRoute)
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
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
