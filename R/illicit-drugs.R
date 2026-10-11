# Illicit (drug-of-abuse) research models and the opt-in that gates them
#
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer;
# tests/testthat/test-illicit-drugs.R.
#
# Some drugs in the library are evidence-labelled research models of drugs of
# abuse (the "Illicit drugs" category of drugDefaults_global.csv).  They are
# hidden behind a persistent opt-in in the UI ("Show illicit drugs",
# input$showIllicitDrugs), which is off by default: when it is off these drugs
# are not offered anywhere a drug is chosen (the dose-table autocomplete, the
# Add a dose dialog), and when it is on they are, with their names shown in red
# in the dose table (createHOT()).  They are never ticked in the startup menu
# (startupDrugChoices() leaves the category out), so a fresh session never
# opens with one.

#' Is each drug an illicit (drug-of-abuse) research model?
#'
#' @param drug character vector of drug names
#' @param drugDefaults the drug library
#' @returns a logical vector the same length as `drug`; FALSE for a drug the
#'   library does not know, and all FALSE for a library without the Category
#'   column
#' @noRd
isIllicitDrug <- function(drug, drugDefaults = getDrugDefaultsGlobal()) {
  if (is.null(drugDefaults$Category)) return(rep(FALSE, length(drug)))
  category <- as.character(drugDefaults$Category)[match(drug, drugDefaults$Drug)]
  !is.na(category) & category == ILLICIT_DRUG_CATEGORY
}

#' The illicit drugs in the library
#'
#' @param drugDefaults the drug library
#' @returns the names of the drugs in the illicit category, in library order
#' @noRd
illicitDrugNames <- function(drugDefaults = getDrugDefaultsGlobal()) {
  drugDefaults$Drug[isIllicitDrug(drugDefaults$Drug, drugDefaults)]
}

#' The drugs offered when a drug is chosen, given the opt-in
#'
#' @param showIllicit is the illicit-drug opt-in on?
#' @param drugDefaults the drug library
#' @returns the drug names to offer: every drug when the opt-in is on, every
#'   non-illicit drug when it is off, in library order
#' @noRd
visibleDrugNames <- function(showIllicit, drugDefaults = getDrugDefaultsGlobal()) {
  drugs <- drugDefaults$Drug
  if (isTRUE(showIllicit)) return(drugs)
  drugs[!isIllicitDrug(drugs, drugDefaults)]
}
