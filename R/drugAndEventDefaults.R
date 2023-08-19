drugUnitsExpand <- function(units) {
  strsplit(units, ",")
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}

#' Return the stanpumpR drug defaults table, with expansion applied
#'
#' @returns A data.frame containing concentrations and bolus units, suggested colors and plasma/effect levels for drug effect endpoints
#'
#' @export
getDrugDefaultsGlobal <- function()
{
  if (exists("drugDefaults_global") == FALSE) {
    # Annoying, Shiny's autoload won't load sysdata.rda, only .R files!
    # So, when loaded as a package, sysdata.rda will load but not when
    # the Shiny app is executed using runApp()
    load('data/sysdata.rda')
  }

  drugDefaultsDataset <- drugDefaults_global
  drugDefaultsDataset$Units <- strsplit(drugDefaultsDataset$Units, ",")
  drugDefaultsDataset
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
  drugDefaults_global <- getDrugDefaultsGlobal();
  drugDefaults_global[drugDefaults_global$Drug == drug, ]
}

# TODO: to different R file

getDrugAndEventDefaultsGlobal <- function()
{
  if (!exists("drugDefaults_global") || !exists("eventDefaults")) {
    # Annoying, Shiny's autoload won't load sysdata.rda, only .R files!
    # So, when loaded as a package, sysdata.rda will load but not when
    # the Shiny app is executed using runApp()
    load('data/sysdata.rda')
  }

  drugDefaultsDataset <- drugDefaults_global
  drugDefaultsDataset$Units <- strsplit(drugDefaultsDataset$Units, ",")
  list(drugDefaultsDataset=drugDefaultsDataset, eventDefaults=eventDefaults)
}
