drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}

#' Return the stanpumpR drug defaults table, with expansion applied
#'
#' @examples
#' getDrugDefaultsGlobal()
#'
#' @export
getDrugDefaultsGlobal <- function()
{
  if (exists("drugDefaults_global") == FALSE) {
    # Annoying, Shiny's autoload won't load sysdata.rda, only .R files!
    # So, when loaded as a package, sysdata.rda will load but not when
    # the Shiny app is executed using runApp()
    load('R/sysdata.rda')
  }

  drugDefaultsDataset <- drugDefaults_global
  drugDefaultsDataset$Units <- strsplit(drugDefaultsDataset$Units, ",")
  drugDefaultsDataset
}

#' Returns the stanpumpR drug defaults table for a single drug
#'
#' @examples
#' getDrugDefaults('remifentanil')
#'
#' @export
getDrugDefaults <- function(drug)
{
  drugDefaults_global <- getDrugDefaultsGlobal();
  drugDefaults_global[drugDefaults_global$Drug == drug, ]
}
