#' Process a dose table for multiple drugs using the internal defaults for each drug
#'
#' See \code{vignette("stanpumpR-multi-PK", package = "stanpumpR")} for an example
#'
#' @param dose table of individual doses
#' @param events table of events
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as string: "female" or "male"
#' @param maximum maximum length of simulation in minutes
#' @param plotRecovery (current broken, leave set to FALSE) should recovery parameters be calculated?
#'
#' @returns a list of data frames with the output of the a single drug simulation
#'
#' @export
simulateDrugsWithCovariates <- function (dose, events, weight, height, age, sex, maximum, plotRecovery)
{
  drugList <- unique(dose$Drug)
  output <- c()
  for (drug in drugList)
  {
    drugDefaults <- getDrugDefaults(drug)
    PK <- getDrugPK(drug, weight, height, age, sex, drugDefaults)
    currentDT <- dose[dose$Drug == drug,]
    X <- simCpCe(currentDT, events, PK, maximum, plotRecovery)

    output[[drug]]$Drug                <- drugDefaults$Drug
    output[[drug]]$Concentration.Units <- drugDefaults$Concentration.Units
    output[[drug]]$Color               <- drugDefaults$Color
    output[[drug]]$endCe               <- drugDefaults$endCe
    output[[drug]]$endCeText           <- drugDefaults$endCeText

    output[[drug]]$DT                  <- currentDT
    output[[drug]]$ET                  <- events

    output[[drug]]$results             <- X$results
    output[[drug]]$equiSpace           <- X$equiSpace
    output[[drug]]$max                 <- X$max
  }
  return(output)
}
