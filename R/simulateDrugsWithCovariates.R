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
#' @param plotRecovery should the "time until threshold" be calculated?  See
#'   \code{simCpCe()}.
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#'   Only drugs whose model declares it are affected.
#'
#' @returns a list of data frames with the output of the a single drug
#'   simulation.  A drug that forms an active metabolite adds its contribution
#'   to the metabolite drug's entry, which is created if that drug was not
#'   dosed directly.
#'
#' @export
simulateDrugsWithCovariates <- function (dose, events, weight, height, age, sex,
                                         maximum, plotRecovery,
                                         cyp2d6 = CYP2D6_DEFAULT)
{
  if (length(sex) != 1 || !sex %in% SEX_VALUES) {
    stop("Invalid sex: ", paste(sex, collapse = ", "),
         ". Must be one of: ", paste(SEX_VALUES, collapse = ", "))
  }
  drugList <- unique(dose$Drug)
  # The inhaled gases are simulated by simulateGases() / advanceClosedFormGas(),
  # not here: they have no drugs_*.R covariate function and no mass dose, so
  # getDrugPK() and simCpCe() cannot handle them.
  drugList <- drugList[!isGasDrug(drugList)]
  output <- c()

  attach <- function(output, drug, PK, drugDefaults) {
    output[[drug]]$Drug                <- drugDefaults$Drug
    output[[drug]]$Concentration.Units <- drugDefaults$Concentration.Units
    output[[drug]]$Color               <- drugDefaults$Color
    output[[drug]]$endCe               <- drugDefaults$endCe
    # foldMetabolites() finishes the receiving drug's row, which needs the
    # drug's name and MEAC, so carry the resolved PK alongside the defaults.
    output[[drug]]$drug           <- PK$drug
    output[[drug]]$MEAC           <- PK$MEAC
    output[[drug]]$metaboliteName <- PK$metaboliteName
    output
  }

  for (drug in drugList)
  {
    drugDefaults <- getDrugDefaults(drug)
    PK <- getDrugPK(drug, weight, height, age, sex, drugDefaults, cyp2d6 = cyp2d6)
    # simCpCe() reads the emergence threshold off PK$endCe, which getDrugPK()
    # does not set: its own `emerge` field reads a drugDefaults$Emerge column
    # that does not exist, the CSV calls it endCe.  The Shiny path works
    # because recalculatePK() assigns it by hand.  Without this line every
    # time until threshold computed through this function is zero.
    PK$endCe <- drugDefaults$endCe
    currentDT <- dose[dose$Drug == drug,]
    X <- simCpCe(currentDT, events, PK, maximum, plotRecovery)

    output <- attach(output, drug, PK, drugDefaults)
    output[[drug]]$DT                  <- currentDT
    output[[drug]]$ET                  <- events

    output[[drug]]$results             <- X$results
    output[[drug]]$equiSpace           <- X$equiSpace
    output[[drug]]$max                 <- X$max

    output[[drug]]$wideOwn             <- X$wide
    output[[drug]]$wide                <- X$wide
    output[[drug]]$metaboliteSeries    <- X$metaboliteSeries
  }

  # A metabolite that was never given directly still needs a row to appear in.
  for (drug in drugList)
  {
    target <- output[[drug]]$metaboliteName
    if (is.null(target) || !nzchar(target)) next
    if (target %in% drugList) next
    targetDefaults <- getDrugDefaults(target)
    targetPK <- getDrugPK(target, weight, height, age, sex, targetDefaults,
                          cyp2d6 = cyp2d6)
    output <- attach(output, target, targetPK, targetDefaults)
  }

  return(foldMetabolites(output, maximum, plotRecovery))
}
