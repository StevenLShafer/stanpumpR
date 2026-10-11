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
#' @param maximum end of the simulation, in minutes.  As in \code{simCpCe()},
#'   each drug's result covers 0 to \code{maximum} only: doses at or after it
#'   are ignored.
#' @param plotRecovery should the "time until threshold" be calculated?  See
#'   \code{simCpCe()}.
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#' @param cyp2c19 CYP2C19 metaboliser phenotype, one of \code{CYP2C19_VALUES}.
#'   Only drugs whose model declares it are affected.
#' @param cyp2c9 CYP2C9 metaboliser phenotype, one of \code{CYP2C9_VALUES}.
#'   Only drugs whose model declares it (phenytoin) are affected.
#' @param osmolality baseline serum osmolality in mOsm/kg.  Only an osmotic
#'   agent (mannitol), which is reported as serum osmolality, is affected.
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for the patient's age and sex.  Only the renally cleared models are
#'   affected.
#' @param adjustToFFM scale each model's volumes and clearances to the patient's
#'   fat-free mass (the default) rather than total body weight; see
#'   `docs/weight-adjustment.md`.
#'
#' @returns a list of data frames with the output of the a single drug
#'   simulation.  A drug that forms an active metabolite adds its contribution
#'   to the metabolite drug's entry, which is created if that drug was not
#'   dosed directly.
#'
#' @export
simulateDrugsWithCovariates <- function (dose, events, weight, height, age, sex,
                                         maximum, plotRecovery,
                                         cyp2d6 = CYP2D6_DEFAULT,
                                         adjustToFFM = TRUE,
                                         osmolality = OSMOLALITY_DEFAULT,
                                         creatinine = NULL,
                                         cyp2c19 = CYP2C19_DEFAULT,
                                         cyp2c9 = CYP2C9_DEFAULT)
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
    PK <- getDrugPK(drug, weight, height, age, sex, drugDefaults, cyp2d6 = cyp2d6,
                    cyp2c19 = cyp2c19, cyp2c9 = cyp2c9,
                    osmolality = osmolality, creatinine = creatinine,
                    adjustToFFM = adjustToFFM)
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
    # foldMetabolites() solves the receiving drug's time until threshold from
    # these; see R/recoveryStates.R.
    output[[drug]]$recoveryStatesOwn        <- X$recoveryStates
    output[[drug]]$metaboliteRecoveryStates <- X$metaboliteRecoveryStates
    output[[drug]]$tci                 <- X$tci
    output[[drug]]$scheduled           <- X$scheduled
  }

  # A metabolite that was never given directly still needs a row to appear in.
  for (drug in drugList)
  {
    target <- output[[drug]]$metaboliteName
    if (is.null(target) || !nzchar(target)) next
    if (target %in% drugList) next
    targetDefaults <- getDrugDefaults(target)
    targetPK <- getDrugPK(target, weight, height, age, sex, targetDefaults,
                          cyp2d6 = cyp2d6, cyp2c19 = cyp2c19, cyp2c9 = cyp2c9,
                          osmolality = osmolality,
                          creatinine = creatinine, adjustToFFM = adjustToFFM)
    output <- attach(output, target, targetPK, targetDefaults)
  }

  return(foldMetabolites(output, maximum, plotRecovery))
}
