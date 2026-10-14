# -----------------------------------------------------------------------------
# 9-Hydroxyrisperidone: formed from risperidone, never dosed directly
# -----------------------------------------------------------------------------
# The metabolite half of Storset 2024 (see R/drugs_risperidone.R).  Its
# parameters are divided by F x fmet, which the source could not identify:
#
#     Vm/(F fmet)  = 96 L
#     CLm/(F fmet) = 8.0 L/h x [1 - 0.013 x max(age - 39, 0)]
#
# These reproduce the metabolite concentrations formed from oral risperidone,
# and only those.  9-Hydroxyrisperidone is paliperidone, but paliperidone
# given as itself has its own absolute disposition, which these are not; so
# this entry has no units of its own and appears only as risperidone's
# metabolite.  The V and CL values are the brief's, not yet checked against
# Storset's Table 2; the age term was checked against the text.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (serum).  Body size: no weight covariate, scaled to
# fat-free mass like the parent (legacyVolume = 1).
# -----------------------------------------------------------------------------

HYDROXYRISPERIDONE_V  <- 96    # L,   Vm/(F fmet)
HYDROXYRISPERIDONE_CL <- 8.0   # L/h, CLm/(F fmet), below 40 years

#' 9-Hydroxyrisperidone pharmacokinetics (as risperidone's metabolite)
#'
#' Storset et al. (2024): one compartment, parameters scaled by the
#' unidentified F x fmet.  Not dosed directly.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
hydroxyrisperidone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  cl <- HYDROXYRISPERIDONE_CL * risperidoneAgeFactor(age, 0.013, 39)   # L/h

  default <- list(
    v1 = HYDROXYRISPERIDONE_V * size$volume,
    v2 = 1,                                        # one compartment
    v3 = 1,
    cl1 = cl / 60 * size$clearance,                # L/min
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Storset E et al., Eur J Clin Pharmacol 2024;80:1531-1541. ",
      "https://doi.org/10.1007/s00228-024-03721-6 (metabolite parameters ",
      "scaled by F x fmet; formed from risperidone only)"
    )
  )
}
