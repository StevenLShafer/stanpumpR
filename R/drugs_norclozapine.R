# -----------------------------------------------------------------------------
# Norclozapine (N-desmethylclozapine), formed from clozapine (Li 2012)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-12.  Metabolite only: norclozapine is not
# given as a drug, so it has no units and no category; it is drawn when
# clozapine is given (R/drugs_clozapine.R, whose header gives the source,
# Li 2012, Table 2): CLM 32.7 L/h and VM 624 L in a female nonsmoker, CLM
# x 1.242 in men and x 1.543 in smokers.  Like clozapine's, the parameters are
# apparent: the formation fraction (0.66) was fixed, so only the ratio of
# formation to the metabolite's clearance was estimated.  Written for the
# reference man and scaled by fat-free mass as clozapine is; with the switch
# off the source's sex term is applied.  The nonsmoker is the reference.
# -----------------------------------------------------------------------------

#' Norclozapine (Li 2012)
#'
#' One compartment; formed from clozapine.  See the header of
#' \code{R/drugs_norclozapine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
norclozapine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1,
                        legacyClearance = if (sex == SEX_FEMALE) 1 / NORCLOZAPINE_MALE_CL else 1)
  default <- list(
    v1 = NORCLOZAPINE_V * size$volume, v2 = 1, v3 = 1,
    cl1 = NORCLOZAPINE_CL_FEMALE * NORCLOZAPINE_MALE_CL / 60 * size$clearance,
    cl2 = 0, cl3 = 0
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
      "Li LJ et al., Acta Pharmacol Sin 2012;33:1409-1416: one compartment, ",
      "CLM 32.7 L/h (female nonsmoker; x 1.242 in men), VM 624 L, formed from ",
      "clozapine. https://doi.org/10.1038/aps.2012.71")
  )
}
