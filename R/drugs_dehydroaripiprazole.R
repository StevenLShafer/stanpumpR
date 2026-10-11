# -----------------------------------------------------------------------------
# Dehydroaripiprazole: formed from aripiprazole, never dosed directly
# -----------------------------------------------------------------------------
# The metabolite half of Kim 2008 (see R/drugs_aripiprazole.R).  Its parameters
# are divided by the unidentified fraction metabolised, fm:
#
#     CLm/fm = 8.02 L/h,  Vm/fm = 587 L      (t1/2 51 h)
#
# They reproduce the metabolite formed from oral aripiprazole and nothing
# else, so this entry has no units of its own.  Both values are the brief's,
# not yet checked against Kim's Table 3.  Its plotted concentration does not
# enter Kim 2012's occupancy model, which is of parent only.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (plasma).  Body size: no weight covariate, scaled to
# fat-free mass like the parent (legacyVolume = 1).
# -----------------------------------------------------------------------------

DEHYDROARIPIPRAZOLE_V  <- 587    # L,   Vm/fm
DEHYDROARIPIPRAZOLE_CL <- 8.02   # L/h, CLm/fm

#' Dehydroaripiprazole pharmacokinetics (as aripiprazole's metabolite)
#'
#' Kim et al. (2008): one compartment, parameters scaled by the unidentified
#' fraction metabolised.  Not dosed directly.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dehydroaripiprazole <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = DEHYDROARIPIPRAZOLE_V * size$volume,
    v2 = 1,                                                  # one compartment
    v3 = 1,
    cl1 = DEHYDROARIPIPRAZOLE_CL / 60 * size$clearance,      # L/min
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
      "Kim JR et al., Br J Clin Pharmacol 2008;66:802-810. ",
      "https://doi.org/10.1111/j.1365-2125.2008.03223.x (metabolite parameters ",
      "scaled by fm; formed from aripiprazole only)"
    )
  )
}
