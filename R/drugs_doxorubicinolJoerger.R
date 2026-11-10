# -----------------------------------------------------------------------------
# Doxorubicinol: the metabolite of doxorubicin (Joerger 2007)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of doxorubicinol.
#
# Drafted by Claude Code, 2026-11-06.  Metabolite only: it has no units of
# its own and is not dosed; R/drugs_doxorubicinJoerger.R forms it.  Read from
# Joerger 2007 (Clin Pharmacokinet 46:1051-1068), Table IV and Eq. 11, on the
# page images.
#
# THE MODEL
# =========
# One compartment, fitted jointly with doxorubicin in umol.  The fraction of
# doxorubicin converted (fm) is not identifiable, so the metabolite is
# parameterised as
#
#     V3/fm = 1580 L      CL_DOL/fm = 108 L/h x (CLcr/80)^0.54
#
# with CLcr the Cockcroft-Gault creatinine clearance in mL/min (the weight it
# used is not stated: total weight is used, or the pharmacokinetic weight
# with the switch on).  Its half-life is 10.1 h at CLcr 80, within 9% of the
# parent's terminal phase, so 24 h of data separate the two only weakly.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine through adultEquivalentCreatinine()
# (an adult equation), or the library's assumed normal creatinine when blank.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# V3/fm scales by the library's volume factor with the switch on and is fixed
# with it off; the clearance carries its own renal covariate.
# -----------------------------------------------------------------------------

# Joerger 2007, Table IV and Eq. 11
DOXORUBICINOL_JOERGER <- list(
  cl = 108,           # L/h at CLcr 80 mL/min (CL_DOL/fm)
  clcrReference = 80, # mL/min
  clcrExponent = 0.54,
  v = 1580            # L (V3/fm)
)

#' Doxorubicinol, the metabolite of doxorubicin (Joerger 2007)
#'
#' One compartment, a Cockcroft-Gault term on clearance; formed by
#' \code{doxorubicin()}.  See the header of \code{R/drugs_doxorubicinolJoerger.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinolJoerger <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  p <- DOXORUBICINOL_JOERGER
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  clcr <- creatinineClearanceCG(w, age, sex, scr)                     # mL/min

  default <- list(
    v1  = p$v * size$volume,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = p$cl * (clcr / p$clcrReference)^p$clcrExponent / 60,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Joerger M, Huitema ADR, Richel DJ, et al. Clin Pharmacokinet 2007;46:1051-1068 ",
    "(doxorubicinol formed from doxorubicin: one compartment, V/fm 1580 L, CL/fm 108 ",
    "L/h x (CLcr/80)^0.54, Cockcroft-Gault from the entered or an assumed normal ",
    "creatinine). https://doi.org/10.2165/00003088-200746120-00005"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
