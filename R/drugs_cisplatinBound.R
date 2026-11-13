# -----------------------------------------------------------------------------
# Protein-bound plasma platinum after cisplatin (Urien 2004)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= ug/L) of platinum.
#
# Drafted by Claude Code, 2026-11-06.  Metabolite only: it has no units of
# its own and is not dosed; R/drugs_cisplatin.R forms it, and its header
# gives the source and the checks.  Urien and Lokiec modelled the platinum
# bound irreversibly to plasma proteins as a metabolite formed from the
# unbound central compartment with its own first-order elimination; total
# plasma platinum is the unbound plus this.  Vm is not identifiable, so
#
#     fm/Vm = 0.017 /L x (dose per m2 / 25)^-0.48 x (PROT/70)^1.33
#                      x (BSA/1.7)^-0.83 x (CLCr/81)^-0.36
#     CLm0/Vm = 0.014 /h        (half-life 49.5 h; the paper's 50)
#
# carried here as one compartment of volume 1/(fm/Vm), 58.8 L at the
# references, and clearance (CLm0/Vm)/(fm/Vm).  The dose per m2 is held at
# 25 mg/m2 and the serum protein at 70 g/L (see R/drugs_cisplatin.R).  Only
# models with a pregnancy registry row read the pregnancy flag, so this
# metabolite does not: in pregnancy its creatinine clearance is not raised,
# and fm/Vm is (1.5)^0.36, 16%, higher than a consistent treatment would give.
# -----------------------------------------------------------------------------

#' Protein-bound platinum after cisplatin (Urien 2004)
#'
#' One compartment, formed from unbound platinum by \code{cisplatin()}.  See
#' the headers of \code{R/drugs_cisplatin.R} and
#' \code{R/drugs_cisplatinBound.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cisplatinBound <- function(weight, height, age, sex, adjustToFFM = TRUE,
                           creatinine = NULL)
{
  p <- CISPLATIN_URIEN
  x <- cisplatinCovariates(weight, height, age, sex, adjustToFFM,
                           creatinine, pregnant = FALSE)
  fmOverVm <- p$fmOverVm * (x$bsa / p$bsaReference)^(-p$clBSAExponent) *
    (x$clcr / p$clcrReference)^(-p$clCrExponent)     # dose 25 mg/m2, protein 70 g/L
  v <- 1 / fmOverVm                                                   # L

  default <- list(
    v1  = v,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = p$kBound * v / 60,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Urien S, Lokiec F. Br J Clin Pharmacol 2004;57:756-763 (platinum bound ",
    "irreversibly to plasma proteins, formed from unbound platinum: fm/Vm 0.017 /L x ",
    "(BSA/1.7)^-0.83 x (CLCr/81)^-0.36, dose per m2 held at 25 mg/m2 and protein at ",
    "70 g/L; CLm0/Vm 0.014 /h). https://doi.org/10.1111/j.1365-2125.2004.02082.x"
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
