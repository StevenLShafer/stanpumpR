# -----------------------------------------------------------------------------
# Salicylate: the metabolite of aspirin (Koh 2025)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of salicylic acid.
#
# Salicylic acid (SA) is never dosed in the library: it is formed from aspirin
# (R/drugs_aspirin.R), which carries the formation, the first-pass fraction
# and the molecular-weight ratio, and foldMetabolites() puts it on this row.
# Two-compartment disposition from Koh et al., Table 1:
#
#     CLm/F 2.76 x (W/Wmed)^1.42 L/h    V4/F 7.5 L
#     Q/F   0.08 L/h (fixed)            V5/F 1.98 L (fixed)
#
# Q, V5 and the pre-systemic k24 were fixed from an earlier study to help
# the fit converge.  Apparent parameters, on the same unidentified
# bioavailability as aspirin's.  Wmed is taken as 68.35 kg (see the aspirin
# header).
#
# LINEAR AT LOW DOSE ONLY.  Salicylate is eliminated by saturable pathways
# (glycine and glucuronide conjugation); at analgesic doses its clearance
# falls and its half-life lengthens from about 2-3 h to 15-30 h.  Koh fitted
# 100 mg of aspirin, so this model underpredicts salicylate after analgesic
# or anti-inflammatory doses, increasingly with dose.
#
# No effect site (tPeak = 0) and no band.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CLm/F carries its own weight covariate, at the pharmacokinetic weight with
# the switch on and total weight with it off.  The volumes and Q/F take the
# library's factors (legacyVolume = 1).
#
# References
# ----------
# Koh J et al., Drug Des Devel Ther 2025;19:7853-7863.
#   https://doi.org/10.2147/DDDT.S533428
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Salicylate pharmacokinetics (formed from aspirin)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
salicylate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  wt   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 7.5  * size$volume,
    v2  = 1.98 * size$volume,
    v3  = 1,                                                   # two compartments
    cl1 = 2.76 * (wt / ASPIRIN_WT_MEDIAN)^1.42 / 60,           # own covariate
    cl2 = 0.08 / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Koh J et al., Drug Des Devel Ther 2025;19:7853-7863 (salicylic acid ",
    "formed from low-dose enteric-coated aspirin; linear, so low dose only). ",
    "https://doi.org/10.2147/DDDT.S533428"
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
