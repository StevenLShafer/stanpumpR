# -----------------------------------------------------------------------------
# Diclofenac: three-compartment intravenous and oral model (Standing 2011)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma diclofenac.
#
# DISPOSITION
# ===========
# Standing et al. fitted a three-compartment NONMEM model to 375 samples from
# 111 children aged 1 to 14 years given diclofenac intravenously, as an oral
# suspension and as suppositories, with published adult dispersible-tablet
# and suspension data added.  Final parameters, per 70 kg:
#
#     CL 16.5 L/h    V1 3.68 L
#     Q2 1.75 L/h    V2 7.48 L
#     Q3 7.21 L/h    V3 3.79 L
#
# clearances scaled by (W / 70)^0.75 and volumes by W / 70.  The intravenous
# evidence is 65 concentrations from 10 children; the 70 kg intravenous
# disposition is an allometric extrapolation, not an adult intravenous
# validation.  Terminal half-life at the reference man about 1.9 h.
#
# ORAL ROUTE
# ==========
# Each oral formulation divides its bioavailable dose between two lagged
# first-order absorption paths.  The dispersible tablet is the one offered:
#
#     F 0.35, applied once to the whole dose
#     path 1: 26% of the absorbed dose, lag 0.06 h, ka 2.95 /h
#     path 2: 74%, lag 0.75 h, ka 2.23 /h
#
# The two paths are the first and second oral depots of the engine (ka_PO and
# ka_PO2; getDrugPK() splits F between them by fraction_PO2), so the curve is
# exactly the published one.  The suspension (F 0.36; 31% at lag 0.12 h,
# ka 1.65 /h; 69% at lag 0.50 h, ka 2.04 /h) is not offered.  Neither set of
# estimates applies to the enteric-coated tablet, the commonest adult
# formulation, whose absorption is delayed and erratic; nor to suppositories
# (F 0.63 in the source).
#
# EFFECT SITE
# ===========
# None: tPeak = 0, and the plotted concentration is plasma.  No analgesic
# concentration-effect relationship suitable for a typical range was found,
# so no band is drawn and there is no recovery threshold.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The library's fat-free-mass scaling on the 70 kg parameters; with the switch
# off, the published allometry on total weight (legacyVolume = W / 70,
# legacyClearance = (W / 70)^0.75).
#
# NOT MODELLED
# ============
# CYP2C9 genotype (no fitted coefficient), hepatic impairment, maturation in
# infants under a year (the source's youngest child was 1 year old).  No
# CYP2D6 adjustment: the model fitted none.
#
# References
# ----------
# Standing JF et al., Paediatr Anaesth 2011;21:316-324.
#   https://doi.org/10.1111/j.1460-9592.2010.03509.x
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Diclofenac pharmacokinetics
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
diclofenac <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling: published allometry on total weight when the switch is off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  default <- list(
    v1  = 3.68 * size$volume,
    v2  = 7.48 * size$volume,
    v3  = 3.79 * size$volume,
    cl1 = 16.5 / 60 * size$clearance,
    cl2 = 1.75 / 60 * size$clearance,
    cl3 = 7.21 / 60 * size$clearance,
    # Dispersible tablet: F once, split between two lagged depots
    ka_PO              = 2.95 / 60,     # 1/min, path 1
    tlag_PO            = 0.06 * 60,     # min
    bioavailability_PO = 0.35,
    ka_PO2             = 2.23 / 60,     # 1/min, path 2
    tlag_PO2           = 0.75 * 60,     # min
    fraction_PO2       = 0.74
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Standing JF et al., Paediatr Anaesth 2011;21:316-324 (three-compartment ",
    "intravenous and oral model; dispersible tablet, two lagged absorption ",
    "paths). https://doi.org/10.1111/j.1460-9592.2010.03509.x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    # No established range: no band (see the header)
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
