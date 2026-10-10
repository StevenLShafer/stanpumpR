# -----------------------------------------------------------------------------
# Quetiapine, immediate release, oral only
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief.  The PK numbers and the final
# covariate equations (Eq 6-7) were checked against the full text of Zheng
# 2024; the D2 fit against the full text of Nord 2011.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  The source reports L/h and 1/h.
#
# THE SOURCE
# ==========
# Zheng Z-Q et al., Front Psychiatry 2024;15:1497119.  99 Chinese inpatients
# with bipolar affective disorder (66 men), 17 to 69 years, 43 to 119 kg,
# sampled at steady state.  One compartment, first-order absorption:
#
#     CL/F = 76.1 L/h x (weight/70)^0.75
#     V/F  = 530 L    x (weight/70)
#     ka   = 1.46 /h, FIXED from earlier literature (not estimated)
#
# No other covariate survived (25 comedications and the laboratory values were
# tested).  V/F is imprecise: bootstrap 90% interval 210 to 2088 L.  The
# parameters are APPARENT (divided by an unmeasured F), so quetiapine is
# offered orally only and carries bioavailability_PO = 1.
#
# Immediate release only.  Extended-release quetiapine needs the transit
# absorption of Brogren and Nyberg 2010, whose published abstract does not give
# the transit number, Q/F or the volumes, so no XR profile is registered.
# Relabelling this ka as XR would be wrong.  See antipsychoticProfiles().
#
# Body size: the source carries its own allometric weight terms, so with the
# fat-free-mass switch off this reproduces them exactly (legacyVolume =
# weight/70, legacyClearance = (weight/70)^0.75); with it on, the library's
# fat-free-mass factors replace them (docs/weight-adjustment.md).
#
# EFFECT
# ======
# No effect site.  Nord 2011 related striatal D2 occupancy directly to PLASMA
# quetiapine in 11 healthy men: Emax 100% fixed, 50% occupancy at 1369 nmol/L
# (525 ng/mL).  That is a receptor biomarker, not a therapeutic target, and is
# carried in antipsychoticProfiles() (R/antipsychotics.R), not in the MEAC or the
# band, which are zero.  Norquetiapine is not modelled.
# -----------------------------------------------------------------------------

QUETIAPINE_CL <- 76.1    # L/h, apparent, at 70 kg
QUETIAPINE_V  <- 530     # L, apparent, at 70 kg
QUETIAPINE_KA <- 1.46    # 1/h, fixed in the source

#' Quetiapine pharmacokinetics (immediate release, oral)
#'
#' Zheng et al. (2024): one compartment, apparent oral parameters with
#' allometric weight scaling.  Plasma only; see \code{antipsychoticProfile()}
#' for the D2 occupancy fit.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
quetiapine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = weight / 70,
                        legacyClearance = (weight / 70)^0.75)

  default <- list(
    v1 = QUETIAPINE_V * size$volume,
    v2 = 1,                                        # one compartment
    v3 = 1,
    cl1 = QUETIAPINE_CL / 60 * size$clearance,     # L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = QUETIAPINE_KA / 60,                    # 1/min
    bioavailability_PO = 1,                        # apparent (/F) parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Zheng ZQ et al., Front Psychiatry 2024;15:1497119. ",
    "https://doi.org/10.3389/fpsyt.2024.1497119 (immediate release; apparent ",
    "oral parameters, bipolar inpatients); D2 occupancy: Nord M et al., ",
    "Int J Neuropsychopharmacol 2011;14:1357-1366. ",
    "https://doi.org/10.1017/S1461145711000514"
  )

  list(
    PK = PK,
    tPeak = 0,           # occupancy is a direct function of plasma
    MEAC = 0,
    typical = 0,         # no validated target: no band
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
