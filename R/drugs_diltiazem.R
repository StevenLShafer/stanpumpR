# -----------------------------------------------------------------------------
# Diltiazem, intravenous (Hoglund 1988): plasma diltiazem in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  The
# source's full text could not be reached; every number below is from its
# PubMed abstract, which is enough to fix the model and to check it.  The full
# text is on the list of papers to obtain.
#
# THE SOURCE
# ==========
# Hoglund P, Nilsson LG.  Physiological disposition of intravenously
# administered 14C-labeled diltiazem in healthy volunteers.  Ther Drug Monit
# 1988;10:401-409 (doi 10.1097/00007691-198804000-00006).  Ten healthy
# volunteers given 20 mg as an intravenous infusion over 20 minutes; a
# two-compartment model, and the abstract reports the means
#
#     alpha half-life                  0.40 h
#     beta half-life                   2.77 h
#     systemic clearance               992 mL/min
#     central volume                   119 L
#     volume at steady state           209 L
#
# The metabolites (deacetyl-, N-demethyl- and N-demethyl-deacetyldiltiazem)
# were too low for any parameter to be calculated, which is why a parent-only
# model is right after an intravenous dose; the label says the same.
#
# THE DERIVATION, AND ITS CHECK
# =============================
# Five numbers for four free parameters.  As for labetalol
# (R/drugs_labetalol.R), the model is built from clearance, the central
# volume and the two rates (twoCompartmentFromCentralVolume(), R/vasoactive.R)
# and the printed steady-state volume is the check: the derived V1 + V2 is
# 203.6 L against 209 printed, 2.6% apart.  Building from Vss instead returns
# a central volume of 129 L against the measured 119, 8.6% apart.  The
# result is V1 119 L, V2 84.6 L, CL 0.992 L/min and Q 1.222 L/min.
#
# DOSE BASIS
# ==========
# Not stated in the abstract.  Intravenous diltiazem is labelled as the
# hydrochloride, so a dose is entered as labelled; if the source dosed the
# base, every concentration here is 9% high (450.98 / 414.52).
#
# NONLINEARITY
# ============
# Diltiazem's clearance falls as the infusion rate rises: the US label gives
# 64 L/h at 4.8 mg/h falling to 48 L/h at 13.2 mg/h in healthy volunteers,
# and Dias 1992 found 42 L/h at 10 mg/h and 31 L/h at 15 mg/h in atrial
# fibrillation.  This model's 59.5 L/h is the low-dose value, so it suits a
# bolus and a short or low-rate infusion; a long infusion at 15 mg/h will run
# higher than plotted.  Dias 1992 (supplied, read in full) also quotes
# healthy volunteers' mean peaks after 24-hour infusions of 10 and 15 mg/h,
# 170 and 270 ng/mL (Weir 1989); this model gives 168 and 252, which the
# test checks.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are fixed values with no size term, written for the source's
# adults and scaled to fat-free mass with the switch on; with it off they are
# the published values for everyone (legacyVolume = 1).
#
# WHAT THIS MODEL IS NOT
# ======================
# Ten healthy volunteers, mean values, not a population model; the
# variability is large (central volume 119 +/- 77 L).  No effect site and no
# band: the source correlated the PQ interval with the logarithm of the
# concentration but the abstract gives no coefficients, and no heart rate or
# conduction effect is predicted here.
# -----------------------------------------------------------------------------

# Hoglund 1988, abstract
DILTIAZEM_CL  <- 0.992        # L/min
DILTIAZEM_V1  <- 119          # L
DILTIAZEM_VSS <- 209          # L, the check, not used to build
DILTIAZEM_THALF_ALPHA <- 0.40 * 60   # min
DILTIAZEM_THALF_BETA  <- 2.77 * 60   # min

#' Diltiazem, intravenous (Hoglund 1988)
#'
#' Two compartments from the means reported for ten healthy volunteers; see
#' the header of \code{R/drugs_diltiazem.R} for the derivation and its check.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
diltiazem <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  p <- twoCompartmentFromCentralVolume(DILTIAZEM_CL, DILTIAZEM_V1,
                                       DILTIAZEM_THALF_ALPHA,
                                       DILTIAZEM_THALF_BETA)
  default <- list(
    v1  = p$v1  * size$volume,
    v2  = p$v2  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl1 * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Hoglund P, Nilsson LG, Ther Drug Monit 1988;10:401-409 (abstract).  ",
    "Two compartments in 10 healthy volunteers given 20 mg IV over 20 min: ",
    "CL 992 mL/min, central volume 119 L, half-lives 0.40 and 2.77 h; V2 and ",
    "Q derived (Vss 203.6 L against 209 printed).  Clearance falls at higher ",
    "infusion rates. https://doi.org/10.1097/00007691-198804000-00006"
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
