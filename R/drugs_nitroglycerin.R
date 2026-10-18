# -----------------------------------------------------------------------------
# Nitroglycerin, intravenous (Armstrong 1983): plasma glyceryl trinitrate in
# congestive cardiac failure
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  The
# source is in PubMed Central only as a scan (PMC1428029), which this
# environment could not read; every number below is from its PubMed
# abstract.  The scan is free and is on the list of papers to obtain.
#
# THE SOURCE
# ==========
# Armstrong PW, Watts DG, Moffat JA.  Steady state pharmacokinetic
# haemodynamic studies of intravenous nitroglycerin in congestive cardiac
# failure.  Br J Clin Pharmacol 1983;16:385-390
# (doi 10.1111/j.1365-2125.1983.tb02182.x).  Ten patients with congestive
# cardiac failure given 21 mcg/min.  The abstract reports that plasma
# nitroglycerin "rose to a peak of approximately 4 ng/ml at 30 min and was
# maintained", and that a one-compartment model fitted to the data gave
#
#     time constant of the approach to steady state   8.1 min (SE 1.9)
#     clearance                                       6.2 L/min (SD 2.7)
#
# A one-compartment model has two parameters, and these are they: the time
# constant is V / CL, so V = 6.2 x 8.1 = 50.2 L, a half-life of 5.6 min.  The
# check is the plateau: 21 mcg/min / 6.2 L/min = 3.4 ng/mL, against the
# "approximately 4 ng/ml" observed (a mean of individual clearances need not
# give the mean plateau exactly).
#
# WHY THIS SOURCE, AND HOW FAR TO TRUST IT
# ========================================
# Nitroglycerin's plasma kinetics are notoriously erratic.  McNiff 1981 (J
# Pharm Sci 70:1054, abstract), the source the US label quotes, studied eight
# healthy volunteers and found an apparent clearance of 0.3-1 L/min/kg (21 to
# 70 L/min in a 70 kg adult), a volume of about 3 L/kg and a half-life of
# about 3 minutes -- but reported them as ranges, not a fitted set, with
# concentrations in two subjects oscillating too much to analyse at all.
# Clearance is extrahepatic, falls with cardiac output, and depends on where
# the blood is drawn.  So a healthy volunteer's clearance can be three to ten
# times the one plotted here, and this model, from patients in heart failure
# whose cardiac output is low, is the one fitted, consistent set of numbers
# the literature offers, not a precise one.  Older infusion sets of PVC
# adsorbed nitroglycerin, which slows the early rise; whether this study used
# them is not in the abstract and is one of the questions the full text
# should answer.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed values with no size term, written for the source's adults and scaled
# to fat-free mass with the switch on; with it off they are the published
# values for everyone (legacyVolume = 1).
#
# NO EFFECT SITE, NO BAND
# =======================
# The haemodynamic responses (pulmonary artery, wedge and right atrial
# pressures) followed the concentration with time constants of 8.1 to 9.7
# minutes, the same as the drug's, so there is no measurable delay to model;
# but the abstract gives no concentration-effect coefficients, and no pressure
# is predicted here.  Tolerance develops within a day of continuous infusion.
# -----------------------------------------------------------------------------

# Armstrong 1983, abstract
NITROGLYCERIN_CL  <- 6.2     # L/min
NITROGLYCERIN_TAU <- 8.1     # min, the time constant V / CL

#' Nitroglycerin, intravenous (Armstrong 1983)
#'
#' One compartment from the clearance and time constant reported for ten
#' patients in congestive cardiac failure; see the header of
#' \code{R/drugs_nitroglycerin.R} for how far to trust it.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
nitroglycerin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = NITROGLYCERIN_CL * NITROGLYCERIN_TAU * size$volume,
    v2  = 1,                        # one compartment
    v3  = 1,
    cl1 = NITROGLYCERIN_CL * size$clearance,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Armstrong PW, Watts DG, Moffat JA, Br J Clin Pharmacol 1983;16:385-390 ",
    "(abstract).  One compartment in 10 patients with congestive cardiac ",
    "failure given 21 mcg/min: CL 6.2 L/min and a time constant of 8.1 min, ",
    "so V 50.2 L.  Healthy volunteers' clearance was 3-10 times higher ",
    "(McNiff 1981). https://doi.org/10.1111/j.1365-2125.1983.tb02182.x"
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
