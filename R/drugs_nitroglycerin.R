# -----------------------------------------------------------------------------
# Nitroglycerin, intravenous (Armstrong 1983): plasma glyceryl trinitrate in
# congestive cardiac failure
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  The
# full text, which he supplied, was read after the model was first built from
# the abstract; every number below is in it (Results, Table 1).
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
# 6.2 L/min is the paper's clearance from each patient's average concentration
# at 30, 60 and 120 min; the fitted steady state, 3.65 ng/mL, gives 5.75 L/min
# instead.  The plateau the model predicts at the study's rate, 3.4 ng/mL, lies
# between the fitted 3.65 and Table 1's measured 3.0-4.1 ng/mL.
#
# The full text settles two questions the abstract left open.  The infusion
# ran through Teflon tubing, which does not adsorb nitroglycerin (so the slow
# rise is not adsorption), and the samples were ARTERIAL (radial); the same
# group found a 60% arteriovenous extraction, so venous concentrations are
# far lower.  It also shows the limit of one compartment: after an infusion
# stops the authors measured half-lives of 1.1 to 2.8 minutes (1.9 in their
# earlier work), faster than the 5.6 minutes implied by the 8.1-minute rise.
# So the model reaches its plateau at the measured pace but falls after a
# stop too slowly; the true offset is a few minutes.
#
# A BOLUS IS BEYOND THE MODEL.  Two studies of 0.25 and 0.5 mg IV boluses
# for uterine relaxation at caesarean delivery measured venous plasma 1 and 5
# minutes after the dose: David M et al., Obstet Gynecol 1998;91:119-124 (doi
# 10.1016/s0029-7844(97)00594-2; medians 38.1 and 80.3 ng/mL at 1 min, a
# third of that at 5 min, a half-life of 2-2.5 min) and David M et al., Am J
# Obstet Gynecol 2000;182:955-961 (0.5 mg: 80 at 1 min, 3.2 at 5 min).
# After 0.25 mg this model gives 4.4 ng/mL at 1 min, a ninth of the
# measured, and falls only 1.6-fold by 5 min.  The 1-minute samples came
# from the arm of the injection and are probably inflated, and the two
# papers disagree on the fall, so they give no model; but the drug's early
# volume after a bolus is far smaller, and its fall far faster, than drawn.
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
# the literature offers, not a precise one.  The source itself saw the
# concentration at 21 mcg/min range from 2 to 6 ng/mL between patients.
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
    "One compartment in 10 patients with congestive cardiac ",
    "failure given 21 mcg/min: CL 6.2 L/min and a time constant of 8.1 min, ",
    "so V 50.2 L; arterial plasma.  Healthy volunteers' clearance was 3-10 times higher ",
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
