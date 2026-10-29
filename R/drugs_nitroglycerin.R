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
# The clearance is used; the time constant is not (see THE VOLUME, below).  The
# 6.2 L/min is the paper's clearance from each patient's average concentration
# at 30, 60 and 120 min; the fitted steady state, 3.65 ng/mL, gives 5.75 L/min
# instead.  The plateau the model predicts at the study's rate, 3.4 ng/mL, lies
# between the fitted 3.65 and Table 1's measured 3.0-4.1 ng/mL.
#
# The full text settles two questions the abstract left open.  The infusion
# ran through Teflon tubing, which does not adsorb nitroglycerin, and the
# samples were ARTERIAL (radial); the same group found a 60% arteriovenous
# extraction, so venous concentrations are far lower.  It also shows that the
# slow rise is not the drug's: after an infusion stops the authors measured
# half-lives of 2.8 and 1.1 minutes, "consistent with our previously reported
# value of 1.9 min" (Armstrong PW, Armstrong JA, Marks GS, Circulation
# 1980;62:160-166, PMID 6769608), and they had expected a plateau within 10
# to 15 minutes, as their haemodynamics had reached one.
#
# THE VOLUME: FROM THE 1.9-MINUTE HALF-LIFE, NOT THE 8.1-MINUTE RISE
# ==================================================================
# In any linear model the fall after a long infusion mirrors the rise, so an
# 8.1-minute time constant (a 5.6-minute half-life) on the way up and a
# 1-3 minute half-life on the way down cannot both be the drug's.  Two later
# studies with specific assays side with the fall:
#
#   Sun JX et al., J Clin Pharmacol 1995;35:390-397 (PMID 7650229): 18
#   healthy men given 15N-labelled nitroglycerin at 0.25 mg/h IV for 14 h.
#   Figure 1: plasma is at its plateau (about 400-500 pg/mL) at the first
#   sample, 15 minutes, and in the 5 minutes after the infusion stops falls
#   from about 370 pg/mL (the mean of the three arms) to about 20-70 (70 in
#   the highest arm), a half-life near 2 minutes.  Sun prints no half-life
#   or clearance; these are read from the figure.
#
#   Kim CO et al., Medicine 2018;97:e9829 (PMID 29489681): 9 healthy men
#   given 20 mcg/min for 30 min through 100 cm sets of three materials,
#   LC-MS/MS.  Plasma "reached steady state after approximately 5 minutes",
#   and most samples 3, 6 and 10 minutes after the stop were below 0.1
#   ng/mL.  Through polyurethane or PVC the peak was 0.21 and 0.33 of that
#   through polyolefin: the administration set changes what reaches the vein.
#
# So the slow rise in Armstrong 1983 belongs to the delivery, not to the
# drug (its Teflon tubing did not adsorb, but a low-flow delivery path can
# still lag), and the volume is taken from the half-life after a stop:
# V = CL x 1.9 / ln 2 = 6.2 x 2.741 = 17.0 L.  With it the plateau is the
# same (it depends on CL alone), 90% of it is reached in 6.3 minutes, and 5
# minutes after a stop 16% remains, matching Sun's figure.  Until 2026-10-27
# the volume was 50.2 L, from the rise, which drew the offset too slowly.
#
# A BOLUS IS BEYOND THE MODEL, SO NONE IS OFFERED: the drug's units are
# infusion rates only (mcg/min, mcg/kg/min; the bolus unit was withdrawn
# 2026-10-24 on Dr. Shafer's decision after the adversarial review).  Two studies of 0.25 and 0.5 mg IV boluses
# for uterine relaxation at caesarean delivery measured venous plasma 1 and 5
# minutes after the dose: David M et al., Obstet Gynecol 1998;91:119-124 (doi
# 10.1016/s0029-7844(97)00594-2; medians 38.1 and 80.3 ng/mL at 1 min, a
# third of that at 5 min, a half-life of 2-2.5 min) and David M et al., Am J
# Obstet Gynecol 2000;182:955-961 (0.5 mg: 80 at 1 min, 3.2 at 5 min).
# After 0.25 mg this model gives 10.2 ng/mL at 1 min, a quarter of the
# measured, and falls 4.3-fold by 5 min (with the 50.2 L volume used until
# 2026-10-27, 4.4 ng/mL and 1.6-fold).  The 1-minute samples came from the
# arm of the injection and are probably inflated, and the two papers
# disagree on the fall, so they give no model; but the drug's early volume
# after a bolus is smaller than drawn, and one compartment cannot show it.
#
# WHY THIS SOURCE, AND HOW FAR TO TRUST IT
# ========================================
# Nitroglycerin's plasma kinetics are notoriously erratic.  McNiff 1981 (J
# Pharm Sci 70:1054, abstract), the source the US label quotes, studied eight
# healthy volunteers and found an apparent clearance of 0.3-1 L/min/kg (21 to
# 70 L/min in a 70 kg adult), a volume of about 3 L/kg and a half-life of
# about 3 minutes -- but reported them as ranges, not a fitted set, with
# concentrations in two subjects oscillating too much to analyse at all.
# Clearance is extrahepatic, fell with cardiac output in rats (Fung 1987;
# not shown in people), and depends on where
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
# pressures) followed the measured concentration with time constants of 8.1
# to 9.7 minutes, the same as its rise, so on the same delivery there is no
# measurable delay to model; the paper gives no concentration-effect
# coefficients, and no pressure is predicted here.  Tolerance develops within
# a day of continuous infusion.
#
# NOONAN 1985: THE HALF-LIFE AGREES; CLEARANCE DEPENDS ON DOSE AND HISTORY
# =========================================================================
# Noonan PK, Williams RL, Benet LZ, J Pharmacokinet Biopharm 1985;13:143-157
# (PMID 3932639, read in full): six healthy volunteers, 10, 20, 40 and again
# 10 mcg/min for 40 min each through a set adsorbing < 1% (rates checked by
# HPLC), venous plasma.  Half-life after the last stop 1.4-3.2 min, mean 2.3
# (Table II), which agrees with the 1.9 min used.  Clearance (Table III)
# fell from 31 L/min at the first 10 mcg/min to about 11 L/min at 40 and at
# the second 10, where it stayed: dose- and time-dependent, which one linear
# compartment cannot show.  Armstrong's patients, already on the drug, are
# nearer the later, lower value.
#
# THE INFUSION SET: BENCH LOSSES
# ==============================
# Curry SH, Aburawi SM, Biopharm Drug Dispos 1985;6:235-280 (PMID 3929851,
# read in full) reviews them: up to about 85% lost through plastic sets,
# flow-dependent (Cossum); about 10% through a glass syringe and 80 cm of
# high-density polyethylene; filters up to 55%, bags and giving sets > 50%,
# up to 80% together (Baaske); none from glass.
#
# SAMPLING SITE
# =============
# The curve is ARTERIAL plasma.  Armstrong PW, Moffat JA, Marks GS,
# Circulation 1982;66:1273-1276 (PMID 6814785, abstract; also quoted by Chiou,
# Clin Pharmacokinet 1989;17:175-199, PMID 2680213): during infusion in 20
# patients with heart failure, 25.1 ng/mL radial arterial, 29.8 pulmonary
# arterial, 7.3 peripheral venous; 17% extraction across the lung, 61% across
# the arm.  A peripheral venous sample reads about a third of the curve.
#
# CURRY 1993: CORROBORATES THE FAST KINETICS, NOT USED
# ====================================================
# Curry SH et al., Biopharm Drug Dispos 1993;14:107-118 (PMID 8453021, read
# in full): 0.1 mg IV in six men with coronary disease at catheterisation,
# femoral venous plasma for 15 min, two compartments (Table 2: alpha
# half-life 0.41 min, beta 7.37 min, k12 0.30, k21 0.13, k10 1.58 /min).
# Those rate constants put an infusion at 84% of its plateau in 2 min and at
# 10% of it 5 min after a stop, as fast as this model's 1.9 min or faster.
# The printed V1 (2.0 L) and CL (2.08 L/min) are means of individual values
# that disagree with each other (V1 x k10 = 3.2 L/min) and with the dose over
# the mean AUC (100 mcg / 224.6 ng.min/mL = 0.45 L/min), and the injection
# site is not stated, so no number is taken from it.
#
# THE ADMINISTRATION SET CHANGES THE DOSE
# =======================================
# The curve assumes the whole dose reaches the vein.  It often does not:
# nitroglycerin is sorbed by PVC and polyurethane tubing.  Kim 2018 (crossover
# through three 100 cm sets; 8 men analysed): peak concentration through PVC
# 0.33, through polyurethane 0.21, of that through polyolefin; AUC 0.31 and
# 0.14 by the paper's text, 0.26 and 0.09 by its Table 3 (unresolved); more
# with longer sets and lower flows in other drugs' studies.  So through such a
# set the patient receives, and the plasma holds, a third to a seventh of what
# is drawn (a quarter to an eleventh by Table 3).  The help page
# leads with this, and the reference string (the References panel) says it.
# -----------------------------------------------------------------------------

# Armstrong 1983
NITROGLYCERIN_CL  <- 6.2     # L/min
# Armstrong 1980, quoted by Armstrong 1983: the half-life after a stop
NITROGLYCERIN_HALF_LIFE <- 1.9   # min
# Armstrong 1983's time constant of the rise, not used (see the header)
NITROGLYCERIN_TAU_RISE  <- 8.1   # min

#' Nitroglycerin, intravenous (Armstrong 1983)
#'
#' One compartment from the clearance reported for ten patients in
#' congestive cardiac failure and the 1.9-minute half-life after an infusion
#' stops; see the header of
#' \code{R/drugs_nitroglycerin.R} for how far to trust it.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
nitroglycerin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = NITROGLYCERIN_CL * NITROGLYCERIN_HALF_LIFE / log(2) * size$volume,
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
    "failure given 21 mcg/min: CL 6.2 L/min; arterial plasma.  V 17.0 L from the ",
    "1.9-min half-life after a stop (Armstrong 1980), consistent with the fast fall ",
    "Sun 1995 and Kim 2018 saw; the 8.1-min rise is the delivery's.  Healthy volunteers' clearance was ",
    "3-10 times higher (McNiff 1981).  THE INFUSION SET CHANGES THE DOSE: the curve ",
    "assumes the whole dose reaches the vein; through PVC or polyurethane sets Kim 2018 ",
    "measured a third to a seventh of the exposure through polyolefin. ",
    "https://doi.org/10.1111/j.1365-2125.1983.tb02182.x"
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
