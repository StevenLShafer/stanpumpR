# -----------------------------------------------------------------------------
# Unfractionated heparin on cardiopulmonary bypass: anti-factor Xa activity
# driving the activated clotting time (Delavenne 2017)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# international units (IU).  The plotted curve is the ACTIVATED CLOTTING TIME
# (ACT) in seconds, computed from the central anti-factor Xa activity by a
# direct Emax link (R/act.R).  Concentration.Units "ACT".
#
# This is a SECOND, distinct heparin model from the library's `heparin` (Jia
# 2015, anti-IIa activity during bypass).  This one is the anti-factor Xa /
# ACT pharmacokinetic-pharmacodynamic model, and its plotted output is the ACT,
# not an activity: it is the heparin analogue of warfarin, whose plotted output
# is the INR rather than a concentration.  The two heparins must never be
# combined (anti-IIa and anti-Xa are different activities).
#
# SOURCE
# ======
# Delavenne X, Ollier E, Chollet S, Sandri F, Lanoiselee J, Hodin S, Montmartin
# A, Fuzellier J-F, Mismetti P, Gergele L.  Pharmacokinetic/pharmacodynamic
# model for unfractionated heparin dosing during cardiopulmonary bypass.  Br J
# Anaesth 2017;118:705-712.  Fifty consecutive adults at cardiac surgery with
# bypass, 300 IU/kg bolus then 5000-IU boluses to keep ACT < 400 s; 361 anti-Xa
# activities and 341 ACT values; MONOLIX, joint PK/PD.  Table 2, read from the
# full text on 2026-10-20.
#
# STRUCTURE
# =========
# A two-compartment model with linear elimination describes the anti-factor Xa
# activity (Table 2):
#
#     Vc = 3.1 L     Vp = 2.23 L     Q = 4.67 L/h     CL = 0.841 L/h
#
# Only body weight was a covariate: on Vc (regression coefficient 1.02,
# estimated and SET at 1, so Vc scales directly with weight) and on CL (0.767),
# both normalised to a 70 kg reference.  No age, sex, GFR or ASA effect was
# retained.
#
# The ACT is a direct Emax function of the central anti-Xa activity (Table 2),
# with NO effect compartment (the link is instantaneous):
#
#     ACT = ACT0 + Emax x C / (C50 + C),   ACT0 = 116 s, Emax = 720 s,
#                                          C50 = 3.49 IU/mL
#
# At the 300 IU/kg bolus a 70 kg patient reaches about 6.8 IU/mL, an ACT near
# 590 s, which the paper's Figures 2 and 3 show; the authors found a bolus +
# infusion (55 IU/kg/h) held the ACT above 400 s with less variability than
# repeated boluses.
#
# WHAT THIS MODEL IS
# ==================
# Linear anti-Xa kinetics fitted in the setting of bypass: hemodilution, high
# doses, a short course.  It is research and teaching, NOT a validated clinical
# ACT, a heparin dose, a protamine-reversal plan or a safe anticoagulation
# target: the ACT measured by different devices is not directly comparable (the
# authors fitted a Hemochron Junior2 ACT), the model is small (50 patients), and
# clinical use and enforcement of its limits are outside it.  See
# inst/help/drugs/heparinCPB.md.  No MEAC and no time until threshold.  The band
# is 480 to 600 s, typical 480: an ACT above 480 s is the EACTS/EACTAIC/EBCP
# 2024 threshold on bypass (section 9.3.1), and the upper edge is Steven L.
# Shafer's choice (2026-10-27, kept 2026-10-28): 600 s was the constant
# result of his search, and only the lower limit matters, so a narrower range
# would add precision titration does not need
# (inst/extdata/therapeuticRanges.csv).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Vc and CL carry the model's own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight) and at total body
# weight with it off.  Vp and Q have no size term in the source and take the
# library's fat-free-mass factors with the switch on; with it off they are the
# published values (legacyVolume = 1).  Heparin is dosed per kilogram (IU/kg),
# and that scaling of the dose is independent of the model's.
#
# (Claude Code, 2026-10-20, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Unfractionated heparin PK/PD on cardiopulmonary bypass (ACT, Delavenne 2017)
#'
#' A two-compartment anti-factor Xa model whose plotted output is the activated
#' clotting time it produces; see the header of \code{R/drugs_heparinCPB.R}.  A
#' second, distinct heparin model from the library's \code{heparin} (Jia 2015,
#' anti-IIa activity).
#'
#' @param weight weight in kg (the model's covariate on Vc and CL)
#' @param height height in cm; no published covariate, used by
#'   \code{pkSizeFactors()} for the fat-free mass when \code{adjustToFFM} is TRUE
#' @param age age in years (no effect)
#' @param sex sex as a string (no effect; feeds the fat-free mass)
#' @param adjustToFFM when \code{TRUE}, evaluate the weight terms at the
#'   pharmacokinetic weight and scale the size-free parameters to fat-free mass
#' @returns a list in the shape \code{getDrugPK()} expects, with an \code{act}
#'   block (the Emax link from anti-Xa activity to the ACT)
#' @export
heparinCPB <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 3.1 * (pkW / 70),                      # own weight covariate (fixed 1)
    v2  = 2.23 * size$volume,
    v3  = 1,                                      # two compartments
    cl1 = 0.841 * (pkW / 70)^0.767 / 60,         # own weight covariate
    cl2 = 4.67 / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    act = list(act0 = 116, emax = 720, c50 = 3.49),   # ACT = ACT0 + Emax C/(C50+C)
    tPeak = 0,
    MEAC = 0,
    typical = 480,      # ACT band, s (header)
    upperTypical = 600,
    lowerTypical = 480,
    reference = paste0(
      "Delavenne X et al., Br J Anaesth 2017;118:705-712 (two-compartment ",
      "anti-factor Xa model with a direct Emax link to the activated clotting ",
      "time during cardiopulmonary bypass; body weight on Vc and clearance, no ",
      "other covariate; plotted as the ACT in seconds). ",
      "https://doi.org/10.1093/bja/aex044"
    )
  )
}
