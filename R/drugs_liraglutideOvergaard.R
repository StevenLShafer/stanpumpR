# -----------------------------------------------------------------------------
# Liraglutide (Overgaard 2016): one compartment, subcutaneous once daily, with
# sex and weight on clearance (Saxenda's population)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mg, concentrations in ng/mL.  Concentration only: no weight, glucose or
# HbA1c effect is plotted.  Drafted by Claude Code, 2026-11-19, at the
# request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv (liraglutide_sc_saxenda).
#
# SOURCE
# ======
# Overgaard RV, Petri KC, Jacobsen LV, Jensen CB.  Liraglutide 3.0 mg for
# weight management: a population pharmacokinetic analysis.  Clin
# Pharmacokinet 2016;55:1413-1422 (PMID 27193270, read in full with its
# supplement).  2,931 adults with overweight or obesity in two phase 3
# trials (SCALE Obesity and Prediabetes, and SCALE Diabetes, 20% with type 2
# diabetes), 1.8 and 3.0 mg once daily; mean weight 106 kg, 72% women.  One
# compartment, first-order absorption, covariates on CL/F only (Section 2.2;
# Online Resource Table S1, the full model, which is the published one):
#
#     CL/F = 0.86 L/h x (WT / 100 kg)^0.68 x exp(0.27 [man])
#            x exp(-0.10 [age 70 or over]) x exp(0.18 [type 2 diabetes])
#            x race, ethnicity and dose-level factors (not applied)
#     V/F  = 24.6 L
#     ka   = 0.0806 /h, fixed
#
# The reference subject is a woman under 70 weighing 100 kg, White,
# non-Hispanic, without diabetes, taking 3.0 mg.  The covariates are
# exponential: the forest plot's ratios of exposure (Fig. 2: men 0.76, age 70
# or over 1.10, diabetes 0.84) are exp(-coefficient).  ka is the text's 0.0806
# /h, from a clinical pharmacology study, fixed; Table S1 prints it rounded as
# 0.09.
#
# WHY A THIRD LIRAGLUTIDE.  liraglutide (Roge 2015, 29 adults with type 2
# diabetes) and liraglutideWatson (Watson 2010) scale clearance with weight
# alone.  Two analyses find a sex effect: Overgaard's men clear 31% more
# (exposure 24% lower) at the same weight, and Carlsson Petri 2015 (PMID
# 25603819, the paediatric analysis, Online Resource Table 1) finds men x
# 1.44 on CL/F in adults and adolescents with type 2 diabetes.  This model is
# the largest (2,931 subjects) and carries the sex effect.  Shafer, 2026-11-19:
# added beside the others; which is the default liraglutide is a later
# decision.
#
# NOT APPLIED: race and ethnicity (no field: Black x 0.92, Hispanic x 1.08 on
# CL/F), the dose level (1.8 against 3.0 mg, x 1.02, its interval spanning 1)
# and type 2 diabetes (no field): the curve is the reference subject's, the
# Saxenda population without diabetes; a patient with type 2 diabetes clears
# 20% more (exp(0.18)).  Renal function (CKD-EPI, post hoc) and injection site
# changed exposure by less than 5% and are not in the model.
#
# BODY SIZE: the source's own weight term on CL/F, at the pharmacokinetic
# weight with the switch on and total weight with it off; V/F has no size
# covariate and takes the library's fat-free-mass volume factor with the
# switch on (docs/weight-adjustment.md: parameters the source left without a
# size term), the published value with it off.  A model with its own size
# covariate is not anchored at the cohort.  Fitted from 60 to 234 kg; adults only.
#
# CHECKS (test-drugs-liraglutideOvergaard.R): Table S1's values; the
# forest-plot ratios of Fig. 2 (60 kg 1.41, 234 kg 0.56, men 0.76, 70 or over
# 1.10); section 3.1.1's CL/F of 0.9 L/h for a woman and 1.1-1.4 for a man.
#
# References
# ----------
# Overgaard RV et al., Clin Pharmacokinet 2016;55:1413-1422.
#   https://doi.org/10.1007/s40262-016-0410-7
# Carlsson Petri KC, Jacobsen LV, Klein DJ.  Comparable liraglutide
#   pharmacokinetics in pediatric and adult populations with type 2 diabetes.
#   Clin Pharmacokinet 2015;54:663-670.  https://doi.org/10.1007/s40262-014-0229-z
# -----------------------------------------------------------------------------

LIRAGLUTIDE_OG_CL       <- 0.86     # L/h, the reference subject
LIRAGLUTIDE_OG_WT_REF   <- 100      # kg
LIRAGLUTIDE_OG_WT_EXP   <- 0.68
LIRAGLUTIDE_OG_MALE     <- 0.27     # exp() on CL/F
LIRAGLUTIDE_OG_AGE70    <- -0.10    # exp() on CL/F, age 70 or over
LIRAGLUTIDE_OG_V        <- 24.6     # L
LIRAGLUTIDE_OG_KA       <- 0.0806   # /h

#' Liraglutide pharmacokinetics (Overgaard 2016)
#'
#' One compartment, subcutaneous, with weight, sex and age on CL/F (Overgaard
#' 2016, the Saxenda trials); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
liraglutideOvergaard <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # V/F has no size term: the library's volume factor with the switch on, the
  # published value (a factor of 1) with it off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- LIRAGLUTIDE_OG_CL * (pkW / LIRAGLUTIDE_OG_WT_REF)^LIRAGLUTIDE_OG_WT_EXP *
    exp(LIRAGLUTIDE_OG_MALE * (sex == SEX_MALE)) *
    exp(LIRAGLUTIDE_OG_AGE70 * (age >= 70))

  default <- list(
    v1  = LIRAGLUTIDE_OG_V * size$volume, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = LIRAGLUTIDE_OG_KA / 60,
    bioavailability_SC = 1,   # apparent CL/F and V/F
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Overgaard 2016 (2931 adults with overweight or obesity, 1.8 and 3.0 mg): one ",
      "compartment, CL/F 0.86 L/h x (weight/100)^0.68 x exp(0.27) in men x exp(-0.10) ",
      "at 70 or over, V/F 24.6 L, ka 0.0806/h; without diabetes (x exp(0.18) with type ",
      "2 diabetes, not applied); concentration only. ",
      "https://doi.org/10.1007/s40262-016-0410-7"
    )
  )
}
