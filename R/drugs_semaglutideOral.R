# -----------------------------------------------------------------------------
# Oral semaglutide: two compartments, first-order oral absorption (Rybelsus)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 nmol/L = 4.11358 ng/mL; molecular weight
# 4113.58 g/mol).  Concentration only: no glucose, HbA1c or weight effect is
# plotted.  Drafted by Claude Code, 2026-10-18, at the request of Steven L.
# Shafer; see the antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Overgaard RV, Navarria A, Ingwersen SH, Baekdal TA, Kildemoes RJ.  Clinical
# pharmacokinetics of oral semaglutide: analyses of data from clinical
# pharmacology trials.  Clin Pharmacokinet 2021;60:1335-1348.  376 subjects
# (16 with type 2 diabetes) in six clinical pharmacology trials whose dosing
# was supervised on site, 10 mg tablets co-formulated with 300 mg of SNAC (the
# absorption enhancer of Rybelsus).  The disposition is the two-compartment
# subcutaneous/intravenous model of Overgaard 2019 (Diabetes Ther 10:649),
# fixed; the oral data estimated the absorption.  Equations 1-12 of the paper
# and ESM Table 1 (online resource 1, 40262_2021_1025_MOESM1_ESM, supplied by
# Steven L. Shafer), the reference subject 85 kg:
#
#     CL = 0.0348 L/h x (WT/85)^1.01 x 1.09 (type 2 diabetes)
#     Q  = 0.304 L/h  x (WT/85)^1.01
#     Vc = 3.59 L     x (WT/85)^0.923
#     Vp = 4.1 L      x (WT/85)^0.923
#     ka = (2.09 - 1.35) exp(-0.112 (Tfast - 30)) + 1.35   /h, Tfast in min
#     F  = 0.795% x (WT/85)^0.741 x 0.637^(240 mL of water)
#          x (1 - exp(-0.0266 Tfast)) / (1 - exp(-0.0266 x 30))
#
# Tfast is the post-dose fasting time and the water term applies to 240 mL
# (F is the same from 50 to 120 mL).  The curve is for the label's dosing
# conditions, 30 minutes of fasting and up to 120 mL of water, where the two
# terms are 1: ka 2.09/h and F 0.795% at 85 kg.  A dose taken with 15 minutes
# of fasting would have F x 0.60, and one with 120 minutes F x 1.74 (the
# paper's plateau of about 1.4%); the Patient Profile has no field for them.
#
# The weight exponent on F is positive (0.741, 95% CI 0.199-1.19): a heavier
# subject absorbs a larger fraction, though less than the clearance rises,
# so exposure still falls with weight.  The sign is confirmed by the paper's
# Table 2, below.
#
# TYPE 2 DIABETES: the clearance factor, 1.09, fixed from the disposition
# model (ESM Table 1; Overgaard 2019's own table prints 1.12), is applied,
# since the product is indicated in type 2 diabetes (as in ertugliflozin).
# The separate re-estimation in 55 subjects with type 2 diabetes gave a
# factor of 0.841 on F (95% CI 0.607-1.15), not significant, and the paper
# concludes that F does not differ; it is not applied.
#
# NOT IN THE MODEL: hepatic and renal impairment, age 65 or over and sex were
# tested and removed (ESM; paper section 2.4).  The variability is large and
# not plotted: 53.5% between subjects and 137% between doses in F.
#
# BODY SIZE: the model's own weight covariates, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
# The weight term on F, the only one this analysis estimated, is held to the
# oral trials' range, 51.1 to 122 kg (Table 1); the disposition terms come
# from the subcutaneous and intravenous data and are not held.
#
# CHECKS
# ======
# The paper's Table 2 (geometric means of the subjects' typical values at the
# recommended conditions) at the groups' mean weights: healthy, 80.7 kg, CL
# 0.0330 L/h, Vc 3.42 L, Vp 3.91 L, Q 0.288 L/h, F 0.765%, t1/2 159 h (paper
# 0.033, 3.4, 3.9, 0.28, 0.76%, 158 h); type 2 diabetes, 89.3 kg, CL 0.0399,
# Vc 3.76, F 0.825% before the 0.841 factor, 0.69% after it, t1/2 145 h
# (paper 0.039, 3.7, 0.69%, 145 h).
#
# EXTERNAL: PIONEER.  Overgaard 2021b (Cell Rep Med 2:100387) re-estimated F
# on 2345 outpatients of the PIONEER trials, whose dosing was not supervised;
# the geometric mean steady-state average concentration at 14 mg daily was
# 14.6 nmol/L.  This model, which describes supervised dosing at the
# recommended conditions, gives 29.4 nmol/L at 14 mg for an 88 kg patient
# with type 2 diabetes: twice the outpatient exposure.  The outpatient F is
# in that paper's Methods S1, not supplied.  See
# tests/testthat/test-drugs-semaglutideOral.R.
#
# PRODUCT: Rybelsus 3, 7 and 14 mg (each with 300 mg SNAC).  The 2026
# Ozempic tablets (1.5, 4 and 9 mg, bioequivalent to 3, 7 and 14 mg) are not
# substitutable mg for mg and are not this drug.
#
# References
# ----------
# Overgaard RV et al., Clin Pharmacokinet 2021;60:1335-1348.
#   https://doi.org/10.1007/s40262-021-01025-x
# Overgaard RV et al., Cell Rep Med 2021;2:100387.
#   https://doi.org/10.1016/j.xcrm.2021.100387
# -----------------------------------------------------------------------------

SEMAGLUTIDE_ORAL_WEIGHT_REF   <- 85             # kg
SEMAGLUTIDE_ORAL_WEIGHT_RANGE <- c(51.1, 122)   # kg, the oral trials (Table 1)
SEMAGLUTIDE_ORAL_T2D_CL       <- 1.09           # ESM Table 1

#' Oral semaglutide pharmacokinetics (Rybelsus)
#'
#' Two compartments with first-order oral absorption at the recommended
#' dosing conditions (Overgaard 2021); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
semaglutideOral <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  r    <- pkW / SEMAGLUTIDE_ORAL_WEIGHT_REF
  rF   <- min(max(pkW, SEMAGLUTIDE_ORAL_WEIGHT_RANGE[1]),
              SEMAGLUTIDE_ORAL_WEIGHT_RANGE[2]) / SEMAGLUTIDE_ORAL_WEIGHT_REF

  default <- list(
    v1  = 3.59 * r^0.923, v2 = 4.1 * r^0.923, v3 = 1,
    cl1 = 0.0348 * r^1.01 * SEMAGLUTIDE_ORAL_T2D_CL / 60,
    cl2 = 0.304 * r^1.01 / 60, cl3 = 0,
    ka_PO = 2.09 / 60,                       # 30 min of post-dose fasting
    bioavailability_PO = 0.00795 * rF^0.741, # up to 120 mL of water
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Overgaard 2021 (376 subjects, six clinical pharmacology trials, supervised ",
      "dosing of 10 mg tablets with SNAC): two compartments, CL 0.0348 L/h x ",
      "(weight/85)^1.01 x 1.09 (type 2 diabetes), Q 0.304 L/h x (weight/85)^1.01, Vc ",
      "3.59 L and Vp 4.1 L x (weight/85)^0.923, ka 2.09/h and F 0.795% x ",
      "(weight/85)^0.741 at 30 min of post-dose fasting and up to 120 mL of water; ",
      "concentration only. Outpatient (PIONEER) exposure is about half. ",
      "https://doi.org/10.1007/s40262-021-01025-x"
    )
  )
}
