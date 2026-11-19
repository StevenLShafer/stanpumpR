# -----------------------------------------------------------------------------
# Oral semaglutide: two compartments, first-order oral absorption (Rybelsus),
# as patients take it at home (the PIONEER trials)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 nmol/L = 4.11358 ng/mL; molecular weight
# 4113.58 g/mol, Methods S1 MW = 4113.6).  Concentration only: no glucose,
# HbA1c or weight effect is plotted.  Drafted by Claude Code, 2026-10-18, at
# the request of Steven L. Shafer; rewritten 2026-11-19 on his decision to
# replace the supervised model with the outpatient one (below); see the
# antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# WHY THE OUTPATIENT MODEL (Steven L. Shafer, 2026-11-19)
# ==========================================
# Two population models of the same tablets exist, from the same group:
#
#   Overgaard 2021a (Clin Pharmacokinet 60:1335-1348): 376 subjects in six
#   clinical pharmacology trials, every dose taken on site under
#   supervision, fasting, with up to 120 mL of water, 30 minutes before food.
#   F 0.795% at 85 kg.  It was this drug until 2026-11-19.
#
#   Overgaard 2021b (Cell Rep Med 2:100387): 2,345 patients with type 2
#   diabetes in the PIONEER 1, 2, 3, 5, 8 and 9 trials, who took the tablets
#   themselves at home.  F 0.471% for the reference woman of 85 kg, men x
#   0.814.  This drug.
#
# For the same patient the outpatient model gives about half the supervised
# exposure: at 14 mg daily, 13.6 nmol/L for an 88 kg man against the
# supervised model's 29.4, where PIONEER's own geometric mean was 14.6.  Both
# describe adults with type 2 diabetes taking the same product, so the
# library's rule of choosing the model whose source population matches the
# patient (R/modelSelection.R) cannot choose between them, and a clinician
# has no basis to choose either (the program chooses the model, never the
# physician).  Shafer chose the outpatient model, because what separates the
# two is how the tablet is taken, and patients at home do not take a drug as
# a supervised study does: doses are missed, and the label's conditions
# (fasting, little water, 30 minutes before food) are not always kept.
# Glasser SP, Drugs don't work in patients who don't take them, J Clin
# Hypertens 2018;20:1504-1506 (PMID 30238616; supplied by Shafer), is the
# general case, with Osterberg and Blaschke, N Engl J Med 2005;353:487-497
# (PMID 16079372) and Blaschke, Osterberg, Vrijens and Urquhart, Annu Rev
# Pharmacol Toxicol 2012;52:275-301 (PMID 21942628: variable adherence "vies
# with pharmacokinetics as the leading source of variance in drug response
# in ambulatory care"); inst/app-help/cautions.md carries them for the
# library.
# Overgaard 2021b itself leaves the cause open: "Whether these observations
# reflect real problems with drug absorption and/or compliance remains
# uncertain."  The PIONEER patients were still in trials, where adherence is
# better than in practice (Glasser), so a typical clinic patient may absorb
# less than this curve.
#
# SOURCE
# ======
# Overgaard RV, Hertz CL, Ingwersen SH, Navarria A, Drucker DJ.  Levels of
# circulating semaglutide determine reductions in HbA1c and body weight in
# people with type 2 diabetes.  Cell Rep Med 2021;2:100387 (PMID 34622228;
# read in full with its supplement, PMID_34622228_mmc1.pdf).  10,527
# concentrations from 2,345 patients, a single sample at each visit at the
# investigator's discretion, the last dose's time recorded.  The disposition
# and the absorption rate are fixed from the clinical pharmacology model
# (Overgaard 2019, Diabetes Ther 10:649, with 2021a's ka); only the
# bioavailability and its covariates were estimated.  Methods S1 (the NONMEM
# control stream) and Supplementary Table 1, reference a woman of 85 kg:
#
#     CL = 0.039 L/h x (BW/85)^1.01      (0.0348 x 1.12, Overgaard 2019's
#                                          type 2 diabetes clearance)
#     Q  = 0.304 L/h x (BW/85)^1.01
#     Vc = 3.59 L    x (BW/85)^0.923
#     Vp = 4.1 L     x (BW/85)^0.923
#     ka = 2.09 /h
#     F  = 0.471% x 0.814 (men) x (BW/85)^0.374
#            x 1.10 (Asian) x 1.13 (upper GI disease) x trial effects
#
# NOT APPLIED, for want of a field: Asian race (x 1.10) and upper GI disease
# (x 1.13, mostly gastro-oesophageal reflux), so the curve is for a patient
# who is neither; and the trial effects (x 0.6 and x 0.758, which correct
# PIONEER 3 and 5's lower exposure to that of PIONEER 1, 2, 8 and 9; the
# supplement's table labels them PIONEER 2 and 3, its text and code 3 and 5).
# Age, Black race, Hispanic ethnicity, renal function and dose were tested and
# not kept.  Variability is not plotted: 84.3% between patients in F.
#
# BODY SIZE: the model's own weight covariates, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
# No term is held: the paper prints its weight distribution (88.2 +/- 21.9
# kg; 5th and 95th percentiles 56 and 129 kg, Figure S1) but not its range,
# and every term is a power of weight that stays finite and positive.
#
# CHECK: Cavg = F x dose / CL at steady state.  At 14 mg daily: 17.1 nmol/L
# for the 85 kg woman, 13.9 for an 85 kg man, 13.6 for an 88 kg man, about
# the PIONEER geometric mean of 14.6 nmol/L (Overgaard 2021b).
#
# PRODUCT: Rybelsus 3, 7 and 14 mg (each with 300 mg SNAC).  The 2026
# Ozempic tablets (1.5, 4 and 9 mg, bioequivalent to 3, 7 and 14 mg) are not
# substitutable mg for mg and are not this drug.
#
# References
# ----------
# Overgaard RV et al., Cell Rep Med 2021;2:100387.
#   https://doi.org/10.1016/j.xcrm.2021.100387
# Overgaard RV et al., Clin Pharmacokinet 2021;60:1335-1348.
#   https://doi.org/10.1007/s40262-021-01025-x
# Glasser SP, J Clin Hypertens 2018;20:1504-1506.
#   https://doi.org/10.1111/jch.13380
# -----------------------------------------------------------------------------

SEMAGLUTIDE_ORAL_WEIGHT_REF   <- 85         # kg, Methods S1
SEMAGLUTIDE_ORAL_F            <- 0.00471    # the reference woman, Table S1
SEMAGLUTIDE_ORAL_MALE_F       <- 0.814      # Table S1
SEMAGLUTIDE_ORAL_WEIGHT_F     <- 0.374      # exponent on F, Table S1

#' Oral semaglutide pharmacokinetics (Rybelsus), outpatient use
#'
#' Two compartments with first-order oral absorption, the bioavailability
#' estimated in the PIONEER trials (Overgaard 2021, Cell Rep Med); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
semaglutideOral <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  r    <- pkW / SEMAGLUTIDE_ORAL_WEIGHT_REF
  male <- isTRUE(sex == SEX_MALE)

  default <- list(
    v1  = 3.59 * r^0.923, v2 = 4.1 * r^0.923, v3 = 1,
    cl1 = 0.039 * r^1.01 / 60,
    cl2 = 0.304 * r^1.01 / 60, cl3 = 0,
    ka_PO = 2.09 / 60,
    bioavailability_PO = SEMAGLUTIDE_ORAL_F *
      (if (male) SEMAGLUTIDE_ORAL_MALE_F else 1) * r^SEMAGLUTIDE_ORAL_WEIGHT_F,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Overgaard 2021, Cell Rep Med (2,345 outpatients with type 2 diabetes in the ",
      "PIONEER trials, dosing themselves): two compartments, CL 0.039 L/h x ",
      "(weight/85)^1.01, Q 0.304 L/h x (weight/85)^1.01, Vc 3.59 L and Vp 4.1 L x ",
      "(weight/85)^0.923, ka 2.09/h, F 0.471% x 0.814 (men) x (weight/85)^0.374; ",
      "not Asian, no upper GI disease; concentration only. About half the exposure ",
      "of supervised dosing (Overgaard 2021, Clin Pharmacokinet). ",
      "https://doi.org/10.1016/j.xcrm.2021.100387"
    )
  )
}
