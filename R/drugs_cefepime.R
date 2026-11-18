# -----------------------------------------------------------------------------
# Cefepime (Barreto 2023): two compartments in critically ill adults, clearance
# in proportion to the de-indexed CKD-EPI eGFR
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total serum.
#
# Drafted by Claude Code, 2026-11-17, at the request of Steven L. Shafer, who
# supplied the paper and its supplement that day.  Source, read in full with
# its supplementary appendix: Barreto EF, Chang J, Rule AD, Mara KC, Meade LA,
# Paul J, Jannetto PJ, Athreya AP, Scheetz MH, for the BLOOM Study Group.
# Population pharmacokinetic model of cefepime for critically ill adults: a
# comparative assessment of eGFR equations.  Antimicrob Agents Chemother
# 2023;67(11):e00810-23.  doi 10.1128/aac.00810-23 (PMID 37882514).
#
# THE SOURCE
# ==========
# 120 critically ill adults at the Mayo Clinic, 2018-2022 (61 +/- 17 years,
# 60% men, 87 +/- 26 kg, body surface area 2.0 +/- 0.3 m2, serum creatinine
# 0.9 +/- 0.4 mg/dL; 82% with sepsis, 47% on vasopressors), 379 serum
# samples from a richly sampled cohort of 20 and an opportunistically sampled
# cohort of 100; total cefepime by LC-MS/MS; Monolix.  AKI of stage II or
# higher, renal replacement and ECMO were excluded.  Two compartments with
# first-order elimination (Table 3 and the supplement's Table S2):
#
#     CL = 7.84 L/h x eGFR / 7.2        (eGFR in L/h, so 7.2 L/h = 120 mL/min)
#     V1 = 15.56 L x WT / 70           Q 7.09 L/h (fixed)   V2 10.63 L (fixed)
#
# CL and V1 are the abstract's 7.84 L/h and 15.6 L; Table 3 prints V1 and V2
# to two decimals.  Clearance has no non-renal part.
#
# THE eGFR: CREATININE ONLY (Shafer's decision, 2026-11-17)
# =========================================================
# The source's eGFR is the 2012 CKD-EPI creatinine-cystatin C equation,
# de-indexed to mL/min, which had the lowest BICc of the eleven it compared
# (Table 2).  The Patient Profile has no cystatin C, and the source prints no
# model refitted on a creatinine-only eGFR.  Shafer chose (2026-11-17, option
# 1 of three put to him) to put the creatinine-only equation the source found
# best, the 2012 CKD-EPI eGFRcr in mL/min (Table 2: BICc 2,533 against the
# combined equation's 2,490), into the published model.  The 2012 CKD-EPI
# creatinine equation is the 2009 one (Table S1 prints the same
# coefficients), so it is egfrCKDEPI2009() without the race term, de-indexed
# by body surface area / 1.73 as Table S1 says.  The source does not name the
# body surface area formula; DuBois is used, as egfrDeindexed() uses it.
#
# The substitution has a known bias: in the source's patients the
# creatinine-only eGFR averaged 96 +/- 36 mL/min against 87 +/- 39 for the
# combined one (Table 2), so this model's clearance is about 10% higher on
# average than the published model's for the same patient, and more where
# creatinine overstates filtration (the loss of muscle in critical illness,
# which the source's Discussion names).  The help page says so.
#
# THE LOWEST CLEARANCE (Shafer's decision, 2026-11-17)
# ====================================================
# Clearance in proportion to eGFR tends to zero with renal function, and the
# source studied no patient on dialysis or with AKI of stage II or higher;
# it prints no range of eGFR.  The label (Hospira, ANDA 065441, 2024, read
# in full) says cefepime's total clearance "decreased proportionally with
# creatinine clearance in patients with abnormal renal function", which
# supports the proportion, and gives the half-life in patients on
# hemodialysis as 13.5 (+/- 2.7) h with a steady-state volume of 18 L:
# 0.693 x 18 / 13.5 = 0.92 L/h, the clearance left without renal function.
# Its 85% urinary recovery of unchanged cefepime from a normal 7.2 L/h
# leaves about 1.1 L/h of non-renal clearance, the same order.  So clearance
# is held at 0.92 L/h or more (CEFEPIME_CL_MIN), reached at an eGFR of about
# 14 mL/min.  The floor keeps the curve from flattening without limit as the
# creatinine rises; it changes nothing above that eGFR.
#
# CHECKS (the label, not fitted)
# ==============================
# The label's healthy men (n = 9, 30-min infusions, Table 7): peak 81.7 and
# 163.9 mg/L after 1 and 2 g, AUC 148.5 h.mg/L after 1 g, total clearance
# 120 (+/- 8) mL/min, half-life 2 h, Vss 18 L.  This model at the library's
# reference man (70 kg, 170 cm, 35 years, creatinine 0.9 mg/dL; eGFR 115
# mL/min, clearance 7.54 L/h = 126 mL/min): peaks 51.9 and 103.7 mg/L, AUC
# 132.7 h.mg/L after 1 g, half-lives 0.5 and 2.9 h, Vss 26.2 L.  Clearance
# agrees within 5%; the volumes are larger, as expected of critically ill
# patients against healthy volunteers, so the peaks are lower (the label
# measured them at the end of the infusion, where a small central volume
# matters most) and the terminal half-life longer.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's weight term on V1, and the body surface area that de-indexes
# the eGFR, at the pharmacokinetic weight with the fat-free-mass switch on
# and at total body weight with it off.  The source standardised weight to
# 70 kg and explored allometric scaling of clearance, which did not improve
# the fit.  Fitted in adults only, and offered at every age: the age the
# eGFR equation sees is the patient's, and a child's creatinine is put on
# the adult scale (adultEquivalentCreatinine()).
#
# PREGNANCY
# =========
# See the pregnancy registry row.  The eGFR takes the 50% rise in filtration
# of late pregnancy when the serum creatinine is blank (pregnancyGfrFactor()).
#
# DOSE
# ====
# The label: "cefepime for injection ... equivalent to 1 g [or 2 g] of
# cefepime" (as the hydrochloride), so doses are milligrams of cefepime, as
# in the source (1 or 2 g, Table 1).
#
# THE MIC LINE AND THE BAND
# =========================
# The line is free drug at the MIC, read from R/antibioticThresholds.R: 8
# mg/L for Pseudomonas, 10 mg/L of total cefepime, kept there on Shafer's
# decision of 2026-11-18 over the Enterobacterales breakpoint of 1 mg/L (1.25
# total).  There is no band (antibioticNoBands()).
# -----------------------------------------------------------------------------

# Barreto 2023, Table 3 and Table S2
CEFEPIME_CL       <- 7.84    # L/h at an eGFR of 7.2 L/h (120 mL/min)
CEFEPIME_EGFR_REF <- 120     # mL/min
CEFEPIME_V1       <- 15.56   # L at 70 kg
CEFEPIME_WT_REF   <- 70      # kg
CEFEPIME_Q        <- 7.09    # L/h, fixed
CEFEPIME_V2       <- 10.63   # L, fixed
# The label's clearance on hemodialysis: Vss 18 L x ln 2 / 13.5 h
CEFEPIME_CL_MIN   <- 18 * log(2) / 13.5   # L/h

#' Cefepime clearance (Barreto 2023)
#'
#' @param egfr de-indexed CKD-EPI eGFR, mL/min
#' @returns clearance, L/h, held at \code{CEFEPIME_CL_MIN} or more
#' @keywords internal
cefepimeClearance <- function(egfr)
{
  max(CEFEPIME_CL * egfr / CEFEPIME_EGFR_REF, CEFEPIME_CL_MIN)
}

#' Cefepime (Barreto 2023)
#'
#' Two compartments in critically ill adults; clearance in proportion to the
#' de-indexed CKD-EPI creatinine eGFR, central volume in proportion to
#' weight.  See the header of \code{R/drugs_cefepime.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the eGFR by
#'   \code{PREGNANCY_GFR_FACTOR} when the creatinine is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cefepime <- function(weight, height, age, sex, adjustToFFM = TRUE,
                     creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  egfr <- egfrDeindexed(pkW, height, age, sex, scr) * gfrPregnancy
  cl <- cefepimeClearance(egfr)

  default <- list(
    v1  = CEFEPIME_V1 * pkW / CEFEPIME_WT_REF,
    v2  = CEFEPIME_V2,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = CEFEPIME_Q / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Barreto EF, Chang J, Rule AD, et al., Antimicrob Agents Chemother ",
    "2023;67:e00810-23, Table 3 and Table S2: two compartments, CL = 7.84 L/h x ",
    "eGFR/120 mL/min, V1 15.56 L x WT/70, Q 7.09 L/h, V2 10.63 L; eGFR the 2012 ",
    "CKD-EPI creatinine equation de-indexed to mL/min (the source's cystatin C ",
    "equation replaced, Shafer 2026-11-17), ", sprintf("%.0f", egfr), " mL/min, ",
    creatinineNote, "; clearance held at the label's 0.92 L/h on hemodialysis or more. ",
    "https://doi.org/10.1128/aac.00810-23")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("eGFR x ", PREGNANCY_GFR_FACTOR, " (physiological rise in filtration)")))

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
