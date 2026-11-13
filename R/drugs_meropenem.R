# -----------------------------------------------------------------------------
# Meropenem (the meropenem-vaborbactam population model, FDA review of NDA
# 209776): two compartments, clearance a sigmoid in MDRD eGFR
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The equations below were read on the page image of the FDA's
# Clinical Pharmacology Review of NDA 209776 (Vabomere, meropenem and
# vaborbactam; Reference ID 4108785, review pages 55-57, equations (1)-(3) and
# Table 4.2-2, "Source: Applicant's population PK report (Study 00373-1
# Report), Page 51, Table 10"), and checked against the published analysis
# of the same trials, read in full: Trang M, Griffith DC, Bhavnani SM, et al.
# Population pharmacokinetics of meropenem and vaborbactam based on data
# from noninfected subjects and infected patients.  Antimicrob Agents
# Chemother 2021;65:e02606-20 (doi 10.1128/AAC.02606-20; PMC8370236).
#
# THE SOURCE
# ==========
# Two phase 1 studies (healthy subjects, study 501; renal impairment and
# end-stage renal disease, study 504) and two phase 3 studies (complicated
# urinary tract infection, study 505; carbapenem-resistant infections, study
# 506): about 400 adults aged 18 to 92 years, weighing 40 to 177 kg (median
# 75), with an MDRD eGFR of 4.5 to 338 mL/min/1.73 m2 (Trang 2021, Table 1),
# given 1 or 2 g of meropenem over 3 h with vaborbactam; plasma and urine
# by LC-MS/MS (0.02-100 mg/L); NONMEM.  Two compartments with zero-order
# input.  The model the FDA reviewed (the review's equations (1)-(3) and
# Table 4.2-2):
#
#     CL = [3.78 (1 - RGRP) + 3.78 x 0.349 x RGRP
#           + 6.60 eGFR^1.94 / (40.8^1.94 + eGFR^1.94)] x (AGE/58)^-0.430   L/h
#     Vc = 17.4 x (WTKG/80)^0.487  L       CLd = 1.52 L/h
#     Vp = 2.50 x (WTKG/80)^0.324  L
#
# with eGFR the MDRD estimate in mL/min/1.73 m2 and RGRP 1 when eGFR <= 30,
# 0 otherwise.  The non-renal intercept is 3.78 L/h above an eGFR of 30 and
# 3.78 x 0.349 = 1.32 L/h at or below it.  The review's prose calls this a
# "reduction in the CLNR clearance by 35%", which its own equation and
# table contradict (0.349 is what is LEFT, a reduction of 65%); Trang 2021,
# refitting the same trials with the final data of study 506, prints the
# shift as -0.650, i.e. x 0.35.  The equation and both tables agree, so the
# prose is the slip.
#
# WHY THE REVIEW'S MODEL AND NOT THE PUBLISHED FINAL ONE
# ======================================================
# Trang 2021's final model (Table 2: CLNR 3.85, CLR,max 6.58, eGFR50 40.0,
# Hill 1.95, age exponent -0.526, allometric weight on every parameter) is a
# refit with 27 more patients, and its clearance at normal renal function
# differs from the review's by under 5%.  It does not print the weight or
# age its allometric and age terms are normalised to, so it cannot be
# evaluated as written; the review prints both (80 kg, 58 years).
#
# CHECKS
# ======
# The published clearance at "a 58-year-old patient with an eGFR of 100" is
# "about 10 liters/h" (Trang 2021, Discussion): this model gives 9.4 L/h
# (3.78 + 5.66).  At an eGFR of 40 it gives 7.0 L/h, against the text's
# "approximately 7 liters/h", and the renal fraction at an eGFR of 100 is
# 60% (the text: "approximately 59%").  The reference man of the library
# (35 years, serum creatinine 0.9 mg/dL, eGFR 96) clears 11.6 L/h, with
# half-lives of 0.7 and 1.5 h, against the Merrem label's "approximately 1
# hour" in subjects with normal renal function.  Not used in the fit: the
# Merrem label (Reference ID 4035314, 2016, sec. 12.3) gives healthy
# volunteers' peaks after a 30-min infusion of "approximately 23 mcg/mL (range
# 14-26)" after 500 mg and "49 mcg/mL (range 39-58)" after 1 g, and "about 1
# mcg/mL at 6 hours" after 500 mg; this model at the reference man gives 25.3,
# 50.6 and 0.80.  Christensson 1992 (Antimicrob Agents Chemother 36:1532,
# 500 mg over 30 min, six men of 79 kg with an iohexol GFR of 100, HPLC; read
# on the page images by a delegated reader) measured a peak of 30.3 mg/L and
# an AUC of 36.0 mg.h/L; this model for a 30-year-old 79-kg man with a normal
# creatinine gives 24.0 and 40.6, its clearance 11% below theirs (12.3
# against about 13.9 L/h, dose/AUC), the peak lower by a larger central volume.
#
# THE STEP AT AN eGFR OF 30
# =========================
# RGRP is an indicator, so clearance falls by 2.5 L/h (41% at 58 years)
# between an eGFR just above 30 and 30 itself.  It is the source's model,
# fitted to study 504's subjects with severe impairment and end-stage renal
# disease; it is kept as printed rather than smoothed, which would be a model
# the source did not fit.
#
# RENAL FUNCTION, AGE AND THE FLOORS
# ==================================
# eGFR is the MDRD equation (egfrMDRD(), the 175 IDMS form, without the
# race term), indexed to 1.73 m2 as the source used it, from the Patient
# Profile's serum creatinine put on the adult scale (adultEquivalentCreatinine();
# blank means the assumed normal creatinine for age and sex) and floored at
# 0.5 mg/dL as the source floored it ("serum creatinine was also capped at a
# lower bound of 0.5 mg/dl", Trang 2021, Methods).  The age term is a
# negative power, so it sees max(age, 18): the youngest age studied, and the
# library's floor for an adult term (AGENTS.md, Age terms).  The model was
# fitted in adults only.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carries its own weight covariates, on both volumes, evaluated
# at the pharmacokinetic weight with the fat-free-mass switch on and at total
# body weight with it off.  Clearance has no weight term: the source found
# none, and its eGFR is indexed to body surface area.
#
# PREGNANCY
# =========
# See the pregnancy registry row.  The renal part of clearance takes the
# 50% rise in filtration of late pregnancy when the serum creatinine is blank
# (pregnancyGfrFactor(), applied to eGFR).
#
# DOSE
# ====
# Merrem IV vials contain "500 mg or 1 gram (as the trihydrate ...) of sterile
# meropenem powder", so doses are milligrams of meropenem, as in the source.
#
# THE MIC LINE AND THE BAND
# =========================
# Free drug, read from R/antibioticThresholds.R and R/antibioticMbc.R.
# -----------------------------------------------------------------------------

# FDA review of NDA 209776, equations (1)-(3) and Table 4.2-2
MEROPENEM_CLNR        <- 3.78    # L/h
MEROPENEM_CLNR_SHIFT  <- 0.349   # multiplier on CLNR when eGFR <= 30
MEROPENEM_CLR_MAX     <- 6.60    # L/h
MEROPENEM_EGFR50      <- 40.8    # mL/min/1.73 m2
MEROPENEM_HILL        <- 1.94
MEROPENEM_EGFR_LOW    <- 30      # mL/min/1.73 m2, RGRP = 1 at or below
MEROPENEM_AGE_REF     <- 58      # years
MEROPENEM_AGE_EXP     <- -0.430
MEROPENEM_VC          <- 17.4    # L at 80 kg
MEROPENEM_VC_EXP      <- 0.487
MEROPENEM_VP          <- 2.50    # L at 80 kg
MEROPENEM_VP_EXP      <- 0.324
MEROPENEM_CLD         <- 1.52    # L/h
MEROPENEM_WT_REF      <- 80      # kg
MEROPENEM_SCR_MIN     <- 0.5     # mg/dL, the source's lower bound

#' Meropenem clearance (FDA review of NDA 209776)
#'
#' @param egfr MDRD eGFR, mL/min/1.73 m2
#' @param age years
#' @returns total clearance, L/h
#' @keywords internal
meropenemClearance <- function(egfr, age)
{
  clnr <- MEROPENEM_CLNR * (if (egfr <= MEROPENEM_EGFR_LOW) MEROPENEM_CLNR_SHIFT else 1)
  clr <- MEROPENEM_CLR_MAX * egfr^MEROPENEM_HILL /
    (MEROPENEM_EGFR50^MEROPENEM_HILL + egfr^MEROPENEM_HILL)
  (clnr + clr) * (max(age, ADULT_AGE_TERM_FLOOR) / MEROPENEM_AGE_REF)^MEROPENEM_AGE_EXP
}

#' Meropenem (FDA review of NDA 209776)
#'
#' Two compartments; clearance a sigmoid in MDRD eGFR with a lower non-renal
#' clearance at or below 30 mL/min/1.73 m2, and an age term.  See the header
#' of \code{R/drugs_meropenem.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the eGFR by
#'   \code{PREGNANCY_GFR_FACTOR} when the creatinine is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
meropenem <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- max(adultEquivalentCreatinine(creatinine, age, sex), MEROPENEM_SCR_MIN)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  egfr <- egfrMDRD(age, sex, scr) * gfrPregnancy
  cl <- meropenemClearance(egfr, age)

  default <- list(
    v1  = MEROPENEM_VC * (pkW / MEROPENEM_WT_REF)^MEROPENEM_VC_EXP,
    v2  = MEROPENEM_VP * (pkW / MEROPENEM_WT_REF)^MEROPENEM_VP_EXP,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = MEROPENEM_CLD / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "FDA Clinical Pharmacology Review of NDA 209776 (Vabomere, 2017), meropenem ",
    "population model: two compartments, CL = [3.78 (x 0.349 if eGFR <= 30) + ",
    "6.60 eGFR^1.94/(40.8^1.94 + eGFR^1.94)] x (age/58)^-0.43 L/h, ",
    "Vc 17.4 (WT/80)^0.487 L, CLd 1.52 L/h, Vp 2.50 (WT/80)^0.324 L; MDRD eGFR ",
    sprintf("%.0f", egfr), " mL/min/1.73 m2, ", creatinineNote, "; checked against ",
    "Trang M et al., Antimicrob Agents Chemother 2021;65:e02606-20. ",
    "https://doi.org/10.1128/AAC.02606-20")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("eGFR x ", PREGNANCY_GFR_FACTOR, " (physiological rise in filtration)")))

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band <- antibioticMbcBand("meropenem")$plotted
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band[["typical"]],
    upperTypical = band[["upper"]],
    lowerTypical = band[["lower"]],
    reference = reference
  )
}
