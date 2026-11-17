# -----------------------------------------------------------------------------
# Piperacillin, given as piperacillin-tazobactam (Kong 2025): two compartments,
# allometric weight, maturation and decline with postmenstrual age, and serum
# creatinine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L), total plasma
# piperacillin.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read in the full text of the source (Europe
# PMC, PMC11762590: Methods, Tables 1 and 3, Equations 8 and 17-20) and in its
# supplementary NONMEM control stream (ESM3), whose equations and final
# estimates were read directly; Tables 1 and 3 by a delegated reader and the
# control stream again by Claude Code.
#
# THE SOURCE
# ==========
# Kong D, Roberts JA, Lipman J, et al.  A pooled pharmacokinetic analysis for
# piperacillin/tazobactam across different patient populations: from
# premature infants to the elderly.  Clin Pharmacokinet 2025;64:107-126 (doi
# 10.1007/s40262-024-01460-6).  13 studies, 415 patients from premature
# neonates to 94 years: the adults are all critically ill or febrile and
# neutropenic (Roberts, Taccone, Sime, Tsai, Alobaid, Sukarnjanaset, Dhaese,
# Udy, Felton, Weinelt), the children critically ill or infected; 2,855 total
# and 888 unbound plasma piperacillin concentrations; NONMEM, FOCE-I.  Two
# compartments with linear elimination (a parallel Michaelis-Menten pathway
# did not improve the fit).  The control stream:
#
#     FSIZE  = WT/70
#     RPMA   = 35 + 40/52                         (PMA in years)
#     FMAT   = PMA^g1 / (PMA^g1 + MAT50^g1),          RFMAT at RPMA
#     FDEC   = 1 - PMA^g2 / (PMA^g2 + DEC50^g2),      RFDEC at RPMA
#     SCRstd = exp(1.42 - 1.17 (PMA/100)^-0.5 - 0.203 (PMA/100)^-0.5 ln(PMA/100))
#     FSCR   = exp(-thetaSCR (SCR - SCRstd))
#     CL = 10.6 FSIZE^0.75 (FMAT/RFMAT) (FDEC/RFDEC) FSCR   L/h
#     V1 = 10.4 FSIZE   V2 = 11.6 FSIZE   Q = 15.2 (V2/11.6)^0.75   L, L/h
#
# with Table 3's g1 3.35, MAT50 54.2 weeks, g2 1.92, DEC50 89.1 years and
# thetaSCR 0.346 dL/mg (the final estimates' exponentials: V1 exp(2.34), CL
# exp(2.36), V2 exp(2.45), Q exp(2.72), thetaSCR exp(-1.06)).  The study terms
# for Sime's patients and for the dried-blood-spot and unbound samples are 1
# for everyone else, as here.  SCR is in mg/dL (the abstract's "mg L-1" is a
# slip; the methods and the coefficient's dL/mg say otherwise).  The printed
# Equations 17-18 omit the normalisation to the reference age that the code
# applies; the code is followed.  At the reference (70 kg, 35 years, SCR
# 0.83) it gives the abstract's V1 10.4, CL 10.6, V2 11.6 and Q 15.2, and the
# Discussion's 11.0 L/h at 30 years; its 7.33 L/h at 70 years with a
# creatinine of 1.12 the code gives as 7.57, 3% higher, unexplained.
#
# WHY THIS SOURCE
# ===============
# Read in full and rejected: Hemmersbach-Miller 2023 (121 general inpatients,
# Cockcroft-Gault, but one compartment from three samples each, nearly all
# 4-h infusions: it predicts half the label's peak after a 30-min infusion);
# Felton 2014 (nonparametric, means and medians far apart); Felton 2012 (11
# patients); Lee 2025 (12 healthy Koreans, kept as a check).  Kong pools the
# most patients and samples, has two compartments, is the only one spanning
# all ages, and was fitted to both bolus-like and extended infusions.
#
# CHECKS (not used in the fit)
# ============================
# The Zosyn label (Zosyn in Galaxy Containers, NDA 050750, DailyMed
# 2023-03-24, Table 7; the same table as the vials' label of 2022, Table 9;
# healthy subjects, 30-min infusions): after 4.5 g (4 g piperacillin) Cmax
# 298 mg/L, AUC 322 mg.h/L, CL 210 mL/min (12.6 L/h), V 15.4 L, half-life
# 0.84 h.  This model for a
# 25-year-old 75-kg man with a creatinine of 0.9: CL 11.2 L/h (11% lower),
# AUC 356 (11% higher), peak 221 (26% lower), terminal half-life 1.8 h.
# Clearance agrees; the larger volumes and the longer terminal phase are the
# critically ill population's and the richer late sampling's (AGENTS.md:
# compare clearance first).  Lee 2025's healthy volunteers: 11.2 L/h.
#
# RENAL FUNCTION AND THE CEILING ON CREATININE
# ============================================
# The creatinine is the patient's own (patientCreatinine(): the source used
# children's own creatinine against an age-dependent normal, SCRstd), blank
# meaning the assumed normal for age and sex.  FSCR is an exponential with no
# non-renal floor: at 5 mg/dL it leaves a quarter of normal clearance, at 10
# mg/dL 4%, where the label says the half-life only doubles below a creatinine
# clearance of 20 mL/min.  The adults' highest creatinine was 5.82 mg/dL
# (Table 1, Felton), so the creatinine is held at 5.82 or below: the model is
# not extrapolated past its data.  Within its data it still predicts less
# clearance in severe impairment than the label implies (24% of normal at 5
# mg/dL against about 50%); that is said on the help page.
#
# AGE AND GESTATIONAL AGE
# =======================
# PMA is the postnatal age plus the Patient Profile's gestational age at
# birth (postmenstrualAge(); blank is a term birth, 40 weeks), in years.  The
# source fitted children and premature neonates (PMA from 0.5 years), so the
# maturation and age terms are used as fitted at every age.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carries its own allometric weight terms, evaluated at the
# pharmacokinetic weight with the fat-free-mass switch on and at total body
# weight with it off (as published).
#
# PREGNANCY
# =========
# Heikkila 1991 (caesarean, abstract) found larger volumes and faster
# clearance at term, unquantified; see the registry row.  The renal term
# takes the 50% rise in filtration of late pregnancy when the creatinine is
# blank (pregnancyGfrFactor()), as a creatinine 1/1.5 of the assumed normal.
#
# DOSE
# ====
# Zosyn is labelled by its combined weight ("4.5 g" holds "piperacillin sodium
# equivalent to 4 g piperacillin and tazobactam sodium equivalent to 0.5 g
# tazobactam") and ordered that way, so a unit naming the product
# (piperacillinTazobactamUnits, "mg pip-tazo") carries 4/4.5 of
# its weight as piperacillin; plain mg units are piperacillin, as in the
# source.  Tazobactam is not plotted.
#
# THE MIC LINE AND THE BAND
# =========================
# Free drug, read from R/antibioticThresholds.R and R/antibioticMbc.R.
# -----------------------------------------------------------------------------

# Kong 2025, Table 3 and the control stream (ESM3)
PIPERACILLIN_CL       <- 10.6    # L/h at 70 kg, 35 years + 40 weeks, SCR = SCRstd
PIPERACILLIN_V1       <- 10.4    # L at 70 kg
PIPERACILLIN_V2       <- 11.6    # L at 70 kg
PIPERACILLIN_Q        <- 15.2    # L/h at 70 kg
PIPERACILLIN_WT_REF   <- 70      # kg
PIPERACILLIN_PMA_REF  <- 35 + 40 / 52   # years
PIPERACILLIN_MAT50    <- 54.2 / 52      # years (54.2 weeks)
PIPERACILLIN_MAT_HILL <- 3.35
PIPERACILLIN_DEC50    <- 89.1    # years
PIPERACILLIN_DEC_HILL <- 1.92
PIPERACILLIN_SCR_COEF <- 0.346   # dL/mg
PIPERACILLIN_SCR_MAX  <- 5.82    # mg/dL, the adults' highest (Table 1)
# Zosyn: 4 g piperacillin per 4.5 g of product
PIPERACILLIN_PER_PRODUCT <- 4 / 4.5

#' Kong 2025's age-dependent normal serum creatinine (SCRstd)
#'
#' @param pma postmenstrual age, years
#' @returns mg/dL
#' @keywords internal
piperacillinScrStd <- function(pma)
{
  x <- pma / 100
  exp(1.42 - 1.17 * x^-0.5 - 0.203 * x^-0.5 * log(x))
}

#' Piperacillin clearance (Kong 2025)
#'
#' @param weight kg
#' @param pma postmenstrual age, years
#' @param scr serum creatinine, mg/dL
#' @returns L/h
#' @keywords internal
piperacillinClearance <- function(weight, pma, scr)
{
  mat <- function(p) p^PIPERACILLIN_MAT_HILL /
    (p^PIPERACILLIN_MAT_HILL + PIPERACILLIN_MAT50^PIPERACILLIN_MAT_HILL)
  dec <- function(p) 1 - p^PIPERACILLIN_DEC_HILL /
    (p^PIPERACILLIN_DEC_HILL + PIPERACILLIN_DEC50^PIPERACILLIN_DEC_HILL)
  ref <- PIPERACILLIN_PMA_REF
  scr <- min(scr, PIPERACILLIN_SCR_MAX)
  PIPERACILLIN_CL * (weight / PIPERACILLIN_WT_REF)^0.75 *
    mat(pma) / mat(ref) * dec(pma) / dec(ref) *
    exp(-PIPERACILLIN_SCR_COEF * (scr - piperacillinScrStd(pma)))
}

#' Piperacillin, given as piperacillin-tazobactam (Kong 2025)
#'
#' Two compartments; allometric weight, maturation and decline with
#' postmenstrual age, and serum creatinine.  See the header of
#' \code{R/drugs_piperacillin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param gestationalAge gestational age at birth, weeks, or NULL for a term
#'   birth
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the filtration
#'   by \code{PREGNANCY_GFR_FACTOR} when the creatinine is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
piperacillin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, gestationalAge = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w    <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  pma  <- postmenstrualAge(age, gestationalAge) / 52
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  scr  <- patientCreatinine(creatinine, age, sex) / gfrPregnancy
  cl   <- piperacillinClearance(w, pma, scr)
  fsize <- w / PIPERACILLIN_WT_REF

  default <- list(
    v1  = PIPERACILLIN_V1 * fsize,
    v2  = PIPERACILLIN_V2 * fsize,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = PIPERACILLIN_Q * fsize^0.75 / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(signif(scr * gfrPregnancy, 3)), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Kong D et al., Clin Pharmacokinet 2025;64:107-126: two compartments in 415 ",
    "patients from premature neonates to 94 years, CL 10.6 L/h, V1 10.4 L, ",
    "V2 11.6 L, Q 15.2 L/h at 70 kg, 35 years and a creatinine at the normal for ",
    "age; allometric weight, maturation and decline with postmenstrual age, ",
    "creatinine (held at 5.82 mg/dL or less); ", creatinineNote, ". ",
    "https://doi.org/10.1007/s40262-024-01460-6")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("filtration x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise; creatinine 1/", PREGNANCY_GFR_FACTOR, " of the assumed normal)")))

  # Band: free drug at the expected MBC of the common pathogens, placed on
  # the plotted curve (R/antibioticMbc.R).
  band <- antibioticMbcBand("piperacillin")$plotted
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band[["typical"]],
    upperTypical = band[["upper"]],
    lowerTypical = band[["lower"]],
    reference = reference,
    doseProducts = stats::setNames(PIPERACILLIN_PER_PRODUCT, PIPERACILLIN_TAZOBACTAM_PRODUCT)
  )
}
