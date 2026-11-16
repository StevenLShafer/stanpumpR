# -----------------------------------------------------------------------------
# Flecainide (Doki 2012): one compartment, apparent oral, metabolic and renal
# clearance, CYP2D6 genotype steepening the decline with age
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# supplied the source's PDF after the first pass could not reach it.  The
# numbers below were read in that PDF (Methods, Table 1, the final-model
# equations on p. 780, checked on the page image, and Table 3) and in the
# flecainide acetate tablets label (NDA 018830, s024), read in full.
#
# THE SOURCE
# ==========
# Doki K, Homma M, Kuga K, Aonuma K, Kohda Y.  CYP2D6 genotype affects
# age-related decline in flecainide clearance: a population pharmacokinetic
# analysis.  Pharmacogenet Genomics 2012;22:777-783 (doi
# 10.1097/FPC.0b013e3283588fe5).  163 Japanese outpatients with
# supraventricular tachyarrhythmia (128 men; 60.6 +/- 12.6 years, 66.2 +/-
# 10.5 kg, eGFR 73 +/- 16 mL/min/1.73 m2), all genotyped for CYP2D6, on
# flecainide acetate for at least 2 months (1.2-5.0 mg/kg/day); 222 serum
# concentrations from monitoring, 10.6 +/- 4.7 h after a dose; HPLC with
# fluorescence; Phoenix NLME.  One compartment, first-order absorption, ka
# and V/F FIXED at 0.62 /h and 6.0 L/kg (the source's references 17 and 18),
# apparent oral parameters.  The final model:
#
#     CL/F = CLm + CLr  (L/h)
#     CLm  = 8.83 x 0.713^SEX x (60/AGE)^(1.62 [AGE >= 60] hetEM)
#                             x (55/AGE)^(2.83 [AGE >= 55] IMPM)
#     CLr  = 9.79 x (GFR / 73) x (WT / 66)
#
# with SEX 1 in women, GFR the Japanese eGFR (Matsuo 2009, egfrMatsuo() in
# R/renalFunction.R) in mL/min/1.73 m2, and WT in kg.  The paper's own
# numbers are reproduced: CL/F 18.6 L/h in a 66-kg man with an eGFR of 73.
#
# CYP2D6
# ======
# The source's groups (EM alleles *1 and *2, IM allele *10, PM alleles *4,
# *5, *14, *21, *36): homozygous EM (EM/EM), heterozygous EM (EM/IM or
# EM/PM, mostly *1/*10) and IM/PM (IM/IM or IM/PM; one true PM/PM).  Below 55
# genotype has no effect; above 55 or 60 it steepens the fall of metabolic
# clearance with age.  The Patient Profile's CYP 2D6 field maps onto them by
# Claude Code's inference, not the paper's: normal and ultrarapid -> homozygous
# EM; intermediate -> heterozygous EM (a Western intermediate is typically one
# functional and one null allele, closest in activity to *1/*10); poor ->
# IM/PM (extrapolated: the group was almost all *10/*10 and *10/*5).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms, on renal clearance and on V/F (6.0 L/kg),
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# and at total body weight with it off.  The metabolic clearance had no
# weight term and is used as published.
#
# CHECKED AGAINST THE LABEL (not used in the fit)
# ===============================================
# The label: half-life about 20 h (12-27) in patients with PVCs, 14 h in
# healthy subjects, peak about 3 h (1-6).  This model at the reference man
# (35 years, 70 kg, serum creatinine 0.9): CL/F 20.0 L/h, half-life 14.6 h;
# a 65-year-old 80-kg man, 18-22 h by genotype; peak 3 h.  V/F is fixed, so
# the half-life checks clearance only.  Tjandra-Maga 1986's healthy
# volunteers cleared it at 25.2 L/h (21% more; their renal clearance, 10.0
# L/h at 70 kg, agrees with this model's 9.8), the shortfall being metabolic,
# plausibly the Japanese prevalence of CYP2D6*10.
#
# DOSE BASIS
# ==========
# The source's doses are flecainide acetate, as the US tablets (50, 100 and
# 150 mg) are labelled, so a dose is entered as prescribed.  Whether its
# concentrations are expressed as the base or the acetate (12.7% apart) is
# not stated; nor is it in the label's range.
#
# RENAL FUNCTION AND PREGNANCY
# ============================
# A blank serum creatinine is an assumed normal value for age and sex
# (assumedCreatinine()), put on the adult scale for a child
# (adultEquivalentCreatinine()).  In pregnancy the eGFR takes the 50% rise
# in filtration of late pregnancy when the creatinine is blank
# (pregnancyGfrFactor()), on the renal part of clearance only.  The model is
# for adults: the label gives children a half-life of about 8 h.
#
# THE BAND
# ========
# The label: "The large majority of patients successfully treated with
# flecainide acetate tablets were found to have trough plasma levels between
# 0.2 and 1.0 mcg/mL. The probability of adverse experiences, especially
# cardiac, may increase with higher trough plasma levels, especially when
# these exceed 1.0 mcg/mL."
# -----------------------------------------------------------------------------

# Doki 2012, Table 3 and p. 779
FLECAINIDE_CLM       <- 8.83    # L/h
FLECAINIDE_FEMALE    <- 0.713
FLECAINIDE_AGE60_HET <- 1.62    # exponent on 60/AGE from 60, heterozygous EM
FLECAINIDE_AGE55_IM  <- 2.83    # exponent on 55/AGE from 55, IM/PM
FLECAINIDE_CLR       <- 9.79    # L/h at an eGFR of 73 and 66 kg
FLECAINIDE_GFR_REF   <- 73      # mL/min/1.73 m2
FLECAINIDE_WT_REF    <- 66      # kg
FLECAINIDE_V_PER_KG  <- 6.0     # L/kg, fixed
FLECAINIDE_KA        <- 0.62    # 1/h, fixed

#' Flecainide (Doki 2012)
#'
#' One compartment, apparent oral, metabolic and renal clearance with CYP2D6
#' genotype steepening the decline with age; see the header of
#' \code{R/drugs_flecainide.R}.
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 phenotype, one of \code{CYP2D6_VALUES}
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant third trimester (TRUE) or not
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
flecainide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       cyp2d6 = CYP2D6_DEFAULT, creatinine = NULL, pregnant = FALSE)
{
  if (!(length(cyp2d6) == 1 && cyp2d6 %in% CYP2D6_VALUES))
    stop("Invalid cyp2d6 for flecainide: ", paste(cyp2d6, collapse = ", "))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfr  <- egfrMatsuo(age, sex, scr) * pregnancyGfrFactor(pregnant, creatinine)

  hetEM <- cyp2d6 == CYP2D6_INTERMEDIATE
  imPM  <- cyp2d6 == CYP2D6_POOR
  clm <- FLECAINIDE_CLM * (if (sex == SEX_FEMALE) FLECAINIDE_FEMALE else 1) *
    (if (hetEM && age >= 60) (60 / age)^FLECAINIDE_AGE60_HET else 1) *
    (if (imPM && age >= 55) (55 / age)^FLECAINIDE_AGE55_IM else 1)
  clr <- FLECAINIDE_CLR * gfr / FLECAINIDE_GFR_REF * pkW / FLECAINIDE_WT_REF

  default <- list(
    v1 = FLECAINIDE_V_PER_KG * pkW, v2 = 1, v3 = 1,
    cl1 = (clm + clr) / 60, cl2 = 0, cl3 = 0,
    ka_PO = FLECAINIDE_KA / 60,
    bioavailability_PO = 1,           # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Doki K et al., Pharmacogenet Genomics 2012;22:777-783: one compartment, ",
    "apparent oral, CL/F = metabolic ", sprintf("%.2f", clm), " + renal ",
    sprintf("%.2f", clr), " L/h (9.79 x eGFR/73 x WT/66; Japanese eGFR ",
    sprintf("%.0f", gfr), " mL/min/1.73 m2, ", creatinineNote, "), V/F 6.0 L/kg, ",
    "ka 0.62 /h; CYP2D6 ", cyp2d6, ". https://doi.org/10.1097/FPC.0b013e3283588fe5"
  )
  if (isTRUE(pregnant) && pregnancyGfrFactor(pregnant, creatinine) != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("eGFR x ", PREGNANCY_GFR_FACTOR, " (physiological rise in filtration)")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 600,
    upperTypical = 1000,
    lowerTypical = 200,
    reference = reference
  )
}
