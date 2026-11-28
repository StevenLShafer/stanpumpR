# -----------------------------------------------------------------------------
# Teicoplanin: Kim 2024, healthy adults, three compartments (a candidate of
# the teicoplanin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-27, on Steven L. Shafer's decisions of that
# day: Kim 2024 for every adult and Zhang 2020 for children
# (R/drugs_teicoplaninZhang.R).  Wang 2023 (Drug Des Devel Ther 17:2259-2271,
# PMID 37546521, critically ill adults) was read in full and REJECTED: its
# assay measured the A2-2 component only ("We chose A2-2 to be detected in
# place of all components", about half of the drug) and does not say that it
# was scaled to total teicoplanin, and its clearance is about twice Kim's.
#
# THE SOURCE
# ==========
# Kim 2024 (Pharmaceutics 16:499, PMID 38675160, doi
# 10.3390/pharmaceutics16040499), read in full from PubMed Central
# (PMC11053983) on 2026-11-27.  12 healthy Korean adults (6 men, 6 women;
# inclusion 19-55 years; Table 1 medians: age 32, height 164 cm, weight
# 67.9 kg, BMI 24.8); 200 mg over 30 min; venous plasma at 33, 36, 45 and
# 90 min, 4, 8, 48-120 and 168-240 h; HPLC-MS/MS; NONMEM 7.5, FOCE-I,
# ADVAN11.  Table 2:
#
#     CL = 0.693 L/h x (CE / 105.27)^0.785
#     V1 = 3.96 L,  Q2 = 4.45 L/h,  V2 = 8.24 L,  Q3 = 1.76 L/h
#     V3 = 69.8 L x (WT / 67.85)^1.73
#
# CE is the CKD-EPI creatinine eGFR (the 2021 equation, Table 1 footnote c,
# egfrCKDEPI2021()) multiplied by BSA/1.73 (footnote e), in mL/min.
# [source] The paper does not name its BSA formula; Du Bois (bsaDuBois()) is
# used.  Table 3: terminal half-life 103 h (mean), against the NCA's 54.6 h
# over a sampling that ended at 168-240 h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL's renal term already carries size (a de-indexed eGFR), so it takes no
# size factor; V3 keeps its weight term at the pharmacokinetic weight (total
# weight with the switch off).  V1, Q2, V2 and Q3 have no size term and take
# the FFM factors anchored at the cohort (cohortSizeFactors(), Table 1's
# medians).  With the switch off they are the published values.
#
# PREGNANCY
# =========
# No pharmacokinetic study of teicoplanin in pregnancy was found (PubMed,
# 2026-11-27).  Teicoplanin is cleared by the kidneys, so the rise in
# filtration of late pregnancy is applied to the eGFR when the serum
# creatinine is blank (pregnancyGfrFactor()).
#
# BAND AND THRESHOLD: the trough range of Abdul-Aziz 2020 and the MIC line,
# antibioticTroughBands() and antibioticMicTable() (R/antibioticMbc.R,
# R/antibioticThresholds.R).
#
# DOSE BASIS: teicoplanin as labelled (Targocid vials).
# -----------------------------------------------------------------------------

TEICOPLANIN_KIM_CL       <- 0.693   # L/h at CE 105.27 mL/min
TEICOPLANIN_KIM_CL_CE    <- 0.785
TEICOPLANIN_KIM_CE_REF   <- 105.27  # mL/min
TEICOPLANIN_KIM_V1       <- 3.96    # L
TEICOPLANIN_KIM_Q2       <- 4.45    # L/h
TEICOPLANIN_KIM_V2       <- 8.24    # L
TEICOPLANIN_KIM_Q3       <- 1.76    # L/h
TEICOPLANIN_KIM_V3       <- 69.8    # L at 67.85 kg
TEICOPLANIN_KIM_V3_WT    <- 1.73
TEICOPLANIN_KIM_WT_REF   <- 67.85   # kg

#' Teicoplanin (Kim 2024)
#'
#' Three compartments in healthy adults, clearance on the de-indexed CKD-EPI
#' eGFR and the deep volume on weight; see the header of
#' \code{R/drugs_teicoplaninKim.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for an assumed normal
#'   value for the patient's age and sex
#' @param pregnant TRUE for late pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
teicoplaninKim <- function(weight, height, age, sex, adjustToFFM = TRUE,
                           creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = cohortSizeFactors(weight = 67.9, age = 32, men = 6,
                                                   women = 6, height = 164))
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  ce <- egfrCKDEPI2021(age, sex, adultEquivalentCreatinine(creatinine, age, sex)) *
    bsaDuBois(weight, height) / 1.73 * gfrPregnancy
  cl <- TEICOPLANIN_KIM_CL * (ce / TEICOPLANIN_KIM_CE_REF)^TEICOPLANIN_KIM_CL_CE

  default <- list(
    v1  = TEICOPLANIN_KIM_V1 * size$volume,
    v2  = TEICOPLANIN_KIM_V2 * size$volume,
    v3  = TEICOPLANIN_KIM_V3 * (pkW / TEICOPLANIN_KIM_WT_REF)^TEICOPLANIN_KIM_V3_WT,
    cl1 = cl / 60,
    cl2 = TEICOPLANIN_KIM_Q2 * size$clearance / 60,
    cl3 = TEICOPLANIN_KIM_Q3 * size$clearance / 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kim 2024, Pharmaceutics 16:499, doi 10.3390/pharmaceutics16040499. 12 healthy ",
    "adults, three compartments: CL 0.693 L/h x (CE/105.27)^0.785 (CKD-EPI eGFR x BSA/1.73, ",
    "mL/min), V1 3.96 L, Q2 4.45 L/h, V2 8.24 L, Q3 1.76 L/h, V3 69.8 L x (weight/67.85)^1.73.",
    if (is.null(creatinine)) " Serum creatinine assumed normal for age and sex." else "")
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("eGFR x ", PREGNANCY_GFR_FACTOR, " (physiological rise in filtration)")))
  teicoplaninResult(PK, reference)
}

#' The band and threshold every teicoplanin model returns
#'
#' The trough range of \code{antibioticTroughBands()}; see
#' \code{R/antibioticMbc.R}.
#' @param PK the PK sets
#' @param reference the reference string
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
teicoplaninResult <- function(PK, reference)
{
  band <- antibioticTroughBands()
  band <- band[band$Drug == "teicoplanin", ]
  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = band$Typical,
    upperTypical = band$Upper,
    lowerTypical = band$Lower,
    reference = reference
  )
}
