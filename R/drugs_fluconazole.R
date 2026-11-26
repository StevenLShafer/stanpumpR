# -----------------------------------------------------------------------------
# Fluconazole: Vuong 2025, critically ill adults, two compartments, with oral
# absorption from McLachlan 1996 and the Diflucan label's bioavailability
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-26.  Steven L. Shafer supplied the paper and
# its ESM1 (with the NONMEM control stream) and decided that day that this
# model is the library's fluconazole for every adult, and that the oral route
# is offered on its disposition.
#
# THE SOURCE
# ==========
# Vuong ML, Elkayal O, et al. A fluconazole population pharmacokinetics study
# to improve target attainment in critically ill patients. Infection 2025
# (PMID 41085604, doi 10.1007/s15010-025-02663-0).  177 ICU patients from
# eight studies (Belgium, the Netherlands, Germany, Australia), intravenous
# fluconazole, 1616 concentrations; NONMEM 7.5, FOCE-I, ADVAN13; 70 multiply
# imputed datasets pooled by Rubin's rules.  Table 2 (final model, pooled) and
# Equations 3-4, matching the ESM1 control stream:
#
#     CL (off CRRT) = 0.614 L/h x (eGFR / 80 mL/min/1.73 m2)^0.532
#     CL (on CRRT)  = 1.56 L/h                    (no covariate)
#     Vc            = 39.4 L x (TBW / 80 kg)^0.908
#     Q  = 12.1 L/h,   Vp = 8.47 L                 (no covariates)
#
# eGFR is CKD-EPI "without race", the authors citing Sharma 2022 (Clin
# Pharmacol Ther 112:946), which recommends the 2021 refit, so it is
# egfrCKDEPI2021() on the adult-equivalent creatinine (an assumed normal one
# when the field is blank).  No cap is applied in the source (ESM1); its eGFR
# off CRRT ran from 7 to 213 mL/min/1.73 m2, its weights from 34 to 142 kg.
#
# CRRT: the Patient Profile has no renal replacement field, so the off-CRRT
# clearance is used for every patient.  On CRRT, Vuong's clearance is 1.56 L/h,
# 2.5 times the typical off-CRRT value; a patient on CRRT is drawn too high.
#
# THE ICU AND THE HEALTHY VOLUNTEER
# =================================
# The Diflucan label (Roerig, rev. 2026): "Mean body clearance in adults is
# reported to be 0.23 (17%) mL/min/kg", 0.97 L/h at 70 kg, and a terminal
# half-life of about 30 hours (20-50).  This model gives 0.61 L/h at an eGFR of
# 80 (about 40% less) and a terminal half-life near 54 hours at 80 kg.
# Clearance is the parameter to compare (AGENTS.md), and the difference is
# real: critically ill patients clear fluconazole more slowly than healthy
# volunteers at the same creatinine-based eGFR, which overstates filtration in
# the ICU.  Shafer chose this model for every adult on 2026-11-26 knowing it;
# for an outpatient with healthy kidneys the curve is likely too high.
#
# THE ORAL ROUTE
# ==============
# Vuong had intravenous data only.  Bioavailability is the label's: "In normal
# volunteers, the bioavailability of orally administered fluconazole is over
# 90% compared with intravenous administration", taken as 0.9.  The
# absorption rate is McLachlan and Tett 1996 (Br J Clin Pharmacol 41:291,
# PMID 8730974; 113 men with HIV, oral and intravenous doses), Table 2's
# P-PHARM estimate, 3.07 /h (NONMEM's 5.02 /h carried a CV of 234%).  No lag.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Vc keeps the source's weight term, evaluated at the pharmacokinetic weight
# with the switch on (at the total body weight with it off).  The clearance's
# only covariate is an eGFR indexed to 1.73 m2, which is not size, so it also
# takes the clearance factor; Q and Vp, with no size term, take the FFM
# factors.  BODY SIZE ANCHOR: these are anchored at the reference man, not at
# Vuong's cohort.  Table 1 prints the cohort's median weight (80 kg), age (62
# years) and sexes (115 men of 177), but no height or BMI, and the cohort
# spans four countries, so its typical height would have to be assembled from
# NCD-RisC's heights for each, which this session did not do.  With the
# switch off, CL, Q and Vp are the published values (legacy factors 1).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No measured pharmacokinetics in pregnancy (PubMed, 2026-11-26: a PBPK model
# in Chinese pregnant women, Song 2020, and case reports).  Fluconazole is
# cleared mainly unchanged in urine (the label: about 80%), so the 50% rise in
# filtration of late pregnancy is applied to the eGFR term when the
# creatinine is blank (pregnancyGfrFactor()): clearance x 1.5^0.532 = 1.24.
#
# EFFECT SITE AND BAND
# ====================
# No effect site and no band: Abdul-Aziz 2020 (Intensive Care Med 46:1127,
# read in full) gives AUC/MIC targets only and neither recommends nor
# discourages fluconazole TDM.  Vuong's own toxicity threshold, a trough of
# 80 mg/L, is not a range.
#
# DOSE BASIS
# ==========
# Doses are fluconazole as labelled (tablets, suspension, injection).
# -----------------------------------------------------------------------------

FLUCONAZOLE_CL      <- 0.614   # L/h, off CRRT, at eGFR 80
FLUCONAZOLE_CL_EGFR <- 0.532   # exponent on eGFR / 80
FLUCONAZOLE_EGFR_REF <- 80     # mL/min/1.73 m2
FLUCONAZOLE_VC      <- 39.4    # L at 80 kg
FLUCONAZOLE_VC_WT   <- 0.908   # exponent on TBW / 80
FLUCONAZOLE_WT_REF  <- 80      # kg
FLUCONAZOLE_Q       <- 12.1    # L/h
FLUCONAZOLE_VP      <- 8.47    # L
FLUCONAZOLE_KA      <- 3.07    # /h, McLachlan 1996 Table 2 (P-PHARM)
FLUCONAZOLE_F       <- 0.9     # Diflucan label, "over 90%"

#' Fluconazole (Vuong 2025)
#'
#' Two compartments in critically ill adults, the clearance on the CKD-EPI
#' 2021 eGFR and the central volume on weight, with oral absorption from
#' McLachlan 1996 and the label's bioavailability; see the header of
#' \code{R/drugs_fluconazole.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for an assumed normal
#'   value for the patient's age and sex
#' @param pregnant TRUE for late pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fluconazole <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  egfr <- egfrCKDEPI2021(age, sex, adultEquivalentCreatinine(creatinine, age, sex)) *
    gfrPregnancy
  cl <- FLUCONAZOLE_CL * (egfr / FLUCONAZOLE_EGFR_REF)^FLUCONAZOLE_CL_EGFR *
    size$clearance
  vc <- FLUCONAZOLE_VC * (pkW / FLUCONAZOLE_WT_REF)^FLUCONAZOLE_VC_WT

  default <- list(
    v1  = vc,
    v2  = FLUCONAZOLE_VP * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = cl / 60,
    cl2 = FLUCONAZOLE_Q * size$clearance / 60,
    cl3 = 0,
    ka_PO = FLUCONAZOLE_KA / 60,
    bioavailability_PO = FLUCONAZOLE_F,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Vuong ML, Elkayal O, et al. A fluconazole population pharmacokinetics ",
    "study to improve target attainment in critically ill patients. ",
    "Infection 2025, doi 10.1007/s15010-025-02663-0. 177 ICU patients, two ",
    "compartments: CL 0.614 L/h x (eGFR/80)^0.532 (CKD-EPI 2021, off CRRT), ",
    "Vc 39.4 L x (weight/80)^0.908, Q 12.1 L/h, Vp 8.47 L. Clearance about ",
    "40% below the Diflucan label's healthy adults. ",
    if (is.null(creatinine)) "Serum creatinine assumed normal for age and sex. " else "",
    "Oral: F 0.9 (Diflucan label), ka 3.07 /h (McLachlan AJ, Tett SE. ",
    "Br J Clin Pharmacol 1996;41:291-298)."
  )
  if (isTRUE(pregnant) && gfrPregnancy != 1)
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("eGFR x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))

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
