# -----------------------------------------------------------------------------
# Fludarabine, intravenous, in children and adults (Langenhorst 2019): plasma
# F-ara-A (2-fluoro-ara-A), dosed as fludarabine phosphate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L) of F-ara-A; doses entered in mg of
# fludarabine PHOSPHATE, the approved dose form.
#
# Drafted by Claude Code, 2026-11-07, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C34).
# The parameters were read from the PubMed Central text (Tables 1-2 and Eq.
# 7); the PDF is still to confirm them.  The eGFR units and the dose basis
# are Shafer's decisions (2026-11-07), the latter with the fludarabine
# phosphate injection label he supplied (NDA 022137, revised 12/2010).
#
# THE SOURCE
# ==========
# Langenhorst JB, et al.  Population pharmacokinetics of fludarabine in
# children and adults during conditioning prior to allogeneic hematopoietic
# cell transplantation.  Clin Pharmacokinet 2019;58:627-637 (doi
# 10.1007/s40262-018-0715-9).  258 patients, 0.2-74 years, 4.3-130 kg (134
# aged 20 or under), eGFR 25-140 mL/min/1.73 m2; fludarabine phosphate 40
# (or 10) mg/m2/day over 1 h on days -5 to -2; plasma F-ara-A by LC-MS (LLOQ
# 0.001 mg/L); NONMEM 7.3, FOCE-I.
#
# THE MODEL (Table 2, Eq. 7)
# ==========================
# Three compartments, every parameter allometric in weight about 70 kg:
#
#     CL = (3.24 + 0.782 x eGFR) x (WT/70)^0.75   L/h, eGFR in L/h (below)
#     V1 39 x WT/70,  V2 20 x WT/70,  V3 50 x WT/70   L
#     Q2 8.6 x (WT/70)^0.75,  Q3 3.8 x (WT/70)^0.75   L/h
#
# The abstract prints the non-renal and renal terms as 3.24 and 0.782 x eGFR
# "L/h/70 kg"; Eq. 7's text says eGFR enters "as l/h/kg", which would make
# the renal share 2% against the paper's "approximately 65%".  On Shafer's
# decision (2026-11-07) eGFR is read in L/h (mL/min/1.73 m2 x 0.06), scaled
# per 70 kg with the rest of the clearance: the reading that reproduces the
# 65% renal share (63% at eGFR 120, 67% at 140, 70 kg) and the paper's
# adult median cumulative AUC (26.1 against 26 mg.h/L for 160 mg/m2 at 78
# kg, eGFR 110, 1.9 m2).
#
# eGFR (the Methods)
# ==================
# Cockcroft-Gault (with actual weight) from 17 years in girls and 14 in boys;
# below that Schwartz 1976 (k x height / creatinine: 0.45 under a year, 0.55,
# and 0.70 for a boy of 13), each with the patient's own creatinine
# (patientCreatinine(); blank means the assumed normal for age and sex).
# The result is capped at 140 mL/min/1.73 m2, a cap that rises from 35 at
# birth to 140 at 1.5 years (taken as linear, the paper giving only its ends)
# and falls by 8 per decade after 30 years.  Cockcroft-Gault's mL/min is
# used on the same scale as the paper's mL/min/1.73 m2 without a BSA
# correction, which the paper does not describe.
#
# DOSE BASIS (Shafer, 2026-11-07)
# ===============================
# The dose is fludarabine PHOSPHATE (2-fluoro-ara-AMP, C10H13FN5O7P, 365.2
# g/mol by the label), which is dephosphorylated within minutes to F-ara-A
# (C10H12FN5O4, 285.24 g/mol), the analyte.  The paper does not state its
# dose basis; its AUCs reproduce on the F-ara-A basis (as the phosphate the
# adult AUC would be 33.4 mg.h/L against 26).  So a mg of the phosphate is
# 285.24/365.21 = 0.781 mg of F-ara-A (FLUDARABINE_FARAA_FRACTION; standard
# atomic weights, the phosphate's 365.21 the label's 365.2), carried by
# dividing every volume and clearance by that fraction.  The dephosphorylation
# is taken as instantaneous.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own weight covariate, at the pharmacokinetic weight with the
# switch on and total weight with it off; Cockcroft-Gault takes the same
# weight.  Schwartz uses height.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pregnancy pharmacokinetics found.  About 65% of the clearance is renal,
# so the eGFR takes the rise in filtration of pregnancy (x 1.5,
# pregnancyGfrFactor(), only when the creatinine is blank), after the cap.
#
# NO EFFECT SITE, NO BAND: the paper's target is a cumulative AUC (an
# exposure), not a concentration.
# -----------------------------------------------------------------------------

# Langenhorst 2019, Table 2 and the abstract: per 70 kg (L, L/h)
FLUDARABINE_LANGENHORST <- list(
  clNonRenal = 3.24, renalSlope = 0.782,
  v1 = 39, v2 = 20, v3 = 50, q2 = 8.6, q3 = 3.8,
  egfrMax = 140, egfrBirth = 35, egfrMaturedAge = 1.5,   # mL/min/1.73 m2, years
  egfrDeclineAge = 30, egfrDeclinePerYear = 0.8
)

# F-ara-A over fludarabine phosphate (285.24 / 365.21 g/mol)
FLUDARABINE_FARAA_FRACTION <- 285.24 / 365.21

#' The eGFR of Langenhorst 2019, mL/min/1.73 m2
#'
#' Cockcroft-Gault from 17 years (girls) or 14 (boys), Schwartz 1976 below,
#' with the paper's age-dependent cap.
#' @keywords internal
fludarabineEGFR <- function(weight, height, age, sex, scr)
{
  p <- FLUDARABINE_LANGENHORST
  child <- age < (if (sex == SEX_FEMALE) 17 else 14)
  egfr <- if (child) {
    k <- if (age < 1) 0.45 else if (sex == SEX_MALE && age >= 13) 0.70 else 0.55
    k * height / scr
  } else creatinineClearanceCG(weight, age, sex, scr)
  cap <- if (age < p$egfrMaturedAge)
    p$egfrBirth + (p$egfrMax - p$egfrBirth) * age / p$egfrMaturedAge
  else if (age > p$egfrDeclineAge)
    p$egfrMax - p$egfrDeclinePerYear * (age - p$egfrDeclineAge)
  else p$egfrMax
  min(egfr, cap)
}

#' Fludarabine, intravenous, in children and adults (Langenhorst 2019)
#'
#' Three compartments allometric in weight, eGFR on clearance; doses entered
#' as fludarabine phosphate, the curve F-ara-A.  See the header of
#' \code{R/drugs_fludarabine.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the eGFR by the rise in filtration of pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fludarabine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, pregnant = FALSE)
{
  p <- FLUDARABINE_LANGENHORST
  f <- FLUDARABINE_FARAA_FRACTION
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- patientCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  egfr <- fludarabineEGFR(w, height, age, sex, scr) * gfrPregnancy   # mL/min/1.73 m2
  wv <- w / 70
  wc <- wv^0.75
  cl <- (p$clNonRenal + p$renalSlope * egfr * 0.06) * wc                # L/h, F-ara-A

  default <- list(
    v1  = p$v1 * wv / f,
    v2  = p$v2 * wv / f,
    v3  = p$v3 * wv / f,
    cl1 = cl / 60 / f,
    cl2 = p$q2 * wc / 60 / f,
    cl3 = p$q3 * wc / 60 / f
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Langenhorst JB, et al. Population pharmacokinetics of fludarabine in children ",
    "and adults during conditioning prior to allogeneic hematopoietic cell ",
    "transplantation. Clin Pharmacokinet 2019;58:627-637. 258 patients 0.2-74 years: ",
    "three compartments, CL (3.24 + 0.782 x eGFR [L/h]) x (WT/70)^0.75 L/h, V1 39, V2 ",
    "20, V3 50 L per 70 kg, Q2 8.6, Q3 3.8 L/h per 70 kg^0.75; eGFR Cockcroft-Gault ",
    "or Schwartz with the patient's own or an assumed normal creatinine, capped by ",
    "age. Doses as fludarabine phosphate (365.2 g/mol), the model run in F-ara-A ",
    "(285.24 g/mol, x 0.781); plasma F-ara-A",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "eGFR x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1007/s40262-018-0715-9"
  )

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
