# -----------------------------------------------------------------------------
# Lixisenatide: one compartment, first-order subcutaneous absorption (Adlyxin)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# ng, concentrations in pg/mL (= ng/L, the paper's unit).  Concentration
# only: no glucose, insulin or HbA1c endpoint.  Drafted by Claude Code,
# 2026-10-17, at the request of Steven L. Shafer; see the antidiabetic
# registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Frank T.  Population pharmacokinetics of lixisenatide, a once-daily human
# glucagon-like peptide-1 receptor agonist, in healthy subjects and in
# patients with type 2 diabetes.  J Pharm Drug Deliv Res 2013;2:1.  1223
# subjects in 7 studies (2 phase 1, 2 phase 2, 3 phase 3), single and
# multiple subcutaneous doses; antibody-positive visits excluded.  One
# compartment with first-order absorption (ADVAN2/TRANS1), parameterised by
# the mean absorption time, MAT = 1/ka.  Table 2 (supplied by Steven L.
# Shafer as an image; the web print collapses it):
#
#     CL/F = 38.6 L/h x (1 + 0.099 x (CLCR - 6.21 L/h))
#     V/F  = 41.2 L x (WT / 82.1 kg)^1.56
#     MAT  = 3.25 h x (Dose / 15 mcg)^0.236
#
# CLCR is the Cockcroft-Gault creatinine clearance in L/h (6.21 L/h, the
# population median, is 103.5 mL/min).  The text: CL/F falls 9.9% per 1 L/h
# below the median, and "moderate renal impairment (CLCR of 1.8 to 3.0 l/h)
# may ... have a 1.5- to 1.8-fold increase in exposure" (the model: 1.47 and
# 1.78).  MAT, 3.25 h, is longer than the elimination time V/CL, 1.07 h, so
# the terminal phase is absorption (flip-flop), t1/2 = ln 2 x MAT, about 2.3 h.
#
# THE DOSE TERM.  MAT depends on the size of each injection, a property of
# the dose, not the patient, so the model takes `dose` (mg per injection, as
# simCpCe() passes it) and declares doseDependent: simCpCe() runs each
# injection size as its own linear system with its own ka (doseSizePK(),
# R/getDrugPK.R) and adds them.  With no dose the reference, 15 mcg, is used
# (the help tables).  10 and 20 mcg, the marketed doses, give MAT 2.95 and
# 3.48 h.  The term is held to the studied doses, 5 to 30 mcg per injection
# (Table 1).
#
# RENAL FUNCTION: Cockcroft-Gault (creatinineClearanceCG()), as the paper, at
# the patient's creatinine (a child's on the adult scale) or an assumed normal
# creatinine for age and sex when the field is blank.  The linear term is not
# capped (the paper gives none); at CLCR 0 CL/F is 38% of the median.
#
# BODY SIZE: the model's own weight covariate on V/F, and the weight in the
# Cockcroft-Gault clearance, evaluated at the pharmacokinetic weight with the
# switch on and at total weight with it off.  The weight on V/F is held to
# the studied range, 43.8 to 163 kg (Table 1, all participants; supplied by
# Steven L. Shafer).  Table 1 gives no numeric range of CLCR, only that the
# renal study enrolled severe impairment (CLCR below 1.8 L/h), so CLCR is not
# held; nor is age, which is not a covariate (studied 20 to 82 years).
#
# NOT APPLIED.  Anti-lixisenatide antibodies raise exposure markedly; the
# model describes antibody-negative patients only (the paper excluded every
# antibody-positive or unknown visit).
#
# CHECKS: V/CL 41.2 / 38.6 = 1.07 h (paper: 1.07); the renal fold changes
# above; a single 20 mcg dose at the medians peaks at 88 pg/mL 1.8 h after
# the dose, and AUC is dose / CL/F (520 pg.h/mL at 20 mcg).  See
# tests/testthat/test-drugs-lixisenatide.R.  Scheen 2015 (Clin Pharmacokinet
# 54:1-21) tabulates a dedicated renal study (Liu et al., 5 mcg once) with
# CL/F 20.0, 19.0, 16.0 and 14.0 L/h for normal, mild, moderate and severe
# impairment: a lower clearance than this pooled model's at a quarter of its
# reference dose, which the help page notes.
#
# References
# ----------
# Frank T, J Pharm Drug Deliv Res 2013;2:1.  https://doi.org/10.4172/2325-9604.1000112
# -----------------------------------------------------------------------------

LIXISENATIDE_CLCR_MEDIAN <- 6.21      # L/h
LIXISENATIDE_WEIGHT_MEDIAN <- 82.1    # kg
LIXISENATIDE_DOSE_REF <- 15           # mcg per injection
LIXISENATIDE_WEIGHT_RANGE <- c(43.8, 163)   # kg, studied (Table 1)
LIXISENATIDE_DOSE_RANGE <- c(5, 30)         # mcg per injection, studied

#' Lixisenatide pharmacokinetics (subcutaneous)
#'
#' One compartment with first-order absorption whose mean absorption time
#' depends on the dose (Frank 2013); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param dose the injection, mg, or NULL for the reference 15 mcg
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lixisenatide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL, dose = NULL)
{
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  clcr <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex)) *
    60 / 1000                                                   # L/h
  mcg  <- if (is.null(dose)) LIXISENATIDE_DOSE_REF else dose * 1000
  mcg  <- min(max(mcg, LIXISENATIDE_DOSE_RANGE[1]), LIXISENATIDE_DOSE_RANGE[2])
  wV   <- min(max(pkW, LIXISENATIDE_WEIGHT_RANGE[1]), LIXISENATIDE_WEIGHT_RANGE[2])

  cl  <- 38.6 * (1 + 0.099 * (clcr - LIXISENATIDE_CLCR_MEDIAN))   # L/h
  mat <- 3.25 * (mcg / LIXISENATIDE_DOSE_REF)^0.236               # h

  default <- list(
    v1  = 41.2 * (wV / LIXISENATIDE_WEIGHT_MEDIAN)^1.56, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = 1 / (mat * 60),
    bioavailability_SC = 1,   # apparent parameters
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    doseDependent = TRUE,
    reference = paste0(
      "Frank 2013 (1223 healthy subjects and patients with type 2 diabetes, 7 ",
      "studies): one compartment, CL/F 38.6 L/h x (1 + 0.099 (Cockcroft-Gault CLcr ",
      "- 6.21 L/h)) from the entered or an assumed normal creatinine, V/F 41.2 L x ",
      "(weight/82.1)^1.56, first-order absorption with mean absorption time 3.25 h ",
      "x (dose/15 mcg)^0.236; antibody-negative patients; concentration only. ",
      "https://doi.org/10.4172/2325-9604.1000112"
    )
  )
}
