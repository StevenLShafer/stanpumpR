# -----------------------------------------------------------------------------
# Procainamide, intravenous (Grasela 1984): plasma procainamide in patients
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Equations 5-12, Tables II-IV and Figure 2.
#
# THE SOURCE
# ==========
# Grasela TH, Sheiner LB.  Population pharmacokinetics of procainamide from
# routine clinical data.  Clin Pharmacokinet 1984;9:545-554
# (doi 10.2165/00003088-198409060-00004).  Thirty-nine inpatients (34 men,
# 24 with mild congestive heart failure; age 59.7 +/- 13.7 years, weight
# 72.4 +/- 13.5 kg, serum creatinine 1.22 +/- 0.34 mg/dL), 116 routine plasma
# concentrations and 14 urine collections, oral and intravenous dosing, one
# compartment, NONMEM.  The final model (Table III) for a standard individual
# -- 70 kg, 170 cm, creatinine clearance 120 mL/min, i.e. a healthy
# 20-year-old man -- is
#
#     CL_R  14.4 L/h  x SIZE x FR x (1 - 0.11 CHF)     renal
#     CL_A  10.1 L/h  x SIZE      x (1 - 0.11 CHF)     acetylation to NAPA
#     CL_O   1.2 L/h  x SIZE                           other metabolism
#     Vd   136.0 L    x SIZE
#
# with SIZE = WT/70 (the weight power was 0.78 but not distinguishable from 1,
# and height added nothing, Table II), and the fraction of normal renal
# function from Equation 7,
#
#     FR = (140 - AGE) / ((113 + 20 GEN) x SCr),   GEN = 1 for a man
#
# THE SEX TERM, AS PRINTED
# ========================
# The paper calls Equation 7 "a modification of the Cockcroft and Gault
# equation", but as printed it gives a WOMAN 18% more renal clearance than a
# man at the same creatinine (133/113), where Cockcroft-Gault gives her 15%
# less.  The paper's own Table IV settles which form was fitted: its average
# patient (72.4 kg, creatinine 1.2, 59.7 years, a man) has CL_R 7.2 L/h, and
# the printed equation gives 7.49 (7.25 without the size factor) where the
# Cockcroft-Gault direction gives 8.82.  So the equation is used as printed,
# which is the model that was fitted; only 5 of the 39 patients were women,
# so the female branch rests on very little and is flagged on the help page.
#
# DOSE BASIS
# ==========
# The model's doses are procainamide BASE: Figure 2 simulates "1000 mg
# procainamide hydrochloride (850 mg procainamide base)" every 6 hours, and
# its curve (peak about 7.3, trough about 2.7 mg/L) is what the model gives
# for 850 mg (7.34 and 2.70), not for 1000 mg (8.63 and 3.17).  Injections are
# labelled as the hydrochloride, so a dose here is entered as labelled and
# every volume and clearance is divided by the base fraction 235.33/271.79
# = 0.866 -- exact for a linear model, as for azilsartan
# (R/drugs_azilsartan.R).
#
# WHAT IS FIXED, AND WHAT IS NOT MODELLED
# =======================================
# Congestive heart failure (CHF) is held at 0: the Patient Profile has no
# such field, and the effect is small (11% on renal and acetylation
# clearance, none on volume or other clearance).  The bimodal acetylator
# phenotype could not be resolved from routine data and is not a covariate,
# so CL_A is the population's average.  N-acetylprocainamide (NAPA), itself
# active, is not plotted: the source modelled only its urinary excretion.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine (mg/dL, through
# adultEquivalentCreatinine(), so a child's creatinine is put on the adult
# scale), or an assumed normal creatinine for age and sex when blank.  The
# fraction is not capped.  The 1984 creatinine assay (Jaffe) read somewhat
# higher than today's enzymatic methods, which would make this model's renal
# clearance slightly low for a modern creatinine.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter is linear in the model's own weight term, so it is
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, which reproduces the
# published equations exactly.  The renal fraction, like Cockcroft-Gault
# here, is for a patient "if he or she weighed 70 kg" and carries no weight.
#
# WHAT THIS MODEL IS NOT
# ======================
# One compartment fitted to routine, mostly oral, steady-state data; the
# early distribution phase of an intravenous loading dose (a half-life of
# about 10 minutes in children, Singh 1982) is not in it, so a fast bolus
# overstates the late concentration and understates the early one.  Give the
# loading dose as an infusion over 25-30 minutes, as labelled.  No effect
# site and no band; the usual therapeutic range of 4-10 mg/L is not drawn.
# -----------------------------------------------------------------------------

# Grasela 1984, Table III (standard individual, base)
PROCAINAMIDE_CLR <- 14.4    # L/h
PROCAINAMIDE_CLA <- 10.1    # L/h
PROCAINAMIDE_CLO <- 1.2     # L/h
PROCAINAMIDE_VD  <- 136.0   # L
PROCAINAMIDE_CHF <- 0.11    # fractional fall in CL_R and CL_A with CHF
# Equation 7's denominator: 113 + 20 x GEN, GEN = 1 for a man, as printed
PROCAINAMIDE_FR_BASE <- 113
PROCAINAMIDE_FR_MALE <- 20
# Procainamide hydrochloride and base, g/mol
PROCAINAMIDE_HCL_MW  <- 271.79
PROCAINAMIDE_BASE_MW <- 235.33

#' Fraction of normal renal function, Grasela and Sheiner's Equation 7
#'
#' \code{(140 - age) / ((113 + 20 GEN) x SCr)}, with GEN 1 for a man, as
#' printed; see the header of \code{R/drugs_procainamide.R} on its sex term.
#'
#' @param age years
#' @param sex "male" or "female"
#' @param scr serum creatinine, mg/dL
#' @returns the fraction of a healthy 70 kg 20-year-old man's renal function
#' @keywords internal
procainamideRenalFraction <- function(age, sex, scr)
  (140 - age) / ((PROCAINAMIDE_FR_BASE +
                    PROCAINAMIDE_FR_MALE * (sex == "male")) * scr)

#' Procainamide, intravenous (Grasela and Sheiner 1984)
#'
#' One compartment with renal, acetylation and other clearances; doses are
#' the hydrochloride.  See the header of \code{R/drugs_procainamide.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
procainamide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w    <- pkW / 70
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  fr   <- procainamideRenalFraction(age, sex, scr)
  chf  <- 0
  # A labelled hydrochloride dose carries this fraction of base
  base <- PROCAINAMIDE_BASE_MW / PROCAINAMIDE_HCL_MW

  clr <- PROCAINAMIDE_CLR * w * fr * (1 - PROCAINAMIDE_CHF * chf)
  cla <- PROCAINAMIDE_CLA * w * (1 - PROCAINAMIDE_CHF * chf)
  clo <- PROCAINAMIDE_CLO * w

  default <- list(
    v1  = PROCAINAMIDE_VD * w / base,
    v2  = 1,                        # one compartment
    v3  = 1,
    cl1 = (clr + cla + clo) / 60 / base,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")

  reference <- paste0(
    "Grasela TH, Sheiner LB, Clin Pharmacokinet 1984;9:545-554. ",
    "One compartment in 39 patients: renal 14.4 L/h x renal fraction, ",
    "acetylation 10.1 L/h, other 1.2 L/h, Vd 136 L for a 70 kg 20-year-old ",
    "man, linear in weight; no heart failure; doses as the hydrochloride ",
    "(the model's base x 271.79/235.33); ", creatinineNote, ". NAPA is not ",
    "plotted. https://doi.org/10.2165/00003088-198409060-00004"
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
