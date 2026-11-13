# -----------------------------------------------------------------------------
# Tranexamic acid (Lanoiselee 2018): two compartments, creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mg/L (mcg/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common perioperative drugs the library lacked.  Every number
# below was read from the full text of the source in PubMed Central
# (PMC5777664: Table 1, Table 2, the Methods and the Discussion).
#
# THE SOURCE
# ==========
# Lanoiselee J, Zufferey PJ, Ollier E, Hodin S, Delavenne X.  Is
# tranexamic acid exposure related to blood loss in hip arthroplasty?  A
# pharmacokinetic-pharmacodynamic study.  Br J Clin Pharmacol 2018;84:310-319
# (doi 10.1111/bcp.13460).  166 adults having primary total hip arthroplasty
# (age 66, 24-94 years; weight 75, 37-125 kg; 52% women; preoperative
# creatinine 72, 36-272 umol/L; Cockcroft-Gault creatinine clearance 86,
# 15-204 mL/min), given 1 g as an "instantaneous intravenous bolus" after the
# start of anaesthesia, then 1 g over 8 h or placebo.  827 venous plasma
# concentrations at 3 and 20 min, the end of surgery, 3 h and 8 h, by LC-MS/MS
# (linear 0.8-200 mg/L; none below it).  Monolix, two compartments.  Table 2:
#
#     CL = 5.32 L/h x (CrCL / 85)^0.548
#     Vc = 6.09 L   x (Wt / 70)
#     Q  = 23.3 L/h            Vp = 9.55 L
#
# with CrCL the Cockcroft-Gault creatinine clearance in mL/min "using Wt as
# the body size descriptor" and Wt the total body weight.  Weight is on the
# central volume only (lean body weight "did not improve the fit").  Renal
# impairment was in the data (creatinine clearance down to 15 mL/min), which
# is why, the authors write, this model found the renal covariate that the
# earlier adult models (Grassin-Delyle 2013, Dowd 2002) did not.
#
# Checked against the label (Cyklokapron, NDA 019281, revised 08/2025):
# "initial volume of distribution is about 9 to 12 liters", "plasma clearance
# (110 to 116 mL/min)", "half-life of about 2 hours", more than 95% excreted
# unchanged in the urine.  The model gives a central volume of 6.1 L and a
# clearance of 89 mL/min at 70 kg and a creatinine clearance of 85 mL/min,
# and a terminal half-life of 2.2 h, against the label's 110-116 mL/min,
# for which the label names no population or renal function (the reference
# man's own Cockcroft-Gault clearance, 113 mL/min, gives 104 mL/min).
#
# RENAL FUNCTION
# ==============
# Creatinine clearance by Cockcroft-Gault (creatinineClearanceCG()), from the
# Patient Profile's serum creatinine through adultEquivalentCreatinine(), so a
# child's is put on the adult scale; blank means an assumed normal creatinine
# for age and sex (0.9 mg/dL for an adult man, 0.8 for a woman).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate (Vc, and Cockcroft-Gault on CL),
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, which reproduces the
# published equations.  Q and Vp, which the source left unscaled, are scaled
# by the library's factors with the switch on and fixed with it off.
#
# NOT MODELLED
# ============
# Cardiopulmonary bypass: these patients were not on bypass, where
# haemodilution, hypothermia and the circuit change the kinetics (Dowd 2002
# found the elimination rate constant 36% lower during bypass); see the help
# page.  Oral and intramuscular routes are not offered: no adult absorption
# model could be read in full (Grassin-Delyle 2022, abstract only).
#
# THRESHOLD
# =========
# No therapeutic range is established.  10 mg/L, the in vitro concentration
# for about 80% inhibition of fibrinolysis that the source calls "the
# therapeutic target", is the time-until-threshold level, so the plot gives
# the time the concentration stays above it.  The source found no relation
# between that time and blood loss.
#
# PREGNANCY
# =========
# Tranexamic acid is cleared by glomerular filtration, more than 95% unchanged
# in urine, and this model reads renal function, so the rise in filtration of
# late pregnancy is applied through pregnancyGfrFactor() (x1.5 on creatinine
# clearance when the serum creatinine is blank).  At the reference woman (70
# kg, 165 cm, 30 years, switch on) this raises clearance 1.25-fold, from 5.5
# to 6.9 L/h.  Women at caesarean delivery cleared more: 9.4 L/h (Li 2021,
# 30 women, given it at cord clamping) and 8.6 L/h per 80 kg (Dunn 2025, 211
# women); no study compared them with the same women when not pregnant, so
# nothing beyond the filtration rise is applied.
# -----------------------------------------------------------------------------

# Lanoiselee 2018, Table 2
TRANEXAMIC_CL       <- 5.32    # L/h at a creatinine clearance of 85 mL/min
TRANEXAMIC_CRCL_EXP <- 0.548
TRANEXAMIC_CRCL_REF <- 85      # mL/min
TRANEXAMIC_VC       <- 6.09    # L at 70 kg
TRANEXAMIC_Q        <- 23.3    # L/h
TRANEXAMIC_VP       <- 9.55    # L

#' Tranexamic acid (Lanoiselee 2018)
#'
#' Two compartments with creatinine clearance on clearance and weight on the
#' central volume; see the header of \code{R/drugs_tranexamicAcid.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tranexamicAcid <- function(weight, height, age, sex, adjustToFFM = TRUE,
                           creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy

  default <- list(
    v1  = TRANEXAMIC_VC * pkW / 70,
    v2  = TRANEXAMIC_VP * size$volume,
    v3  = 1,                          # placeholder: two compartments
    cl1 = TRANEXAMIC_CL / 60 * (crcl / TRANEXAMIC_CRCL_REF)^TRANEXAMIC_CRCL_EXP,
    cl2 = TRANEXAMIC_Q / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")

  reference <- paste0(
    "Lanoiselee J et al., Br J Clin Pharmacol 2018;84:310-319: two ",
    "compartments in 166 adults having hip arthroplasty, CL 5.32 L/h x ",
    "(Cockcroft-Gault CrCL/85)^0.548, Vc 6.09 L x (Wt/70), Q 23.3 L/h, ",
    "Vp 9.55 L; ", creatinineNote, ". https://doi.org/10.1111/bcp.13460"
  )
  if (isTRUE(pregnant) && gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR,
             " (physiological rise in filtration)")))
  }

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
