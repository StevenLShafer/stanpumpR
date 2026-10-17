# -----------------------------------------------------------------------------
# Dabigatran: apparent oral two-compartment model with an absorption lag, from
# the RE-LY atrial-fibrillation trial (Liesenfeld 2011)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma dabigatran.  Doses are entered as mg of
# the prodrug dabigatran etexilate (the capsule strength, e.g. 150 mg); the
# parameters are apparent (CL/F, V/F), so the ~6.5% absolute bioavailability of
# dabigatran from the etexilate capsule is already inside them.
#
# SOURCE
# ======
# Liesenfeld K-H, Lehr T, Dansirikul C, et al.  Population pharmacokinetic
# analysis of the oral thrombin inhibitor dabigatran etexilate in patients with
# non-valvular atrial fibrillation from the RE-LY trial.  J Thromb Haemost
# 2011;9:2168-2175.  27 706 concentrations from 9522 patients, 110 or 150 mg
# twice daily.  NONMEM; two-compartment disposition with first-order absorption
# and a lag time.  Table 5, final model, read from the full text on 2026-10-17:
#
#     CLmax/F = 124 L/h     V2/F = 673 L     Q/F = 35.5 L/h     V3/F = 345 L
#     ka = 0.754 /h         ALAG = 0.634 h   F = 1.00
#     EC50_CRCL = 56.7 mL/min    POWER_CRCL = 1.29
#
# Creatinine clearance enters clearance a priori through an Emax (Hill) function
# (Table 5 footnote):
#
#     CL/F = CLmax/F . CRCL^POWER / (EC50^POWER + CRCL^POWER)
#              . (1 + theta_AGE . (AGE - 72)) . theta_ETHN . theta_HF . theta_SEX
#     V2/F = V2 . (1 + theta_WT . (WT - 80.3)) . (1 + theta_HGB . (HGB - 14.3))
#
# with theta_AGE = -0.0041 /year, theta_SEX = 0.917 (female), theta_ETHN = 0.797
# (South Asian), theta_HF = 0.933 (heart failure class II-IV), theta_WT =
# 0.0077 /kg and theta_HGB = -0.0399 per g/dL.  At the RE-LY medians (CRCL
# 68.6 mL/min, age 72, male) CL/F is 69.6 L/h, reproducing the paper's typical
# value.  CRCL is Cockcroft-Gault (the paper's method).
#
# WHAT IS FIXED HERE
# ==================
# The clinical AF patient, on no interacting co-medication.  The library has no
# input for ethnicity, heart failure, haemoglobin or co-medication, so the
# patient is taken to be non-South-Asian (theta_ETHN = 1), without heart failure
# (theta_HF = 1) and at the median haemoglobin 14.3 g/dL (the V2/F haemoglobin
# term is then 1, and haemoglobin had no effect on steady-state exposure
# anyway).  No bioavailability co-medication factor is applied; in RE-LY
# verapamil raised F 23%, amiodarone 12% and PPIs lowered it 12.5%.  A South
# Asian patient has 20.3% lower CL/F (25.5% higher exposure) and one with heart
# failure 6.7% lower CL/F, which this fixed model does not represent.
#
# CREATININE CLEARANCE
# ====================
# Cockcroft-Gault (creatinineClearanceCG(), R/renalFunction.R) in mL/min, from
# the Patient Profile's serum creatinine on the adult scale
# (adultEquivalentCreatinine()); with the field blank it is the ASSUMED NORMAL
# creatinine for age and sex, so renal impairment is then not represented.
# RE-LY excluded CRCL < 30 mL/min, so below that the Hill function extrapolates.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Body weight is the model's own covariate on V2/F only; the paper found no
# weight effect on clearance ("Weight has no effect on AUCss").  V2/F is
# evaluated at the pharmacokinetic weight with the switch on and at total body
# weight with it off.  CL/F, Q/F and V3/F carry no size term in the source and
# take the library's fat-free-mass factors with the switch on; with it off they
# are as published (legacyVolume = 1), so the switch-off position reproduces the
# source's weight-independent clearance.  Weight also enters CL/F through the
# Cockcroft-Gault CLcr, as the published model does.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC
# and no time until threshold: no therapeutic range is established, and a
# simulated level is not an ecarin or dilute-thrombin-time activity or a
# procedure-safe time.
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

DABIGATRAN_CLMAX   <- 124      # L/h, maximum apparent clearance
DABIGATRAN_EC50    <- 56.7     # mL/min, CRCL at half-maximal clearance
DABIGATRAN_POWER   <- 1.29     # Hill coefficient of the CRCL-CL/F relationship
DABIGATRAN_AGE_REF <- 72       # years, reference age
DABIGATRAN_WT_REF  <- 80.3     # kg, reference weight for V2/F

#' Dabigatran pharmacokinetics (apparent oral, atrial fibrillation)
#'
#' Liesenfeld 2011; see the header of \code{R/drugs_dabigatran.R}.  Doses are
#' entered as mg of the prodrug dabigatran etexilate.
#'
#' @inheritParams apixaban
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dabigatran <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  female <- sex == SEX_FEMALE

  crcl <- creatinineClearanceCG(pkW, age, sex,                 # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))

  cl <- DABIGATRAN_CLMAX *
    crcl^DABIGATRAN_POWER / (DABIGATRAN_EC50^DABIGATRAN_POWER + crcl^DABIGATRAN_POWER) *
    (1 - 0.0041 * (age - DABIGATRAN_AGE_REF)) *
    (if (female) 0.917 else 1) *
    size$clearance                                             # L/h

  default <- list(
    v1  = 673 * (1 + 0.0077 * (pkW - DABIGATRAN_WT_REF)),      # V2/F, median Hgb
    v2  = 345 * size$volume,                                   # V3/F
    v3  = 1,                                                   # two compartments
    cl1 = cl / 60,
    cl2 = 35.5 / 60 * size$clearance,                          # Q/F
    cl3 = 0,
    ka_PO = 0.754 / 60,                                        # 1/min
    tlag_PO = 0.634 * 60,                                      # 38.0 min
    bioavailability_PO = 1                                     # apparent scale
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Liesenfeld K-H et al., J Thromb Haemost 2011;9:2168-2175 (apparent oral ",
      "two-compartment model with a 0.634 h lag, RE-LY atrial-fibrillation ",
      "patient on no interacting co-medication, non-South-Asian, no heart ",
      "failure, median haemoglobin; Emax creatinine-clearance effect on ",
      "clearance; Cockcroft-Gault creatinine clearance from the entered ",
      "creatinine or an assumed normal value for age and sex; doses as mg ",
      "dabigatran etexilate). https://doi.org/10.1111/j.1538-7836.2011.04498.x"
    )
  )
}
