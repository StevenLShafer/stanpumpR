# -----------------------------------------------------------------------------
# Sotalol (PEAKS, Yellepeddi 2025): two compartments, creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is mcg/L (ng/mL).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read in full: the source in PubMed Central
# (PMC12001255: Methods, Table 1, Table 2, Table 3), the FDA Clinical
# Pharmacology Review of NDA 22-306 (2009; its Table 9 read on the page image)
# and the Betapace and sotalol hydrochloride injection labels.
#
# THE DISPOSITION
# ===============
# Yellepeddi VK, Ismail M, Bunch TJ, et al.  Population pharmacokinetics and
# pharmacodynamics of sotalol following expedited intravenous loading in
# patients with atrial arrhythmias.  CPT Pharmacometrics Syst Pharmacol
# 2025;14:658-667 (doi 10.1002/psp4.13302), from the PEAKS registry.  22 adults with atrial fibrillation
# or flutter (age 69, 48-79; weight 104, 68.7-185 kg; 19 men; serum
# creatinine 1.0, 0.7-1.26 mg/dL; Cockcroft-Gault creatinine clearance 92.4,
# 64.3-306 mL/min) given 60-120 mg intravenously over 1 h by the package
# insert, then two oral doses 12 h apart; 99 plasma samples by UPLC-MS/MS
# (LLOQ 5 ng/mL).  NONMEM, two compartments.  Table 2:
#
#     CL = 9.5 L/h x (CrCl / 92.4)^0.7     (CrCl by Cockcroft-Gault, mL/min)
#     Vc = 38.2 L x (WT / 104)^1.1
#     Q  = 71.3 L/h        Vp = 89.1 L
#
# Sotalol is not metabolised and is excreted unchanged in the urine, so the
# renal term is its whole clearance.  The paper's oral absorption cannot be
# used: Ka 0.2 /h and an "oral bioavailability relative to IV" of 1.6, which
# is impossible, both from oral samples drawn only 2-4 h after each dose.
#
# THE ORAL ROUTE
# ==============
# Bioavailability from the Betapace label (NDA 019865, 2023): "In healthy
# subjects, the oral bioavailability of sotalol is 90 to 100%", so F = 1.
# Absorption from the FDA reviewer's own population model of the oral and
# intravenous crossover in 15 healthy subjects behind the injection's approval
# (Clinical Pharmacology Review, NDA 22-306, 2009, Table 9): ka 0.605 /h,
# lag 0.231 h.  Its disposition (CL 12.0 L/h, Vc 77.1 L at 70 kg, no renal
# term) is not used, because its weight exponents (1.64 on CL, 2.41 on Vc)
# came from 60-80 kg volunteers and do not extrapolate.  The label: "peak
# plasma concentrations are reached in 2.5 to 4 hours"; this model peaks at
# about 3 h.
#
# DOSE BASIS
# ==========
# Every US product is SOTALOL HYDROCHLORIDE (Betapace tablets; the injection,
# "150 mg/10 mL (15 mg/mL) ... sotalol hydrochloride"), and PEAKS dosed "by
# the full prescribing information", so a dose in mg is sotalol hydrochloride
# as labelled, on the basis the model was fitted.
#
# RENAL FUNCTION
# ==============
# Cockcroft-Gault (creatinineClearanceCG()) from the Patient Profile's serum
# creatinine, through adultEquivalentCreatinine(); blank means the assumed
# normal for age and sex.  PEAKS required a creatinine clearance above 60
# mL/min, so below it the power is an extrapolation, in the direction the
# label states (half-life up to 69 h in renal failure).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariates (Vc, and Cockcroft-Gault on CL),
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on and
# at total body weight with it off; Q and Vp are scaled by the library's
# factors with the switch on and fixed with it off.
#
# THRESHOLD
# =========
# No therapeutic range: dosing follows the QT interval, which lengthens in
# proportion to the concentration ("QTc prolongation is directly related to
# the concentration of sotalol", injection label).  No band is drawn.
#
# PREGNANCY
# =========
# Sotalol is cleared by the kidney unchanged, so the rise in filtration of
# late pregnancy is applied through pregnancyGfrFactor() (x1.5 on creatinine
# clearance when the creatinine is blank).  O'Hare 1983 (6 women, 32-36 weeks
# against postpartum; abstract only) measured clearance 2.4 against 1.5
# mL/min/kg, the same direction and larger.
# -----------------------------------------------------------------------------

# Yellepeddi 2025, Table 2
SOTALOL_CL       <- 9.5     # L/h at a creatinine clearance of 92.4 mL/min
SOTALOL_CRCL_REF <- 92.4    # mL/min
SOTALOL_CRCL_EXP <- 0.7
SOTALOL_VC       <- 38.2    # L at 104 kg
SOTALOL_WT_REF   <- 104     # kg
SOTALOL_VC_EXP   <- 1.1
SOTALOL_Q        <- 71.3    # L/h
SOTALOL_VP       <- 89.1    # L
# FDA Clinical Pharmacology Review, NDA 22-306, Table 9
SOTALOL_KA       <- 0.605   # 1/h
SOTALOL_TLAG     <- 0.231   # h

#' Sotalol (PEAKS, Yellepeddi 2025)
#'
#' Two compartments with Cockcroft-Gault creatinine clearance on clearance;
#' oral absorption from the FDA review; see the header of
#' \code{R/drugs_sotalol.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE} raises the
#'   creatinine clearance by \code{PREGNANCY_GFR_FACTOR} when the creatinine
#'   is blank.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sotalol <- function(weight, height, age, sex, adjustToFFM = TRUE,
                    creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy

  default <- list(
    v1  = SOTALOL_VC * (pkW / SOTALOL_WT_REF)^SOTALOL_VC_EXP,
    v2  = SOTALOL_VP * size$volume,
    v3  = 1,                          # placeholder: two compartments
    cl1 = SOTALOL_CL / 60 * (crcl / SOTALOL_CRCL_REF)^SOTALOL_CRCL_EXP,
    cl2 = SOTALOL_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = SOTALOL_KA / 60,
    bioavailability_PO = 1,
    tlag_PO = SOTALOL_TLAG * 60
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")

  reference <- paste0(
    "Yellepeddi VK et al. (PEAKS), CPT Pharmacometrics Syst Pharmacol ",
    "2025;14:658-667: two compartments, CL 9.5 L/h x (Cockcroft-Gault CrCl/92.4)^0.7, ",
    "Vc 38.2 L x (WT/104)^1.1, Q 71.3 L/h, Vp 89.1 L; oral F 1 (Betapace ",
    "label), ka 0.605 /h and lag 0.231 h (FDA review of NDA 22-306); doses ",
    "are sotalol hydrochloride; ", creatinineNote, ". ",
    "https://doi.org/10.1002/psp4.13302"
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
