# -----------------------------------------------------------------------------
# Dofetilide (the sponsor's phase III population model, FDA review of NDA
# 20-931): one compartment, oral, clearance linear in creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read on the page images of the FDA's
# Clinical Pharmacology and Biopharmaceutics Review of NDA 20-931 (Tikosyn,
# approved 1 October 1999; accessdata.fda.gov,
# 20-931_Tikosyn_biopharmr_P1.pdf, review pages 20-21) and in the Tikosyn
# label (Reference ID 4474154, 2019), read in full.
#
# THE SOURCE
# ==========
# The sponsor's population analysis of the phase III trials (14 trials, 1445
# patients with supraventricular or ventricular arrhythmias, 10335 plasma
# concentrations, oral doses of 62.5 to 500 mcg once or twice daily),
# evidently the data set Tunblad 2008 (J Pharmacokinet Pharmacodyn 35:503)
# published, whose full text was not reachable.  As the review gives it:
# "A one-compartment disposition model with first order absorption proved to
# be adequate ... 14.1 L/h (1%) for apparent clearance (CL/F), 247 L (1.5%)
# for apparent volume of distribution (V/F), 1.69 /h (9.7%) for Ka ... The
# slope and intercept calculated for the relationship between dofetilide
# CL/F and CLCr were 0.128 L/h per ml/min and 4.5 L/h, respectively.  The V/F
# changed by 18.5 L for every 10 kg that weight changed from the median value
# of 80 kg.  Gender effect was found (p<0.001) on CL/F (a 12% lower CL/F for
# females was estimated)."  So
#
#     CL/F = (4.5 + 0.128 CLcr) x (0.88 in women)  L/h, CLcr in mL/min
#     V/F  = 247 + 1.85 (weight - 80)               L
#     ka   = 1.69 /h
#
# (at the median 75 mL/min the line gives 14.1, as printed).  The review does
# not say how the sex factor combines with the line; it is applied to the
# whole clearance, as the label's "women have approximately 12-18% lower
# dofetilide oral clearances than men ... after correction for weight and
# creatinine clearance" reads.  CLcr is Cockcroft-Gault, as the label
# calculates it for dosing.
#
# BIOAVAILABILITY
# ===============
# The label: "The oral bioavailability of dofetilide is >90%"; the review's
# study 115-212 (9 men): 0.98 +/- 0.09.  So the apparent parameters are
# taken as absolute, F 1.
#
# CHECKED (not used in the fit)
# =============================
# The label: terminal half-life "approximately 10 hours", volume of
# distribution 3 L/kg; this model at the reference man (CLcr 113 mL/min):
# CL/F 19.0 L/h, V 228 L (3.3 L/kg), half-life 8.3 h; at the median patient
# (75 mL/min, 80 kg) 12.1 h.  The review's intravenous study: CL 20.9 L/h.
# Allen 2000 (Br J Clin Pharmacol 50:247, read), healthy men at 500 mcg twice
# daily: trough 1.24 and AUC 25.4 ng.h/mL, which this model reproduces (1.24,
# 25.1); its peak, 3.80 ng/mL, is 22% above this model's 2.95, the cost of a
# one-compartment fit to sparse data.  The label: "The range of dofetilide
# plasma concentrations achieved with the 500 mcg BID dose adjusted for
# creatinine clearance is 1-3.5 ng/mL."
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on V/F and the Cockcroft-Gault weight are
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on,
# at total body weight with it off (as published).
#
# RENAL FUNCTION AND PREGNANCY
# ============================
# A blank serum creatinine is an assumed normal value for age and sex
# (assumedCreatinine()), put on the adult scale for a child
# (adultEquivalentCreatinine()).  The label contraindicates a creatinine
# clearance under 20 mL/min.  Dofetilide is cleared mainly unchanged in the
# urine, and no pregnancy pharmacokinetics exist, so in pregnancy the
# creatinine clearance takes the 50% rise in filtration of late pregnancy
# when the serum creatinine is blank (pregnancyGfrFactor()).
#
# DOSE BASIS
# ==========
# Tikosyn capsules are dofetilide, 125, 250 and 500 mcg; no salt.
#
# THE BAND
# ========
# None: no therapeutic range is established.  The review relates torsades de
# pointes to an AUC above 60 ng.h/mL and to Cmax; dosing follows the QTc and
# the creatinine clearance.
# -----------------------------------------------------------------------------

# FDA review of NDA 20-931, phase III population model
DOFETILIDE_CL_INTERCEPT <- 4.5     # L/h
DOFETILIDE_CL_SLOPE     <- 0.128   # L/h per mL/min of creatinine clearance
DOFETILIDE_FEMALE       <- 0.88    # "12% lower CL/F for females"
DOFETILIDE_V            <- 247     # L at 80 kg
DOFETILIDE_V_SLOPE      <- 1.85    # L per kg ("18.5 L for every 10 kg")
DOFETILIDE_WT_REF       <- 80      # kg
DOFETILIDE_KA           <- 1.69    # 1/h

#' Dofetilide (phase III population model, NDA 20-931)
#'
#' One compartment, oral, clearance linear in the Cockcroft-Gault creatinine
#' clearance; see the header of \code{R/drugs_dofetilide.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant third trimester (TRUE) or not
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dofetilide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr) * gfrPregnancy
  cl <- (DOFETILIDE_CL_INTERCEPT + DOFETILIDE_CL_SLOPE * crcl) *
    (if (sex == SEX_FEMALE) DOFETILIDE_FEMALE else 1)
  # The weight line stays positive above 80 - 247 / 1.85 = -53 kg
  v <- DOFETILIDE_V + DOFETILIDE_V_SLOPE * (pkW - DOFETILIDE_WT_REF)

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_PO = DOFETILIDE_KA / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL") else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "FDA Clinical Pharmacology Review of NDA 20-931 (Tikosyn, 1999), phase III ",
    "population model: one compartment, CL/F 4.5 + 0.128 x CLcr L/h (x 0.88 ",
    "in women), V/F 247 + 1.85 x (weight - 80) L, ka 1.69 /h, F 1; CLcr ",
    sprintf("%.0f", crcl), " mL/min (Cockcroft-Gault), ", creatinineNote, ". ",
    "https://www.accessdata.fda.gov/drugsatfda_docs/nda/99/20-931_Tikosyn_biopharmr_P1.pdf"
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
