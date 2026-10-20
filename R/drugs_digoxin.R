# -----------------------------------------------------------------------------
# Digoxin, oral (Yukawa 2001): serum digoxin in patients with heart failure
# and atrial fibrillation, as a constant daily oral rate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, who
# supplied the article and asked for digoxin beside the vasoactive and
# resuscitation drugs (inst/help/models/vasoactive.md,
# inst/extdata/vasoactiveRegistry.csv).  Every number below was read from the
# published article: Tables I-IV and VI and Equation 10.
#
# THE SOURCE
# ==========
# Yukawa E, Suematu F, Yukawa M, Minemoto M, Ohdo S, Higuchi S, Goto Y,
# Aoyama T.  Population pharmacokinetics of digoxin in Japanese patients: a
# 2-compartment pharmacokinetic model.  Clin Pharmacokinet 2001;40:773-781.
# 106 Japanese patients with heart failure and atrial fibrillation (43 men,
# 63 women; age 21-83; weight 31.5-88.8 kg, mean 50; Cockcroft-Gault creatinine
# clearance 11-131 mL/min, mean 56), 246 routine steady-state serum
# concentrations on once-daily digoxin TABLETS (125-250 mcg/day), fluorescence
# polarisation immunoassay; NONMEM, two compartments.  Final model (Table IV):
#
#     CL/F = (0.036 TBW + 0.112 CLcr) x 0.770^SPI x 0.784^CCB   L/h
#     V1/F = 1.83 L/kg x TBW,  V2/F = 22.6 L/kg x TBW,  Q/F = 0.629 L/h/kg x TBW
#
# with TBW total body weight (kg), CLcr Cockcroft-Gault creatinine clearance
# (mL/min), and SPI and CCB 1 with concomitant spironolactone or a calcium
# antagonist.  The paper's own Table VI is the check: at 70 kg its clearance
# is 13.72 L/h at CLcr 100 and 3.08 at CLcr 5, and the maintenance dose for an
# average concentration of 1 mcg/L is clearance x 24 h, 329 and 74 mcg/day.
#
# WHY ORAL, AND WHY A DAILY RATE
# ==============================
# Every dose was a tablet, so every parameter is APPARENT, divided by an
# unknown bioavailability (the paper says so).  An apparent model predicts
# oral concentrations correctly and intravenous ones wrong by 1/F, so it may
# not offer an intravenous unit (docs/adding-a-drug.md, "Apparent parameters
# restrict the route").  And the source fitted no absorption at all: it found
# none in its routine data and gave each tablet as a bolus into the central
# compartment.  There is no published absorption rate to give a first-order
# oral dose, so the model is offered, as amiodarone is, only as a constant
# oral daily rate ("mg/day PO"), applied to the apparent parameters as it
# stands.  That reproduces the steady-state average concentration the source
# was built to predict.  The approach to it is slow: the published V2/F,
# 22.6 L/kg (95% interval -7.8 to 53, Table IV), is about four times the
# steady-state volume of every other digoxin source on record (3-7 L/kg), so
# the terminal half-life is 126 h at a normal creatinine and 90% of steady
# state takes about 16 days (4 weeks at a creatinine of 2 mg/dL), where the
# other sources put it near a week; the rise and
# fall after each daily tablet are not drawn.  (The source's own structure,
# each tablet a bolus into a central volume of 1.83 L/kg, would put a spike
# of dose / V1 on every dose that real absorption does not produce, so it is
# not a better representation of the peak.)  An intravenous loading
# dose needs an intravenous study (Cheng 1997, Koup 1975), which is on the
# list of papers to obtain.
#
# WHAT IS HELD FIXED
# ==================
# Spironolactone (23% lower clearance) and calcium antagonists (22% lower:
# diltiazem, nicardipine, nifedipine, verapamil) are not Patient Profile
# fields and are held at 0.  A patient on either clears digoxin more slowly
# than drawn.
#
# RENAL FUNCTION AND BODY SIZE (docs/weight-adjustment.md)
# ========================================================
# The model's own weight covariate is total body weight, linear, on every
# parameter, and its renal term is Cockcroft-Gault, which also reads weight.
# As for vancomycin (R/drugs_vancomycin.R), both are evaluated at the
# pharmacokinetic weight with the fat-free-mass switch on and at total body
# weight with it off, which reproduces the published equation exactly.  The
# creatinine is the Patient Profile's, through adultEquivalentCreatinine(),
# or an assumed normal creatinine for age and sex when blank.
#
# NO EFFECT SITE; THE BAND
# ========================
# No effect site.  The band is the paper's therapeutic range, 0.5 to
# 2.0 mcg/L, which is serum concentration, as plotted; current practice
# targets the lower half of it in heart failure.
#
# PREGNANCY
# =========
# Hebert and colleagues gave 0.25 mg of oral digoxin to 13 women at 28 to 32
# weeks of gestation and again to the same women 6 to 10 weeks postpartum.
# The unbound renal clearance of digoxin was 272 +/- 45 mL/min in pregnancy
# against 183 +/- 37 mL/min postpartum (x 1.49), and its unbound net
# secretion 109 against 58 mL/min (x 1.88): filtration and renal
# P-glycoprotein both rise.  Digoxin is only 20 to 30% bound (Iisalo 1977), so
# the total renal clearance changes almost as the unbound one does.  Luxford
# and Kellaway (15 women, third trimester against 6 to 12 weeks postpartum)
# found digoxin renal clearance and creatinine clearance both higher in
# pregnancy, but the serum digoxin higher too, which they put down to greater
# absorption; their abstract gives no numbers, and that is not applied.
#
# With the Patient Profile's Pregnant field on and the serum creatinine left
# blank, the creatinine clearance in Yukawa's renal term is multiplied by
# PREGNANCY_GFR_FACTOR = 1.5 (pregnancyGfrFactor(), R/pregnancy.R), which
# reproduces Hebert's x 1.49 in renal clearance; the weight (nonrenal) term,
# the volumes and Q are unchanged.  At 60 kg and a creatinine clearance of
# 100 mL/min, CL/F = 2.16 + 11.2 = 13.4 L/h becomes 2.16 + 16.8 = 19.0 L/h,
# x 1.42.  A measured creatinine is not multiplied: a pregnant woman's
# creatinine is already low, and the Cockcroft-Gault estimate made from it
# already carries the rise.  The bioavailability of digoxin in pregnancy is
# not known, and Yukawa's parameters are all apparent, so none is applied.
# Digoxin-like immunoreactive substances rise in pregnancy and cross-react in
# some immunoassays, so a measured serum digoxin in a pregnant woman may read
# higher than the curve.  Late pregnancy is the reference state.  (Claude
# Code, 2026-10-19.)
#
# References
# ----------
# Yukawa E, Suematu F, Yukawa M, et al.  Clin Pharmacokinet 2001;40:773-781.
#   https://doi.org/10.2165/00003088-200140100-00005
# Hebert MF, Easterling TR, Kirby B, et al.  Effects of pregnancy on CYP3A
#   and P-glycoprotein activities as measured by disposition of midazolam and
#   digoxin: a University of Washington specialized center of research
#   study.  Clin Pharmacol Ther 2008;84:248-253.
#   https://doi.org/10.1038/clpt.2008.1 (pregnancy)
# Luxford AM, Kellaway GS.  Pharmacokinetics of digoxin in pregnancy.  Eur J
#   Clin Pharmacol 1983;25:117-121.  https://doi.org/10.1007/BF00544027
#   (pregnancy)
# Iisalo E.  Clinical pharmacokinetics of digoxin.  Clin Pharmacokinet
#   1977;2:1-16.  https://doi.org/10.2165/00003088-197702010-00001
# -----------------------------------------------------------------------------

# Yukawa 2001, Table IV
DIGOXIN_CL_WT   <- 0.036    # L/h per kg of total body weight (theta 1)
DIGOXIN_CL_CRCL <- 0.112    # L/h per mL/min of creatinine clearance (theta 5)
DIGOXIN_V1      <- 1.83     # L/kg (theta 2)
DIGOXIN_Q       <- 0.629    # L/h/kg (theta 3)
DIGOXIN_V2      <- 22.6     # L/kg (theta 4)
DIGOXIN_SPI     <- 0.770    # with spironolactone (theta 6), held at none
DIGOXIN_CCB     <- 0.784    # with a calcium antagonist (theta 7), held at none

#' Digoxin, oral (Yukawa 2001)
#'
#' Two compartments, apparent (/F), offered as a constant oral daily rate; see
#' the header of \code{R/drugs_digoxin.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   left blank, multiplies the creatinine clearance of the renal term by 1.5
#'   (Hebert 2008; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
digoxin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                    creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)              # mL/min
  # Pregnancy (see the header): the renal term alone.
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- crcl * gfrPregnancy
  spironolactone <- 0
  calciumAntagonist <- 0

  cl <- (DIGOXIN_CL_WT * pkW + DIGOXIN_CL_CRCL * crcl) *
    DIGOXIN_SPI^spironolactone * DIGOXIN_CCB^calciumAntagonist     # L/h

  default <- list(
    v1  = DIGOXIN_V1 * pkW,
    v2  = DIGOXIN_V2 * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = cl / 60,
    cl2 = DIGOXIN_Q * pkW / 60,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")

  reference <- paste0(
    "Yukawa E et al., Clin Pharmacokinet 2001;40:773-781. ",
    "Two compartments, apparent (/F), in 106 Japanese patients on digoxin ",
    "tablets: CL/F (0.036 TBW + 0.112 CLcr) L/h, V1/F 1.83 L/kg, V2/F ",
    "22.6 L/kg, Q/F 0.629 L/h/kg; no spironolactone or calcium antagonist; ",
    creatinineNote, ".  Oral daily rate only. ",
    "https://doi.org/10.2165/00003088-200140100-00005"
  )
  if (gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("creatinine clearance x ", PREGNANCY_GFR_FACTOR),
      paste0("Hebert MF et al., Clin Pharmacol Ther 2008;84:248-253. ",
             "https://doi.org/10.1038/clpt.2008.1")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 1.0,
    upperTypical = 2.0,
    lowerTypical = 0.5,
    reference = reference
  )
}
