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
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
digoxin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                    creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)              # mL/min
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
