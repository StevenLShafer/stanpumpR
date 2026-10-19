# -----------------------------------------------------------------------------
# Digoxin, intravenous (Cheng 1997, with Hornestam 2003's time to peak effect)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, from
# full texts he supplied.  An absolute intravenous parameter set, separate
# from the apparent oral digoxin (Yukawa 2001), as haloperidol and
# haloperidolIV are.
#
# THE PHARMACOKINETICS: CHENG 1997
# ================================
# Cheng JW, Charland SL, Shaw LM, Kobrin S, Goldfarb S, Stanek EJ, Spinler SA.
# Is the volume of distribution of digoxin reduced in patients with renal
# dysfunction?  Determining digoxin pharmacokinetics by fluorescence
# polarization immunoassay.  Pharmacotherapy 1997;17:584-590.  Eighteen
# adults (mean 44 years) in three groups of six by measured creatinine
# clearance (<10, 10-50, >50 mL/min), given 7 or 10 mcg/kg intravenously,
# 15 serum samples over 5-7 days, two compartments fitted per group.  The
# paper's regressions on creatinine clearance (Results):
#
#     CL  (L/h/kg) = 0.0003 x CLcr + 0.0676      (r2 0.41)
#     Vss (L/kg)   = 0.0115 x CLcr + 5.289       (r2 0.24)
#
# The central volume was 0.6 L/kg in the groups below 10 and above 50 mL/min
# (0.7 between), and is fixed at 0.6.  The intercompartmental clearance is
# not printed; it is solved, 0.615 L/h/kg, so that the normal-renal group's
# own means (CL 0.11, Vc 0.6, Vss 6.4 L/kg) give its distribution rate
# constant, alpha 1.3 /h.  That system's terminal half-life is 46 h against
# the 49 h printed, and Q is held at that value for every patient.
#
# This was chosen over Hornestam 2003's intravenous clearance, CL 9.88 L/h x
# (1 + 0.02 x (CrCL - 70)), because Hornestam's slope was fixed at the bound
# that kept clearance non-negative over its data and so gives zero
# clearance at 20 mL/min; Cheng's clearance has a non-renal floor.
#
# Renal function is Cockcroft-Gault from an adult-equivalent creatinine
# (Cheng measured it; Cockcroft-Gault is the bedside estimate), at the
# pharmacokinetic weight, held to 0-140 mL/min (the studied range reaches the
# mid-130s).  A blank creatinine field means an assumed normal creatinine for
# age and sex.
#
# THE EFFECT SITE: HORNESTAM 2003
# ===============================
# Hornestam B, Jerling M, Karlsson MO, Held P.  Intravenously administered
# digoxin in patients with acute atrial fibrillation: a population
# pharmacokinetic/pharmacodynamic analysis based on the Digitalis in Acute
# Atrial Fibrillation trial.  Eur J Clin Pharmacol 2003;58:747-755.  In 105
# patients the fall in ventricular rate followed an effect compartment with
# ke0 0.184 /h (half-time 3.8 h).  With Hornestam's own typical plasma model
# (CL 9.88 L/h, Vc 27.8 L, Q 71.8 L/h, Vp 444 L) that ke0 puts the peak
# effect 87 minutes after a bolus, and the effect site here is solved from
# that time to peak, so it carries over to Cheng's plasma curve.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Per kilogram of actual weight, as Cheng dosed and reported: total weight
# with the switch off, the pharmacokinetic weight with it on.
#
# BAND
# ====
# 0.5-2 ng/mL, the serum range used for the oral digoxin, meaningful after
# distribution (6 h or more after a dose).
# -----------------------------------------------------------------------------

# Cheng 1997
DIGOXIN_IV_CL_INTERCEPT <- 0.0676   # L/h/kg
DIGOXIN_IV_CL_SLOPE     <- 0.0003   # L/h/kg per mL/min
DIGOXIN_IV_VSS_INTERCEPT <- 5.289   # L/kg
DIGOXIN_IV_VSS_SLOPE     <- 0.0115  # L/kg per mL/min
DIGOXIN_IV_V1  <- 0.6               # L/kg
DIGOXIN_IV_Q   <- 0.615             # L/h/kg, solved from group 3's alpha
DIGOXIN_IV_CRCL_RANGE <- c(0, 140)  # mL/min
# Hornestam 2003
DIGOXIN_IV_TPEAK <- 87              # min

#' Digoxin, intravenous (Cheng 1997; Hornestam 2003's time to peak effect)
#'
#' Two compartments per kilogram with clearance and steady-state volume
#' rising with creatinine clearance, and an effect site peaking 87 minutes
#' after a bolus; see the header of \code{R/drugs_digoxinIV.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
digoxinIV <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)              # mL/min
  crcl <- min(max(crcl, DIGOXIN_IV_CRCL_RANGE[1]), DIGOXIN_IV_CRCL_RANGE[2])

  clPerKg  <- DIGOXIN_IV_CL_INTERCEPT + DIGOXIN_IV_CL_SLOPE * crcl     # L/h/kg
  vssPerKg <- DIGOXIN_IV_VSS_INTERCEPT + DIGOXIN_IV_VSS_SLOPE * crcl   # L/kg

  default <- list(
    v1  = DIGOXIN_IV_V1 * pkW,
    v2  = (vssPerKg - DIGOXIN_IV_V1) * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = clPerKg * pkW / 60,
    cl2 = DIGOXIN_IV_Q * pkW / 60,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Cheng JW, Charland SL, Shaw LM, et al., Pharmacotherapy 1997;17:584-590: ",
    "two compartments, CL 0.0676 + 0.0003 x CLcr L/h/kg, Vss 5.289 + 0.0115 x ",
    "CLcr L/kg, Vc 0.6 L/kg (Cockcroft-Gault CLcr ", round(crcl), " mL/min from ",
    creatinineNote, ").  Time to peak effect 87 min from Hornestam B et al., ",
    "Eur J Clin Pharmacol 2003;58:747-755. https://pubmed.ncbi.nlm.nih.gov/9165563/"
  )

  list(
    PK = PK,
    tPeak = DIGOXIN_IV_TPEAK,
    MEAC = 0,
    typical = 1.0,
    upperTypical = 2.0,
    lowerTypical = 0.5,
    reference = reference
  )
}
