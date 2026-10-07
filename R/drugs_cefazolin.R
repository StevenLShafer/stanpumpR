# -----------------------------------------------------------------------------
# Cefazolin: unbound two-compartment disposition (Komatsu 2024)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L).
#
# THE PLOTTED CONCENTRATION IS UNBOUND CEFAZOLIN
# ==============================================
# Komatsu et al. jointly fitted total and unbound serum cefazolin from 152
# adults having prostatectomy or nephrectomy, with 15-minute infusions.  The
# disposition in their final model is written on UNBOUND concentration: a
# linear two-compartment system whose clearance acts on free drug, with total
# concentration recovered afterwards through a saturable albumin binding
# relationship, Ct = Cu + Bmax Cu / (Kd + Cu).
#
# stanpumpR's engine is linear, so it simulates the part of that model that
# is linear, the unbound disposition, and plots UNBOUND cefazolin.  That is
# also the concentration that matters pharmacodynamically: the cefazolin
# target is the fraction of the dosing interval the FREE concentration spends
# above the organism's MIC (fT > MIC).  Anyone comparing the curve against a
# measured TOTAL level must apply the binding map themselves; at the
# concentrations a 2 g dose produces, total is roughly two to four times
# unbound, and the ratio falls as the level rises because binding saturates.
#
# The source's two-compartment "volumes" (36.6 and 42.2 L) are dose-accounting
# volumes for free drug, not anatomical spaces; dividing a dose by them gives
# the free concentration directly, which is why the free curve is implemented
# exactly as published and not through a bound reservoir.
#
# RENAL FUNCTION
# ==============
# CLu = 29.3 x (CrCL / 70)^0.586 L/h, with CrCL the Cockcroft-Gault creatinine
# clearance in mL/min, at the patient's serum creatinine from the Patient
# Profile.  When none is entered CrCL is estimated from age, sex and body size
# at an ASSUMED NORMAL creatinine (R/renalFunction.R); renal impairment is
# then not represented, and the curve in a patient with a raised creatinine
# will decline too fast.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source retained no body-size term on its volumes or on Qu.  Those
# parameters follow the library convention: volumes x FFM / FFM_ref,
# clearances x that ratio ^ 0.75, with the switch off leaving them as
# published.  The renal term already carries size through Cockcroft-Gault,
# which uses the pharmacokinetic weight with the switch on and total body
# weight with it off.  CLu itself receives no further size factor.
#
# NOT MODELLED
# ============
# Albumin (the binding capacity Bmax = 319 x (albumin/32)^0.401 mg/L) plays no
# part in the free curve and is not an input.  No active metabolite.  Doses
# are cefazolin equivalents: the sodium salt label already states them so.
#
# References
# ----------
# Komatsu T et al., Antimicrob Agents Chemother 2024;68:e00267-24.
#   https://doi.org/10.1128/aac.00267-24
# -----------------------------------------------------------------------------

#' Cefazolin pharmacokinetics (unbound)
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM \code{TRUE} (the default) scales the model to the
#'   patient's fat-free mass as described in the
#'   \href{https://github.com/StevenLShafer/stanpumpR/blob/master/docs/weight-adjustment.md}{weight-adjustment guide};
#'   \code{FALSE} reproduces the published size scaling exactly.  Each drug
#'   file's header says what the switch changes for that model; for cefazolin
#'   it also sets the weight the creatinine-clearance estimate uses.
#' @param creatinine the patient's serum creatinine in mg/dL, or NULL for the
#'   assumed normal value for the patient's sex (R/renalFunction.R)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cefazolin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL)
{
  # Size scaling (see docs/weight-adjustment.md and the header).  The
  # published volumes and Qu carry no size term, so with the switch off they
  # are used as published (legacyVolume = 1).  pkW is the weight the renal
  # covariate sees: the pharmacokinetic weight with the switch on, total
  # weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- creatinineClearanceCG(pkW, age, sex,       # mL/min
                                patientCreatinine(creatinine, sex))

  # Komatsu 2024, unbound: CLu 29.3 (CrCL/70)^0.586 L/h, Vcu 36.6 L,
  # Qu 54.4 L/h, Vpu 42.2 L
  cl1 <- 29.3 * (crcl / 70)^0.586 / 60   # L/min, renal covariate carries size
  v1  <- 36.6 * size$volume
  cl2 <- 54.4 / 60 * size$clearance
  v2  <- 42.2 * size$volume
  v3  <- 1                                # no third compartment
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # No effect site: an antibiotic's effect is its plasma exposure relative to
  # the MIC, and the engine has nothing to equilibrate it into.
  tPeak <- 0
  MEAC  <- 0

  # Band, unbound mg/L: Komatsu's illustrative MIC scenarios of 0.5 and 1 mg/L
  # and the 2 mg/L susceptibility breakpoint for S. aureus.  Orientation only.
  typical      <- 1
  upperTypical <- 2
  lowerTypical <- 0.5

  reference <- paste0(
    "Komatsu T et al., Antimicrob Agents Chemother 2024;68:e00267-24. ",
    "Unbound two-compartment model; the plotted concentration is UNBOUND ",
    "cefazolin. Creatinine clearance is from the entered creatinine or an ",
    "assumed normal one. https://doi.org/10.1128/aac.00267-24"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
