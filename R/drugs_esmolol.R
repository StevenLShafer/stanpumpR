# -----------------------------------------------------------------------------
# Esmolol (de Bruijn 1987): whole-blood esmolol in anaesthetised cardiac
# surgical patients
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Tables 1-4 and the Methods.
#
# THE SOURCE
# ==========
# de Bruijn NP, Reves JG, Croughwell N, Clements F, Drissel DA.
# Pharmacokinetics of esmolol in anesthetized patients receiving chronic beta
# blocker therapy.  Anesthesiology 1987;66:323-326
# (doi 10.1097/00000542-198703000-00010).  Nineteen patients for coronary
# artery bypass grafting (17 men; 77.6 +/- 12.8 kg; 57 +/- 8.4 years), all on
# chronic beta blockers, anaesthetised with diazepam, pancuronium and
# enflurane; esmolol by peripheral vein in a stepped regimen (500 and 100-300
# mcg/kg/min, Table 1) for 54.8 minutes on average; venous blood from a
# cannula in the right internal jugular vein; GC-MS.  Thirteen patients'
# decay curves were fitted with a two-compartment model (NONLIN), and Tables
# 3 and 4 give the means
#
#     distribution half-life     1.34 min
#     elimination half-life      9.9 min
#     total clearance            128 mL/kg/min
#     central volume (Vc)        0.413 L/kg   ("the apparent volume of the
#                                               central compartment")
#     V-beta                     1.90 L/kg
#
# THE DERIVATION, AND ITS CHECK
# =============================
# The abstract's "V epsilon" is the central volume: the Results say so.  As
# for labetalol, the model is built from clearance, the central volume and
# the two half-lives (twoCompartmentFromCentralVolume(), R/vasoactive.R),
# giving V2 0.567 L/kg and Q 0.0663 L/min/kg (Vss 0.98 L/kg), with V-beta the
# check: CL / beta is 1.83 L/kg against the 1.90 printed, 4% apart, as means
# of individual estimates should be.  The decomposition is well conditioned
# (k12 is 27% of alpha + beta), unlike Sum 1983's (docs/antihypertensives.md
# section 4).  The individual parameter sets of Table 3 do not all close: for
# four of the thirteen patients the printed central volume and the two rates
# admit no mammillary model, which is one more reason to build from the means
# rather than any single patient.
#
# WHAT IS PLOTTED, AND WHERE IT WAS SAMPLED
# =========================================
# Esmolol in WHOLE BLOOD, mcg/mL, as assayed.  The red-cell esterases that
# hydrolyse it make the sampling site matter: the paper itself cites an animal
# study in which arterial concentrations were 3-7 times peripheral venous.
# These samples were central venous (internal jugular), drawn far from the
# peripheral infusion site.  The clearance found, 128 mL/kg/min, is less
# than half of Sum's 285 in awake volunteers (site unreported); the authors
# attribute the difference to anaesthesia and chronic beta blockade.
#
# DOSE BASIS
# ==========
# Esmolol hydrochloride, as the paper names it and as it is labelled; a dose
# is entered as labelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter is per kilogram, the model's own size covariate, so it is
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off.
#
# NO EFFECT SITE, NO BAND
# =======================
# No concentration-effect relation was fitted; no heart rate is predicted.
# -----------------------------------------------------------------------------

# de Bruijn 1987, Tables 3 and 4 (means of 13 patients)
ESMOLOL_CL    <- 0.128   # L/min/kg
ESMOLOL_V1    <- 0.413   # L/kg
ESMOLOL_THALF_ALPHA <- 1.34   # min
ESMOLOL_THALF_BETA  <- 9.9    # min
ESMOLOL_VBETA <- 1.90    # L/kg, the check, not used to build

#' Esmolol (de Bruijn 1987)
#'
#' Two compartments from the means reported for thirteen anaesthetised
#' cardiac surgical patients; see the header of \code{R/drugs_esmolol.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
esmolol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  p <- twoCompartmentFromCentralVolume(ESMOLOL_CL, ESMOLOL_V1,
                                       ESMOLOL_THALF_ALPHA, ESMOLOL_THALF_BETA)
  default <- list(
    v1  = p$v1  * pkW,
    v2  = p$v2  * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl1 * pkW,
    cl2 = p$cl2 * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "de Bruijn NP et al., Anesthesiology 1987;66:323-326. ",
    "Two compartments in 13 anaesthetised CABG patients on chronic beta ",
    "blockers: CL 128 mL/kg/min, central volume 0.413 L/kg, half-lives 1.34 ",
    "and 9.9 min; V2 and Q derived (CL/beta 1.83 L/kg against V-beta 1.90). ",
    "Whole blood, central venous; arterial esmolol runs several times higher. ",
    "https://doi.org/10.1097/00000542-198703000-00010"
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
