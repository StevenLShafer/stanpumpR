# -----------------------------------------------------------------------------
# Labetalol, intravenous (Kanto 1981): plasma labetalol in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  The
# source's full text could not be reached; every number below is from its
# PubMed abstract, which is enough to fix the model and to check it, as the
# derivation shows.  The full text is on the list of papers to obtain.
#
# THE SOURCE
# ==========
# Kanto J, Allonen H, Kleimola T, Mantyla R.  Pharmacokinetics of labetalol
# in healthy volunteers.  Int J Clin Pharmacol Ther Toxicol 1981;19:41-44
# (PMID 7203731).  Four healthy volunteers given 1.5 mg/kg intravenously; a
# two-compartment model, and the abstract reports the means
#
#     distribution half-life           4.9 min
#     elimination half-life            4.9 h
#     total plasma clearance           24.80 mL/min/kg
#     central volume (Vdc)             1.1 L/kg
#     volume at steady state (Vdss)    9.41 L/kg
#
# THE DERIVATION, AND ITS CHECK
# =============================
# Five numbers for a model with four free parameters, so one is a check.  The
# decomposition uses the four that fix the model directly and stably --
# clearance, the central volume and the two exponential rates (l1, l2) --
#
#     k10 = CL / V1,   k21 = l1 l2 / k10,   k12 = l1 + l2 - k10 - k21,
#     V2  = k12 V1 / k21,   Q = k12 V1
#
# giving V1 1.1 L/kg, V2 7.918 L/kg, CL 0.0248 L/min/kg and Q 0.1171 L/min/kg,
# every one positive with k21 between the two rates.  The fifth number is the
# check: the derived steady-state volume, V1 + V2 = 9.02 L/kg, against the
# 9.41 printed, 4% apart, which is what means of four subjects' individual
# estimates should give.  Using Vss in place of V1 instead (the clevidipine
# route, R/drugs_clevidipine.R) is ill-conditioned here: it divides by
# l1 + l2 - l1 l2 Vss / CL, a difference of two nearly equal numbers, and
# returns V1 = 1.43 L/kg, a third above the measured 1.1.  So the measured
# central volume is used and the printed Vss is the check
# (tests/testthat/test-drugs-labetalol.R repeats both).
#
# DOSE BASIS
# ==========
# Not stated in the abstract.  Labetalol for injection is labelled as the
# hydrochloride in the US and was so in Europe, so a dose is entered as
# labelled; if the source dosed the base, every concentration here is 11% high
# (364.9 / 328.4).  The full text should settle it.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every reported parameter is per kilogram, the model's own size covariate,
# linear in weight.  As for any model with its own weight covariate, it is
# evaluated at the pharmacokinetic weight when the fat-free-mass switch is on
# (size$pkWeight) and at total body weight when it is off, so with the switch
# off the published per-kilogram values are reproduced exactly.
#
# WHAT THIS MODEL IS NOT
# ======================
# Four healthy volunteers, mean values, not a population model.  Clearance in
# surgical patients is lower: about half after aortobifemoral bypass (Chauvin
# 1987, Br J Anaesth, abstract), and a third, slower phase has been seen in
# renal failure (Halstenson 1986).  Pregnancy changes it again.  No effect
# site and no band: the source related the fall in blood pressure to the
# peripheral compartment's concentration, but the abstract gives no
# coefficients, and no blood pressure is predicted here.
# -----------------------------------------------------------------------------

# Kanto 1981, abstract
LABETALOL_CL     <- 24.80 / 1000   # L/min/kg
LABETALOL_V1     <- 1.1            # L/kg
LABETALOL_VSS    <- 9.41           # L/kg, the check, not used to build
LABETALOL_THALF_DISTRIBUTION <- 4.9        # min
LABETALOL_THALF_ELIMINATION  <- 4.9 * 60   # min

#' Labetalol, intravenous (Kanto 1981)
#'
#' Two compartments from the means reported for four healthy volunteers; see
#' the header of \code{R/drugs_labetalol.R} for the derivation and its check.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
labetalol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  p <- twoCompartmentFromCentralVolume(LABETALOL_CL, LABETALOL_V1,
                                       LABETALOL_THALF_DISTRIBUTION,
                                       LABETALOL_THALF_ELIMINATION)
  default <- list(
    v1  = p$v1 * pkW,
    v2  = p$v2 * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl1 * pkW,
    cl2 = p$cl2 * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kanto J et al., Int J Clin Pharmacol Ther Toxicol 1981;19:41-44 ",
    "(abstract).  Two compartments in 4 healthy volunteers given 1.5 mg/kg IV: ",
    "CL 24.8 mL/min/kg, central volume 1.1 L/kg, half-lives 4.9 min and ",
    "4.9 h; V2 and Q derived (Vss 9.02 L/kg against 9.41 printed).  ",
    "Surgical patients clear it about half as fast. ",
    "https://pubmed.ncbi.nlm.nih.gov/7203731/"
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
