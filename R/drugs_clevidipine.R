# -----------------------------------------------------------------------------
# Clevidipine (Ericsson 1999): whole-blood concentration in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# his handoff specification for antihypertensive models.  The specification
# listed this record's individual volumes and intercompartmental clearance as
# unavailable; they are in fact uniquely determined by the four quantities the
# source does report, and the derivation is below.  Verified by
# tests/testthat/test-drugs-clevidipine.R, which re-derives the vector from the
# published numbers independently of this file and checks that the simulated
# curve reproduces them.  See docs/antihypertensives.md.
#
# WHAT IS PLOTTED
# ===============
# Clevidipine in WHOLE BLOOD, ng/mL, which is the matrix the source assayed.
# Clevidipine is hydrolysed by blood and tissue esterases, and its
# concentration is reported in whole blood, not plasma.  The postoperative
# cardiac-surgery study (Bailey 2002) reported PLASMA concentrations and
# threshold C50 values of 9.7 and 26.3 mcg/L for a 10% and a 20% fall in mean
# arterial pressure; those must NOT be read against this curve, both because
# the matrix differs with no measured blood:plasma ratio and because a
# threshold C50 from a logistic fit is not the EC50 of a continuous effect.
# No blood pressure is predicted here: see "NO EFFECT SITE" below.
#
# THE SOURCE
# ==========
# Ericsson H, Fakt C, Jolin-Mellgard A, Nordlander M, Sohtell L, Sunzel M,
# Regardh CG.  Clinical and pharmacokinetic results with a new ultrashort-
# acting calcium antagonist, clevidipine, following gradually increasing
# intravenous doses to healthy volunteers.  Br J Clin Pharmacol
# 1999;47:531-538 (doi 10.1046/j.1365-2125.1999.00933.x).  Twenty-five healthy
# men, twenty-one of them dosed twice, so forty-six study entries; 20-minute
# infusions at target dose rates from 0.12 to 48 nmol/min/kg.  Concentrations
# of clevidipine and its primary metabolite H 152/81 were followed in whole
# blood.  A population analysis of the sparse data found an open
# two-compartment model best, and the paper reports:
#
#     clearance                        0.121 L/min/kg
#     volume of distribution at steady state  0.56 L/kg
#     initial half-life                1.8 min
#     terminal half-life               9.5 min
#
# and, as a fifth statement, that the initial half-life contributes "more than
# 80%" of the total area under the blood concentration-time curve after an
# intravenous bolus.  The concentration-dose-rate relation was linear over the
# whole range studied, so a linear model is the source's own finding.
#
# THE DERIVATION, AND WHY IT IS UNIQUE
# ====================================
# The abstract does not print V1, V2 and Q, and the full text (a scanned PDF
# in PubMed Central) has not been read here.  The abstract does not need to:
# a two-compartment model has exactly four free parameters, and the four
# reported quantities fix them.  (An adversarial review of 2026-10-27 states
# that the full text prints a fitted V1 of 0.36 L/kg.  Unverified, and 0.36
# L/kg is exactly the companion Eur J Clin Pharmacol 1999 study's V1, below,
# so the claim may conflate the two papers.  If confirmed, it is a second
# check: the derived 0.3689 is 2.5% above it.)  Writing l1 and l2 for the
# two exponential rates (log(2)/1.8 and log(2)/9.5 per minute), the standard relations are
#
#     l1 + l2 = k10 + k12 + k21,      l1 l2 = k10 k21,
#     CL      = V1 k10,               Vss   = V1 (1 + k12/k21).
#
# Eliminating k10, k12 and k21 leaves one linear equation in V1:
#
#     V1 = CL / (l1 + l2 - l1 l2 Vss / CL)
#
# and then k21 = l1 l2 V1 / CL, V2 = Vss - V1, k12 = k21 V2 / V1, Q = k12 V1.
# For the published numbers that gives
#
#     V1 0.3689 L/kg   V2 0.1911 L/kg   CL 0.121 L/min/kg   Q 0.01637 L/min/kg
#
# with every parameter positive and k21 between l2 and l1, which is exactly
# the condition for a physically admissible mammillary model.  This is the
# same identifiability test R/drugs_codeine.R applies and fails: there,
# clearance, Vss and a terminal slope gave three equations for three unknowns
# because the distribution half-life was NOT reported, so one compartment was
# used instead.  Here the second half-life IS reported, so the system closes.
#
# The parameters are DERIVED, not fitted, and are labelled so in
# inst/extdata/antihypertensiveParameters.csv.  Two independent checks
# support them.  First, the total area under the curve per unit dose comes to
# 8.26 (mg/L per mg/kg)*min, which is 1/CL exactly, as it must be.  Second,
# the fast phase carries 81.7% of that area -- the paper's own "more than 80%"
# -- and this is a quantity the derivation does not use, so it is a genuine
# out-of-sample check on the decomposition.  Perturbing any one of the four
# published numbers by 5% moves V1 by at most 4%, V2 by at most 10% and Q by
# at most 13%, so the decomposition is well conditioned.
#
# Compare esmolol, whose record is NOT implemented for the opposite reason:
# Sum 1983 reports both half-lives, clearance, and a central volume (one
# quarter of the apparent volume of distribution), which formally
# over-determines its two-compartment model -- but k12 is then only 1.7% of
# l1 + l2, a difference of nearly equal large numbers, and the two defensible
# readings of the reported volume give peripheral volumes a factor of four
# apart.  Formal identifiability is not enough; the decomposition has to be
# stable.
#
# WHAT THIS MODEL IS NOT
# ======================
# Healthy young men, not hypertensive or postoperative patients.  The source
# itself notes the blood-pressure effect was marginal in this population,
# "probably due to a compensatory baroreflex activation", so this is a
# disposition model from a population whose haemodynamics were deliberately
# intact.  Clearance in postoperative cardiac-surgery patients was reported as
# 4.3 L/min for a three-compartment model (Bailey 2002) and in a 72-hour
# hypertension study as roughly 26-33 L/min by cohort; those are different
# populations, different matrices and, in Bailey's case, a model this vector
# cannot represent (three compartments need six parameters, and that paper
# reports three numbers).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every published parameter is per kilogram, which is the model's own size
# covariate, linear in weight.  As for any model with its own weight
# covariate, it is evaluated at the pharmacokinetic weight when the fat-free-
# mass switch is on (size$pkWeight, 70 kg x FFM / FFM_ref) and at total body
# weight when it is off, so with the switch off the published per-kilogram
# values are reproduced exactly.
#
# NO EFFECT SITE, NO BAND
# =======================
# tPeak = 0, so getDrugPK() leaves ke0 at zero and only the blood
# concentration is plotted.  The source fitted an Emax model to the ratio of
# mean arterial pressure to heart rate -- a surrogate for vascular resistance,
# not a blood pressure -- and the numeric coefficients of that fit have not
# been read in the full text; reachable secondary sources disagree with each
# other about them.  The 2026-10-27 review states that the Results give Emax
# 45.4% and EC50 12.1 nmol/L, the handoff specification's figures, which is
# agreement between two unverified sources.  They are recorded as unverified
# in the parameter audit and are not used: even if confirmed, the endpoint is
# MAP/HR in healthy men with compensatory tachycardia, not a pressure.
# MEAC = 0 and the band is zero: there is no therapeutic whole-blood range
# to draw, and Bailey's plasma thresholds are a different matrix.
# -----------------------------------------------------------------------------

# Ericsson 1999, as published
CLEVIDIPINE_CL    <- 0.121   # L/min/kg, population mean, compartmental
CLEVIDIPINE_VSS   <- 0.56    # L/kg, volume of distribution at steady state
CLEVIDIPINE_THALF_INITIAL  <- 1.8   # min
CLEVIDIPINE_THALF_TERMINAL <- 9.5   # min

#' Two-compartment parameters from clearance, Vss and two half-lives
#'
#' The unique mammillary decomposition derived in the header of
#' R/drugs_clevidipine.R.  Exported for the unit test, which re-derives the
#' same vector from the published numbers by a different route.
#'
#' @param CL clearance, per unit weight
#' @param Vss steady-state volume of distribution, per unit weight
#' @param thalfInitial,thalfTerminal the two exponential half-lives, minutes
#' @returns a list of \code{v1}, \code{v2}, \code{cl1} and \code{cl2}, in the
#'   units of \code{CL} and \code{Vss}
#' @keywords internal
twoCompartmentFromHalfLives <- function(CL, Vss, thalfInitial, thalfTerminal)
{
  l1 <- log(2) / thalfInitial
  l2 <- log(2) / thalfTerminal
  denominator <- l1 + l2 - l1 * l2 * Vss / CL
  v1 <- CL / denominator
  v2 <- Vss - v1
  k21 <- l1 * l2 * v1 / CL
  k12 <- k21 * v2 / v1
  if (!is.finite(v1) || v1 <= 0 || v2 <= 0 || k12 <= 0 || k21 <= 0)
    stop("Two-compartment decomposition is not physically admissible: ",
         "check CL, Vss and the two half-lives.")
  list(v1 = v1, v2 = v2, cl1 = CL, cl2 = k12 * v1)
}

#' Clevidipine pharmacokinetics (intravenous, whole blood)
#'
#' Ericsson et al. (1999), two compartments in healthy men, with the volumes
#' and intercompartmental clearance derived from the reported clearance,
#' steady-state volume and both half-lives; see the header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
clevidipine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Per-kilogram published parameters: the model's own size covariate, so it
  # is evaluated at the pharmacokinetic weight with the switch on and at total
  # body weight with it off (docs/weight-adjustment.md).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  perKg <- twoCompartmentFromHalfLives(
    CLEVIDIPINE_CL, CLEVIDIPINE_VSS,
    CLEVIDIPINE_THALF_INITIAL, CLEVIDIPINE_THALF_TERMINAL)

  default <- list(
    v1  = perKg$v1  * pkW,
    v2  = perKg$v2  * pkW,
    v3  = 1,                      # two compartments
    cl1 = perKg$cl1 * pkW,
    cl2 = perKg$cl2 * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no identified equilibration delay; whole blood only
  MEAC  <- 0

  # No therapeutic whole-blood range exists to draw, and the postoperative
  # threshold concentrations are plasma, a different matrix.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Ericsson H et al., Br J Clin Pharmacol 1999;47:531-538. ",
    "Two-compartment whole-blood population model in 25 healthy men. ",
    "The individual volumes and intercompartmental clearance are derived, ",
    "uniquely, from the reported clearance (0.121 L/min/kg), steady-state ",
    "volume (0.56 L/kg) and initial and terminal half-lives (1.8 and 9.5 ",
    "min); the derived split puts 81.7% of the area in the initial phase, ",
    "against the paper's own 'more than 80%'. ",
    "https://doi.org/10.1046/j.1365-2125.1999.00933.x"
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
