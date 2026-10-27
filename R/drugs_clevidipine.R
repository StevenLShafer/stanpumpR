# -----------------------------------------------------------------------------
# Clevidipine (Ericsson 1999): whole-blood concentration in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# his handoff specification for antihypertensive models, and revised
# 2026-10-27 from the full text, which he supplied.  Verified by
# tests/testthat/test-drugs-clevidipine.R, which recomputes the paper's
# derived quantities from the implemented vector independently of this file
# and checks the simulated curve.  See docs/antihypertensives.md section 17.
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
# 1999;47:531-538 (PMID 10336577, doi 10.1046/j.1365-2125.1999.00933.x).
# Twenty-five healthy men, twenty-one of them dosed twice, so forty-six study
# entries; 20-minute infusions at target dose rates from 0.12 to 48
# nmol/min/kg.  Clevidipine and its primary metabolite H 152/81 were followed
# in venous whole blood.  A population analysis (P-PHARMA) of the sparse data
# found an open two-compartment model best.  The Methods say CL, V1, k12 and
# k21 were "obtained directly from the fitting function" and every other
# parameter was "calculated from the estimated population mean values".
# Table 1:
#
#     fitted     CL 0.121 L/min/kg     V1 0.36 L/kg     (k12, k21 not printed)
#     derived    Vss 0.56 L/kg   t1/2 initial 1.8 min   t1/2 terminal 9.5 min
#                C1 95.9%   C2 4.1%   share of AUC in the initial phase 81.6%
#
# The concentration-dose-rate relation was linear over the whole range
# studied, so a linear model is the source's own finding.
#
# THE IMPLEMENTED VECTOR
# ======================
# CL and V1 are the published fitted values.  k12 and k21 are not printed, so
# they are chosen to reproduce the six derived quantities as closely as their
# printed rounding allows: least squares, each quantity's error measured in
# units of half its last printed digit, with CL and V1 held at Table 1.  That
# gives
#
#     k12 0.04645 /min   k21 0.08629 /min
#     V1 0.36 L/kg   V2 0.1938 L/kg   CL 0.121 L/min/kg   Q 0.01672 L/min/kg
#
# and reproduces Vss 0.554, half-lives 1.75 and 9.45 min, C1 95.98% and an
# initial-phase AUC share of 81.56%: each within about one and a half units
# of its printed last digit.  No vector does better: Table 1's rounded
# numbers are not exactly consistent with one another (freed, the best fit
# puts V1 at 0.366, outside 0.36's rounding), so some disagreement at the
# last digit is unavoidable.
#
# Until 2026-10-27 the model DERIVED V1 = 0.3689, V2 = 0.1911 and Q = 0.01637
# from CL, Vss and the two half-lives alone (twoCompartmentFromHalfLives(),
# below), because only the abstract had been read and it prints no V1.  That
# reproduced Vss and the half-lives exactly but put V1 2.5% above the fitted
# value and the AUC share at 81.7%.  An adversarial review pointed out that
# the full text prints V1; the switch to the published V1 was Dr. Shafer's
# decision.  The derivation is kept, because the companion study's check
# (docs/antihypertensives.md section 11) and the labetalol test use it.
#
# Compare esmolol, whose record is NOT implemented: Sum 1983 reports both
# half-lives, clearance, and a central volume (one quarter of the apparent
# volume of distribution), which formally over-determines its two-compartment
# model -- but k12 is then only 1.7% of l1 + l2, a difference of nearly equal
# large numbers, and the two defensible readings of the reported volume give
# peripheral volumes a factor of four apart.  Formal identifiability is not
# enough; the decomposition has to be stable.
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
# concentration is plotted.  The source fitted a simple Emax model, with no
# effect compartment, to the percentage fall in the ratio of mean arterial
# pressure to heart rate -- a surrogate for vascular resistance, not a blood
# pressure -- against the blood concentration: Emax 45.4% and EC50
# 12.1 nmol/L (Results, read in the full text).  It is recorded in the
# parameter audit and not used: the endpoint is MAP/HR in healthy men whose
# baroreflex tachycardia offset most of the fall in pressure (MAP fell about
# 10% at the highest rate), so it is neither a pressure nor transferable to
# the patients clevidipine is given to.  MEAC = 0 and the band is zero: there
# is no therapeutic whole-blood range to draw, and Bailey's plasma thresholds
# are a different matrix.
# -----------------------------------------------------------------------------

# Ericsson 1999, Table 1: the fitted population means
CLEVIDIPINE_CL  <- 0.121     # L/min/kg
CLEVIDIPINE_V1  <- 0.36      # L/kg
# Not printed: chosen to reproduce Table 1's derived quantities (header)
CLEVIDIPINE_K12 <- 0.04645   # /min
CLEVIDIPINE_K21 <- 0.08629   # /min

#' Two-compartment parameters from clearance, Vss and two half-lives
#'
#' The unique mammillary decomposition when a source reports clearance,
#' steady-state volume and both half-lives but no central volume.  Clevidipine
#' used it until its source's fitted V1 was read (see the header of
#' R/drugs_clevidipine.R); it is kept for the companion study's check and the
#' labetalol test.
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
#' Ericsson et al. (1999), two compartments in healthy men: the published
#' fitted clearance and central volume, with the unprinted k12 and k21 chosen
#' to reproduce the paper's derived quantities; see the header.
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

  default <- list(
    v1  = CLEVIDIPINE_V1 * pkW,
    v2  = CLEVIDIPINE_V1 * CLEVIDIPINE_K12 / CLEVIDIPINE_K21 * pkW,
    v3  = 1,                      # two compartments
    cl1 = CLEVIDIPINE_CL * pkW,
    cl2 = CLEVIDIPINE_V1 * CLEVIDIPINE_K12 * pkW,
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
    "Two-compartment whole-blood population model in 25 healthy men: ",
    "fitted CL 0.121 L/min/kg and V1 0.36 L/kg (Table 1); k12 0.04645 and ",
    "k21 0.08629 /min, which the paper does not print, chosen to reproduce ",
    "its derived Vss (0.56 L/kg), half-lives (1.8 and 9.5 min) and ",
    "initial-phase share of AUC (81.6%) to their printed rounding. ",
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
