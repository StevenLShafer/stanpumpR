# -----------------------------------------------------------------------------
# Amiodarone IV: acute intravenous therapy, the first one to three days
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-08, at the request of Steven L. Shafer.
# Every number below was checked against the abstract of the source on
# PubMed (PMID 8877675); the full text was not available.  Verified by
# tests/testthat/test-drugs-amiodaroneIV.R, which derives its expectations
# independently of this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of amiodarone.  The source reports
# volumes in L/kg and clearances in L/h/kg; they are multiplied by the 70 kg
# reference weight here and the clearances divided by MINS_PER_HOUR.  Doses
# are entered as labelled; no salt or molecular-weight conversion is applied.
#
# WHY A SEPARATE DRUG
# ===================
# One drug runs one pharmacokinetic model, and "amiodarone"
# (R/drugs_amiodarone.R) runs Pollak, Bouillon and Shafer's model of LONG-TERM
# ORAL therapy.  Its parameters are apparent, divided by the unmeasured oral
# bioavailability F, and no choice of F turns them into these: the clearance
# here, 0.22 L/h/kg, is 15.4 L/h or 369.6 L/day at 70 kg, while the oral
# model's true clearance is 229 L/day x F, which is at most 229 L/day for any
# F <= 1.  The central volumes disagree as well (21 L here, 882 L x F
# there).  The oral paper itself calls its model ill-suited to single doses
# or short intravenous infusions.  So the two cannot be one drug with two
# routes, and intravenous amiodarone is this separate entry.  Each entry's
# help page links to the other.
#
# THE SOURCE
# ==========
# Korth-Bradley, Rose, de Vane, Peters and Chiang 1996: 245 patients given
# intravenous amiodarone for the short-term treatment of refractory,
# hemodynamically destabilising ventricular tachycardia or fibrillation.  A
# population analysis; two compartments, with proportional models for the
# intersubject variability and an additive-proportional residual error.
# Typical values (abstract):
#
#     CL   0.22 L/h/kg    (13%)
#     V1   0.30 L/kg      (11%)
#     V2   10.0 L/kg      (9.5%)
#     Q    0.71 L/h/kg    (16%)
#
# The percentages are NOT between-subject variability.  The abstract gives
# each "mean (% coefficient of variation, CV)", the CV of the estimate,
# that is, its precision.  The intersubject variances are reported
# separately, as 1.52 (31%) for clearance, 0.37 (46%) for V1, 0.37 (67%) for
# V2 and 0.44 (39%) for Q, the percentages again being their precision; read
# as variances of proportional random effects they are CVs of roughly 120%,
# 60%, 60% and 65%.  Residual error 0.53 (13%).  None enters the model, which
# is the typical patient.
#
# At 70 kg: V1 21 L, V2 700 L, CL 15.4 L/h, Q 49.7 L/h, giving half-lives of
# 13.2 minutes and 42.0 hours.
#
# VALIDITY: ACUTE THERAPY ONLY
# ============================
# The patients were sampled during short-term treatment, and the abstract
# itself warns that "estimates of pharmacokinetic parameters made during
# short periods of observation may not be entirely consistent with
# parameters estimated during prolonged periods of observation".  The
# 42-hour terminal half-life is such an estimate: after a single intravenous
# dose followed for 77 days the serum half-life exceeds 14 days (Shiga 2011),
# and in long-term oral therapy it is 55 days (Pollak 2000).  This model is
# for the first one to three days of intravenous therapy.  It must not be
# used to predict accumulation over longer courses, where it reaches a
# steady state (rate / CL) in days that the real drug approaches only over
# weeks to months; the long-term oral entry is for that.  The help page
# (inst/help/drugs/amiodaroneIV.md) says so prominently.  The drug is
# deliberately NOT in LONG_TERM_DRUGS (R/constants.R): it belongs on plots of
# hours to days.
#
# NO METABOLITE
# =============
# Desethylamiodarone is not formed here.  The source has no metabolite model,
# and the long-term entry's metabolite parameters are apparent on the oral
# model's scale, so they cannot be attached to these.  It matters little in
# the window this model covers: after a 15-minute infusion of 5 mg/kg the
# rate of desethylamiodarone formation is slow and its concentration low
# relative to amiodarone, so that it is unlikely to contribute significantly
# to the pharmacologic activity during intravenous therapy of up to two weeks
# (Vadiei 1996).
#
# NO EFFECT SITE
# ==============
# No human ke0 for the antiarrhythmic effect has been published.  tPeak is
# therefore zero, ke0 is zero, and the plot shows the plasma concentration.
# No metabolite is named, so the help page describes the drug as plasma only,
# not as a prodrug.  MEAC is zero: amiodarone is not an opioid.
#
# NO BAND AND NO THRESHOLD
# ========================
# The 1.0 to 2.5 mg/L therapeutic window that the long-term entry draws is a
# range for trough concentrations in chronic therapy, when serum is in
# equilibrium with the tissues.  During intravenous loading it is not, and
# the window says little about effect, so no band is drawn
# (inst/extdata/drugDefaults_global.csv: Lower, Upper, Typical zero) and
# there is no default threshold (endCe zero).  For reference, the label's
# first-day regimen (150 mg over 10 minutes, 1 mg/min for 6 hours, then
# 0.5 mg/min) gives 0.87 to 1.38 mg/L from the first hour to the 24th in this
# model: at the bottom edge of that window.
#
# UNITS
# =====
# Intravenous only: "mg" and "mg/kg" boluses, "mg/min" and "mg/hr"
# infusions, "mg" by default.  Intravenous amiodarone is given as a
# continuous infusion with supplemental boluses when needed, not on a fixed
# schedule, so no qd/bid/tid/qid unit is offered.  Oral amiodarone is the
# long-term entry.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are per kilogram, and no other covariate (age, gender,
# height, serum creatinine, serum alkaline phosphatase, ejection fraction or
# therapeutic response) contributed to their variability.  That is the class
# "V1 per kilogram with fixed rate constants", as for ketamine and etomidate:
# the published values times 70 kg are the reference man's, volumes scale by
# the fat-free-mass ratio and clearances by that ratio ^ 0.75 by default, and
# with the switch off volumes and clearances both scale by weight / 70
# (pkSizeFactors()'s default legacy factors), which reproduces the published
# per-kilogram model exactly.  The abstract does not give the cohort's
# weights or heights, so the 70 kg reference cannot be checked against them.
# The help page's child and infant are extrapolation, evaluated only because
# every model in the library is tabulated at them.
#
# References
# ----------
# Korth-Bradley JM, Rose GM, de Vane PJ, Peters J, Chiang ST. Population
#   pharmacokinetics of intravenous amiodarone in patients with refractory
#   ventricular tachycardia/fibrillation. J Clin Pharmacol 1996;36:715-719.
#   https://pubmed.ncbi.nlm.nih.gov/8877675/
# Pollak PT, Bouillon T, Shafer SL. Population pharmacokinetics of long-term
#   oral amiodarone therapy. Clin Pharmacol Ther 2000;67:642-652.
#   https://pubmed.ncbi.nlm.nih.gov/10872646/
# Vadiei K, O'Rangers EA, Klamerus KJ, et al. Pharmacokinetics of intravenous
#   amiodarone in patients with impaired left ventricular function. J Clin
#   Pharmacol 1996;36:720-727. https://pubmed.ncbi.nlm.nih.gov/8877676/
# Shiga T, Tanaka T, Irie S, Hagiwara N, Kasanuki H. Pharmacokinetics of
#   intravenous amiodarone and its electrocardiographic effects on healthy
#   Japanese subjects. Heart Vessels 2011;26:274-281 (online 2010).
#   https://pubmed.ncbi.nlm.nih.gov/21052689/
# -----------------------------------------------------------------------------

# Korth-Bradley 1996, abstract.  Per kilogram, litres and litres per hour,
# converted to the 70 kg reference and to per minute in the model.
AMIODARONE_IV_V1 <- 0.30   # L/kg, central volume
AMIODARONE_IV_V2 <- 10.0   # L/kg, peripheral volume
AMIODARONE_IV_CL <- 0.22   # L/h/kg, clearance
AMIODARONE_IV_Q  <- 0.71   # L/h/kg, intercompartmental clearance

AMIODARONE_IV_REFERENCE <- paste0(
  "Korth-Bradley JM, Rose GM, de Vane PJ, Peters J, Chiang ST. Population ",
  "pharmacokinetics of intravenous amiodarone in patients with refractory ",
  "ventricular tachycardia/fibrillation. J Clin Pharmacol 1996;36:715-719. ",
  "https://pubmed.ncbi.nlm.nih.gov/8877675/"
)

#' Amiodarone pharmacokinetics (acute intravenous therapy)
#'
#' The two-compartment population model of Korth-Bradley and colleagues
#' (1996), for the first one to three days of intravenous therapy.  See the
#' file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, scale
#'   both by weight / 70, the published per-kilogram model.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
amiodaroneIV <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Per-kilogram parameters with fixed rate constants (see the header): the
  # default legacy factors, weight / 70 for volumes and clearances alike.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  v1  <- AMIODARONE_IV_V1 * 70 * size$volume                          # 21 L
  v2  <- AMIODARONE_IV_V2 * 70 * size$volume                          # 700 L
  v3  <- 1                                                            # no third compartment
  cl1 <- AMIODARONE_IV_CL * 70 / MINS_PER_HOUR * size$clearance       # 15.4 L/h
  cl2 <- AMIODARONE_IV_Q  * 70 / MINS_PER_HOUR * size$clearance       # 49.7 L/h
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

  # No band: the 1.0 to 2.5 mg/L window is for chronic troughs (see the
  # header).  The CSV carries the same zeros.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = AMIODARONE_IV_REFERENCE
    )
  )
}
