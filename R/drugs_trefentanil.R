# -----------------------------------------------------------------------------
# trefentanil (ANQ-3665, also A-3665): investigational fentanyl congener
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, arterial plasma.
#
# Trefentanil is a 4-phenylpiperidine mu-agonist that reached phase 1 and was
# never marketed.  It is in the library because Lemmens and colleagues
# characterised it against fentanyl and alfentanil in the same volunteers, with
# the spectral edge of the EEG as a continuous measure of drug effect, which
# makes it one of the very few fentanyl analogs with a complete, identified
# human pharmacokinetic AND pharmacodynamic model.  The teaching point is the
# paper's own Figure 6: trefentanil's effect-site concentration decays faster
# than alfentanil's or fentanyl's after a long infusion, which is a statement
# about context-sensitive decrement rather than about elimination half-life.
#
# SOURCE AND WHICH PHASE OF IT
# ============================
# Lemmens HJM, Dyck JB, Shafer SL, Stanski DR.  Clin Pharmacol Ther
# 1994;56(3):261-271.  The study had two phases that both produced trefentanil
# parameters:
#
#   Table IV / Table III  infusion rate assessment phase, n = 9 (PK) and 7 (PD)
#   Table  V / Table  VI  crossover phase, n = 5, against fentanyl and alfentanil
#
# This model implements the CROSSOVER phase.  It is the set the paper's own
# abstract and discussion quote as trefentanil's (elimination clearance 0.444
# L/min, steady-state volume 37 L, elimination half-life 127 minutes), its
# pharmacokinetics and pharmacodynamics come from the same five subjects, and
# it is the arm that was designed as the head-to-head against the two congeners
# already in this library.  The paper reports no difference in trefentanil's
# parameters between the two phases.  The infusion rate assessment phase would
# give a larger model (V1 4.5 L, Vss 39.3 L, clearance 0.483 L/min); it is not
# implemented, and the help page says so.
#
# WHY THE PARAMETERS ARE DERIVED PER SUBJECT AND THEN AVERAGED
# ============================================================
# The paper does not print micro-constants.  Table V gives, for each subject,
# the FRACTIONAL coefficients and hybrid rate constants of the triexponential
# disposition function together with the central volume:
#
#     c(t)/Dose = (1 / Vc) * (A e^-alpha*t + B e^-beta*t + C e^-gamma*t),
#     A + B + C = 1
#
# which identifies the three-compartment model completely.  The obvious move,
# inverting the printed MEAN row, is wrong: the inversion is non-linear, so the
# mean of the coefficients does not carry the mean of the volumes and
# clearances.  Inverting the mean row gives a steady-state volume of 40.0 L
# against the paper's own reported mean of 37, a clearance of 0.482 against
# 0.444 and a distribution clearance of 1.31 against 1.11 - errors of 8%, 9%
# and 18% against numbers printed in the same table.
#
# So each of the five subjects is inverted separately and the resulting volumes
# and clearances are averaged.  That reproduces every subject's OWN reported
# steady-state volume, elimination clearance and distribution clearance to
# within 2%, the residual being the two significant figures the coefficients
# are printed to, and the averages then land on the paper's means:
#
#                 derived here      Table V mean
#     Vss            36.60 L           37 L
#     CL_elim       0.4392 L/min      0.444 L/min
#     CL_dist       1.1020 L/min      1.11 L/min
#
# At the five subjects' mean weight of 79.8 kg the derived set is
#
#     V1 3.480 L   V2 8.193 L   V3 24.926 L
#     Cl1 0.43920  Cl2 0.87772  Cl3 0.22434  L/min
#
# SIZE SCALING
# ============
# The crossover subjects weighed 77, 88, 78, 81 and 75 kg, a mean of 79.8 kg,
# and the paper fits no weight model at all.  The library's reference patient
# is a 70 kg, 170 cm man (docs/weight-adjustment.md), who must receive the
# model's parameters unchanged, so the derived set above is carried to 70 kg
# allometrically on total weight - volumes by 70/79.8, clearances by
# (70/79.8)^0.75 - before pkSizeFactors() scales it to the patient.  Heights
# were not reported, so the subjects' fat-free mass cannot be computed and this
# step cannot be done on the fat-free scale.  Skipping it altogether would
# hand a 70 kg patient the parameters of a 79.8 kg one: volumes 14% too large
# and clearances 10% too large.
#
# NO MEAC, AND WHY THE EC50 IS NOT ONE
# ====================================
# Table VI gives trefentanil an EEG EC50 of 429 +/- 313 ng/mL.  That is the
# effect-site concentration producing half the maximal slowing of the EEG
# spectral edge, and it is NOT a minimum effective analgesic concentration.
# The same table gives fentanyl an EEG EC50 of 9.8 ng/mL, against the 0.6 ng/mL
# this library carries as fentanyl's MEAC - sixteen-fold apart.  The paper is
# explicit about the gap: "a direct calibration of EEG and clinical opioid
# effects (analgesia and anesthesia) has not been performed to date.  Therefore
# the predictive validity of the EEG as a surrogate effect measurement to the
# clinical situation cannot be uncritically assumed."  Trefentanil was never
# given to patients, so no analgesic target exists to put in the MEAC slot.
# MEAC is 0 and the band is 0; the EEG EC50 is on the help page instead.
#
# THE EFFECT SITE
# ===============
# ke0 is supplied directly rather than through tPeak, because the paper
# measured it: the equilibration half-time between arterial concentration and
# EEG effect was 1.20 +/- 0.50 minutes (Table VI, crossover mean), estimated
# semiparametrically by collapsing the hysteresis loop.  ke0 = log(2) / 1.20.
# Averaging the five subjects' ke0 values instead of their half-times would
# give 0.647 /min rather than 0.578, 12% higher, because the reciprocal is
# convex; the paper's own reported mean half-time is used.  getDrugPK() solves
# the true time to peak effect from this ke0 and the disposition above.
# -----------------------------------------------------------------------------

# The crossover-phase disposition, derived per subject from Table V and
# averaged, then carried from the subjects' mean 79.8 kg to the library's
# 70 kg reference man (see the header).
TREFENTANIL_STUDY_WEIGHT <- 79.8          # kg, mean of the five crossover subjects
TREFENTANIL_V1 <- 3.0526316               # L, at the 70 kg reference
TREFENTANIL_V2 <- 7.1867893
TREFENTANIL_V3 <- 21.8652447
TREFENTANIL_CL1 <- 0.3980894              # L/min, at the 70 kg reference
TREFENTANIL_CL2 <- 0.7955694
TREFENTANIL_CL3 <- 0.2033395
TREFENTANIL_T_HALF_KE0 <- 1.20            # min, Table VI crossover mean
TREFENTANIL_EEG_EC50 <- 429               # ng/mL, Table VI; NOT a MEAC

#' Trefentanil pharmacokinetics
#'
#' Investigational fentanyl congener (ANQ-3665), three-compartment disposition
#' with an effect site, from the crossover phase of Lemmens 1994.  Plasma only:
#' the published EC50 is an EEG spectral-edge concentration, not an analgesic
#' target, so MEAC and the band are zero.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex "male" or "female"
#' @param adjustToFFM scale to fat-free mass (docs/weight-adjustment.md)
#' @returns the drug's PK list, tPeak, MEAC, band and reference
#' @export
trefentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  # Size scaling (see docs/weight-adjustment.md): the constants above describe
  # the 70 kg, 170 cm reference male.  Volumes scale with fat-free mass
  # relative to him, clearances with that ratio ^ 0.75.  With the switch off,
  # the library's default legacy scaling applies: both linear in weight / 70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  default <- list(
    v1  = TREFENTANIL_V1  * size$volume,
    v2  = TREFENTANIL_V2  * size$volume,
    v3  = TREFENTANIL_V3  * size$volume,
    cl1 = TREFENTANIL_CL1 * size$clearance,
    cl2 = TREFENTANIL_CL2 * size$clearance,
    cl3 = TREFENTANIL_CL3 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # The paper measured the equilibration half-time, so ke0 is given directly
  # and tPeak is left out; getDrugPK() solves the time to peak effect from it.
  ke0 <- log(2) / TREFENTANIL_T_HALF_KE0

  # No analgesic target exists: the published EC50 is an EEG surrogate and
  # trefentanil was never given to patients.  See the header.
  MEAC <- 0
  typical <- 0
  lowerTypical <- 0
  upperTypical <- 0

  reference <- paste0(
    "Lemmens HJM, Dyck JB, Shafer SL, Stanski DR. ",
    "Clin Pharmacol Ther 1994;56(3):261-271. ",
    "Crossover phase, five healthy men, arterial plasma, EEG spectral edge. ",
    "The three-compartment volumes and clearances are derived from the ",
    "fractional coefficients and hybrid rate constants of Table V, inverted ",
    "for each subject and then averaged, which reproduces each subject's own ",
    "reported steady-state volume and clearances to within 2%; the ",
    "equilibration half-time of 1.20 min is from Table VI. ",
    "https://doi.org/10.1038/clpt.1994.136"
  )

  return(
    list(
      PK = PK,
      ke0 = ke0,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
