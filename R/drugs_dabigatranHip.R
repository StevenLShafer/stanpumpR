# -----------------------------------------------------------------------------
# Dabigatran after primary elective total hip replacement (Trocóniz 2007)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, dabigatran
# (total BIBR 953 ZW, free + glucuronides) concentration in µg/L (= ng/mL), the
# plotted curve.  Doses are entered as mg of the prodrug dabigatran etexilate
# (BIBR 1048), as in the trial.  Concentration.Units "ng".
#
# SOURCE
# ======
# Trocóniz IF, Tillmann C, Liesenfeld K-H, Schäfer H-G, Stangier J.  Population
# Pharmacokinetic Analysis of the New Oral Thrombin Inhibitor Dabigatran
# Etexilate (BIBR 1048) in Patients Undergoing Primary Elective Total Hip
# Replacement Surgery.  J Clin Pharmacol 2007;47:371-382.  287 patients, 4604
# total BIBR 953 ZW plasma concentrations, once- or twice-daily 12.5-300 mg for
# up to 10 days after surgery; NONMEM V.  Tables III (parameter estimates) and
# IV (final covariate structure), read from the typeset original on 2026-10-19.
#
# This is a SECOND, distinct dabigatran model from the library's `dabigatran`
# (Liesenfeld 2011, the steady-state RRE-LY atrial-fibrillation model).  This
# one is the post-operative hip-replacement model, whose defining feature is a
# TIME-VARYING disposition: absorption and apparent clearance are lower on the
# day of surgery and the next day than on days 2 to 10, which the authors
# attribute to reduced gastrointestinal motility and raised gastric pH just
# after surgery.
#
# STRUCTURE
# =========
# Apparent (/F) two-compartment model with first-order oral input and a lag,
# fitted to total BIBR 953 ZW.  Two phases:
#
#   Phase 1, "days 0 and 1" (the parameters the paper labels <24 h):
#     CL/F = 43.4  × (1 + 0.633 × GAST/34.58)                       L/h
#     ka   = 0.022 /h          (no covariate; a flip-flop rate, t1/2 ~31.5 h)
#   Phase 2, "days 2 to 10" (the parameters the paper labels >24 h):
#     CL/F = 82.1  × (CRCL/76.17) × (1 + 0.294 × GAST/34.58)        L/h
#     ka   = 0.265 × (1 - 0.363 × SCR/0.964) × (1 - 0.447 × AGE/66.97)  /h
#   Shared:
#     V2/F (central) = 30.8 L   Q/F = 13.6 L/h   V3/F (peripheral) = 136 L
#     ALAG1 (oral lag) = 0.4 h
#
# At the population-median gastrin of 34.58 pmol/L and creatinine clearance of
# 76.17 mL/min the typical CL/F is 43.4 × 1.633 = 70.87 L/h (phase 1) and
# 82.1 × 1.294 = 106.2 L/h (phase 2), the values the abstract quotes.  At the
# median serum creatinine (0.964 mg/dL) and age (66.97 y) the phase-2 ka is
# 0.265 × 0.637 × 0.553 = 0.093 /h, the paper's Figure 5 median line.
#
# WHICH PHASE A DOSE USES  (dose-time semantics; a documented choice)
# ===================================================================
# The paper's breakpoint is defined relative to SURGERY, which is not an input
# to the simulator.  The protocol gave the first dose 4-8 h after surgery (mean
# time-to-first-dose 6.4 h), so this model takes the first simulated time as the
# time of surgery and splits the doses at DABIGATRAN_HIP_SWITCH = 24 h,
# following the parameters' own "<24 h / >24 h" names.  The time to first dose
# and its between-patient variability are not represented, and a user who dates
# t = 0 from the first post-operative dose rather than from surgery shifts the
# split by that interval.
#
# Each dose keeps the phase it was GIVEN in (dose-time semantics): a dose given
# before 24 h is absorbed and cleared at the day-0-1 rates for its whole life,
# a dose given at or after 24 h at the day-2-10 rates.  The source's NONMEM
# control stream, which would say whether absorption still in progress at 24 h
# instead accelerates to the day-2 rate, is not available; dose-time semantics
# were chosen because the two phases are then independent linear systems that
# superpose, so each runs through the accurate first-order oral engine
# (`advanceClosedFormPO_IM_IN()`) and the result is their sum, with no
# discontinuity in plasma at 24 h.  `simCpCe()` carries the split as the drug's
# `dosePhase` block.
#
# COVARIATES AND FIELDS
# =====================
#   * Serum gastrin (GAST) raises CL/F in both phases.  There is no gastrin
#     field in the Patient Profile, so it is fixed at the population median
#     34.58 pmol/L, i.e. the (1 + θ × 1) multipliers above; the app therefore
#     does not show the gastrin effect, and θ_CL is the (unused) zero-gastrin
#     intercept, not a typical value.
#   * Creatinine clearance (Cockcroft-Gault) raises CL/F on days 2 to 10 only;
#     serum creatinine (SCR) and age lower the day-2-to-10 absorption rate.
#     Both read the Patient Profile's serum creatinine (`adultEquivalentCreatinine`,
#     so a child's value is put on the adult scale), which when blank is the
#     assumed normal for age and sex (`assumedCreatinine()`), referenced to the
#     median 0.964 mg/dL and CRCL 76.17 mL/min.  The covariates were fitted over
#     SCR 0.66-1.81 mg/dL, CRCL 29-161 mL/min and age 35-88 y; beyond that range
#     the absorption factors are held positive (they would otherwise drive ka
#     non-positive) and the result is an extrapolation.
#   * BODY SIZE (docs/weight-adjustment.md): the source found no weight, sex,
#     smoking or alcohol effect, so every apparent parameter is size-free and
#     takes the library's fat-free-mass factors with the adjustToFFM switch on
#     (size$clearance / size$volume) and the published values with it off
#     (legacyVolume = 1); weight enters only through the Cockcroft-Gault CRCL,
#     evaluated at the pharmacokinetic weight with the switch on.
#
# THRESHOLDS
# ==========
# None: tPeak = 0 and the plotted concentration is plasma.  No band, no MEAC and
# no time until threshold; no therapeutic range is established, and a simulated
# level is not an ecarin or dilute-thrombin-time activity or a procedure-safe
# time.  A research and teaching model, not validated clinical decision support;
# see inst/help/drugs/dabigatranHip.md.
#
# (Claude Code, 2026-10-19, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

DABIGATRAN_HIP_SWITCH   <- 24 * 60   # min after surgery: doses split here
DABIGATRAN_HIP_GAST     <- 34.58     # pmol/L, median serum gastrin (fixed)
DABIGATRAN_HIP_CRCL_REF <- 76.17     # mL/min, reference creatinine clearance
DABIGATRAN_HIP_SCR_REF  <- 0.964     # mg/dL, reference serum creatinine
DABIGATRAN_HIP_AGE_REF  <- 66.97     # years, reference age
DABIGATRAN_HIP_PHASE2   <- "day2"    # the PK key for doses given on days 2 to 10

#' Dabigatran pharmacokinetics after total hip replacement (Trocóniz 2007)
#'
#' Apparent two-compartment, time-varying disposition (lower absorption and
#' clearance on the day of surgery and the next day); see the header of
#' \code{R/drugs_dabigatranHip.R}.  Doses are entered as mg of the prodrug
#' dabigatran etexilate.
#'
#' @param weight weight in kg (enters the Cockcroft-Gault creatinine clearance)
#' @param height height in cm; no published covariate of its own, but used by
#'   \code{pkSizeFactors()} for the fat-free mass when \code{adjustToFFM} is
#'   \code{TRUE}
#' @param age age in years; lowers the day-2-to-10 absorption rate and feeds the
#'   creatinine clearance
#' @param sex sex as a string; feeds the creatinine clearance and the fat-free
#'   mass
#' @param adjustToFFM when \code{TRUE} the size-free parameters take the library
#'   fat-free-mass factors and the creatinine clearance uses the fat-free-mass
#'   weight; when \code{FALSE} total body weight is used and the parameters are
#'   the published values
#' @param creatinine serum creatinine in mg/dL (the app passes this field);
#'   blank assumes the normal for age and sex. It raises the day-2-to-10
#'   clearance (through CrCl) and lowers that period's absorption rate.
#' @returns a list in the shape \code{getDrugPK()} expects, with two PK sets
#'   (doses given before and after 24 h) and a \code{dosePhase} block
#' @export
dabigatranHip <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  scr  <- adultEquivalentCreatinine(creatinine, age, sex)       # mg/dL
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)             # mL/min

  gast1 <- 1 + 0.633 * DABIGATRAN_HIP_GAST / DABIGATRAN_HIP_GAST   # = 1.633
  gast2 <- 1 + 0.294 * DABIGATRAN_HIP_GAST / DABIGATRAN_HIP_GAST   # = 1.294

  # Days 2 to 10 absorption: serum creatinine and age both lower ka.  Each
  # factor is held positive (the fitted ranges keep it so; see the header).
  scrFactor <- max(1 - 0.363 * scr / DABIGATRAN_HIP_SCR_REF,  1e-3)
  ageFactor <- max(1 - 0.447 * age / DABIGATRAN_HIP_AGE_REF,  1e-3)

  shared <- list(
    v1 = 30.8 * size$volume, v2 = 136 * size$volume, v3 = 1,
    cl2 = 13.6 * size$clearance / 60, cl3 = 0,
    tlag_PO = 0.4 * 60, bioavailability_PO = 1
  )

  phase1 <- c(shared, list(
    cl1 = 43.4 * gast1 * size$clearance / 60,                  # days 0 and 1
    ka_PO = 0.022 / 60
  ))
  phase2 <- c(shared, list(
    cl1 = 82.1 * (crcl / DABIGATRAN_HIP_CRCL_REF) * gast2 * size$clearance / 60,
    ka_PO = 0.265 * scrFactor * ageFactor / 60
  ))

  PK <- list(phase1, phase2)
  names(PK) <- c(PK_EVENT_DEFAULT, DABIGATRAN_HIP_PHASE2)

  list(
    PK = PK,
    dosePhase = list(at = DABIGATRAN_HIP_SWITCH, late = DABIGATRAN_HIP_PHASE2),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Trocóniz IF et al., J Clin Pharmacol 2007;47:371-382 (apparent ",
      "two-compartment, time-varying PK of dabigatran after total hip ",
      "replacement; absorption and clearance lower on days 0-1 than days 2-10; ",
      "gastrin fixed at the median, Cockcroft-Gault creatinine clearance, serum ",
      "creatinine and age on the later absorption; doses as mg dabigatran ",
      "etexilate). https://doi.org/10.1177/0091270006297228"
    )
  )
}
