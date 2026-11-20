# -----------------------------------------------------------------------------
# Irbesartan (Avapro): Karatza and Karalis 2020, apparent oral, plasma
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-20, at the request of Steven L. Shafer, from
# the full text of Karatza E, Karalis V, Delay differential equations for the
# description of irbesartan pharmacokinetics: a population approach to model
# absorption complexities leading to dual peaks, Eur J Pharm Sci
# 2020;153:105498 (doi 10.1016/j.ejps.2020.105498, PMID 32736091), from the
# archive.  Tests: tests/testthat/test-drugs-irbesartan.R.
#
# PROVISIONAL (Steven L. Shafer, 2026-11-20: "Use Karatza").  Shafer chose
# this source over waiting for a model with ordinary first-order absorption,
# knowing the two costs written out below: the absorption is a discretisation
# that misses the published curve by up to 4.7% of the peak, nine times the
# library's standard, and the terminal half-life is shorter than the label's.
# Neither alternative read in full could replace it: Hedaya and Helmy 2015
# (doi 10.1002/bdd.1935) prints no volumes or intercompartmental clearance, so
# its two-compartment model cannot be rebuilt, and Huang 2006 (doi
# 10.1007/BF03190465) prints an apparent clearance twice the label's, with
# rate constants at 300 mg that do not reproduce its own half-life.
#
# THE SOURCE
# ==========
# 32 healthy Asian men, aged 18 to 45 with a BMI of 18.5 to 24.9, in a
# two-period crossover bioequivalence study of two 300 mg tablets (Aprovel and
# a generic), 1664 concentrations in ng/mL from 64 profiles, fitted in
# Monolix.  Two compartments, apparent throughout (oral data only), first-order
# elimination.  Weight, height, age and BMI were tested on every parameter,
# linearly and allometrically, and none was retained.  Table 2, as published,
# with relative standard errors, transcribed into
# inst/extdata/antihypertensiveParameters.csv:
#
#     CL/F    13.5 L/h    (5.15%)
#     Vc/F    13.8 L      (13.2%)
#     Q/F     17.7 L/h    (8.7%)
#     Vp/F    85.8 L      (7.07%)
#     ka      0.304 /h    (4.73%)
#     T       1.68 h      (6.23%), the constant delay of the delay equation
#
# THE INPUT, AND WHY IT IS DISCRETISED
# ====================================
# Most irbesartan profiles in the study had two peaks.  The authors tried lag
# times, transit compartments, gastric-emptying models and enterohepatic
# recirculation, and chose a DELAY DIFFERENTIAL EQUATION for the gut:
#
#     dA0/dt = -ka A0(t - T)
#
# The rate of absorption now is set by the amount that was in the gut T hours
# ago.  Because ka T = 0.511 is above 1/e, the solution oscillates: the
# amount absorbed overshoots to about 105% of the dose by 6 h and then
# settles back to 100%.  With a larger ka T, as in some individuals, that
# oscillation draws a second peak; at the typical values it does not, and the
# typical curve has a single peak at 1.9 h after a dose.  Nothing
# in this package's extravascular path can represent a delayed rate, and no
# non-negative input can represent an overshoot at all.
#
# The text states the history as "A0(t) = 0 for -T <= t <= 0", which would
# absorb nothing for the first 1.68 h.  Table 3 cannot be reproduced that way:
# its times to peak of 1 to 2 h at delays up to 2.3 h need the dose to sit in
# the gut BEFORE time zero, which is Monolix's own convention for a delay
# equation's history.  That reading reproduces Table 3's times to peak, its
# AUC(0-48) within 0.8%, and its peaks within 2% (1586 against 1614 ng/mL at
# 150 mg), so it is the one implemented.  Under it the gut absorbs at the
# constant rate ka x Dose for the first 1.68 h (51% of the dose), then at a
# falling rate.
#
# So each oral dose is given as PULSES of the default oral form
# (`oralPulses`, R/oral-pulses.R), as hydrochlorothiazide's Weibull is, with
# the fractions, delays and depot rate constant below fitted MINIMAX (the
# objective is the worst point of the curve) against an independent numerical
# solution of the exact delay system at the source's typical parameters,
# with the fractions summing to one so that the whole dose is still given and
# the area is unchanged.  The error is measured at every pulse time, where the
# approximation kinks, as R/oral-pulses.R requires.
#
# WHAT IT COSTS, measured on a 0.0005 h grid and at every pulse time against
# the exact solution: 4.7% of the peak at the worst point of the curve (the
# minimax fit spreads the error evenly, so several points in the first five
# hours reach it), with the peak itself 2.3% low and 0.1 h late (2.01 h
# against 1.91 h).  With fat-free-mass scaling on, the error moves with the
# disposition (see BODY SIZE): 5.2% for a 45 kg woman.  That is nine times the 0.53% standard docs/adding-a-drug.md sets, and more
# pulses would not close it: a linear program over ANY number of
# non-negative pulses cannot get below about 3.5% (2026-11-19), because the
# exact input goes negative once the gut "overshoots", and pulses cannot.
# The error is a property of representing this source in this engine.
# Shafer chose this source on 2026-11-20, provisionally, with the error
# stated.  (The 2026-11-19 draft's twelve pulses reached 5.8%; these, fitted
# with the fractions solved exactly as a linear program inside the search over
# delays and rate, reach 4.7%.)
#
# CHECKED AGAINST THE LABEL, WHICH IT WAS NOT FITTED TO
# =====================================================
# The Avapro label (papers/digests/NJ_dailymed-7885b2a8.md):
#   - CL/F 13.5 L/h, inside the label's apparent 11.8-17.6 L/h (CL 157-176
#     mL/min over F 60-80%);
#   - time to peak 1.9 h, inside the label's 1.5-2 h;
#   - terminal half-life 8.2 h, SHORTER than the label's 11-15 h.  The
#     label's other statement on the same point, less than 20% accumulation
#     at steady state, favours the model: 150 mg once daily accumulates by
#     6% here (peak, day 7 over day 1), where a single exponential with a
#     13 h half-life would predict about 39%.  Sampling in the source
#     ran to 48 h.  Which is right for a given patient is not settled here;
#     the trough on daily dosing may be somewhat low.
#   - the label reports concentrations 11-44% higher in women, 20-50% higher
#     in the elderly and about 25% higher in Black subjects.  The source
#     studied young Asian men only and none of this is in the model, beyond
#     the library's own fat-free-mass scaling with the switch on.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate was retained, so the published values are fixed values.
# The source prints only the eligibility ranges (18-45 years, BMI 18.5-24.9),
# not the cohort's central weight, height or age, so by the rule on partial
# data (Shafer, 2026-11-18) the model is NOT anchored at its cohort: it keeps
# the reference anchor, and the published values are the 70 kg reference
# man's.  pkSizeFactors() with legacy factors of 1 reproduces them exactly
# with the fat-free-mass switch off.  The pulses were fitted at the published
# disposition; with the switch on, a patient far from the reference man has a
# different disposition and the error moves: 5.2% of the peak for a 45 kg,
# 155 cm woman and 4.6% for a 120 kg, 185 cm man, both in the tests.  The delay T and the depot are not size quantities and are not
# scaled.
#
# NO RENAL TERM
# =============
# Irbesartan is oxidised mainly by CYP2C9 and glucuronidated; its renal
# clearance is 3.0-3.5 mL/min, about 2% of the total, and about 20% of a dose
# appears in urine as metabolites.  The label reports no change in
# pharmacokinetics in renal impairment or on haemodialysis.  The source
# studied healthy men and tested no renal covariate.  A creatinine entered in
# the Patient Profile does not reach this drug.
#
# NO EFFECT SITE, NO BLOOD PRESSURE, NO BAND
# ==========================================
# Plasma only, as every antihypertensive in this library is
# (docs/antihypertensives.md).  Two blood-pressure models exist for
# irbesartan, both single 300 mg doses in normotensive men and both fitted on
# their own pharmacokinetics, not Karatza's: Hedaya 2015 (SBP Emax 8.3 mmHg,
# EC50 2.1 ug/mL, Hill 1.95, ke0 0.44 /h) and Huang 2006 (SBP Emax 14.8 mmHg,
# EC50 0.29 ug/mL, Hill 0.75, ke0 0.62 /h).  Their EC50s differ sevenfold.
# Steven Shafer has proposed plotting the effect as a percentage of the
# maximum, which removes the baseline; that is a separate piece of work.
# tPeak, MEAC and the band are zero: no therapeutic plasma range is
# established for an angiotensin receptor blocker, which is dosed to pressure.
#
# PREGNANCY
# =========
# Not applicable: angiotensin receptor blockers are contraindicated in the
# second and third trimesters (the Avapro label's boxed warning), so the flag
# changes nothing here.
#
# References
# ----------
# Karatza E, Karalis V.  Eur J Pharm Sci 2020;153:105498.
#   https://doi.org/10.1016/j.ejps.2020.105498
# AVAPRO (irbesartan) tablets, US prescribing information, Sanofi-Aventis,
#   revised 7/2026 (DailyMed setid 7885b2a8-be4e-48ab-8113-4e6ab791eb98).
# -----------------------------------------------------------------------------

# Karatza 2020, Table 2, as published.  Apparent (/F) throughout, at the
# reference man (see BODY SIZE).
IRBESARTAN_CL_F <- 13.5    # L/h   (5.15% RSE)
IRBESARTAN_VC_F <- 13.8    # L     (13.2% RSE)
IRBESARTAN_Q_F  <- 17.7    # L/h   (8.7% RSE)
IRBESARTAN_VP_F <- 85.8    # L     (7.07% RSE)

# The published delay equation, kept although the engine cannot use it,
# because the discretisation below is checked against it and a reader must be
# able to find the shape it stands for.
IRBESARTAN_DDE_KA <- 0.304  # 1/h   (4.73% RSE), the delayed rate constant
IRBESARTAN_DDE_T  <- 1.68   # h     (6.23% RSE), the constant delay

# The discretisation (see the header): fitted, minimax, at the published
# disposition.  A numerical stand-in for a published shape; the fractions say
# nothing about how the tablet is made.
IRBESARTAN_KA <- 1.33987               # 1/h, the depot each pulse is absorbed through
IRBESARTAN_PULSE_FRACTION <- c(0.269396, 0.090533, 0.090634, 0.091040,
                               0.091115, 0.088226, 0.070338, 0.061962,
                               0.052867, 0.042853, 0.031311, 0.019725)
IRBESARTAN_PULSE_DELAY    <- c(1.3796, 19.8584, 38.1029, 56.4681,
                               74.8723, 93.2611, 112.6310, 133.2269,
                               154.9986, 178.1867, 204.1957, 235.2061)  # minutes
IRBESARTAN_PULSE_ERROR_PERCENT <- 4.7  # worst point on the curve, % of peak

#' Irbesartan pharmacokinetics (apparent oral, plasma)
#'
#' Karatza and Karalis (2020), two compartments in 32 healthy men, with a
#' delay-differential absorption that this package discretises into pulses of
#' its default oral form; see the header of \code{R/drugs_irbesartan.R} for
#' the shape it stands for and what the stand-in costs.  Provisional.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
irbesartan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # No size covariate was retained and the cohort's demographics are not
  # printed, so the reference anchor stands (see the header).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = IRBESARTAN_VC_F * size$volume,
    v2  = IRBESARTAN_VP_F * size$volume,
    v3  = 1,                                        # two compartments
    cl1 = IRBESARTAN_CL_F / 60 * size$clearance,    # L/h -> L/min
    cl2 = IRBESARTAN_Q_F  / 60 * size$clearance,
    cl3 = 0,
    # The depot each pulse is absorbed through.  Apparent parameters already
    # carry F, and the delay lives in the pulse delays, not in tlag_PO.
    ka_PO = IRBESARTAN_KA / 60,                     # 1/min
    bioavailability_PO = 1,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # plasma only; see the header
  MEAC  <- 0

  # No therapeutic plasma range is established for irbesartan.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Karatza E, Karalis V. Delay differential equations for the description ",
    "of irbesartan pharmacokinetics. Eur J Pharm Sci 2020;153:105498. ",
    "PROVISIONAL. Two compartments, apparent in bioavailability, from 32 ",
    "healthy men given 300 mg: CL/F 13.5 L/h, Vc/F 13.8 L, Q/F 17.7 L/h, ",
    "Vp/F 85.8 L, no covariates retained. The source's absorption is a DELAY ",
    "differential equation (ka 0.304 /h, delay 1.68 h), built to describe ",
    "the double peaks of individual profiles; the typical curve has one. ",
    "This package cannot run a delay equation, so each dose is given as ",
    length(IRBESARTAN_PULSE_FRACTION), " lagged fractions of the oral dose, ",
    "which reproduce the exact published curve to ",
    IRBESARTAN_PULSE_ERROR_PERCENT, "% of the peak at the worst point. ",
    "Checked against the Avapro label: CL/F and time to peak (1.9 h) inside ",
    "its ranges, terminal half-life 8.2 h against the label's 11-15 h, so the ",
    "trough on daily dosing may be low. Plasma. No blood pressure and no ",
    "therapeutic range are predicted. https://doi.org/10.1016/j.ejps.2020.105498"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Each "mg PO" dose becomes these fractions at these delays, each
      # absorbed through the depot above (R/oral-pulses.R).  The reserved
      # default-form key: irbesartan has one oral product.
      oralPulses = stats::setNames(
        list(list(fraction = IRBESARTAN_PULSE_FRACTION,
                  delay    = IRBESARTAN_PULSE_DELAY)),
        ORAL_PULSE_DEFAULT_FORM)
    )
  )
}
