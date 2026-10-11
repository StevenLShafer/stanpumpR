# -----------------------------------------------------------------------------
# Duloxetine: oral only, one compartment, apparent parameters
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, from
# a specification giving the published values below.  Verified by
# tests/testthat/test-drugs-duloxetine.R, whose expectations are worked out
# from the published numbers by hand, not from this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min (the source's
# L/h divided by 60), rate constants per minute (per hour divided by 60),
# concentrations in ng/mL of total plasma duloxetine.  Doses are mg of
# duloxetine as the capsules are labelled.
#
# THE SOURCE
# ==========
# Zhong et al., Chinese Journal of Clinical Pharmacology 2026 (doi
# 10.13699/j.cnki.1001-6821.2026.10.009): 325 depressed inpatients, 450
# plasma concentrations from therapeutic drug monitoring, fitted with a
# one-compartment model:
#
#     CL/F = 64.6 x (1 - 0.25 x female) L/h     IIV 42.2%
#     V/F  = 1530 L
#     ka   = 0.168 /h, fixed
#     proportional residual error 27.1%
#
# Women therefore clear duloxetine 25% more slowly: 48.45 L/h against 64.6.
# The variability terms are not used: the plot is the typical patient's.
#
# APPARENT SCALE, ORAL ONLY
# =========================
# Every patient took duloxetine by mouth, and duloxetine has no intravenous
# product, so CL and V are divided by the unmeasured bioavailability F (about
# 50% by the label).  Apparent parameters predict oral concentrations
# correctly, because F cancels, and intravenous ones wrong by 1/F
# (docs/adding-a-drug.md, "Apparent parameters restrict the route").  The
# drug is offered only as mg PO, mg PO qd and mg PO bid; bioavailability_PO
# is 1 because the apparent scale already contains F, and there is no lag.
# ka is the source's fixed value, 0.168 /h = 0.0028 /min.
#
# One compartment: v2 and v3 are placeholders of 1 L with cl2 = cl3 = 0.
#
# HALF-LIFE
# =========
# ln 2 x 1530 / 64.6 = 16.4 h in a man (21.9 h in a woman), against the
# label's mean of about 12 h (8 to 17).  The data were monitoring samples,
# mostly troughs, which identify the steady-state average (D / tau / CL)
# better than the shape of the curve within a dosing interval; the longer
# half-life is the source's estimate and is kept.
#
# NO EFFECT SITE
# ==============
# tPeak = 0 (no ke0) and MEAC = 0 (not an opioid): the plot is the plasma
# concentration.  The antidepressant response lags the concentration by weeks,
# through adaptive changes downstream of transporter blockade, and no
# validated equation relates a duloxetine concentration to remission.  PET
# gives transporter occupancy, a biomarker rather than a clinical effect:
# Takano 2006 found serotonin-transporter occupancy with an EC50 of
# 3.7 ng/mL, so SERT is nearly saturated throughout the therapeutic range;
# Moriguchi 2017 found norepinephrine-transporter occupancy with an EC50 of
# 58.0 ng/mL in healthy men, and Moriguchi 2025 71.21 ng/mL in major
# depressive disorder.  None is plotted: an occupancy curve would need an
# Emax, which setting to 1 would be an assumption, and an effect-site delay
# that no study measured.  Yuen 2013's pharmacodynamic model was for
# diabetic neuropathic pain and does not predict response in depression.
#
# The band in inst/extdata/drugDefaults_global.csv is the AGNP 2018 consensus
# therapeutic reference range (Hiemke et al., Pharmacopsychiatry
# 2018;51:9-62), 30 to 120 ng/mL, with 60 as the typical value; typical,
# upperTypical and lowerTypical below mirror it.  endCe is 0.
#
# COVARIATES AND BODY SIZE (docs/weight-adjustment.md)
# ====================================================
# Sex is the only covariate, and it is applied as published (sex ==
# SEX_FEMALE).  The model has no size covariate, so the published values are
# fixed parameters and take the library's default scaling for that class:
# with the switch on, volume x the fat-free-mass ratio and clearance x that
# ratio ^ 0.75; with it off, the published values unscaled (legacyVolume = 1),
# which reproduces Zhong exactly whatever the patient's size.  The published
# values are read as the 70 kg reference man's.  The cohort's body size was
# not available, so no re-anchoring to the cohort's fat-free mass was
# possible; for a Chinese inpatient cohort the reference man is probably
# somewhat larger than the typical patient studied.  Note that sex then acts
# twice with the switch on: a woman has a smaller fat-free mass as well as
# the published 25% lower clearance.  The 25% is the source's finding, made
# without a size covariate, so part of it may be body size; it is kept as
# published.
#
# NOT IMPLEMENTED
# ===============
# Lobo et al. 2009 pooled adult model (sex, smoking, age and dose effects):
# its fixed-effect table could not be verified, so it is not used.  Shibata
# et al. 2023, Japanese children and adolescents with major depressive
# disorder (CL/F 81.4 L/h, V/F 1170 L, ka 0.168 /h): a separate population.
# Neither is mixed with Zhong's estimates.
#
# References
# ----------
# Zhong et al. Chinese Journal of Clinical Pharmacology 2026 (population
#   pharmacokinetics of duloxetine in 325 depressed inpatients).
#   https://doi.org/10.13699/j.cnki.1001-6821.2026.10.009
# Hiemke C et al. Consensus guidelines for therapeutic drug monitoring in
#   neuropsychopharmacology: update 2017. Pharmacopsychiatry 2018;51:9-62.
# Takano A et al. 2006 (SERT occupancy, PET).
# Moriguchi S et al. 2017 (NET occupancy, healthy men); Moriguchi S et al.
#   2025 (NET occupancy, major depressive disorder).
# Lobo ED et al. Clin Pharmacokinet 2009 (pooled adult model; not used).
# Shibata M et al. 2023 (pediatric MDD; not used).
# Yuen E et al. 2013 (neuropathic pain PD; not used).
# -----------------------------------------------------------------------------

# Zhong 2026.  Apparent parameters (divided by F), litres, L/h and per hour.
DULOXETINE_CL         <- 64.6    # L/h, CL/F in men
DULOXETINE_CL_FEMALE  <- 0.25    # fractional reduction of CL/F in women
DULOXETINE_V          <- 1530    # L, V/F
DULOXETINE_KA         <- 0.168   # 1/h, fixed

#' Duloxetine pharmacokinetics
#'
#' Apparent oral one-compartment model of Zhong et al. (2026), 325 depressed
#' inpatients; clearance 25% lower in women.  Oral only.  See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM scale the volume to the patient's fat-free mass and
#'   clearance to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
duloxetine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters, read as the reference man's: legacy factors 1.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  female <- if (identical(sex, SEX_FEMALE)) 1 else 0
  clApparent <- DULOXETINE_CL * (1 - DULOXETINE_CL_FEMALE * female)   # L/h

  default <- list(
    v1 = DULOXETINE_V * size$volume,
    v2 = 1,                                    # one compartment
    v3 = 1,
    cl1 = clApparent / 60 * size$clearance,    # L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = DULOXETINE_KA / 60,                # 1/min
    bioavailability_PO = 1,                    # apparent (/F) parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL plasma: AGNP 2018 therapeutic reference range.  The plot reads
  # the CSV; these mirror it.
  typical      <- 60
  upperTypical <- 120
  lowerTypical <- 30

  reference <- paste0(
    "Zhong et al., Chin J Clin Pharmacol 2026. One compartment, apparent oral ",
    "clearance (25% lower in women) and volume; oral only; plasma only. ",
    "https://doi.org/10.13699/j.cnki.1001-6821.2026.10.009"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
