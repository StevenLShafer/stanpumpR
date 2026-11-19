# -----------------------------------------------------------------------------
# Bupropion (sustained-release tablet), with hydroxybupropion as its active
# metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Verified by tests/testthat/test-drugs-bupropion.R, which derives its
# expectations independently of this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of plasma bupropion (Concentration.Units "ng" for
# both members of the pair, so metaboliteUnitScale() is 1).  The source
# reports clearances in L/h and ka in 1/h; they are divided by MINS_PER_HOUR
# here.  Doses are mg of bupropion hydrochloride as labelled, as the source's
# subjects took them; no salt conversion is applied, F absorbing it.
#
# THE SOURCE
# ==========
# Ghimire et al., J Clin Pharm Ther 2026 (doi 10.1155/jcpt/3265655): a single
# 150 mg sustained-release (SR) tablet in 19 adults, a sample weighted towards
# chronic kidney disease, with bupropion and hydroxybupropion measured in
# plasma.  Two compartments for each, all parameters apparent (divided by the
# unmeasured oral bioavailability F):
#
#   Bupropion                      Hydroxybupropion (conditional on fm)
#     Vc/F   524.6 L                 Vc/F   2.2 L
#     CL/F   148.4 L/h               CL/F   0.9 L/h
#     Vp/F   5006.9 L                Vp/F   17.7 L
#     Q/F    257 L/h                 Q/F    3.1 L/h
#     ka     0.29 /h
#
# Residual errors (0.48 and 0.38) and the incomplete interindividual
# variability are not used.  The covariates the source tested (CKD, vitamin
# D) were not retained, so there is no renal covariate: the model is a
# typical adult's.
#
# These give bupropion half-lives of 0.86 h and 38.5 h, and hydroxybupropion
# disposition half-lives of 0.35 h and 18.9 h.  The terminal bupropion
# half-life is longer than the ~21 h usually quoted; the source's single dose
# and small sample are the likely reason, and the value is used as published.
# Because the parent is then the slower of the pair, a formed
# hydroxybupropion curve falls, late, at the parent's 38.5 h rate, not its own
# 18.9 h (formation-rate limited).
#
# APPARENT PARAMETERS, ORAL ONLY
# ==============================
# No subject received intravenous bupropion (there is no intravenous
# product), so every parameter is apparent.  Apparent parameters predict oral
# concentrations correctly, because F cancels, and would predict intravenous
# ones wrong by 1/F (docs/adding-a-drug.md, "Apparent parameters restrict the
# route").  The drug therefore offers oral units only and carries
# bioavailability_PO = 1.
#
# THE ORAL CURVE IS THE SUSTAINED-RELEASE TABLET'S
# ================================================
# ka = 0.29 /h is the apparent absorption of the SR tablet, which includes
# its release from the matrix.  The units are the generic "mg PO", "mg PO qd"
# and "mg PO bid", but the curve they draw is the SR tablet's.  The
# immediate-release tablet (faster absorption, higher peak) and the
# extended-release XL tablet are NOT modelled: Zhang and colleagues (2019)
# found CL/F 221 L/h at 60.9 kg with an absorption lag for XL, and Connarn
# and colleagues' 2017 IR/SR/ER crossover (AAPS J 2017;19:1513-1522) found
# relative bioavailabilities of 1, 0.955 and 0.68,
# so XL doses entered here would be overpredicted by about a third.  Daily
# exposure on a steady regimen depends only on dose over CL/F, so the
# average concentrations at steady state are less formulation-sensitive than
# the peaks and troughs.  Kharasch and colleagues' 2019 XL steady-state data
# are an external comparison only, not a source of this model.
#
# HYDROXYBUPROPION
# ================
# The source splits bupropion's clearance into a nonforming part,
# CL_B = (1 - fm) x CL, and a formation clearance, CL_F = fm x CL, with fm
# FIXED at 0.1.  The metabolite's input is therefore CL_F x Cp = 0.1 x CL/F
# x Cp, once.  In the library, kFormation is a first-order transfer out of
# the parent's central compartment added on top of a parent disposition
# that stands unchanged, the parent's fitted total CL already containing the
# formation (docs/adding-a-drug.md).  That is exactly the source's structure:
# total CL/F 148.4 L/h, of which 0.1 forms hydroxybupropion.  So
# kFormation = fm x CL/F over Vc/F = 0.1 x 148.4 / 524.6 per hour, computed
# from the SCALED cl1 and v1 (as amiodarone does) so that it stays fm times
# the parent's own k10 under size scaling.  firstPassFraction = 0: the source
# has no first-pass branch, so formation follows the systemic parent.
#
# mwRatio = 1, on a mass basis.  The source's convention for converting the
# formed amount was not recovered; the metabolite's apparent volumes were
# fitted conditional on fm and on whatever convention it used, so the mass
# basis is assumed, as amiodarone's link is.  Had the source converted on a
# molar basis the formed concentrations here would be low by the ratio
# 255.74 / 239.74 = 1.067 (hydroxybupropion over bupropion, free bases).
#
# At steady state the formed hydroxybupropion AUC is fm x CL/F over the
# metabolite's CL/F times the parent's: 14.84 / 0.9 = 16.5-fold.
#
# AN ACTIVE PARENT WITH NO EFFECT SITE, NOT A PRODRUG
# ===================================================
# Bupropion is itself an active noradrenaline-dopamine reuptake inhibitor;
# hydroxybupropion is also active and, by exposure, dominates.  Neither has an
# effect-site model: there is no validated concentration-response relation
# for the antidepressant effect (Golden 1988's responder contrast was tiny),
# and PET gives only context (Learned-Coughlin 2003: about 26% striatal
# dopamine-transporter occupancy on an SR regimen, with no EC50).  tPeak is
# therefore zero and the plot shows plasma concentrations.  prodrug = FALSE
# tells the help page that this is an active parent that lacks an effect-site
# model, not a prodrug.
#
# THE BAND
# ========
# The AGNP therapeutic reference range (update 2026, Hart et al.,
# Pharmacopsychiatry 2026;59:166-295, Table 5) is 850 to 1500 ng/mL of
# HYDROXYBUPROPION, the major active compound; bupropion itself is unstable
# in collected blood.  inst/extdata/drugDefaults_global.csv puts it on the
# hydroxybupropion row and draws no band on the bupropion row (0/0/0).  The model mirrors that here.  MEAC and
# endCe are zero: not an opioid, and no threshold.
#
# COVARIATES AND BODY SIZE (docs/weight-adjustment.md)
# ====================================================
# None retained (weight, BMI, age, sex, race, eGFR, CKD, vitamin D and
# CYP2B6 were tested).  So the published values are the cohort's typical
# patient's, and that patient was not the library's 70 kg reference man:
# Table 2 (read on the PDF, 2026-11-18) gives median weights of 99.2 kg (CKD,
# n 13) and 100.8 kg (controls, n 6), median BMIs of 34 and 34.85, median
# ages of 61 and 52, and 10 women of 19.  The parameters are therefore
# anchored at the cohort (Steven L. Shafer's decision, 2026-11-18, the
# library's rule since: docs/weight-adjustment.md, *Anchoring at the
# source's cohort*): with the
# fat-free-mass switch on, each published value is divided by the size
# factor of the cohort's typical patient (cohortSizeFactors()), so a
# patient like the cohort's receives the published values and the 70 kg
# reference man smaller ones (volumes / 1.092, clearances / 1.068).  The
# typical patient is the count-weighted mean of the two groups' medians
# (99.7 kg, BMI 34.27, 58.2 years; no height is printed, so it is the one the
# weight and BMI imply, 170.6 cm), and the factor is the geometric mean of a
# man's and a woman's, weighted 9 to 10, because a 100 kg man's fat-free
# mass is about a fifth above a 100 kg woman's and the cohort was half each.
# With the switch off the published values are used unscaled, as before.
# Hydroxybupropion makes the identical calls: the pair's apparent scale is
# consistent only if both scale together.  The source studied adults;
# children are extrapolation.
#
# References
# ----------
# Ghimire et al. J Clin Pharm Ther 2026: population pharmacokinetics of
#   bupropion and hydroxybupropion after a single 150 mg SR dose (the
#   article's title is not reproduced here). https://doi.org/10.1155/jcpt/3265655
# Hiemke C et al. Consensus guidelines for therapeutic drug monitoring in
#   neuropsychopharmacology: update 2017. Pharmacopsychiatry 2018;51:9-62.
# Learned-Coughlin SM et al. In vivo activity of bupropion at the human
#   dopamine transporter as measured by positron emission tomography. Biol
#   Psychiatry 2003;54:800-805.
# Zhang F, Li Y, Hu J, Zhong J, Li H. Population pharmacokinetics, safety
#   and tolerability of extended-release bupropion and its three metabolites
#   in Chinese healthy volunteers. Eur J Drug Metab Pharmacokinet
#   2019;44:339-352. https://doi.org/10.1007/s13318-018-0537-z
# Connarn JN et al. Pharmacokinetics and pharmacogenomics of bupropion in
#   three different formulations with different release kinetics in healthy
#   human volunteers. AAPS J 2017;19:1513-1522.
#   https://doi.org/10.1208/s12248-017-0102-8
# -----------------------------------------------------------------------------

# Ghimire 2026.  Apparent parameters (divided by F), litres, litres per hour
# and per hour, converted to per minute in the model.
BUPROPION_V1  <- 524.6    # L, Vc/F
BUPROPION_V2  <- 5006.9   # L, Vp/F
BUPROPION_CL1 <- 148.4    # L/h, CL/F (total, including formation)
BUPROPION_CL2 <- 257      # L/h, Q/F
BUPROPION_KA  <- 0.29     # 1/h, SR tablet
BUPROPION_FM  <- 0.1      # fraction of CL/F forming hydroxybupropion, FIXED

# Ghimire 2026, Table 2: the cohort's typical patient, the count-weighted
# mean of the CKD (n 13) and control (n 6) medians; 9 men and 10 women.  No
# height is printed, so the BMI gives it.
BUPROPION_COHORT <- list(
  weight = (13 * 99.2 + 6 * 100.8) / 19,   # kg
  bmi    = (13 * 34 + 6 * 34.85) / 19,     # kg/m2
  age    = (13 * 61 + 6 * 52) / 19,        # years
  men    = 9,
  women  = 10
)

#' The size factors a patient takes in Ghimire's pair: relative to the
#' cohort's typical patient (cohortSizeFactors()), or the published values
#' unscaled with the switch off.  Shared by bupropion and hydroxybupropion.
#' @noRd
bupropionSizeFactors <- function(weight, height, age, sex, adjustToFFM)
{
  cohort <- with(BUPROPION_COHORT,
                 cohortSizeFactors(weight, age, men, women, bmi = bmi))
  pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1,
                cohort = cohort)
}

# The citation both members of the pair return: one string, so that the
# bibliography lists one item for both drugs.
BUPROPION_REFERENCE <- paste0(
  "Ghimire et al. Bupropion and hydroxybupropion population ",
  "pharmacokinetics after a single 150 mg sustained-release dose. ",
  "J Clin Pharm Ther 2026. https://doi.org/10.1155/jcpt/3265655"
)

#' Bupropion pharmacokinetics (sustained-release tablet), forming
#' hydroxybupropion
#'
#' Apparent oral parameters of Ghimire et al. (2026), single 150 mg SR dose.
#' See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM scale volumes to the patient's fat-free mass relative to
#'   that of Ghimire's typical patient, and clearances to that ratio to the
#'   0.75 power; when \code{FALSE}, use the published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   hydroxybupropion as the active metabolite
#' @noRd
bupropion <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Anchored at Ghimire's cohort; with the switch off, the published values.
  # Hydroxybupropion must make this call identically.
  size <- bupropionSizeFactors(weight, height, age, sex, adjustToFFM)

  v1  <- BUPROPION_V1 * size$volume
  v2  <- BUPROPION_V2 * size$volume
  v3  <- 1                                                  # no third compartment
  cl1 <- BUPROPION_CL1 / MINS_PER_HOUR * size$clearance     # L/min
  cl2 <- BUPROPION_CL2 / MINS_PER_HOUR * size$clearance     # L/min
  cl3 <- 0

  # The SR tablet's apparent absorption.  F is inside the apparent scale.
  ka_PO              <- BUPROPION_KA / MINS_PER_HOUR        # 1/min
  bioavailability_PO <- 1
  tlag_PO            <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      # No band on the parent: the AGNP range is hydroxybupropion's and is
      # drawn on its row.  The CSV carries the same zeros.
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = BUPROPION_REFERENCE,
      # An active parent without an effect site, not a prodrug: read by the
      # help page only.
      prodrug = FALSE,
      metabolite = list(
        name              = "hydroxybupropion",
        # Formation clearance fm x CL/F, as a rate constant out of Vc/F
        kFormation        = BUPROPION_FM * cl1 / v1,
        firstPassFraction = 0,
        mwRatio           = 1
      )
    )
  )
}
