# -----------------------------------------------------------------------------
# Mirtazapine: oral only, one compartment, apparent parameters
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, from
# a specification giving the published values below.  Verified by
# tests/testthat/test-drugs-mirtazapine.R, whose expectations are worked out
# from the published numbers by hand, not from this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min (the source's
# L/h divided by 60), rate constants per minute (per hour divided by 60),
# concentrations in ng/mL of total plasma mirtazapine (racemate).  Doses are
# mg of mirtazapine as the tablets are labelled.
#
# THE SOURCE
# ==========
# Yan et al., Drug Design, Development and Therapy 2026 (doi
# 10.2147/DDDT.S601238): 105 Chinese inpatients with depression, 210 plasma
# concentrations from therapeutic drug monitoring, one compartment.  Table 2:
#
#     CL/F = 28.9 L/h x (1 - 0.294 if BMI >= 28 kg/m2)
#                     x (1 - 0.269 with paroxetine)
#                     x (1 - 0.511 with fluvoxamine)
#     V/F  = 310 L
#     ka   = 1.2 /h, fixed  (the table prints the unit as "L/h", a typo for /h)
#     IIV (variance) 0.0613 on CL/F, 0.131 on V/F; residual 0.0747 (not used)
#
# SOURCE DISCREPANCY.  The abstract gives CL/F = 29.3 L/h and V/F = 348 L,
# which do not match Table 2.  Table 2 is the parameter table of the final
# model and is used here; the discrepancy is recorded in the model's return
# value (sourceDiscrepancy, which getDrugPK() ignores), on the help page and
# in the tests.  With the abstract's values the steady-state average would be
# 1.4% lower and the half-life 8.2 h rather than 7.4 h.
#
# BMI.  BMI = weight / (height / 100)^2 on actual weight, as published, and
# the 29.4% reduction applies at BMI >= 28 (the Chinese threshold for
# obesity).  It is a step, not a gradient.
#
# CO-MEDICATION, NOT IMPLEMENTED.  The paroxetine (x 0.731) and fluvoxamine
# (x 0.489) effects are not applied: the app has no co-medication input, and
# whether the two act together in one patient as a product was not verified.
# The curve is a patient's on neither.  Both inhibit mirtazapine's
# metabolism (fluvoxamine CYP1A2, paroxetine CYP2D6), so a patient taking
# either will have higher concentrations than plotted.
#
# APPARENT SCALE, ORAL ONLY
# =========================
# Every patient took mirtazapine by mouth, and it has no intravenous product,
# so CL and V are divided by the unmeasured bioavailability F (about 50% by
# the label).  Apparent parameters predict oral concentrations correctly,
# because F cancels, and intravenous ones wrong by 1/F (docs/adding-a-drug.md,
# "Apparent parameters restrict the route").  The drug is offered only as
# mg PO, mg PO qd and mg PO bid; bioavailability_PO is 1 because the apparent
# scale already contains F, and there is no lag.  ka is the source's fixed
# 1.2 /h = 0.02 /min.
#
# One compartment: v2 and v3 are placeholders of 1 L with cl2 = cl3 = 0.
#
# HALF-LIFE: A TROUGH-DATA ARTEFACT
# =================================
# ln 2 x 310 / 28.9 = 7.4 h, against the label's 20 to 40 h.  Two
# concentrations per patient, mostly troughs at steady state, identify the
# clearance (the steady-state average is D / tau / CL, whatever V is) but not
# the volume; the small V/F, and so the short half-life, is an artefact of
# the design.  The profile shape within a dosing interval, the peak-to-trough
# swing and the time to steady state are therefore NOT reliable here; the
# steady-state average is what the data identify.  With 30 mg once daily the
# model's curve swings far more over a day than mirtazapine does.
#
# NO EFFECT SITE
# ==============
# tPeak = 0 (no ke0) and MEAC = 0 (not an opioid): the plot is the plasma
# concentration.  The antidepressant response lags the concentration by weeks
# and no validated equation relates a mirtazapine concentration to
# remission.  Sato 2013 found 80-90% histamine H1 receptor occupancy by PET
# after 15 mg, the basis of the drug's sedation, but reported no EC50;
# it is context, not plotted.
#
# The band in inst/extdata/drugDefaults_global.csv is the AGNP 2018 consensus
# therapeutic reference range (Hiemke et al., Pharmacopsychiatry
# 2018;51:9-62), 30 to 80 ng/mL, with 50 as the typical value; typical,
# upperTypical and lowerTypical below mirror it.  endCe is 0.
#
# COVARIATES AND BODY SIZE (docs/weight-adjustment.md)
# ====================================================
# The model has no size covariate on volume or clearance, so the published
# values are fixed parameters, read as the 70 kg reference man's, and take
# the library's default scaling: with the switch on, volume x the
# fat-free-mass ratio and clearance x that ratio ^ 0.75; with it off, the
# published values unscaled (legacyVolume = 1).  The BMI flag is applied as
# published in both positions, so with the switch off Yan's model is
# reproduced exactly.  The cohort's body size was not available, so no
# re-anchoring to its fat-free mass was possible.
#
# The overlap, stated plainly: with the switch on, an obese patient's larger
# fat-free mass RAISES clearance while the BMI flag LOWERS it.  For a 120 kg,
# 170 cm, 50-year-old man (BMI 41.5) the two give 28.9 x 1.2209 x 0.706 =
# 24.9 L/h.  The BMI effect is the source's finding, made without a size
# covariate, and is kept; the fat-free-mass scaling is the library's rule for
# every fixed-parameter model.  With the switch off he receives Yan's 20.4.
#
# NOT MIXED IN
# ============
# Grasmader et al. 2004 (CYP2D6 intermediate metabolisers about 26% lower
# clearance) and Brockmoller et al. 2007 (enantiomer-specific kinetics) are
# separate analyses; neither is combined with Yan's estimates.
#
# References
# ----------
# Yan et al. Drug Des Devel Ther 2026 (population pharmacokinetics of
#   mirtazapine in 105 Chinese inpatients with depression).
#   https://doi.org/10.2147/DDDT.S601238
# Hiemke C et al. Consensus guidelines for therapeutic drug monitoring in
#   neuropsychopharmacology: update 2017. Pharmacopsychiatry 2018;51:9-62.
# Sato H et al. 2013 (H1 receptor occupancy, PET).
# Grasmader K et al. 2004 (CYP2D6; not used).
# Brockmoller J et al. 2007 (enantiomers; not used).
# -----------------------------------------------------------------------------

# Yan 2026, Table 2.  Apparent parameters (divided by F), litres, L/h, per hour.
MIRTAZAPINE_CL            <- 28.9    # L/h, CL/F (abstract: 29.3)
MIRTAZAPINE_V             <- 310     # L, V/F (abstract: 348)
MIRTAZAPINE_KA            <- 1.2     # 1/h, fixed (table prints "L/h")
MIRTAZAPINE_BMI_THRESHOLD <- 28      # kg/m2, BMI >= this lowers CL/F
MIRTAZAPINE_BMI_EFFECT    <- 0.294   # fractional reduction of CL/F

MIRTAZAPINE_SOURCE_DISCREPANCY <- paste0(
  "Yan 2026: Table 2 gives CL/F 28.9 L/h and V/F 310 L (used); the abstract ",
  "gives CL/F 29.3 L/h and V/F 348 L. Table 2 also prints ka's unit as L/h, ",
  "a typo for /h."
)

#' Mirtazapine pharmacokinetics
#'
#' Apparent oral one-compartment model of Yan et al. (2026), 105 Chinese
#' inpatients with depression; clearance 29.4% lower at a BMI of 28 or more.
#' Oral only.  See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string
#' @param adjustToFFM scale the volume to the patient's fat-free mass and
#'   clearance to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.  The BMI effect applies either way.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mirtazapine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters, read as the reference man's: legacy factors 1.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # As published: BMI on actual weight, a step at 28 kg/m2.
  bmi <- weight / (height / 100)^2
  obese <- if (bmi >= MIRTAZAPINE_BMI_THRESHOLD) 1 else 0
  clApparent <- MIRTAZAPINE_CL * (1 - MIRTAZAPINE_BMI_EFFECT * obese)   # L/h

  default <- list(
    v1 = MIRTAZAPINE_V * size$volume,
    v2 = 1,                                    # one compartment
    v3 = 1,
    cl1 = clApparent / 60 * size$clearance,    # L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = MIRTAZAPINE_KA / 60,               # 1/min
    bioavailability_PO = 1,                    # apparent (/F) parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL plasma: AGNP 2018 therapeutic reference range.  The plot reads
  # the CSV; these mirror it.
  typical      <- 50
  upperTypical <- 80
  lowerTypical <- 30

  reference <- paste0(
    "Yan et al., Drug Des Devel Ther 2026. One compartment, apparent oral ",
    "clearance (29.4% lower at BMI >= 28) and volume, Table 2 values; ",
    "oral only; plasma only. https://doi.org/10.2147/DDDT.S601238"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Machine-readable note of the abstract/table mismatch.  getDrugPK()
      # reads named fields only, so this is carried for readers and tests.
      sourceDiscrepancy = MIRTAZAPINE_SOURCE_DISCREPANCY
    )
  )
}
