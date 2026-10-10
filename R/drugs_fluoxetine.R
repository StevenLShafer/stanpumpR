# -----------------------------------------------------------------------------
# Fluoxetine: apparent oral one-compartment model, with norfluoxetine as its
# active metabolite (Han 2025)
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Parameters from the full text (via PubMed Central, PMC12736537); the
# conventions were confirmed against the paper's own simulations (Table S4 of
# its Supplementary Materials, supplied by Dr Shafer).  Verified by
# tests/testthat/test-drugs-fluoxetine.R, which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  The source reports L and L/h.
#
# READ THIS FIRST: ONLY THE STEADY-STATE TROUGH IS VALID
# =====================================================
# Han and colleagues fitted 241 fluoxetine and 241 norfluoxetine
# concentrations from 198 Chinese psychiatric patients (median age 17, range
# 12 to 56; 74% women; 20 to 60 mg/day), EVERY ONE a steady-state trough drawn
# before the next dose.  Such data identify how the trough scales with dose
# and sex, and nothing about the time course.  The fitted parameters give
# half-lives of 5.9 hours for fluoxetine (5.1 in men) and 20 minutes for
# norfluoxetine.  The label gives 4 to 6 days for fluoxetine and 4 to 16 days
# for norfluoxetine after chronic dosing.
#
# So on this model:
#   - the trough 24 hours after a once-daily dose at steady state is what the
#     source fitted and is reproduced (below);
#   - steady state is reached within about a day, where real fluoxetine takes
#     about a month and norfluoxetine longer;
#   - after the last dose both curves fall to near zero within a day or two,
#     where real fluoxetine and norfluoxetine persist for weeks.  This matters
#     clinically (the five-week washout before a monoamine oxidase inhibitor),
#     and the curve must not be read as showing it;
#   - the peak within a dosing interval is about four times the trough, where
#     the real fluctuation is small.
# Steven L. Shafer chose, 2026-10-10, to add the model on these terms rather
# than leave fluoxetine out or use Panchaud 2011's parent-only model of
# perinatal women; the help page carries the same warning.
#
# THE UNITS ARE AS PRINTED
# ========================
# The half-lives first suggested a units error.  They are not one: the
# printed values, read as litres, litres per hour and hours, reproduce the
# medians of the paper's own simulated steady-state troughs (Table S4, 1000
# virtual patients per arm) to within 3%:
#
#     20 mg qd               Table S4    this model
#     fluoxetine, female     86.0        83.8 ng/mL
#     fluoxetine, male       58.8        57.1
#     norfluoxetine, female  78.9        79.5
#     norfluoxetine, male    63.7        63.8
#
# and Table S4 is exactly proportional to dose.  This also settles the two
# conventions the text left open: women are the reference (CL/F 2.91 L/h,
# men 16.5% higher), and all of fluoxetine's clearance forms norfluoxetine
# mass for mass (FM = 1, no molecular-weight conversion).
#
# DISPOSITION (Section 3.5)
# =========================
#     fluoxetine      CL/F 2.91 L/h (women; x 1.165 in men)   V/F 24.9 L
#     norfluoxetine   CL/F 3.24 L/h                           V/F 1.52 L
#     ka 0.3 /h, fixed from the literature (trough data cannot estimate it)
# IIV 31.6% on fluoxetine CL/F and 20.9% on norfluoxetine CL/F, not used.
#
# NORFLUOXETINE FORMATION
# =======================
# FM was fixed at 1: every milligram of fluoxetine cleared becomes a
# milligram of norfluoxetine.  In this package's terms kFormation = cl1 / v1
# (the parent's own k10), firstPassFraction = 0 and mwRatio = 1, as for
# amiodarone and desethylamiodarone.  kFormation is computed from the scaled
# cl1 and v1 so it stays the parent's k10 under size scaling.
#
# NO EFFECT SITE, AND NO BAND
# ===========================
# Fluoxetine and norfluoxetine are both active serotonin reuptake inhibitors.
# No concentration-effect relation for antidepressant response exists, and
# no plasma EC50 for transporter occupancy could be verified, so tPeak is
# zero and both rows plot plasma.  prodrug = FALSE marks an active parent.
# The AGNP 2018 range, 120 to 500 ng/mL, is for fluoxetine PLUS
# norfluoxetine, which split about evenly, so neither row carries a band
# (the CSV has zeros); the help page gives the range.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Weight was tested and not retained ("failed" in the covariate
# search of Table S3, a narrow weight range).  Fixed published values, taken as the 70 kg
# reference man's, with the library's fat-free-mass scaling; legacy factors 1
# reproduce the published values with the switch off.  Norfluoxetine is
# scaled identically.  The cohort's median weight was 59 kg, smaller than the
# reference man; no re-anchoring was attempted.
#
# References
# ----------
# Han B, et al. Bridging literature and real-world evidence: external
#   evaluation and development of fluoxetine population pharmacokinetics
#   model. Pharmaceutics 2025;17:1516.
#   https://doi.org/10.3390/pharmaceutics17121516
# Hiemke C, et al. Consensus guidelines for therapeutic drug monitoring in
#   neuropsychopharmacology: update 2017. Pharmacopsychiatry 2018;51:9-62.
# -----------------------------------------------------------------------------

FLUOXETINE_CL1 <- 2.91          # L/h, CL/F in women
FLUOXETINE_MALE_CL <- 1.165     # men's clearance multiplier
FLUOXETINE_V1  <- 24.9          # L, V/F
FLUOXETINE_KA  <- 0.3           # 1/h, fixed in the source

# One citation for both members of the pair, so the bibliography lists it once.
FLUOXETINE_REFERENCE <- paste0(
  "Han B, et al. Bridging literature and real-world evidence: external ",
  "evaluation and development of fluoxetine population pharmacokinetics ",
  "model. Pharmaceutics 2025;17:1516 (steady-state troughs only; band: ",
  "Hiemke et al., Pharmacopsychiatry 2018;51:9-62). ",
  "https://doi.org/10.3390/pharmaceutics17121516"
)

#' Fluoxetine pharmacokinetics, forming norfluoxetine
#'
#' Apparent oral one-compartment model of Han et al. (2025), fitted to
#' steady-state troughs only: the trough is valid, the time course is not.
#' See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years (not used by the source)
#' @param sex sex as a string; men's clearance is 16.5% higher
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published values unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   norfluoxetine as the active metabolite
#' @export
fluoxetine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters: legacy factors 1.  Norfluoxetine must make
  # this call identically.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  sexFactor <- if (sex == SEX_MALE) FLUOXETINE_MALE_CL else 1

  v1  <- FLUOXETINE_V1 * size$volume
  v2  <- 1                                                            # one compartment
  v3  <- 1
  cl1 <- FLUOXETINE_CL1 * sexFactor / MINS_PER_HOUR * size$clearance  # L/min
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = FLUOXETINE_KA / MINS_PER_HOUR,   # 1/min
    bioavailability_PO = 1,                  # inside the apparent scale
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      # The AGNP range is for the sum; no band on either row (CSV zeros).
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = FLUOXETINE_REFERENCE,
      prodrug = FALSE,
      metabolite = list(
        name              = "norfluoxetine",
        # FM = 1: all of the parent's clearance forms the metabolite
        kFormation        = cl1 / v1,
        firstPassFraction = 0,
        mwRatio           = 1
      )
    )
  )
}
