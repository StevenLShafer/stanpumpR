# -----------------------------------------------------------------------------
# Escitalopram: apparent oral one-compartment kinetics with CYP2C19 phenotype
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, from
# his summary of the source.  Verified by
# tests/testthat/test-drugs-escitalopram.R, which derives its expectations
# from the published numbers independently of this file.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of escitalopram (the CSV's Concentration.Units).
# The source reports L/h and 1/h; both are divided by 60 here.
#
# THE SOURCE
# ==========
# Liu et al., Front Pharmacol 2022;13:964758: 106 Chinese psychiatric
# patients on oral escitalopram, sampled mostly at trough (therapeutic drug
# monitoring).  One compartment with first-order absorption:
#
#     CL/F   16.3 L/h  (CYP2C19 normal, "EM", the reference group)
#     V/F    815 L
#     ka     0.6 /h, FIXED (troughs cannot identify absorption)
#
# CYP2C19 multipliers on CL/F, relative to the EM group: intermediate (IM)
# 0.847, poor (PM) 0.479.  Interindividual and residual variability are not
# used: the model is the typical patient's.
#
# Half-life check: ln 2 x 815 / 16.3 = 34.7 h, a little longer than the 27 to
# 32 h of the product label.  Nothing recovered from the source explains the
# difference; a V/F identified mostly from troughs is poorly determined, and
# the half-life is only as good as it.
# Steady state on 10 mg once daily: 10 mg / 24 h / 16.3 L/h = 25.6 ng/mL.
#
# CYP2C19 PHENOTYPE
# =================
# Poor x 0.479, intermediate x 0.847, normal x 1.  Rapid and ultrarapid
# (CYP2C19*17 carriers) were NOT estimated: the source's EM group was the
# reference, and *17 carriers, uncommon in Chinese cohorts, were presumably
# counted with it.  They are given the normal value here.  That probably
# overstates their exposure; it is a gap in the source, not a claim that *17
# has no effect.
#
# AGE: GATED
# ==========
# The source reports an age coefficient (0.0077) on clearance, but the exact
# form of the equation (linear or exponential, the centring age, the sign
# convention) was not recovered.  No age effect is implemented; the age
# argument is accepted and ignored.  Implementing a guessed form could
# change clearance in the elderly by tens of percent in either direction.
#
# APPARENT PARAMETERS: ORAL ONLY
# ==============================
# CL/F and V/F were fitted to oral data, so they predict oral concentrations
# correctly and would predict intravenous ones wrong by 1/F
# (docs/adding-a-drug.md, "Apparent parameters restrict the route").  The CSV
# offers oral units only, and bioavailability_PO is 1 because the apparent
# scale already contains F.  No lag time.
#
# NO EFFECT SITE
# ==============
# The antidepressant effect develops over weeks and has no equilibration
# rate constant; there is no human ke0 for escitalopram.  tPeak = 0 (no
# effect site), MEAC = 0 (not an opioid), and the plot is plasma.  The band
# in the CSV, 15 to 80 ng/mL with 40 as the typical line, is the AGNP 2018
# consensus therapeutic reference range (Hiemke et al., Pharmacopsychiatry
# 2018;51:9-62); typical, upperTypical and lowerTypical mirror it.  The
# desmethyl metabolite is not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carries no size covariate, so the published values are taken
# as the 70 kg reference man's and inherit the library's fat-free-mass
# scaling: volume x size$volume, clearance x size$clearance, and with the
# switch off the published values unscaled (legacyVolume = 1).  The cohort's
# demographics were not available to re-anchor the reference; a Chinese
# psychiatric cohort was probably lighter than 70 kg, in which case the
# scaled values somewhat overstate clearance and volume for everyone.
#
# NOT IMPLEMENTED (alternatives, documented in the help)
# ======================================================
# Jin 2010 (adult MDD: CL/F 23.5 L/h, V/F 884 L; covariates not verified);
# Poweleit 2023 (pediatric: CL/F 14.2 x (BSA/1.73) x CYP2C19 factor, V/F
# 428 L, ka 0.8 /h), never mixed with Liu's parameters; Kim 2017 PET
# occupancy EC50s (units unverified; not plotted).  Liu 2025's external
# validation (doi 10.2147/DDDT.S546904) found the uncalibrated population
# predictions unreliable for individuals.
#
# References
# ----------
# Liu et al., Front Pharmacol 2022;13:964758.
#   https://doi.org/10.3389/fphar.2022.964758
# Hiemke C et al., Pharmacopsychiatry 2018;51:9-62 (AGNP consensus).
#   https://doi.org/10.1055/s-0043-116492
# -----------------------------------------------------------------------------

ESCITALOPRAM_CL <- 16.3   # L/h, CL/F, CYP2C19 normal (EM)
ESCITALOPRAM_V  <- 815    # L, V/F
ESCITALOPRAM_KA <- 0.6    # 1/h, fixed in the source

# CYP2C19 multipliers on CL/F.  Rapid and ultrarapid were not estimated and
# take the normal value; see the header.
ESCITALOPRAM_CYP2C19 <- c(
  poor         = 0.479,
  intermediate = 0.847,
  normal       = 1,
  rapid        = 1,
  ultrarapid   = 1
)

#' Escitalopram pharmacokinetics
#'
#' Apparent oral one-compartment model of Liu et al. (2022) with CYP2C19
#' phenotype on clearance.  Oral only.  See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years (not used: the source's age effect is gated)
#' @param sex sex as a string
#' @param cyp2c19 CYP2C19 metaboliser phenotype, one of \code{CYP2C19_VALUES}
#' @param adjustToFFM scale the volume to the patient's fat-free mass and
#'   clearance to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
escitalopram <- function(weight, height, age, sex, cyp2c19 = CYP2C19_DEFAULT,
                         adjustToFFM = TRUE)
{
  if (length(cyp2c19) != 1 || !cyp2c19 %in% CYP2C19_VALUES) {
    stop("Invalid cyp2c19: ", paste(cyp2c19, collapse = ", "),
         ". Must be one of: ", paste(CYP2C19_VALUES, collapse = ", "))
  }

  # Fixed published parameters (see the header): legacy factors 1.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- ESCITALOPRAM_V * size$volume
  cl1 <- ESCITALOPRAM_CL / 60 * ESCITALOPRAM_CYP2C19[[cyp2c19]] * size$clearance
  v2  <- 1     # one compartment: placeholders, unscaled
  cl2 <- 0
  v3  <- 1
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ESCITALOPRAM_KA / 60,   # 1/min
    bioavailability_PO = 1,         # the apparent scale already carries F
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: AGNP 2018 therapeutic reference range.  The plot reads the
  # CSV; these mirror it.
  typical      <- 40
  upperTypical <- 80
  lowerTypical <- 15

  reference <- paste0(
    "Liu et al., Front Pharmacol 2022;13:964758 (apparent oral one-compartment ",
    "model with CYP2C19; age effect not implemented; band: AGNP consensus, ",
    "Hiemke C et al., Pharmacopsychiatry 2018;51:9-62). ",
    "https://doi.org/10.3389/fphar.2022.964758"
  )

  list(
    PK = PK,
    tPeak = 0,       # no effect-site model; see the header
    MEAC = 0,        # not an opioid
    typical = typical,
    upperTypical = upperTypical,
    lowerTypical = lowerTypical,
    reference = reference
  )
}
