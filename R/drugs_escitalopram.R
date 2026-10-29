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
# in the CSV, 20 to 40 ng/mL with 30 as the typical line, is the therapeutic
# reference range of the AGNP TDM task force's systematic review (Eichentopf
# et al., Front Psychiatry 2022;13:972141; trough, 24 h after a dose), which
# replaced the AGNP 2017 consensus range of 15 to 80 ng/mL (Hiemke et al.,
# Pharmacopsychiatry 2018;51:9-62); typical, upperTypical and lowerTypical
# mirror it, and inst/extdata/therapeuticRanges.csv records both.  The
# desmethyl metabolite is not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source carries no size covariate, so the published values are taken
# as the 70 kg reference man's and inherit the library's fat-free-mass
# scaling: volume x size$volume, clearance x size$clearance, and with the
# switch off the published values unscaled (legacyVolume = 1).
#
# RE-ANCHORED TO LIU'S COHORT (Steven L. Shafer, 2026-10-29).  Liu's
# published values belong to the cohort's typical patient, not to the 70 kg
# reference man.  Table 1 gives the median as 61 kg (37-97), 165 cm (150-180)
# and 45 years, with 59 men among 106 patients.  That patient's fat-free mass,
# weighted 59 men to 47 women, is 45.15 kg against the reference man's 54.48,
# so with the switch on V/F is multiplied by 54.48 / 45.15 = 1.207 and CL/F
# by 1.207^0.75 = 1.151 for the reference man, before the usual fat-free-mass
# scaling.  A patient with the cohort's typical fat-free mass therefore
# receives Liu's published values.  With the switch off the published values are used
# unscaled, as before.
#
# NOT IMPLEMENTED (alternatives, documented in the help)
# ======================================================
# Jin 2010 (adult MDD; base model CL/F 23.5 L/h, V/F 884 L; final model CL/F
# 26 L/h CYP2C19 EM or 19.8 L/h IM/PM x (age/40)^-0.336 x (weight/76)^0.333,
# V/F 947 L x (BMI/27)^1.11);
# Poweleit 2023 (pediatric: CL/F 14.2 x (BSA/1.73) x CYP2C19 factor, V/F
# 428 L, ka 0.8 /h), never mixed with Liu's parameters; Kim 2017 PET
# occupancy EC50s (Table 1, ng/mL of plasma escitalopram: putamen 4.30,
# raphe 2.89, with Emax fixed at 100%; not plotted).  Yan 2025's external
# evaluation of ten escitalopram models, this one among them (Drug Des Devel
# Ther 2025;19:10937-10951, doi 10.2147/DDDT.S546904), found the
# uncalibrated population predictions unreliable for individuals.
#
# References
# ----------
# Liu et al., Front Pharmacol 2022;13:964758.
#   https://doi.org/10.3389/fphar.2022.964758
# Hiemke C et al., Pharmacopsychiatry 2018;51:9-62 (AGNP consensus).
#   https://doi.org/10.1055/s-0043-116492
# Yan K et al., Drug Des Devel Ther 2025;19:10937-10951.
#   https://doi.org/10.2147/DDDT.S546904
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
  # Re-anchored from Liu's typical patient to the reference man (header).
  anchor <- 1
  if (isTRUE(adjustToFFM)) {
    ffmLiu <- (59 * ffmAlSallami(61, 165, 45, SEX_MALE) +
               47 * ffmAlSallami(61, 165, 45, SEX_FEMALE)) / 106    # 45.15 kg
    anchor <- FFM_REFERENCE / ffmLiu                                 # 1.207
  }

  v1  <- ESCITALOPRAM_V * anchor * size$volume
  cl1 <- ESCITALOPRAM_CL / 60 * anchor^0.75 *
    ESCITALOPRAM_CYP2C19[[cyp2c19]] * size$clearance
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

  # Band, ng/mL: Eichentopf 2022 therapeutic reference range.  The plot reads
  # the CSV; these mirror it.
  typical      <- 30
  upperTypical <- 40
  lowerTypical <- 20

  reference <- paste0(
    "Liu et al., Front Pharmacol 2022;13:964758 (apparent oral one-compartment ",
    "model with CYP2C19; age effect not implemented; band: Eichentopf L et al., ",
    "Front Psychiatry 2022;13:972141). ",
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
