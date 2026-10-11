# -----------------------------------------------------------------------------
# Paroxetine: oral, one compartment, apparent parameters, exposure more than
# proportional to the daily dose
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# The dose reduction below is the coordinator's decision, recorded as such.
# Verified by tests/testthat/test-drugs-paroxetine.R, which proves the
# steady-state equivalence by simulation.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of serum paroxetine (Concentration.Units ng).  The
# source reports hours and L/h; they are divided by 60 here.
#
# SOURCE
# ======
# Kim JR et al., Drug Des Devel Ther 2015;9:5247-5254: 271 therapeutic drug
# monitoring concentrations from 127 Korean psychiatric outpatients on
# paroxetine at steady state, Samsung Medical Center, Seoul (NONMEM).  One
# compartment; the parameters are APPARENT (oral data only, divided by the
# unknown F):
#
#     ka   = 0.908 /h                                        fixed
#     V/F  = 1020 L                                          fixed
#     CL/F = 13.1 x (daily dose / 25)^-0.363 x (age / 71)^-0.702   L/h
#
# Interindividual variability of CL/F 40.2% (not used).
#
# REDUCTION: THE DAILY-DOSE TERM AS AN EXPOSURE SCALE ON EACH DOSE
# ================================================================
# A clearance that depends on the dose cannot sit in a linear engine, whose
# parameters are fixed before the doses are seen.  So CL/F is evaluated at
# the reference daily dose, 25 mg:
#
#     CL25 = 13.1 x (age / 71)^-0.702   L/h
#
# and each oral dose D is scaled by (D / 25)^0.363 before it enters the
# engine, as an oralSaturation block of the power form
# (oralSaturationFraction(), R/routes.R).  At steady state on D mg once
# daily, Kim's AUC over a dosing interval is
#
#     D / CL(D) = D / [CL25 x (D/25)^-0.363] = D x (D/25)^0.363 / CL25,
#
# which is exactly the scaled dose on the reference clearance.  So the mean
# steady-state concentration of a once-daily regimen is Kim's at every dose:
# at 50 y (CL25 16.76 L/h), 20 mg daily averages 46 ng/mL and 50 mg daily
# 50 x 2^0.363 / (24 x 16.76) x 1000 = 160 ng/mL (the test simulates it).
# The "fraction" exceeds 1 above 25 mg (1.29 at 50 mg): it is an exposure
# scale on apparent parameters, not a bioavailability.  (Coordinator's decision, 2026-10-10.)
#
# What this does NOT reproduce:
#   - The dependence of the half-life on the dose.  The half-life is the
#     25 mg/day value, ln2 x 1020 / CL25: 54.0 h at 71 y, 42.2 h at 50 y.
#     In Kim's model a 50 mg/day patient's half-life would be 2^-0.363 = 0.78
#     times that.
#   - Divided doses.  Each administration is read as the day's dose.  Twice
#     daily, each half-dose is scaled by ((D/2)/25)^0.363 rather than
#     (D/25)^0.363, so daily exposure is 2^-0.363 = 0.78 of Kim's: 22% low.
#     Three times daily, 3^-0.363 = 0.67, 33% low.  Exact for once daily only.
#   - A single dose or the first days of treatment.  Kim's patients were at
#     steady state, and the dose term describes chronic dosing (presumably
#     the saturation of paroxetine's own CYP2D6 metabolism); it is applied
#     to the first dose too.
#
# THE 54 h HALF-LIFE
# ==================
# The label gives ~21 h.  With trough-only TDM data, V/F was fixed rather
# than estimated, and a fixed 1020 L against a clearance fitted to troughs
# makes the half-life long; it is an artefact of the data, not a property of
# Korean patients.  Mean steady-state concentrations, which depend on CL/F
# alone, are what this model predicts best; the swing within a day is too
# small and the approach to steady state too slow (7.5 days to 90%
# at 71 y, against about 3 by the label).
#
# APPARENT PARAMETERS: ORAL ONLY
# ==============================
# bioavailability_PO = 1, because the apparent scale already contains F, and
# the CSV offers oral units only (docs/adding-a-drug.md, "Apparent
# parameters restrict the route").  There is no intravenous paroxetine.
#
# COVARIATES
# ==========
# Age as published, reference 71 y.  The power of age is unbounded: at 20 y
# CL/F is 2.4 x the reference, and in a small child it grows without limit
# ((0.01/71)^-0.702 = 505).  Kim's patients were adults; children are far
# outside the data, and the model should not be used for them.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Kim tested no size covariate that was retained: the published values are
# taken as the 70 kg reference man's and scaled with pkSizeFactors() -- to
# fat-free mass with the switch on, not at all with it off (legacyVolume = 1).
#
# NO EFFECT SITE
# ==============
# Plasma only: tPeak = 0, MEAC = 0.  The antidepressant effect lags the
# concentration by weeks.  Serotonin transporter occupancy by SPECT
# (Catafau 2006, 10 patients on 20 mg/day: Emax 70.5%, Cp50 2.7 ng/mL) is a
# biomarker, not plotted.
#
# BAND
# ====
# 20-65 ng/mL, typical 40: the AGNP 2018 consensus therapeutic reference
# range (Hiemke 2018).  Yuan 2025 (observational) reported an association of
# response with 20-65 ng/mL.  Orientation only.
#
# ALTERNATIVES NOT IMPLEMENTED
# ============================
# Feng 2006 (171 elderly North Americans, 1970 concentrations, two
# compartments with Michaelis-Menten elimination, CYP2D6 genotype on Vmax):
# BLOCKED.  Its Vmax for extensive metabolisers, 454-474 ug/h, is at most
# 10.9-11.4 mg/day of elimination, below the doses of up to 40 mg/day its
# patients took without accumulating without limit; the input and
# bioavailability convention that would reconcile the two could not be
# resolved.
# Shigetome 2025 (179 Japanese patients with major depression) has a
# population PK model and a MADRS PK/PD module that needs the measured
# week-1 MADRS score as an input; not implemented.
#
# References
# ----------
# Kim JR et al., Drug Des Devel Ther 2015;9:5247-5254.
#   https://doi.org/10.2147/DDDT.S84718
# Hiemke C et al., Pharmacopsychiatry 2018;51:9-62.
#   https://doi.org/10.1055/s-0043-116492
# Feng Y et al., Br J Clin Pharmacol 2006;61:558-569 (not used).
#   https://doi.org/10.1111/j.1365-2125.2006.02629.x
# Shigetome K et al., CPT Pharmacometrics Syst Pharmacol 2025;14:1119-1127
#   (not used).  https://doi.org/10.1002/psp4.70032
# Catafau AM et al., Psychopharmacology 2006;189:145-153 (SERT occupancy).
#   https://doi.org/10.1007/s00213-006-0540-y
# -----------------------------------------------------------------------------

PAROXETINE_DREF     <- 25       # mg/day, Kim's reference daily dose
PAROXETINE_DOSE_EXP <- 0.363    # minus Kim's exponent of daily dose on CL/F

#' Paroxetine pharmacokinetics (oral)
#'
#' Kim et al. (2015), one compartment with apparent parameters and clearance
#' on age.  Kim's power of the daily dose on clearance is carried as a scale
#' on each oral dose through the returned \code{oralSaturation} block, which
#' reproduces steady-state exposure for once-daily dosing (see the header).
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales the volume and
#'   clearance to the patient's fat-free mass; \code{FALSE} uses the
#'   published values for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
paroxetine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published values for the reference man
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- 1020 * size$volume
  # CL/F at the reference daily dose of 25 mg (the dose term is carried by
  # oralSaturation below), age as published
  cl1 <- 13.1 * (age / 71)^-0.702 / 60 * size$clearance   # L/min
  v2  <- 1                                                 # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = 0.908 / 60,          # 1/min
    bioavailability_PO = 1,      # apparent parameters already contain F
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: AGNP 2018 therapeutic reference range.  Orientation only.
  typical      <- 40
  upperTypical <- 65
  lowerTypical <- 20

  reference <- paste0(
    "Kim JR et al., Drug Des Devel Ther 2015;9:5247-5254. One compartment, ",
    "apparent parameters, clearance on age; the power of the daily dose on ",
    "clearance carried as (D/25)^0.363 on each dose, exact at steady state ",
    "for once-daily dosing; oral only. https://doi.org/10.2147/DDDT.S84718"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,                 # no effect site: see the header
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Kim 2015: CL/F proportional to (daily dose / 25)^-0.363, carried as
      # an exposure scale on each oral dose.  Applied by simCpCe().
      oralSaturation = list(form = ORAL_SATURATION_POWER,
                            exponent = PAROXETINE_DOSE_EXP,
                            Dref = PAROXETINE_DREF,
                            exampleDoses = c(10, 20, 25, 40, 60))
    )
  )
}
