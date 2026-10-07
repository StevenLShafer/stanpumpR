# -----------------------------------------------------------------------------
# Ceftriaxone: two-compartment total-plasma model (Sanz-Codina 2023)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), TOTAL plasma.
#
# DISPOSITION
# ===========
# Sanz-Codina et al. gave 2 g over 30 minutes to six healthy men and fitted
# total venous plasma concentrations: CL 1.21 L/h, Vc 5.76 L, Q 2.92 L/h,
# Vp 2.97 L.  Clearance acts on TOTAL concentration in this fit, so the model
# is linear as published and is implemented exactly.
#
# Ceftriaxone is 85-95% albumin-bound with saturable binding, and the paper's
# main purpose was to compare two ways of measuring the free fraction
# (ultrafiltration against microdialysis), which gave different binding
# constants.  Neither binding map is applied here: the curve is total
# ceftriaxone, which is what a laboratory reports.  The target that matters
# is free time above MIC, and free is roughly a tenth of total at the trough
# and a larger fraction at the peak.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE ceftriaxone equals the MIC of 1 mg/L for the
# Enterobacterales, the susceptible breakpoint CLSI M100 (2026: S <= 1, I 2,
# R >= 4) and EUCAST v16.1 (S <= 1, R > 2) share.  Neither committee now has
# an MSSA ceftriaxone breakpoint.
#
# Binding is saturable, so the total for a given free concentration comes
# from the binding fit itself.  Sanz-Codina's own ultrafiltration fit, in the
# same six men as the plotted curve, is
#     C_total = C_free + B C_free / (Kd + C_free),   Kd = 23.7 mg/L,
# with B = 354 mg/L back-calculated from the abstract's mean peak pair (total
# 297.4, free 52.8 mg/L; the 8-hour pair gives the same B).  Free 1 mg/L is
# then 1 + 354/24.7 = 15.3 mg/L total (free fraction 0.065), stored as 15.
# The answer depends on method and albumin: equilibrium dialysis reads more
# binding than ultrafiltration (about 19-22 mg/L total on that basis), and B
# scales with albumin -- about 46 g/L in these young men; at 40 g/L, usual in
# surgical adults, the threshold would be about 13 mg/L.  See
# R/antibioticThresholds.R.
#
# POPULATION
# ==========
# Six healthy men.  Critically ill patients have a different, independently
# fitted model (Heffernan 2022) with larger volumes and altered clearance;
# their coefficients are not mixed into this one.  Renal function is not a
# covariate: ceftriaxone has substantial biliary elimination and the healthy
# cohort gave no renal spread to fit.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The published parameters are fixed values for a typical healthy man.
# Volumes scale with fat-free mass relative to the 70 kg, 170 cm reference
# male, clearances with that ratio ^ 0.75; the switch off uses them as
# published.
#
# References
# ----------
# Sanz-Codina M et al., J Antimicrob Chemother 2023;78:380-388.
#   https://doi.org/10.1093/jac/dkac400
# Stoeckel K et al., Clin Pharmacol Ther 1981;29:650-657 (free fraction 0.04
#   to 0.17, rising with concentration).  https://doi.org/10.1038/clpt.1981.90
# CLSI M100, 36th ed., 2026; EUCAST Clinical Breakpoint Tables v16.1, 2026:
#   ceftriaxone, Enterobacterales, S <= 1 mg/L.
# -----------------------------------------------------------------------------

#' Ceftriaxone pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled. No renal-function estimate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ceftriaxone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, so
  # legacyVolume = 1 leaves them unscaled with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Sanz-Codina 2023, total plasma
  v1  <- 5.76 * size$volume
  v2  <- 2.97 * size$volume
  v3  <- 1                              # no third compartment
  cl1 <- 1.21 / 60 * size$clearance     # L/min
  cl2 <- 2.92 / 60 * size$clearance
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Band, TOTAL mg/L.  With 85-95% binding, a free concentration above a
  # 1-2 mg/L MIC needs a total of roughly 10-40 mg/L.  Orientation only.
  typical      <- 20
  upperTypical <- 50
  lowerTypical <- 10

  reference <- paste0(
    "Sanz-Codina M et al., J Antimicrob Chemother 2023;78:380-388. ",
    "Two-compartment model of TOTAL plasma ceftriaxone in six healthy men. ",
    "https://doi.org/10.1093/jac/dkac400"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
