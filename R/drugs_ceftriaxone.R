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
# -----------------------------------------------------------------------------

#' Ceftriaxone pharmacokinetics
#'
#' @inheritParams cefazolin
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
