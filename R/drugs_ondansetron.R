# -----------------------------------------------------------------------------
# Ondansetron: two-compartment intravenous reference model (Chiang 2021)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# DISPOSITION
# ===========
# Chiang and colleagues gave intravenous ondansetron to 14 adults aged 45-70
# years and sampled plasma and cerebrospinal fluid for 3 hours.  Their
# two-compartment plasma model (Table 4), linear elimination from the
# central compartment, has the reference values
#
#     CL 24.6 L/h,  Vc 63.3 L,  Q 211 L/h,  Vp 107 L
#
# as absolute (not per-kilogram) parameters.  Half-times about 0.10 and
# 5.2 h.  Beyond 3 hours the curve extrapolates the sampled window.
#
# WHAT IS LEFT OUT
# ================
# Chiang reported central volume falling with age (power exponent -4.91).
# Its exact parameterisation, and the CL/Vc covariance, could not be verified
# from the retrieved table, so the age term is NOT applied: this is the reference
# patient's model, not the full covariate model.  The source has no
# children; a child here is the adult reference scaled to fat-free mass,
# which is an extrapolation (Mondick 2010 fitted ages 1-48 months, but its
# full parameter table was not available to verify).  Interindividual
# variability (CL 49.7%, Vc 41.9% CV) and residual error are not simulated:
# stanpumpR plots the typical patient.
#
# Oral ondansetron is not offered: no oral absorption model was assembled.
#
# NO EFFECT SITE, NO CALIBRATED THRESHOLD
# =======================================
# There is no published equilibration delay or concentration-response model
# for antiemesis in surgical patients.  Cox 1999 estimated, in an adult
# ipecac challenge, that the hazard of emesis halves for each 1.4 ng/mL of
# ondansetron; that is quoted in the help as orientation only and is not a
# probability of preventing PONV or CINV.  tPeak and MEAC are zero, and the
# band is what 4 to 8 mg produce over the first hours.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Fixed published parameters: volumes scale with fat-free mass relative to
# the reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.
#
# References
# ----------
# Chiang MD et al., Br J Clin Pharmacol 2021;87:516-526.
#   https://doi.org/10.1111/bcp.14412
# Cox EH et al., J Pharmacokinet Biopharm 1999;27:625-644.
#   https://doi.org/10.1023/A:1020930626404
# -----------------------------------------------------------------------------

#' Ondansetron pharmacokinetics
#'
#' Two-compartment intravenous reference model of Chiang 2021, without its
#' age covariate.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ondansetron <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Chiang 2021, Table 4: CL 24.6 L/h, Vc 63.3 L, Q 211 L/h, Vp 107 L
  v1  <- 63.3 * size$volume
  v2  <- 107 * size$volume
  v3  <- 1                                        # no third compartment
  cl1 <- 24.6 / 60 * size$clearance               # L/min
  cl2 <- 211 / 60 * size$clearance
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

  tPeak <- 0     # no equilibration model (header)
  MEAC  <- 0

  # Band, ng/mL: what 4-8 mg produce over the first hours.  Orientation.
  typical      <- 20
  upperTypical <- 40
  lowerTypical <- 5

  reference <- paste0(
    "Chiang MD et al., Br J Clin Pharmacol 2021;87:516-526 (two-compartment ",
    "reference model in adults 45-70 years; age covariate not applied). ",
    "https://doi.org/10.1111/bcp.14412"
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
