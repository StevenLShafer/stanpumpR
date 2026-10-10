# -----------------------------------------------------------------------------
# Ondansetron: two-compartment intravenous reference model (Chiang 2021)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# DISPOSITION
# ===========
# Chiang and colleagues gave 16 mg of intravenous ondansetron over 15 min to
# 15 adults aged 45-70 years and sampled plasma for 3 hours (cerebrospinal
# fluid once); one patient's samples could not be quantified, so the model
# was fitted to 14.  Their two-compartment plasma model (Table 4), linear
# elimination from the central compartment, has the typical values
#
#     CL 24.6 L/h,  Vc 63.3 L,  Q 211 L/h,  Vp 107 L
#
# as absolute (not per-kilogram) parameters.  Half-times about 0.10 and
# 5.2 h at the median age.  Beyond 3 hours the curve extrapolates the
# sampled window.
#
# AGE ON CENTRAL VOLUME
# =====================
# The one covariate retained is age on Vc, in the power form of Chiang's
# Equation 6, P = A (COV / COV_median)^B, with B = -4.91 (Table 4, RSE 19%):
#
#     Vc = 63.3 x (AGE / 58)^-4.91   L
#
# 58 years is the median age of the 14 patients in the fit (Table 1,
# excluding subject 14).  The exponent is steep: Vc is 220 L at 45 and 25 L
# at 70, a ninefold range within the study.  Outside the fitted ages it is
# meaningless (1,600 L at 30, millions of litres for a child), so the age
# term is evaluated only within 45-70 and held at the nearer end beyond it
# (ONDANSETRON_AGE_RANGE).  A patient younger than 45 therefore gets the
# 45-year-old's Vc, an extrapolation in the sense that the source has no one
# younger, and Vc nonetheless large.  Nothing else depends on age.
#
# WHAT IS LEFT OUT
# ================
# The source has no children; a child here is the adult model at the
# 45-year age term, scaled to fat-free mass, which is an extrapolation
# (Mondick 2010 fitted ages 1-48 months, but its full parameter table was
# not available to verify).  Interindividual variability (CL 49.7%, Vc
# 41.9% CV; their covariance was estimated but is not reported) and the
# 19.1% residual error are not simulated: stanpumpR plots the typical
# patient.
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
# them as published.  The age term applies with the switch in either
# position; it is not a size covariate.
#
# References
# ----------
# Chiang MD et al., Br J Clin Pharmacol 2021;87:516-526.
#   https://doi.org/10.1111/bcp.14412
# Cox EH et al., J Pharmacokinet Biopharm 1999;27:625-644.
#   https://doi.org/10.1023/A:1020930626404
# -----------------------------------------------------------------------------

# Chiang 2021 age covariate on Vc (header).
ONDANSETRON_AGE_MEDIAN   <- 58          # years, median of the 14 fitted
ONDANSETRON_AGE_EXPONENT <- -4.91       # Table 4
ONDANSETRON_AGE_RANGE    <- c(45, 70)   # years, the ages fitted

#' Ondansetron pharmacokinetics
#'
#' Two-compartment intravenous model of Chiang 2021, with its age covariate on
#' central volume evaluated within the fitted 45-70 year range.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published fixed parameters unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ondansetron <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Chiang 2021 age on Vc (header): power form about the 58-year median,
  # evaluated only within the fitted ages.
  ageVc <- min(max(age, ONDANSETRON_AGE_RANGE[1]), ONDANSETRON_AGE_RANGE[2])
  vcAge <- (ageVc / ONDANSETRON_AGE_MEDIAN)^ONDANSETRON_AGE_EXPONENT

  # Size scaling (see the header): fixed published parameters, unscaled
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Chiang 2021, Table 4: CL 24.6 L/h, Vc 63.3 L, Q 211 L/h, Vp 107 L
  v1  <- 63.3 * vcAge * size$volume
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
    "model in adults 45-70 years; Vc x (age/58)^-4.91, age held within 45-70). ",
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
