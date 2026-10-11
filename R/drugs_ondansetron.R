# -----------------------------------------------------------------------------
# Ondansetron: two-compartment intravenous models, Mondick 2010 below 18
# years and Chiang 2021 from 18
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# WHICH MODEL
# ===========
# Below ONDANSETRON_PEDIATRIC_AGE (18 years) the model is Mondick 2010, from
# 18 it is Chiang 2021.  THE GAP: neither source covers 4 to 45 years --
# Mondick was fitted to 1-48 months and Chiang to 45-70 years -- so every
# age in that range is an extrapolation of one model or the other.  It is
# filled from both ends: 4-18 years by Mondick's allometric equations
# carried beyond its data (weight and the completed maturation are its only
# covariates, so this is size scaling of the 10.4 kg child), and 18-45
# years by Chiang held at its 45-year age term.
#
# Why 18.  Mondick's model extrapolated to 70 kg gives a clearance of 0.527
# L/h/kg, which the authors call consistent with the 0.4-0.5 L/h/kg observed
# in adults and in surgical patients aged 3-12 years, so its allometry is
# the better-supported way to cover childhood and adolescence.  Moving the
# switch lower (to 4 years, the end of Mondick's data) would give children
# Chiang's held 45-year central volume, about 3.5 times Mondick's; moving it
# higher (to 45) would extrapolate Mondick across all of adulthood.  The
# choice was made with the maintainer (2026-10-11).
#
# The switch is discontinuous at 18.  At 70 kg and age 18, before size
# scaling, Mondick gives CL 37.0 L/h, V1 65.1 L, Q 363 L/h, V2 176 L;
# Chiang gives CL 24.6 L/h, Vc 220 L, Q 211 L/h, Vp 107 L.  The help page
# says so.  No interpolation is attempted: there are no data in the gap to
# say what shape it should take.
#
# CHILDREN: MONDICK 2010
# ======================
# Mondick and colleagues pooled 745 serum samples from 124 patients aged
# 1-48 months (median 16.3 months, 3.3-20.2 kg): surgical patients given
# 0.1 or 0.2 mg/kg and oncology patients given 0.15 mg/kg three times.  Two
# compartments with allometric weight scaling normalised to 10.4 kg and a
# maturation of clearance with age (final model and Table 2):
#
#     CL = 1.53 x WT^0.75 x (1 - 0.760 exp(-(AGE - 1) ln2 / 3.82))  L/h
#     V1 = 0.930 x WT                                               L
#     Q  = 15.0  x WT^0.75                                          L/h
#     V2 = 2.52  x WT                                               L
#
# with WT in kg and AGE in months.  Table 2 reports the parameters per kg
# (L/h/kg^0.75, L/kg) after estimation at the 10.4 kg reference, so these
# products are the published typical values.  The authors set ages below
# 1 month to 1 month (their youngest patient); so does this model.  The
# equations reproduce the paper's checks: clearance reduced 31%, 53% and 76%
# at 6, 3 and 1 months, and 0.629, 0.770 and 0.794 L/h/kg at 6 months/8 kg,
# 12 months/10 kg and 24 months/13 kg.  Maturation is 95% complete by about
# 19 months.  Above 48 months this is an allometric extrapolation; below
# 1 month, an extrapolation to neonates the study did not include.
# Interindividual variability (CL 56.8%, V1 110%, V2 48.0% CV) and residual
# error are not simulated.
#
# ADULTS: CHIANG 2021
# ===================
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
# (ONDANSETRON_AGE_RANGE).  Since Chiang is used only from 18 (WHICH MODEL,
# above), the lower hold applies to adults aged 18-45: they get the
# 45-year-old's Vc, an extrapolation in the sense that the source has no one
# younger, and Vc nonetheless large.  Nothing else in Chiang depends on age.
#
# WHAT IS LEFT OUT
# ================
# Chiang's interindividual variability (CL 49.7%, Vc 41.9% CV; their
# covariance was estimated but is not reported) and the 19.1% residual
# error are not simulated: stanpumpR plots the typical patient.
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
# Mondick carries its own weight covariate, so it is evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight, 70 kg x FFM /
# FFMref) and at total body weight with it off.  Chiang's are fixed
# published parameters: volumes scale with fat-free mass relative to the
# reference male, clearances with that ratio ^ 0.75; the switch off uses
# them as published.  The age terms of both apply with the switch in either
# position; they are not size covariates.
#
# References
# ----------
# Mondick JT et al., Eur J Clin Pharmacol 2010;66:77-86.
#   https://doi.org/10.1007/s00228-009-0730-8
# Chiang MD et al., Br J Clin Pharmacol 2021;87:516-526.
#   https://doi.org/10.1111/bcp.14412
# Cox EH et al., J Pharmacokinet Biopharm 1999;27:625-644.
#   https://doi.org/10.1023/A:1020930626404
# -----------------------------------------------------------------------------

# Chiang 2021 age covariate on Vc (header).
ONDANSETRON_AGE_MEDIAN   <- 58          # years, median of the 14 fitted
ONDANSETRON_AGE_EXPONENT <- -4.91       # Table 4
ONDANSETRON_AGE_RANGE    <- c(45, 70)   # years, the ages fitted

# Mondick 2010 below this age (years), Chiang 2021 from it (header).
ONDANSETRON_PEDIATRIC_AGE <- 18

# Mondick 2010 maturation of clearance (final model, Table 2).
ONDANSETRON_MATURATION_BETA     <- 0.760   # fraction missing at 1 month
ONDANSETRON_MATURATION_HALFLIFE <- 3.82    # months
ONDANSETRON_MATURATION_MIN_AGE  <- 1       # months, the youngest fitted

# Mondick's maturation factor on clearance at an age in months.
ondansetronMaturation <- function(ageMonths)
{
  a <- max(ageMonths, ONDANSETRON_MATURATION_MIN_AGE)
  1 - ONDANSETRON_MATURATION_BETA *
    exp(-(a - ONDANSETRON_MATURATION_MIN_AGE) * log(2) /
          ONDANSETRON_MATURATION_HALFLIFE)
}

#' Ondansetron pharmacokinetics
#'
#' Two-compartment intravenous models: Mondick 2010 (allometric weight and
#' maturation of clearance) below 18 years, Chiang 2021 (age on central
#' volume, evaluated within the fitted 45-70 years) from 18.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM below 18 years, evaluate Mondick's weight terms at the
#'   pharmacokinetic weight (TRUE) or total body weight (FALSE); from 18,
#'   scale Chiang's volumes to fat-free mass and clearances to that ratio to
#'   the 0.75 power, or use them unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ondansetron <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  if (age < ONDANSETRON_PEDIATRIC_AGE) {
    return(ondansetronMondick(weight, height, age, sex, adjustToFFM))
  }

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

  reference <- paste0(
    "Chiang MD et al., Br J Clin Pharmacol 2021;87:516-526 (two-compartment ",
    "model in adults 45-70 years; Vc x (age/58)^-4.91, age held within 45-70; ",
    "used from 18 years). https://doi.org/10.1111/bcp.14412"
  )

  ondansetronResult(PK, reference)
}

# Mondick 2010, below 18 years (header).
ondansetronMondick <- function(weight, height, age, sex, adjustToFFM)
{
  # Own weight covariate (header): pharmacokinetic weight with the switch on,
  # total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1  <- 0.930 * pkW                                              # L
  v2  <- 2.52 * pkW
  v3  <- 1                                        # no third compartment
  cl1 <- 1.53 * pkW^0.75 * ondansetronMaturation(age * 12) / 60   # L/min
  cl2 <- 15.0 * pkW^0.75 / 60
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

  reference <- paste0(
    "Mondick JT et al., Eur J Clin Pharmacol 2010;66:77-86 (two-compartment ",
    "model in patients 1-48 months: allometric weight, maturation of ",
    "clearance with age; used below 18 years). ",
    "https://doi.org/10.1007/s00228-009-0730-8"
  )

  ondansetronResult(PK, reference)
}

# The fields shared by both models.
ondansetronResult <- function(PK, reference)
{
  list(
    PK = PK,
    tPeak = 0,          # no equilibration model (header)
    MEAC = 0,
    # Band, ng/mL: what 4-8 mg produce over the first hours.  Orientation.
    typical = 20,
    upperTypical = 40,
    lowerTypical = 5,
    reference = reference
  )
}
