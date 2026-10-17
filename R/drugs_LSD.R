# -----------------------------------------------------------------------------
# LSD (lysergic acid diethylamide): oral, apparent parameters, plasma only
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# LSD is in the library as an evidence-labelled research model, behind the
# "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).  It is a
# plasma-exposure estimate, not dosing advice, a safety threshold, or a
# prediction of any individual's experience.  It describes pharmaceutical LSD
# base given orally in a controlled study, not material of unknown identity or
# content.
#
# APPARENT PARAMETERS, SO ORAL ONLY
# =================================
# Dolder and colleagues fitted a one-compartment model with first-order input,
# first-order elimination and NO LAG TIME to oral LSD in healthy adults, and
# reported apparent clearance and apparent volume: clearance over
# bioavailability and volume over bioavailability.  Apparent parameters
# predict oral concentrations correctly, because the unknown bioavailability
# cancels, and intravenous ones wrong by 1/F.  LSD is therefore offered as an
# ORAL unit only, with bioavailability carried as 1 because the apparent scale
# already contains it (docs/adding-a-drug.md).
#
# The 2017 study's own two-compartment attempt did not improve the fit: "The
# model fit was not relevantly improved by a two-compartment model... The
# one-compartment model showed better Akaike information criterion values in
# all subjects."
#
# THE PARAMETERS, AND THE DOSE ARM THEY COME FROM
# ===============================================
# Dolder reported each dose arm separately.  This model implements the
# 100 microgram arm (n = 24):
#
#     ka   = 1.4 /h       (0.0233333 /min)
#     CL/F = 12.3 L/h     (0.205 L/min)
#     V/F  = 46 L
#     no lag time
#
# which gives an elimination rate constant of 0.26739/h and a half-life of
# 2.59 h, reproducing the paper's published plasma half-life of 2.6 h, and a
# peak at 1.46 h, reproducing its reported 1.4 h.  The 200 microgram arm
# (n = 16) was fitted separately: ka 1.2 /h, CL/F 9.9 L/h, V/F 37 L, and the
# same 0.27 /h elimination rate constant.  Those
# values are NOT implemented: the library carries one parameter set per drug,
# and a 200 microgram dose is simulated here with the 100 microgram arm's
# parameters, so its exposure is predicted dose-proportionally.  Note what
# that misses: the two arms share an elimination rate constant, so the
# half-life is the same, but the 200 microgram arm's apparent clearance is
# lower (9.9 against 12.3 L/h), so its published exposure per microgram is
# about a quarter higher than this model predicts.  The help page says so.
#
# A CAUTION ABOUT THE HALF-LIFE THIS MODEL CARRIES
# ================================================
# The 2.6 h half-life of this 2017 re-analysis is shorter than the same
# group's later measurements: Holze 2024 (85 and 170 micrograms, n = 28)
# reported 3.7 h and 4.0 h, and the 2025 oral/intravenous crossover reported a
# terminal half-life of 3.8 h with an intravenous clearance of 4.7 L/h and an
# absolute oral bioavailability near 0.80.  Different analytical methods and
# modelling approaches account for much of the difference.  This model
# therefore clears LSD FASTER than the later data, and it must be read as
# Dolder's 2017 model rather than as a consensus.  The 2025 crossover is not
# implemented: its two-compartment intravenous micro-parameters are in its
# supplement, not its main text, and its published Vz (26 L) is a
# non-compartmental terminal volume, not a central volume.
#
# DOSE BASIS
# ==========
# Micrograms of LSD BASE, as the nominal capsule content of the source study
# ("[D]-lysergic acid diethylamide hydrate... administered in a single oral
# dose of 100 or 200 microgram as a capsule"), with no analytical content
# correction.  LSD tartrate, a different mass basis, is not this model's
# input.
#
# NO EFFECT SITE
# ==============
# tPeak = 0, MEAC = 0, no band.  Dolder did fit sigmoid Emax models with an
# effect-site delay, separately for each rating scale and each dose arm
# (visual analogue scales bounded 0-100%, and heart rate in beats per minute,
# with each end point's own EC50, Hill coefficient and ke0).  Those are
# END-POINT-specific fits of subjective and cardiovascular responses, not one
# equilibration constant for "the effect of LSD", and the library's single
# effect site cannot carry them honestly.  No numeric PD constant is coded
# here, and no plotted effect is offered.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Dolder published no size covariate, so the apparent parameters are those of
# a reference adult: with the fat-free-mass switch off they are used as
# published at any weight (legacyVolume = 1), and with it on they scale with
# the Al-Sallami fat-free mass ratio (volume) and its 0.75 power (clearance).
# That scaling is an IMPLEMENTATION CONVENTION, not a published fit.
#
# References
# ----------
# Dolder PC, Schmid Y, Steuer AE, et al.  Pharmacokinetics and
#   pharmacodynamics of lysergic acid diethylamide in healthy subjects.  Clin
#   Pharmacokinet 2017;56(10):1219-1230.
#   https://doi.org/10.1007/s40262-017-0513-9  (the one-compartment oral model
#   with no lag; apparent clearance and volume by dose arm; published plasma
#   half-life 2.6 h and peak at 1.4 h after 100 micrograms).
# Holze F, Becker AM, Kolaczynska KE, et al.  Pharmacokinetics and
#   pharmacodynamics of lysergic acid diethylamide in healthy subjects over
#   different dose ranges.  Br J Clin Pharmacol 2024;90(1):200-208.
#   https://doi.org/10.1111/bcp.15887  (later half-lives, 3.7 and 4.0 h).
# -----------------------------------------------------------------------------

# Dolder 2017, Table 1, the 100 microgram arm (n = 24)
LSD_KA   <- 1.4 / 60     # 1/min, from 1.4 /h
LSD_CL_F <- 12.3 / 60    # L/min, apparent clearance, from 12.3 L/h
LSD_V_F  <- 46           # L, apparent volume

#' LSD pharmacokinetics: plasma, oral, apparent parameters
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM scale to the Al-Sallami fat-free mass; when \code{FALSE},
#'   use the published parameters unscaled (the source fitted no size
#'   covariate)
#' @returns a list in the shape \code{getDrugPK()} expects; plasma only, oral
#'   only, no effect site (see the header)
#' @export
LSD <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source fitted no size covariate, so the published parameters stand
  # unscaled with the switch off (legacyVolume = 1).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- LSD_V_F  * size$volume
  cl1 <- LSD_CL_F * size$clearance
  v2  <- 1                       # one compartment
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
    # Oral: apparent parameters, so bioavailability is 1 by convention, and
    # the source fitted no lag time
    ka_PO = LSD_KA,
    bioavailability_PO = 1,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # end-point-specific PD fits only; see the header
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Dolder PC et al., Clin Pharmacokinet 2017;56(10):1219-1230 ",
    "(https://doi.org/10.1007/s40262-017-0513-9). One-compartment oral model, ",
    "100 microgram arm; apparent clearance and volume, so oral doses of LSD ",
    "base only. Later studies report a longer half-life (3.7-4.0 h). Plasma ",
    "only, no effect site; research use, not dosing advice."
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
