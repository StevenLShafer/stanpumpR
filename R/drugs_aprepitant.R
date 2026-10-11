# -----------------------------------------------------------------------------
# Aprepitant: one-compartment intravenous disposition (Nijstad 2023), given
# directly (aprepitant) or as its prodrug (fosaprepitant)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma aprepitant.
#
# DISPOSITION
# ===========
# Nijstad and colleagues fitted aprepitant in children aged 0.7-17.9 years
# (8.4-66.3 kg) receiving an oral suspension, capsules, or intravenous
# fosaprepitant.  One compartment, linear elimination (Table 2):
#
#     CL = 5.83 x (WT/70)^0.75  L/h,    V = 86.8 x (WT/70)  L
#
# Only the intravenous branch is used here; the oral absorption and
# bioavailability are not.  Only FIVE children received intravenous
# fosaprepitant (4.4-13.8 years, 19.5-52.4 kg).  Everyone else -- infants,
# older adolescents, adults -- is an extrapolation of the weight equations,
# and below 6 months, where CYP3A4 is immature, the model should not be
# used.  One compartment cannot resolve the early distribution phase, so the
# peak at the end of a short infusion is uncertain.  No age maturation or
# volume variability was fitted.  Interindividual (CL 25.1% CV) and
# inter-occasion (CL 30.2%) variability and residual error are not
# simulated.
#
# DOSE BASIS
# ==========
# aprepitant(): mg of aprepitant given intravenously (Cinvanti, Aponvie).
# Its disposition is borrowed from the post-fosaprepitant fit, a formulation
# extrapolation: no formulation-specific population model was available.
#
# fosaprepitant(): mg of FOSAPREPITANT (the free acid, as vials are
# labelled, not the dimeglumine salt).  Conversion to aprepitant is rapid
# (minutes) and taken as instantaneous and complete; the prodrug itself is
# not plotted.  Each mg yields 534.44 / 614.40 = 0.86986 mg aprepitant.  The
# engine has no intravenous bioavailability, so the conversion is applied as
# an exact rescaling of the linear model: V and CL are both divided by the
# fraction, which leaves the rate constant unchanged and multiplies every
# concentration by it.  What is plotted on the fosaprepitant row is plasma
# APREPITANT.
#
# NO EFFECT SITE
# ==============
# Iihara 2026 reanalysed adult PET data: striatal NK1 occupancy
# 98.1 x Cp / (10.4 + Cp), from troughs after repeated oral dosing.  Applied
# to acute intravenous or pediatric profiles it is unvalidated, and it is
# not a probability of preventing nausea or vomiting.  tPeak and MEAC are
# zero.  The band runs from 116 ng/mL, where that relation gives 90%
# occupancy, to 1500 ng/mL, about the end-of-infusion peak after 150 mg of
# fosaprepitant in an adult.  Orientation only.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own weight covariate, so it is evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight, 70 kg x FFM /
# FFMref) and at total body weight with it off.
#
# References
# ----------
# Nijstad AL et al., J Oncol Pharm Pract 2023;29:899-904.
#   https://doi.org/10.1177/10781552221089243
# Iihara H et al., Support Care Cancer 2026;34:933.
#   https://doi.org/10.1007/s00520-026-11175-y
# FDA clinical pharmacology review, fosaprepitant NDA 22023 S-017, 2018.
# -----------------------------------------------------------------------------

APREPITANT_MW    <- 534.44     # g/mol
FOSAPREPITANT_MW <- 614.40     # g/mol, free acid
# mg aprepitant per mg fosaprepitant (1:1 molar)
FOSAPREPITANT_APREPITANT_FRACTION <- APREPITANT_MW / FOSAPREPITANT_MW

# Nijstad 2023 disposition, scaled by doseFraction (see the header).
aprepitantModel <- function(weight, height, age, sex, adjustToFFM, doseFraction,
                            reference)
{
  # Own weight covariate (header): pharmacokinetic weight with the switch on,
  # total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1  <- 86.8 * (pkW / 70) / doseFraction                 # L
  cl1 <- 5.83 * (pkW / 70)^0.75 / 60 / doseFraction       # L/min
  v2  <- 1                                   # one compartment
  v3  <- 1
  cl2 <- 0
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

  return(
    list(
      PK = PK,
      tPeak = 0,     # no equilibration model (header)
      MEAC = 0,
      typical = 500,          # ng/mL, orientation (header)
      upperTypical = 1500,
      lowerTypical = 116,
      reference = reference
    )
  )
}

#' Aprepitant pharmacokinetics
#'
#' One-compartment intravenous disposition of Nijstad 2023, for aprepitant
#' given intravenously; doses are mg of aprepitant.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM evaluate the weight covariates at the pharmacokinetic
#'   weight (TRUE) or at total body weight (FALSE)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
aprepitant <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  aprepitantModel(
    weight, height, age, sex, adjustToFFM, doseFraction = 1,
    reference = paste0(
      "Nijstad AL et al., J Oncol Pharm Pract 2023;29:899-904 (one-compartment ",
      "disposition after intravenous fosaprepitant in children; direct ",
      "intravenous aprepitant is a formulation extrapolation). ",
      "https://doi.org/10.1177/10781552221089243"
    )
  )
}
