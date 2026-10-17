# -----------------------------------------------------------------------------
# Fenoldopam in children (Hammer 2008): plasma concentration during deliberate
# hypotension under general anaesthesia
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# his handoff specification for antihypertensive models.  Verified by
# tests/testthat/test-drugs-fenoldopamPediatric.R.  See
# docs/antihypertensives.md.
#
# WHY A SEPARATE PEDIATRIC DRUG
# =============================
# This is a CHILDREN'S model and the only internally complete fenoldopam
# disposition model the specification's sources supply.  It is named
# fenoldopamPediatric, as R/drugs_methylphenidatePediatric.R is, so that it
# cannot be selected as an adult default.  Weight-adjusted clearance in these
# children was about 1.8 times the adult value the same paper quotes
# (3.06 against 1.66 L/h/kg), and the effective infusion rate was
# correspondingly higher -- 0.8 to 1.2 mcg/kg/min against the adult label's
# 0.05 to 0.3 -- so an adult simulated on this model would be given too much
# clearance.  The adult record (Frederickson 1988) is NOT implemented: it
# reports a half-life of 9.8 min with a clearance of 30.3 mL/min/kg and a
# volume of 582 mL/kg, and those three are mutually inconsistent in one
# compartment (they imply 13.3 min), so the volume is a multi-compartment
# summary and the vector is underdetermined.
#
# THE SOURCE
# ==========
# Hammer GB, Verghese ST, Drover DR, Yaster M, Tobin JR.  Pharmacokinetics and
# pharmacodynamics of fenoldopam mesylate for blood pressure control in
# pediatric patients.  BMC Anesthesiol 2008;8:6 (doi 10.1186/1471-2253-8-6).
# Multicentre, randomised, placebo-controlled: 77 children enrolled, 76
# analysed, 3 weeks to 12.75 years (mean 51.9 months), Tanner stage 1 or 2,
# having surgery in which deliberate hypotension was induced, all under
# general anaesthesia.  A blinded 30-minute interval at placebo or 0.05, 0.2,
# 0.8 or 3.2 mcg/kg/min was followed by open-label titration to a target mean
# arterial pressure.  Plasma fenoldopam by validated HPLC-MS/MS, sparse
# sampling, fitted in NONMEM V.
#
# A single compartment described the data, with population estimates and
# approximate standard errors
#
#     elimination rate constant   13.2 +/- 1.23 /h
#     clearance                   3.06 +/- 0.62 L/h/kg
#
# (the non-compartmental analysis gave 12.4 +/- 0.10 /h and
# 3.02 +/- 0.04 L/h/kg; those are a different analysis and are not mixed in).
# The paper reports NO volume of distribution.  V is taken as CL / kel =
# 0.2318 L/kg, which is a derived quantity, not the paper's, and is labelled
# so in inst/extdata/antihypertensiveParameters.csv.  It is consistent with
# the paper's own arithmetic: log(2)/kel is 3.15 min, the half-life the paper
# prints, and the non-compartmental pair gives 0.244 L/kg, within 5%.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance is published per kilogram and there is no allometric exponent
# anywhere in the paper, so the model's own size covariate is linear in
# weight.  As for any model with its own weight covariate, it is evaluated at
# the pharmacokinetic weight when the fat-free-mass switch is on and at total
# body weight when it is off, so with the switch off the published
# per-kilogram values are reproduced exactly.  The source population was
# children of 14 to 19 kg on average; a linear per-kilogram extrapolation to
# an infant or an adolescent is the paper's own convention, not an estimated
# relationship.
#
# NO BLOOD PRESSURE IS PLOTTED
# ============================
# The paper also fitted a maximum-effect model of mean arterial pressure on
# plasma concentration, with Emax -27 +/- 5.4 mmHg and EC50 9.1 +/- 4.23
# ng/mL (approximate standard errors), and found baseline mean arterial
# pressure a significant covariate on Emax.  Those coefficients are recorded
# in the parameter audit and are available through
# antihypertensiveEndpoint(), which evaluates them against a concentration
# the caller supplies.  They are NOT plotted as a pressure, for two reasons:
# the paper publishes no functional form or coefficient for the baseline-
# pressure covariate, so the fitted model cannot be reproduced as published;
# and the population was anaesthetised children undergoing deliberate
# hypotension, whose baseline is itself an anaesthetic state.  tPeak = 0, so
# ke0 stays zero and only plasma concentration is drawn; the paper reports no
# equilibration rate constant.  MEAC = 0: it is an anaesthetic concept and has
# no meaning here.
# -----------------------------------------------------------------------------

# Hammer 2008, compartmental analysis, as published
FENOLDOPAM_PED_KEL <- 13.2   # /h
FENOLDOPAM_PED_CL  <- 3.06   # L/h/kg

#' Fenoldopam pharmacokinetics in children (intravenous)
#'
#' Hammer et al. (2008), one compartment in anaesthetised children during
#' deliberate hypotension; a children's model, not an adult one.  See the
#' header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
fenoldopamPediatric <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Per-kilogram published clearance: the model's own size covariate, so it is
  # evaluated at the pharmacokinetic weight with the switch on and at total
  # body weight with it off (docs/weight-adjustment.md).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl1PerKg <- FENOLDOPAM_PED_CL / 60                        # L/min/kg
  v1PerKg  <- FENOLDOPAM_PED_CL / FENOLDOPAM_PED_KEL        # L/kg, derived

  default <- list(
    v1  = v1PerKg * pkW,
    v2  = 1,                        # one compartment
    v3  = 1,
    cl1 = cl1PerKg * pkW,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration rate constant published; plasma only
  MEAC  <- 0

  # The band is the steady-state plasma concentration of the infusion rates
  # the paper concluded were effective in these children, 0.8 to 1.2
  # mcg/kg/min with 1.0 typical: rate / clearance, so 0.8 / 0.051 = 15.7 and
  # 1.2 / 0.051 = 23.5 ng/mL.  It is derived from this paper's own dosing
  # conclusion, not a measured therapeutic range.
  typical      <- round(1.0 / cl1PerKg, 1)
  upperTypical <- round(1.2 / cl1PerKg, 1)
  lowerTypical <- round(0.8 / cl1PerKg, 1)

  reference <- paste0(
    "Hammer GB et al., BMC Anesthesiol 2008;8:6. ",
    "One-compartment model in 76 anaesthetised children, 3 weeks to 12.75 ",
    "years, during deliberate hypotension: elimination rate constant ",
    "13.2 /h and clearance 3.06 L/h/kg (approximate standard errors 1.23 ",
    "and 0.62).  The paper reports no volume; the plotted volume is the ",
    "derived CL / kel, 0.232 L/kg.  Children's weight-adjusted clearance ",
    "was about 1.8 times the adult value, so this model is not an adult ",
    "model. https://doi.org/10.1186/1471-2253-8-6"
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
