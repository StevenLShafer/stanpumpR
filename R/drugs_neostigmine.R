# -----------------------------------------------------------------------------
# Neostigmine: a single historical individual fit (Calvey 1979, patient 1)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma, nominal methylsulfate dose.
#
# THERE IS NO VERIFIED ADULT POPULATION MODEL
# ===========================================
# The literature review behind this file found individual two-compartment
# fits in six female surgical patients (Calvey 1979: 15-second injection
# during tubocurarine reversal under halothane, atropine given, first sample
# at two minutes) and a later volunteer study (Heier 2002: 70 mcg/kg over
# two minutes, sampled to four hours) whose abstract reports a clearance of
# 696 mL/min and a 38% fall in central volume with hypothermia but whose
# full parameter table was not recovered.  Neither is a population typical
# vector with covariates.
#
# This file uses CALVEY'S PATIENT 1, as a complete, internally consistent
# row, scaled per kilogram to the reference adult:
#
#     Vc 10.86 mL/kg   k10 0.527 /min   k12 0.398 /min   k21 0.039 /min
#
# giving, at 70 kg, Vc 0.76 L, Vp 7.76 L, CL 0.40 L/min (400 mL/min against
# Heier's 696), Q 0.30 L/min, half-times 0.74 and 31.8 min (Calvey's
# patients ranged 15-32 min).  Averaging the six rows was rejected by the
# review because patient 5's fitted central volume (0.11 mL/kg) is not a
# physiological space.
#
# WHAT THIS MEANS FOR THE CURVE
# =============================
# The first two minutes after a bolus were never observed and are not
# credible here: the tiny central volume puts the instantaneous
# concentration of a 3 mg dose near 4000 ng/mL before a sub-minute
# distribution phase removes most of it.  From a few minutes on, the curve
# reproduces the fitted decline.  Treat the model as the best available
# shape, not as a validated prediction, and replace it when an adult
# individual-data population model with early sampling appears.
#
# The historical assay was calibrated against bromide standards while the
# drug given was the methylsulfate; the exact harmonisation was not
# established, so the dose is the nominal labelled milligrams and no molar
# correction is applied.
#
# EFFECT SITE
# ===========
# Heier 2002 measured the time to maximum antagonism of a vecuronium block
# at 4.6 min after the START of a two-minute infusion.  That is used as
# tPeak, which slightly understates ke0 relative to a true bolus value.
# Calvey saw maximal electromyographic facilitation at 7-15 min against a
# different blocker and endpoint.  No concentration-effect model is
# attached: the plotted effect site is a delayed concentration, not a
# train-of-four prediction.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source is per-kilogram with fixed rate constants.  With the switch on,
# volumes scale with fat-free mass relative to the reference male and
# clearances with that ratio ^ 0.75; with it off, everything scales with
# weight/70, which is the per-kilogram source exactly.
#
# References
# ----------
# Calvey TN et al., Br J Clin Pharmacol 1979;7:149-155.
#   https://doi.org/10.1111/j.1365-2125.1979.tb00915.x
# Heier T et al., Anesthesiology 2002;97:90-95.
#   https://doi.org/10.1097/00000542-200207000-00013
# -----------------------------------------------------------------------------

#' Neostigmine pharmacokinetics (single historical fit)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
neostigmine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): per-kilogram source, so the default
  # legacy factors (weight/70 on everything) reproduce it exactly.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  # Calvey 1979, patient 1
  v1Ref <- 0.01086 * 70   # L at the 70 kg reference
  k10 <- 0.527            # /min
  k12 <- 0.398
  k21 <- 0.039

  v1  <- v1Ref * size$volume
  v2  <- v1Ref * k12 / k21 * size$volume
  v3  <- 1                                  # no third compartment
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
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

  tPeak <- 4.6   # Heier 2002, time to maximum effect; see the header
  MEAC  <- 0

  # Band, ng/mL: the concentrations a few minutes to an hour after 2.5-5 mg.
  typical      <- 100
  upperTypical <- 300
  lowerTypical <- 30

  reference <- paste0(
    "Calvey TN et al., Br J Clin Pharmacol 1979;7:149-155 (patient 1 of six ",
    "individual two-compartment fits; no population model exists); time to ",
    "peak effect from Heier T et al., Anesthesiology 2002;97:90-95. ",
    "https://doi.org/10.1111/j.1365-2125.1979.tb00915.x"
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
