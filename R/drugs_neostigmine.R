# -----------------------------------------------------------------------------
# Neostigmine: Heier 2002, three-compartment population model
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma, nominal methylsulfate dose.
#
# SOURCE
# ======
# Heier and colleagues (Anesthesiology 2002;97:90-95) gave 70 mcg/kg of
# neostigmine over two minutes, after glycopyrrolate 14 mcg/kg, to seven
# anesthetized volunteers (6 men, 1 woman, 23-39 yr, 58-79 kg), each studied
# at normothermia and at mild hypothermia, with venous samples at 1, 2, 4,
# 8, 16, 30, 60, 90, 120 and 240 min (HPLC, LOQ 10 ng/mL).  NONMEM
# selected three compartments, NOT weight-normalised (Table 2: the
# weight-related three-compartment model fitted no better), with
# temperature on V1 only.  Table 3, normothermia (SE):
#
#     Cl 696 (175)   Q2 1480 (175)   Q3 556 (47) mL/min
#     V1 5.59 (0.926)   V2 15.0 (3.54)   V3 110 (73.9) L
#
# Vss 130.6 L; half-lives 1.3, 16.5 and 256 min (derived).  Hypothermia
# lowers V1 by 38% (SE 14%); not applied, since the Patient Profile has no
# temperature.  Interindividual variances did not improve the fit and are
# not reported.  The authors distrust V1 (venous, no sample before 1 min),
# and V3 is poorly estimated (SE 67%) because sampling stopped at 4 h: the
# terminal phase after a few hours is an extrapolation.
#
# Until 2026-10-28 the library used Calvey 1979's patient 1 (one of six
# individual two-compartment fits in women reversing tubocurarine; Br J
# Clin Pharmacol 1979;7:149-155), because Heier's table had not been read.
# That fit had a central volume of 0.76 L and a clearance of 400 mL/min, so
# its first minutes after a bolus were far higher than these.
#
# The historical assay question (bromide standards against methylsulfate
# dosing) applies to Calvey, not to Heier's HPLC; the dose is still the
# nominal labelled milligrams, with no molar correction.
#
# EFFECT SITE
# ===========
# Heier measured the time to maximum antagonism of a vecuronium block at
# 4.6 min (median, normothermia), from injection of the two-minute
# infusion.  It is used as a bolus tPeak on Heier's own PK, which slightly
# understates ke0.  No concentration-effect model is attached: the plotted
# effect site is a delayed concentration, not a train-of-four prediction.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The published parameters are fixed values for these adults (median
# weight near 70 kg); weight was tested and not retained.  With the switch
# on, volumes scale with fat-free mass relative to the 70 kg, 170 cm
# reference male and clearances with that ratio ^ 0.75; with it off, the
# published values are used unscaled.
#
# References
# ----------
# Heier T, Clough D, Wright PMC, Sharma ML, Sessler DI, Caldwell JE.
#   Anesthesiology 2002;97:90-95.
#   https://doi.org/10.1097/00000542-200207000-00013
# Calvey TN et al., Br J Clin Pharmacol 1979;7:149-155 (the previous model).
#   https://doi.org/10.1111/j.1365-2125.1979.tb00915.x
# -----------------------------------------------------------------------------

#' Neostigmine pharmacokinetics (Heier 2002)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published values unscaled, as the source (not weight-normalised) does.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
neostigmine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): the source is not weight-normalised, so
  # with the switch off the published values are used unscaled.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Heier 2002, Table 3, normothermia
  v1  <- 5.59  * size$volume       # L
  v2  <- 15.0  * size$volume
  v3  <- 110   * size$volume
  cl1 <- 0.696 * size$clearance    # L/min
  cl2 <- 1.480 * size$clearance
  cl3 <- 0.556 * size$clearance

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

  # Band, ng/mL: the library's own, set on the earlier Calvey model.  With
  # Heier's PK, 2.5-5 mg peaks at 75-150 ng/mL in the effect site, and the
  # plasma is 8-16 ng/mL at an hour.
  typical      <- 100
  upperTypical <- 300
  lowerTypical <- 30

  reference <- paste0(
    "Heier T et al., Anesthesiology 2002;97:90-95 (Table 3, three-compartment ",
    "population model at normothermia; time to peak effect 4.6 min). ",
    "https://doi.org/10.1097/00000542-200207000-00013"
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
