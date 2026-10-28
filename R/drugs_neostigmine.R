# -----------------------------------------------------------------------------
# Neostigmine: Heier 2002 population model (three compartments)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma, nominal labelled dose.
#
# SOURCE
# ======
# Heier T, Clough D, Wright PMC, Sharma ML, Sessler DI, Caldwell JE,
# Anesthesiology 2002;97:90-95, Table 3: seven healthy anaesthetised
# volunteers (six men, 23-39 y, 58-79 kg) given 70 mcg/kg over 2 min during a
# steady vecuronium block, venous samples from 1 min to 4 h, NONMEM.  Final
# model three compartments, NOT weight-related (weight tested and not
# retained); normothermia:
#
#     CL 0.696 L/min   Q2 1.48 L/min   Q3 0.556 L/min
#     V1 5.59 L        V2 15.0 L       V3 110 L
#
# giving half-lives of 1.3, 16.5 and 256 min and Vss 131 L.  Changed to this
# model by decision of Steven L. Shafer, 2026-10-28.  The interindividual
# variability terms did not improve the fit and are not reported; V3 is
# poorly estimated (SE 73.9 L) and the authors distrust V1 (venous samples,
# none before 1 min).  Hypothermia below 34.5 C lowered V1 by 38% (SE 14%)
# and changed nothing else; the app has no temperature covariate for this
# drug, so the normothermic values are used.
#
# Until 2026-10-28 the library used CALVEY'S PATIENT 1 (Calvey TN et al., Br
# J Clin Pharmacol 1979;7:149-155: one of six individual two-compartment fits
# in women reversing tubocurarine under halothane; Vc 10.86 mL/kg, k10 0.527,
# k12 0.398, k21 0.039 /min), because the earlier review had only Heier's
# abstract and believed no population model existed.  Calvey's tiny central
# volume (0.76 L at 70 kg) put the instantaneous concentration of 3 mg near
# 4000 ng/mL; Heier's gives 537.
#
# EFFECT SITE
# ===========
# Heier's onset, "the time from injection of neostigmine until maximum
# effect" (T1, adductor pollicis, mechanomyography), was a median 4.6 min
# (4.0-5.8) at normothermia, after a TWO-MINUTE infusion, read as counted
# from its start.  On these kinetics a ke0 of 0.201 /min puts the effect-site
# peak of that 2-min infusion at 4.6 min; the bolus time to peak for that
# ke0 is 3.48 min, carried here as tPeak 3.5 (PK and PD are the same study,
# so no Minto conversion between papers is involved; the conversion is from
# an infusion to a bolus).  Until 2026-10-28 the 4.6 min was used as a bolus
# tPeak.  No concentration-effect model is attached: the plotted effect site
# is a delayed concentration, not a train-of-four prediction.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Heier's parameters are absolute values for the study's adults, taken as
# the 70 kg reference adult's: with the switch on, volumes scale with
# fat-free mass relative to the reference male and clearances with that
# ratio ^ 0.75; with it off, the published values are used for everyone
# (legacyVolume = 1), as Heier found weight did not improve the model.
#
# BAND
# ====
# 30-300 ng/mL, the library's own orientation band (not a therapeutic range;
# see inst/extdata/therapeuticRanges.csv), kept unchanged.  With Heier's
# kinetics 2.5-5 mg puts the effect site at 104-207 ng/mL at its peak, and
# it falls below 30 ng/mL at about 20-40 min.
#
# References
# ----------
# Heier T et al., Anesthesiology 2002;97:90-95.
#   https://doi.org/10.1097/00000542-200207000-00013
# Calvey TN et al., Br J Clin Pharmacol 1979;7:149-155 (the former model).
#   https://doi.org/10.1111/j.1365-2125.1979.tb00915.x
# -----------------------------------------------------------------------------

#' Neostigmine pharmacokinetics (Heier 2002)
#'
#' @inheritParams cefazolin
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use
#'   Heier's published values, which are not weight-related, for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
neostigmine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): absolute values, published for everyone
  # with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Heier 2002, Table 3, normothermia
  default <- list(
    v1  = 5.59  * size$volume,
    v2  = 15.0  * size$volume,
    v3  = 110   * size$volume,
    cl1 = 0.696 * size$clearance,
    cl2 = 1.48  * size$clearance,
    cl3 = 0.556 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 3.5   # bolus equivalent of Heier's 4.6 min after a 2-min infusion
  MEAC  <- 0

  # Band, ng/mL: the library's orientation band (see the header).
  typical      <- 100
  upperTypical <- 300
  lowerTypical <- 30

  reference <- paste0(
    "Heier T et al., Anesthesiology 2002;97:90-95 (three-compartment ",
    "population model, Table 3; time to peak effect from the same study). ",
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
