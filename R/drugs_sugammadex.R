# -----------------------------------------------------------------------------
# Sugammadex: two-compartment total-drug model (Kleijn 2011)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL of TOTAL sugammadex (free plus complexed).
#
# DISPOSITION
# ===========
# Kleijn et al. modelled free sugammadex, free rocuronium and their complex
# in 426 patients and 20 volunteers, assigning the complex the same
# disposition as free sugammadex.  Summing the free and complex states
# therefore gives an autonomous two-compartment model of TOTAL sugammadex,
# which is also what the assay measured.  With W total body weight in kg
# and CR the Cockcroft-Gault creatinine clearance in mL/min:
#
#     CL = 5.58 x [1 + 0.00378 (W - 74.5)] x [2 CR / (CR + 119)]^1.29   L/h
#     Vc = 4.42 x [1 - 0.00354 (W - 74.5)] x rS x (W/70)                L
#     Q  = 12.36 x (W/70)^0.75                                          L/h
#     Vp = 6.35 x exp[-0.00305 (CR - 119)] x (W/70)                     L
#
# rS is 1 for the source's non-Asian category and 0.84 for its Asian
# category; it is a reproduction term for the source population and is
# left at 1 here, as stanpumpR collects no such covariate.  The clearance
# receives no extra weight allometry: its renal term already carries size.
#
# Rocuronium binding, and the train-of-four recovery it produces, are NOT
# modelled.  This row is the sugammadex concentration alone; the rocuronium
# row is unaffected by giving sugammadex, which is the main limitation of
# simulating the two side by side.
#
# RENAL FUNCTION
# ==============
# CR is Cockcroft-Gault at the patient's serum creatinine from the Patient
# Profile, or at an ASSUMED NORMAL creatinine when none is entered
# (R/renalFunction.R).  Sugammadex is renally cleared and the label does not
# recommend it below 30 mL/min.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter carries its own weight term, so the model is used with the
# pharmacokinetic weight (70 kg x FFM / FFM_ref) in place of W when the
# switch is on, and with total body weight when it is off.  Both the
# 74.5 kg centring and the 70 kg normalisation are kept where printed.
#
# Doses are sugammadex equivalents, as the Bridion label states them.
#
# References
# ----------
# Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433.
#   https://doi.org/10.1111/j.1365-2125.2011.04000.x
# -----------------------------------------------------------------------------

#' Sugammadex pharmacokinetics (total)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sugammadex <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # Size scaling (see the header): the model's own weight terms are
  # evaluated at the pharmacokinetic weight with the switch on, total body
  # weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  W  <- 70 * size$volume
  CR <- creatinineClearanceCG(W, age, sex,     # mL/min
                              patientCreatinine(creatinine, sex))
  rS <- 1

  cl1 <- 5.58 * (1 + 0.00378 * (W - 74.5)) * (2 * CR / (CR + 119))^1.29 / 60
  v1  <- 4.42 * (1 - 0.00354 * (W - 74.5)) * rS * (W / 70)
  cl2 <- 12.36 * (W / 70)^0.75 / 60
  v2  <- 6.35 * exp(-0.00305 * (CR - 119)) * (W / 70)
  v3  <- 1                                     # no third compartment
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

  # Sugammadex acts in plasma, by encapsulating rocuronium there; it has no
  # effect site of its own, and the reversal it produces is not modelled.
  tPeak <- 0
  MEAC  <- 0

  # Band, total mg/L: what 2-4 mg/kg produce over the first hour, after the
  # first 15 min or so, when 4 mg/kg is still above 30.
  typical      <- 10
  upperTypical <- 30
  lowerTypical <- 5

  reference <- paste0(
    "Kleijn HJ et al., Br J Clin Pharmacol 2011;72:415-433. ",
    "Two-compartment model of TOTAL sugammadex (free plus rocuronium ",
    "complex); creatinine clearance from the entered creatinine or an ",
    "assumed normal one; rocuronium binding not modelled. ",
    "https://doi.org/10.1111/j.1365-2125.2011.04000.x"
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
