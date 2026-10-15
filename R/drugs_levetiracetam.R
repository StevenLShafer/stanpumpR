# -----------------------------------------------------------------------------
# Levetiracetam: one compartment, oral and intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma (under 10% bound).
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer; see
# the antiseizure registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE AND REDUCTION
# ====================
# No single retrievable population model carries both intravenous disposition
# and an explicit creatinine-clearance equation, so a model is assembled from
# sourced parts, and the assembly is recorded in the registry as a reduction:
#
#   - STRUCTURE AND TYPICAL CLEARANCE: Rhee SJ et al. (Epilepsy Res
#     2017;132:8-15), 425 Korean adults, one compartment, first-order
#     absorption, ka 2.44 /h FIXED, CL/F 3.9 L/h at a typical adult; weight
#     on both parameters, estimated glomerular filtration rate on clearance.
#   - VOLUME: 0.6 L/kg, the label's (Keppra: "close to the volume of
#     intracellular and extracellular water", 0.5-0.7 L/kg, with a half-life
#     of 7 +/- 1 h in adults).  Rhee's V/F, 65.3 L, is from 483 mostly trough
#     concentrations in 425 patients, which barely identify a volume; with
#     the clearance it gave a half-life of 11.6 h, against 7 +/- 1 h in every
#     rich-data study and the label.  Until 2026-10-14 the file carried 65.3 L
#     and its header mis-stated the resulting half-life as 8 h.
#   - RENAL SPLIT: the label and mass-balance work put about 66% of a dose
#     into the urine unchanged and about 24% through non-renal hydrolysis.
#     Clearance is therefore split 0.66 renal, proportional to Cockcroft-Gault
#     creatinine clearance against the reference patient's, and 0.34 non-renal
#     and fixed.  This reproduces the proportional fall in clearance with
#     renal function that the label and the critically ill models (Bilbao-
#     Meseguer 2021) show, without importing an intensive-care intercept.
#   - ROUTES: absolute oral bioavailability is essentially complete, so the
#     apparent parameters are read as absolute and the intravenous route (the
#     injection is interchangeable with the tablet mg for mg) uses them with
#     F = 1.
#
# The reference patient (70 kg, 40 y man, assumed creatinine 0.9 mg/dL) has a
# Cockcroft-Gault clearance of 108 mL/min, at which total clearance is Rhee's
# 3.9 L/h.
#
# CHECKS (70 kg adult)
# ====================
# Half-life ln 2 x 42 L / 3.9 L/h = 7.5 h (label 7 +/- 1 h).  Ramael 2006,
# 1500 mg in healthy adults (73 kg): intravenous clearance 3.82 L/h and oral
# 3.51 L/h, AUC 392 and 428 mg.h/L; the model gives a clearance of 3.9 L/h
# and an AUC of 385 mg.h/L for 1500 mg.  Ramael's end-of-infusion peak after
# 1500 mg over 15 min, 50.5 mcg/mL, is above the one-compartment model's 35
# mcg/mL: the early distribution phase is not in a one-compartment model, so
# the peak after a fast intravenous dose is understated.  Levetiracetam
# accumulates in renal impairment, which the renal split captures;
# haemodialysis is not represented.
#
# NOT OFFERED: the extended-release tablet (Keppra XR; a published input model
# was not confirmed) and the weak phenytoin interaction.
#
# RENAL FUNCTION: Cockcroft-Gault at the patient's creatinine (a child's on
# the adult scale) or an assumed normal one (R/renalFunction.R).
#
# BODY SIZE: V per kilogram (0.6 L/kg) and the renal clearance through
# Cockcroft-Gault, both at the pharmacokinetic weight with the switch on.
#
# BAND: 12-46 mcg/mL (ILAE, Patsalos 2008), typical 30.
#
# References
# ----------
# Rhee SJ et al., Epilepsy Res 2017;132:8-15.
#   https://doi.org/10.1016/j.eplepsyres.2017.02.011
# Bilbao-Meseguer I et al., Pharmaceutics 2021;13:1690.
#   https://doi.org/10.3390/pharmaceutics13101690
# Ramael S et al., Clin Ther 2006;28:734-744.
#   https://doi.org/10.1016/j.clinthera.2006.05.004
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Keppra (levetiracetam) US prescribing information, UCB: volume of
#   distribution 0.5-0.7 L/kg, half-life 7 +/- 1 h, 66% renal unchanged.
# -----------------------------------------------------------------------------

#' Fraction of levetiracetam clearance that is renal
#' @keywords internal
LEVETIRACETAM_RENAL_FRACTION <- 0.66

#' Levetiracetam volume of distribution, L/kg (the label's 0.5-0.7 L/kg)
#' @keywords internal
LEVETIRACETAM_V_PER_KG <- 0.6

#' Levetiracetam pharmacokinetics (oral and intravenous)
#'
#' One compartment, clearance split renal (proportional to creatinine
#' clearance) and non-renal; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levetiracetam <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl    <- creatinineClearanceCG(pkW, age, sex,
                                   adultEquivalentCreatinine(creatinine, age, sex))
  crclRef <- creatinineClearanceCG(70, 40, SEX_MALE, adultCreatinine(SEX_MALE))
  f <- LEVETIRACETAM_RENAL_FRACTION
  clTotal <- 3.9 * (f * (crcl / crclRef) + (1 - f))     # L/h

  default <- list(
    v1  = LEVETIRACETAM_V_PER_KG * pkW, v2 = 1, v3 = 1,
    cl1 = clTotal / 60, cl2 = 0, cl3 = 0,
    ka_PO = 2.44 / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 30, upperTypical = 46, lowerTypical = 12,
    reference = paste0(
      "Rhee SJ et al., Epilepsy Res 2017;132:8-15 (one compartment, ka 2.44/h ",
      "fixed, CL/F 3.9 L/h), the volume 0.6 L/kg and the 66% renal / 34% ",
      "non-renal clearance split (renal proportional to Cockcroft-Gault) from ",
      "the Keppra label, which gives a half-life of 7.5 h; intravenous on the ",
      "label's 1:1 conversion. https://doi.org/10.1016/j.eplepsyres.2017.02.011"
    )
  )
}
