# -----------------------------------------------------------------------------
# Argatroban: two-compartment intravenous model in PCI patients (Cox 2004)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (= mcg/L), plasma argatroban.
#
# SOURCE
# ======
# Cox DS, Kleiman NS, Boyle DA, Aluri J, Parchman LG, Holdbrook F, Fossler MJ.
# Pharmacokinetics and pharmacodynamics of argatroban in combination with a
# platelet glycoprotein IIb/IIIa receptor antagonist in patients undergoing
# percutaneous coronary intervention.  J Clin Pharmacol 2004;44:981-990.  152
# PCI patients given a 250 or 300 mcg/kg argatroban bolus then a 15 mcg/kg/min
# infusion, with abciximab or eptifibatide.  NONMEM; a two-compartment model
# with first-order elimination.  Table II, final model (model 4, fitted to the
# combined index and validation sets), read from the full text on 2026-10-17:
#
#     CL = 22.0 + 0.018 x (WT - 87)  L/h   (87 kg = population median weight)
#     V1 = 11.0 L     V2 = 13.0 L     Q = 10.0 L/h
#
# Body weight was the only covariate retained: it enters clearance linearly and
# additively about the 87 kg median, and its effect is small (CL moves from
# 21.7 to 22.2 L/h across 70-100 kg).  Age appeared to affect V1 in the
# screening but was not kept in the final model; race and gender had no effect.
# The half-lives are about 13 and 86 min.  The paper notes the population PK
# parameters are consistent with argatroban in healthy subjects, so the GP
# IIb/IIIa co-administration is not expected to change the kinetics materially.
#
# WHERE IT IS EXTRAPOLATED
# ========================
# PCI patients on a GP IIb/IIIa antagonist, a population with normal-to-mildly
# impaired hepatic function.  Argatroban is cleared hepatically, and the label
# reports markedly reduced clearance in moderate hepatic impairment; this model
# has no hepatic covariate, so in a patient with liver disease it will
# under-predict concentrations.  There is no renal covariate (argatroban is not
# renally cleared), so none is read.
#
# PHARMACODYNAMICS (NOT PLOTTED)
# ==============================
# The study fitted the activated clotting time as a sigmoid Emax model in
# plasma argatroban (E0, Emax, EC50, a Hill coefficient).  It is not plotted:
# the ACT depends on the device and reagent and the fit is the study's, and a
# simulated ACT is not clinical decision support.  tPeak = 0, so the curve is
# plasma.  No band, no MEAC and no time until threshold.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance carries the model's own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight) and at total body
# weight with it off, as for cefazolin.  V1, V2 and Q have no size term in the
# source and take the library's fat-free-mass factors with the switch on; with
# it off they are as published (legacyVolume = 1).
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Argatroban pharmacokinetics (intravenous, PCI patients)
#'
#' Cox 2004; see the header of \code{R/drugs_argatroban.R}.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string, "female" or "male"
#' @param adjustToFFM when \code{TRUE} (the default), evaluate the weight
#'   covariate at the pharmacokinetic weight and scale the size-free parameters
#'   to fat-free mass; when \code{FALSE}, use the published parameters
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
argatroban <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl <- 22.0 + 0.018 * (pkW - 87)                  # L/h, own weight covariate

  default <- list(
    v1  = 11.0 * size$volume,
    v2  = 13.0 * size$volume,
    v3  = 1,                                        # two compartments
    cl1 = cl / 60,
    cl2 = 10.0 / 60 * size$clearance,               # Q
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Cox DS et al., J Clin Pharmacol 2004;44:981-990 (two-compartment model ",
      "in 152 PCI patients on a GP IIb/IIIa antagonist; clearance carries a ",
      "linear body-weight covariate about the 87 kg median; no hepatic or renal ",
      "covariate). https://doi.org/10.1177/0091270004267651"
    )
  )
}
