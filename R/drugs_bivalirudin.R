# -----------------------------------------------------------------------------
# Bivalirudin: two-compartment intravenous model in young healthy volunteers
# (Zhang 2012)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), plasma bivalirudin.
#
# SOURCE
# ======
# Zhang DM, Wang K, Zhao X, Li YF, Zheng QS, Wang ZN, Cui YM.  Population
# pharmacokinetics and pharmacodynamics of bivalirudin in young healthy
# Chinese volunteers.  Acta Pharmacol Sin 2012;33:1387-1394.  36 volunteers in
# four groups: 0.5, 0.75 or 1.05 mg/kg intravenous bolus, or 0.75 mg/kg then
# 1.75 mg/kg/h for 4 h (the PCI dose).  NONMEM; "The final PK model was a
# two-compartment model without covariates", with typical values (abstract,
# read 2026-10-17):
#
#     CL = 0.323 L/h/kg     V1 = 0.086 L/kg
#     Q  = 0.0957 L/h/kg    V2 = 0.0554 L/kg
#
# Every parameter is per kilogram of total body weight with no estimated
# exponent; at 70 kg CL is 22.6 L/h, V1 6.02 L, Q 6.70 L/h, V2 3.88 L, and the
# half-lives are 6.4 and 27 min.  The label's terminal half-life in patients
# with normal renal function is 25 min.
#
# WHERE IT IS EXTRAPOLATED
# ========================
# Young, healthy volunteers, with normal renal function.  Bivalirudin is about
# 20% renally cleared, and the label (Angiomax) reports lower clearance in
# moderate and severe renal impairment and on dialysis, and a clearance of
# 3.4 mL/min/kg (0.204 L/h/kg) in PCI patients against this study's 0.323.
# None of that is in this model, which has no renal covariate: in an older
# patient, or one with renal impairment, it will under-predict
# concentrations.  The label's clearance is not spliced in, because it belongs
# to a different fit with different volumes.
#
# PHARMACODYNAMICS (NOT PLOTTED)
# ==============================
# The study fitted the activated clotting time, ACT = 134 + 318 C / (2.44 + C)
# seconds with C in mg/L, with the EC50 depending on the red cell count
# (coefficient 1.70; the form and reference value are in the full text, not
# the abstract).  It is not plotted: the device and reagent decide the ACT,
# and the fit is the study's.  tPeak = 0, so the curve is plasma.  No band and
# no time until threshold.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# A per-kilogram model fitted on total body weight is scaled to fat-free mass
# with the switch on, like every such model in the library: the parameters
# are written for 70 kg and multiplied by the library's factors.  With the
# switch off it is as published, proportional to total body weight
# (legacyVolume = legacyClearance = weight / 70).
#
# (Claude Code, 2026-10-17, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

#' Bivalirudin pharmacokinetics (intravenous, healthy volunteers)
#'
#' Zhang 2012; see the header of \code{R/drugs_bivalirudin.R}.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string, "female" or "male"
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power (the default); when
#'   \code{FALSE}, use the published parameters unscaled
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
bivalirudin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  default <- list(
    v1  = 0.086  * 70 * size$volume,
    v2  = 0.0554 * 70 * size$volume,
    v3  = 1,                                    # two compartments
    cl1 = 0.323  * 70 / 60 * size$clearance,
    cl2 = 0.0957 * 70 / 60 * size$clearance,
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
      "Zhang DM et al., Acta Pharmacol Sin 2012;33:1387-1394 (two-compartment ",
      "model in 36 young healthy Chinese volunteers, per kg of body weight; no ",
      "renal covariate). https://doi.org/10.1038/aps.2012.37"
    )
  )
}
