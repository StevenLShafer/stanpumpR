# -----------------------------------------------------------------------------
# Empagliflozin: two compartments, oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: the source's fasting glucose
# and HbA1c models, and urinary glucose excretion, are not plotted.  Drafted
# by Claude Code, 2026-10-17, at the request of Steven L. Shafer; see the
# antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Baron KT, Macha S, Broedl UC, Nock V, Retlich S, Riggs M.  Population
# pharmacokinetics and exposure-response (efficacy and safety/tolerability)
# of empagliflozin in patients with type 2 diabetes.  Diabetes Ther
# 2016;7:455-471 (PMC5014782).  2761 patients, 1-100 mg (mostly 10 and 25 mg
# once daily).  "A two-compartment model with a lagged first-order absorption
# and first-order elimination"; "the absorption lag time was fixed to 0.5 h".
# Typical values for the reference patient:
#
#     CL/F = 10.6 L/h    Vc/F = 3.14 L (95% CI 0.00128-4.03)
#     Q/F  = 6.34 L/h    Vp/F = 70.6 L    ka = 0.196 /h    lag 0.5 h, fixed
#
# The paper prints ka in "L/h", a typographical error for /h.  Vc/F is poorly
# identified (its interval runs almost to zero); the early curve is uncertain.
#
# REFERENCE PATIENT AND COVARIATES.  A 50-year-old non-smoking, non-Asian man,
# BMI 25, eGFR 100 mL/min/1.73 m^2 (MDRD).  The covariate coefficients on CL/F
# (eGFR, BMI, total protein, age, sex, smoking, race, alkaline phosphatase)
# are in the paper's supplement, Table S1, which could not be read, so NONE
# is applied: the curve is the reference patient's, scaled by body size.  The
# largest is renal: the paper reports AUC 18.5%, 49.2% and 88.1% higher at an
# eGFR of 60, 30 and 15.  A reduced eGFR therefore does NOT change this curve;
# the drug page says so.
#
# BODY SIZE: the reference parameters are scaled by the library's fat-free-
# mass factors; fixed with the switch off (legacyVolume = 1).
#
# References
# ----------
# Baron KT et al., Diabetes Ther 2016;7:455-471.
#   https://doi.org/10.1007/s13300-016-0174-y
# -----------------------------------------------------------------------------

#' Empagliflozin pharmacokinetics (oral)
#'
#' Two compartments, lagged first-order absorption (Baron 2016); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
empagliflozin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 3.14 * size$volume, v2 = 70.6 * size$volume, v3 = 1,
    cl1 = 10.6 * size$clearance / 60, cl2 = 6.34 * size$clearance / 60, cl3 = 0,
    ka_PO = 0.196 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0.5 * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Baron 2016 (2761 adults with type 2 diabetes): two compartments, CL/F 10.6 ",
      "L/h, Vc/F 3.14 L, Q/F 6.34 L/h, Vp/F 70.6 L, ka 0.196/h, lag 0.5 h; reference ",
      "patient (eGFR 100), covariates not applied; concentration only. ",
      "https://doi.org/10.1007/s13300-016-0174-y"
    )
  )
}
