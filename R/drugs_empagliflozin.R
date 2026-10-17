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
# BMI 25, eGFR 100 mL/min/1.73 m^2 (MDRD).  Each continuous covariate enters
# as a power of its value over the reference, (cov / ref)^theta, and each
# categorical one as theta^indicator (Methods, the TVP equation).  The
# coefficients are in the supplement's Table S1, which was not available.
#
# RENAL FUNCTION IS APPLIED, with a DERIVED exponent.  The paper reports the
# reference patient's AUC 18.5%, 49.2% and 88.1% higher at an eGFR of 60, 30
# and 15.  On its power form, CL/F x (eGFR / 100)^theta, those give theta =
# 0.3323, 0.3323 and 0.3330: one exponent, 0.333, reproduces all three to
# within 0.2%, so the model uses it (marked derived in the parameter audit)
# until Table S1 is transcribed.  eGFR is the four-variable MDRD
# (egfrMDRD(), R/renalFunction.R) at the patient's creatinine (a child's on
# the adult scale) or an assumed normal creatinine for age and sex.
#
# NOT APPLIED: BMI, total protein, age, sex, smoking, race and alkaline
# phosphatase on CL/F, and the covariates on the volumes and ka.  All change
# exposure by less than 25% (the paper's Figure 1: within 0.8 to 1.25 at the
# extremes).
#
# BODY SIZE: the reference parameters are scaled by the library's fat-free-
# mass factors; fixed with the switch off (legacyVolume = 1).
#
# References
# ----------
# Baron KT et al., Diabetes Ther 2016;7:455-471.
#   https://doi.org/10.1007/s13300-016-0174-y
# -----------------------------------------------------------------------------

# The exponent of (eGFR / 100) on CL/F, derived from the paper's AUC at an
# eGFR of 60, 30 and 15; see the header.
EMPAGLIFLOZIN_EGFR_EXPONENT <- 0.333

#' Empagliflozin pharmacokinetics (oral)
#'
#' Two compartments, lagged first-order absorption (Baron 2016); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
empagliflozin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  renal <- (egfr / 100)^EMPAGLIFLOZIN_EGFR_EXPONENT

  default <- list(
    v1  = 3.14 * size$volume, v2 = 70.6 * size$volume, v3 = 1,
    cl1 = 10.6 * size$clearance * renal / 60, cl2 = 6.34 * size$clearance / 60, cl3 = 0,
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
      "L/h x (MDRD eGFR/100)^0.333 from the entered or an assumed normal creatinine ",
      "(exponent derived from the paper's AUC at eGFR 60, 30 and 15), Vc/F 3.14 L, ",
      "Q/F 6.34 L/h, Vp/F 70.6 L, ka 0.196/h, lag 0.5 h; other covariates not ",
      "applied; concentration only. ",
      "https://doi.org/10.1007/s13300-016-0174-y"
    )
  )
}
