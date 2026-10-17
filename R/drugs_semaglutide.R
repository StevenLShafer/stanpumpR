# -----------------------------------------------------------------------------
# Semaglutide: one compartment, subcutaneous once weekly (Ozempic)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 nmol/L = 4.11358 ng/mL; molecular weight
# 4113.58 g/mol).  Concentration only: no glucose, HbA1c or weight effect is
# plotted.  Drafted by Claude Code, 2026-10-17, at the request of Steven L.
# Shafer; see the antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Carlsson Petri KC, Ingwersen SH, Flint A, Zacho J, Overgaard RV.
# Semaglutide s.c. once-weekly in type 2 diabetes: a population
# pharmacokinetic analysis.  Diabetes Ther 2018;9:1533-1547 (PMC6064581).
# The phase 3 (SUSTAIN 1-5) analysis in people with type 2 diabetes, 0.5 and
# 1.0 mg once weekly.  One compartment, first-order absorption:
#
#     ka   = 0.0286 /h, FIXED from richly sampled clinical pharmacology data
#     CL/F = 0.0478 L/h   for the reference subject
#     V/F  = 12.2 L       (no covariate on V/F)
#
# The reference subject is a white non-Hispanic woman under 65, 85 kg, with
# normal renal function, injected in the abdomen.  Covariates are on CL/F
# only; "only body weight was considered important".
#
# BODY WEIGHT.  The paper's weight exponent is in its supplement, which could
# not be retrieved.  It reports the exposure the weight effect gives: "a
# subject weighing 55 kg had on average a 40% increased semaglutide exposure
# and a 127 kg subject had a 27% lower".  With CL/F proportional to
# (WT / 85)^theta those give theta = 0.773 and 0.784; the model uses 0.78,
# a DERIVED value (recorded as such in inst/extdata/antidiabeticParameters.csv)
# until the published exponent is transcribed.  With the switch on the weight
# is the pharmacokinetic weight (size$pkWeight), off it is total body weight;
# V/F, which carries no weight term in the source, is scaled by the library
# factor (legacyVolume = 1).
#
# WHY NOT THE TWO-COMPARTMENT FIT
# ===============================
# Overgaard et al. (Diabetes Ther 2019;10:649-662) fitted intravenous and
# subcutaneous data together (two compartments, F 0.85).  Its 0.0348 L/h is
# the clearance of a typical subject WITH type 2 diabetes at 85 kg, although
# its reference profile is healthy, so a further x1.12 for diabetes would
# count the effect twice; and its absorption rate and intercompartmental
# clearance could not be read from the available text.  It stays in the
# registry until its table is transcribed.  The oral tablets (Rybelsus) are a
# different product with their own bioavailability and are not this drug.
#
# CHECKS (reference subject, switch off)
# ======================================
# Steady-state average concentration F x dose / (CL x 168 h): 1.0 mg weekly
# 124.5 ng/mL = 30.3 nmol/L (paper 29.8 nmol/L, 95% CI 29.4-30.2); 0.5 mg
# 15.1 nmol/L (paper 15.8).  Half-life ln 2 x V / CL = 177 h (about a week).
#
# References
# ----------
# Carlsson Petri KC et al., Diabetes Ther 2018;9:1533-1547.
#   https://doi.org/10.1007/s13300-018-0458-5
# Overgaard RV et al., Diabetes Ther 2019;10:649-662.
#   https://doi.org/10.1007/s13300-019-0581-y
# -----------------------------------------------------------------------------

SEMAGLUTIDE_WEIGHT_EXPONENT <- 0.78

#' Semaglutide pharmacokinetics (subcutaneous, once weekly)
#'
#' One compartment with first-order absorption (Carlsson Petri 2018); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
semaglutide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 12.2 * size$volume, v2 = 1, v3 = 1,
    cl1 = 0.0478 * (pkW / 85)^SEMAGLUTIDE_WEIGHT_EXPONENT / 60,
    cl2 = 0, cl3 = 0,
    ka_SC = 0.0286 / 60,
    bioavailability_SC = 1,   # apparent CL/F and V/F
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Carlsson Petri 2018 (type 2 diabetes, 0.5-1 mg weekly): one compartment, ",
      "ka 0.0286/h fixed, CL/F 0.0478 L/h x (weight/85)^0.78 (exponent derived ",
      "from the paper's 55 and 127 kg exposures), V/F 12.2 L; concentration only. ",
      "https://doi.org/10.1007/s13300-018-0458-5"
    )
  )
}
