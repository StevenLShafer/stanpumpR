# -----------------------------------------------------------------------------
# Glyburide (glibenclamide): two compartments, oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: no glucose or insulin effect
# is plotted, and the active metabolites M1 and M2 are not modelled.  Drafted
# by Claude Code, 2026-10-17, at the request of Steven L. Shafer; see the
# antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Rambiritch V, Naidoo P, Maharaj B, Pillai G.  Population pharmacokinetic
# modeling of glibenclamide in poorly controlled South African type 2
# diabetic subjects.  Clin Pharmacol Adv Appl 2016;8:83-92 (PMC4948726).
# 24 patients, 2.5, 5, 10 and 20 mg/day at steady state.  A two-compartment
# disposition model with first-order absorption, no lag, apparent
# parameters; "no formal covariate analysis was conducted":
#
#     ka   = 0.53 /h     CL/F = 2.16 L/h
#     Vc/F = 11.70 L     Q/F  = 3.84 L/h     Vp/F = 68.1 L
#
# The authors note their peak concentrations were about twice those of other
# studies.  The PD companion paper's EC50 (printed 85.20 mg/L, likely
# mcg/L) is an endpoint of its own and is not used.
#
# BODY SIZE: no covariates; the parameters are scaled by the library's
# fat-free-mass factors, fixed with the switch off (legacyVolume = 1).
#
# References
# ----------
# Rambiritch V et al., Clin Pharmacol Adv Appl 2016;8:83-92.
#   https://doi.org/10.2147/CPAA.S102676
# -----------------------------------------------------------------------------

#' Glyburide (glibenclamide) pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption (Rambiritch 2016); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
glyburide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 11.70 * size$volume, v2 = 68.1 * size$volume, v3 = 1,
    cl1 = 2.16 * size$clearance / 60, cl2 = 3.84 * size$clearance / 60, cl3 = 0,
    ka_PO = 0.53 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Rambiritch 2016 (24 adults with type 2 diabetes): two compartments, ka ",
      "0.53/h, CL/F 2.16 L/h, Vc/F 11.7 L, Q/F 3.84 L/h, Vp/F 68.1 L; no ",
      "covariates; concentration only. https://doi.org/10.2147/CPAA.S102676"
    )
  )
}
