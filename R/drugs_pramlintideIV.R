# -----------------------------------------------------------------------------
# Pramlintide, intravenous: two compartments (research model)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 pmol/L = 0.003949 ng/mL; molecular weight
# 3949.4 g/mol).  Concentration only: the source's model of endogenous
# glucose production and meal glucose is not plotted.  Drafted by Claude
# Code, 2026-10-17, at the request of Steven L. Shafer; see the antidiabetic
# registry, inst/extdata/antidiabeticRegistry.csv.
#
# A RESEARCH ROUTE.  Pramlintide (Symlin) is given subcutaneously.  This is
# the intravenous study model, kept as its own drug, as the specification
# asks; the subcutaneous product has no complete fitted model in hand (its
# label gives bioavailability 30-40%, a peak at about 20 minutes and a
# half-life of about 48 minutes, constraints only), and none is borrowed.
#
# SOURCE
# ======
# Fang J, Landersdorfer CB, Cirincione B, Jusko WJ.  Study reanalysis using a
# mechanism-based pharmacokinetic/pharmacodynamic model of pramlintide in
# subjects with type 1 diabetes.  AAPS J 2013;15(1).  25 men with type 1
# diabetes, three dose levels each as a bolus (2-minute infusion) and a
# 2-hour intravenous infusion.  "A two-compartment linear model with
# first-order elimination including a lag time"; Table II:
#
#     CL  = 0.955 L/min    V1 = 19.1 L
#     CLD = 0.283 L/min    V2 = 11.0 L
#     Tlag = 0.430 min, D1 = 4 min (the short infusion's fitted duration)
#
# The lag (26 seconds) and the short infusion's fitted 4-minute duration
# describe sampling in that study and are not applied: a dose is given as
# entered.  No covariate was found.
#
# BODY SIZE: the parameters are scaled by the library's fat-free-mass
# factors, fixed with the switch off (legacyVolume = 1).  The subjects
# weighed 77 kg on average.
#
# References
# ----------
# Fang J et al., AAPS J 2013;15(1).
#   https://doi.org/10.1208/s12248-012-9409-7
# -----------------------------------------------------------------------------

#' Pramlintide pharmacokinetics (intravenous research model)
#'
#' Two compartments (Fang 2013); see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pramlintideIV <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 19.1 * size$volume, v2 = 11.0 * size$volume, v3 = 1,
    cl1 = 0.955 * size$clearance, cl2 = 0.283 * size$clearance, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Fang 2013 (25 men with type 1 diabetes, intravenous): two compartments, CL ",
      "0.955 L/min, V1 19.1 L, CLD 0.283 L/min, V2 11.0 L; research route (the product ",
      "is subcutaneous); concentration only. https://doi.org/10.1208/s12248-012-9409-7"
    )
  )
}
