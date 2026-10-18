# -----------------------------------------------------------------------------
# Metformin: one compartment, oral immediate release
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L).  Concentration only: the source's
# glucose-response model is not plotted.  Drafted by Claude Code, 2026-10-17,
# at the request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Chae JW, Baek IH, Lee BY, Cho SK, Kwon KI.  Population PK/PD analysis of
# metformin using the signal transduction model.  Br J Clin Pharmacol
# 2012;74:815-823 (PMC3495146).  42 healthy Korean adults (age 21-31,
# 61-78 kg, CLcr 90-123 mL/min; Table 1), one 500 mg tablet.  "A one
# compartment body model with first order absorption and elimination", no
# lag; Table 2 and the final-model equations (full text, supplied by Steven
# L. Shafer):
#
#     CL/F = 52.6 L/h x (CLcr / 106.5)^0.782
#     V/F  = 113 L
#     ka   = 0.41 /h
#
# Table 2 prints V/F's relative standard error as 56.6%, but the text gives
# 4.18% and the bootstrap interval (100 to 126 L) agrees with the text.
#
# CLcr in mL/min.  The paper does not say how it was obtained; the model
# uses Cockcroft-Gault, the usual choice, at the patient's creatinine (a
# child's on the adult scale) or, with the field blank, an assumed normal
# creatinine for age and sex (R/renalFunction.R).  The subjects' CLcr ran
# from 90 to 123 mL/min only, so in renal impairment the power term is an
# extrapolation; it is not capped, because metformin's clearance is mostly
# renal and falls with renal function.
#
# NOT EXTENDED-RELEASE.  The extended-release tablets are a different input
# (a PAGE 2017 abstract, not a complete model) and are not offered; ER doses
# are not this drug.
#
# PHARMACODYNAMICS NOT PLOTTED.  Chae's signal-transduction glucose response
# (Emax 19.8, EC50 3.68 mcg/mL, Hill 0.547, transit 0.5 h) describes the
# single-dose glucose change in healthy adults; it is an endpoint of its own,
# not an effect site, and the app has no glucose output.
#
# BODY SIZE: Cockcroft-Gault at the pharmacokinetic weight with the switch on
# and at total weight with it off; V/F by the library factor
# (legacyVolume = 1).
#
# CHECKS (70 kg, 40 y man, assumed creatinine 0.9 mg/dL)
# =======================================================
# CLcr 108 mL/min, CL/F 53 L/h, apparent half-life 1.5 h, peak about 2.5 h
# after a dose.
#
# References
# ----------
# Chae JW et al., Br J Clin Pharmacol 2012;74:815-823.
#   https://doi.org/10.1111/j.1365-2125.2012.04260.x
# -----------------------------------------------------------------------------

#' Metformin pharmacokinetics (oral, immediate release)
#'
#' One compartment with a renal covariate (Chae 2012); see the header of the
#' file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metformin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))

  default <- list(
    v1  = 113 * size$volume, v2 = 1, v3 = 1,
    cl1 = 52.6 * (crcl / 106.5)^0.782 / 60, cl2 = 0, cl3 = 0,
    ka_PO = 0.41 / 60,
    bioavailability_PO = 1,   # apparent CL/F and V/F
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Chae 2012 (42 healthy adults, 500 mg): one compartment, CL/F 52.6 L/h x ",
      "(Cockcroft-Gault/106.5)^0.782 from the entered or an assumed normal ",
      "creatinine, V/F 113 L, ka 0.41/h; immediate release, concentration only. ",
      "https://doi.org/10.1111/j.1365-2125.2012.04260.x"
    )
  )
}
