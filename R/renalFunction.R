# Renal function for the drug models that need it.
#
# stanpumpR collects weight, height, age and sex, and nothing else.  Several
# of the antibiotic and reversal-agent models carry a renal covariate
# (Cockcroft-Gault creatinine clearance, or a de-indexed CKD-EPI eGFR), so
# the app has to supply one from the covariates it has.  It does so by
# ASSUMING A NORMAL SERUM CREATININE and letting age, sex and body size carry
# the rest.  Every model that calls these helpers says so in its header, and
# the assumption is deliberately visible here rather than buried in a drug
# file.
#
# What this does and does not capture:
#   - It captures the fall in renal function with age and the sex difference
#     that the creatinine-based formulas build in, which is most of the
#     between-patient variation among people with NORMAL kidneys.
#   - It does NOT capture renal impairment.  A patient with a creatinine of
#     3 mg/dL is simulated as if it were 1.0.  Until the app collects a
#     creatinine, a renally cleared drug in a patient with impaired kidneys
#     is overpredicted to clear and underpredicted to accumulate.
#
# Body size.  Cockcroft-Gault contains body weight, and the weight passed in
# is whatever the calling model is scaling on: with the fat-free-mass switch
# on, the pharmacokinetic weight (70 kg x FFM/FFM_ref, see
# docs/weight-adjustment.md); with it off, total body weight.  Cockcroft-Gault
# on total weight is known to overpredict creatinine clearance in obesity,
# and lean weight is the usual correction, so this is consistent with the
# switch rather than a separate decision.

# Assumed serum creatinine when the app has none, mg/dL.  Population medians
# for adults with normal kidneys: about 0.9-1.0 in men and 0.7-0.8 in women.
SCR_ASSUMED_MALE   <- 1.0
SCR_ASSUMED_FEMALE <- 0.8

#' Assumed serum creatinine for a patient the app knows only by sex
#'
#' @param sex `"male"` or `"female"`
#' @return serum creatinine in mg/dL
#' @keywords internal
assumedCreatinine <- function(sex)
{
  if (sex == SEX_FEMALE) SCR_ASSUMED_FEMALE else SCR_ASSUMED_MALE
}

#' Cockcroft-Gault creatinine clearance
#'
#' `(140 - age) x weight / (72 x SCr)`, times 0.85 for a woman, in mL/min.
#' This is the estimator the vancomycin (Thomson 2009), cefazolin (Komatsu
#' 2024) and sugammadex (Kleijn 2011) models were fitted with.
#'
#' @param weight the weight the calling model scales on, kg
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @param scr serum creatinine in mg/dL; defaults to the assumed normal value
#' @return creatinine clearance in mL/min
#' @keywords internal
creatinineClearanceCG <- function(weight, age, sex, scr = assumedCreatinine(sex))
{
  crcl <- (140 - age) * weight / (72 * scr)
  if (sex == SEX_FEMALE) crcl <- crcl * 0.85
  crcl
}

#' Du Bois body surface area
#'
#' @param weight weight in kg
#' @param height height in cm
#' @return body surface area in m^2
#' @keywords internal
bsaDuBois <- function(weight, height)
{
  0.007184 * weight^0.425 * height^0.725
}

#' CKD-EPI 2009 estimated glomerular filtration rate
#'
#' The 2009 creatinine equation (Levey et al., Ann Intern Med 2009;150:604)
#' without the race term, which is the version in use when the gentamicin
#' model (Smit 2020) was fitted.  Indexed to 1.73 m^2.
#'
#' @inheritParams creatinineClearanceCG
#' @return eGFR in mL/min/1.73 m^2
#' @keywords internal
egfrCKDEPI2009 <- function(age, sex, scr = assumedCreatinine(sex))
{
  if (sex == SEX_FEMALE) {
    kappa <- 0.7; alpha <- -0.329; sexFactor <- 1.018
  } else {
    kappa <- 0.9; alpha <- -0.411; sexFactor <- 1
  }
  ratio <- scr / kappa
  141 * min(ratio, 1)^alpha * max(ratio, 1)^(-1.209) * 0.993^age * sexFactor
}

#' De-indexed CKD-EPI 2009 eGFR, in mL/min
#'
#' The indexed estimate multiplied by the patient's body surface area over
#' 1.73 m^2, which is the renal input the gentamicin model takes.
#'
#' @inheritParams creatinineClearanceCG
#' @param height height in cm
#' @return eGFR in mL/min for this patient's body surface
#' @keywords internal
egfrDeindexed <- function(weight, height, age, sex, scr = assumedCreatinine(sex))
{
  egfrCKDEPI2009(age, sex, scr) * bsaDuBois(weight, height) / 1.73
}
