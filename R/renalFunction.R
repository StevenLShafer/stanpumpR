# Renal function for the drug models that need it.
#
# Several models carry a renal covariate (Cockcroft-Gault creatinine
# clearance, or a de-indexed CKD-EPI eGFR): mannitol, vancomycin, gentamicin,
# cefazolin, sugammadex, gabapentin and pregabalin.  Each takes an optional
# `creatinine` argument, the patient's serum creatinine from the Patient
# Profile.
#
# When none is entered the models ASSUME A NORMAL SERUM CREATININE for the
# patient's age and sex and let age, sex and body size carry the rest.  That
# captures the fall in renal function with age and the sex difference, which
# is most of the variation among people with normal kidneys, but not renal
# impairment: a renally cleared drug in a patient with impaired kidneys is
# then overpredicted to clear and underpredicted to accumulate.  Entering
# the creatinine is what represents impairment.
#
# Children.  A child's normal creatinine is far below an adult's, because it
# follows muscle mass: about 0.25 mg/dL through most of infancy, 0.35 at five
# years, 0.5 at ten, and 0.8 in boys and 0.7 in girls at seventeen
# (normalCreatinineChild()).  Under 18 that is the assumed value; at 18 it
# steps up to the adult value below, which sits above the adolescents' medians
# (0.82 and 0.68 just under 18).  But Cockcroft-Gault and CKD-EPI were
# developed in adults, and given a child's own creatinine they overestimate
# renal function badly: a five-year-old boy of 20 kg and 110 cm at his normal
# 0.35 mg/dL has a Cockcroft-Gault clearance of 106 mL/min, against a normal
# GFR of about 48 mL/min for his size (107.3 mL/min/1.73 m^2, what the
# full-age-spectrum equations give at a normal creatinine, times his 0.78
# m^2).  So the models fitted with those equations see a child's creatinine,
# entered or assumed, on the adult scale (adultEquivalentCreatinine()):
# divided by the normal value for the child's age and multiplied by the adult
# assumed value, after the age-adjusted creatinine of Bjork et al. (Kidney Int
# 2021;99:940-947).  A child at the normal creatinine for age then gets the
# adult assumed value (Cockcroft-Gault 38 mL/min for the boy above), and a
# child at twice the normal for age gets half that clearance; the CKD-EPI eGFR
# (gentamicin) falls to about 0.43 of its value instead, 2^-1.209, since both
# creatinines are above its kappa.  At a blank field the step at 18 cancels
# for these models (both sides get the adult value), but an entered
# creatinine reads 18-22% higher on the adult scale just under 18 than just
# over it.  Only the pregabalin model, whose source estimated children's renal
# function from their own creatinine (Schwartz under 13, Cockcroft-Gault from
# 13), takes the unscaled value.
#
# Body size.  Cockcroft-Gault contains body weight, and the weight passed in
# is whatever the calling model is scaling on: with the fat-free-mass switch
# on, the pharmacokinetic weight (70 kg x FFM/FFM_ref, see
# docs/weight-adjustment.md); with it off, total body weight.  Cockcroft-Gault
# on total weight is known to overpredict creatinine clearance in obesity,
# and lean weight is the usual correction, so this is consistent with the
# switch rather than a separate decision.

# Assumed serum creatinine of an adult when the app has none, mg/dL.
# Population medians for adults with normal kidneys: about 0.9-1.0 in men and
# 0.7-0.8 in women.
SCR_ASSUMED_MALE   <- 1.0
SCR_ASSUMED_FEMALE <- 0.8

# Under this age the assumed creatinine is the normal value for the child's
# age, and the adult renal equations see it on the adult scale.
SCR_ADULT_AGE <- 18

# micromol/L of creatinine in 1 mg/dL
SCR_UMOL_PER_MG_DL <- 88.4

#' Assumed serum creatinine of an adult with normal kidneys
#'
#' @param sex `"male"` or `"female"`
#' @return serum creatinine in mg/dL
#' @keywords internal
adultCreatinine <- function(sex)
{
  if (sex == SEX_FEMALE) SCR_ASSUMED_FEMALE else SCR_ASSUMED_MALE
}

#' Normal serum creatinine of a child
#'
#' The median serum creatinine of healthy children of the patient's age and
#' sex, by enzymatic (IDMS-traceable) assay:
#' * 2 years and over: the Q values of the European Kidney Function
#'   Consortium equation (Pottel et al., Ann Intern Med 2021;174:183-191), in
#'   micromol/L, ln Q = 3.200 + 0.259 age - 0.543 ln(age) - 0.00763 age^2 +
#'   0.0000790 age^3 for boys and 3.080 + 0.177 age - 0.223 ln(age) -
#'   0.00596 age^2 + 0.0000686 age^3 for girls.
#' * Under 1 year: Boer et al. (Pediatr Nephrol 2010;25:2107-2113), term
#'   infants, no sex difference: a plateau of 20 micromol/L from day 65 to
#'   day 216 of life, with log10 creatinine falling by 0.07 per doubling of
#'   age before it (the maternal creatinine clearing) and rising by 0.045 per
#'   doubling after it.  This reproduces the paper's 55 micromol/L on day 1
#'   to about 4%, and its 22 in the second month.  Under one day takes the day-1
#'   value.
#' * 1 to 2 years, which neither source covers: interpolated, linear in
#'   ln(creatinine), from Boer at 1 year (21.6 micromol/L) to the EKFC value
#'   at 2 years (27.4 for boys, 25.9 for girls).
#'
#' @param age age in years, under 18
#' @param sex `"male"` or `"female"`
#' @return serum creatinine in mg/dL
#' @keywords internal
normalCreatinineChild <- function(age, sex)
{
  ekfc <- function(age) {
    if (sex == SEX_FEMALE)
      exp(3.080 + 0.177 * age - 0.223 * log(age) - 0.00596 * age^2 + 0.0000686 * age^3)
    else
      exp(3.200 + 0.259 * age - 0.543 * log(age) - 0.00763 * age^2 + 0.0000790 * age^3)
  }
  boer <- function(age) {
    day <- max(age * 365.25, 1)
    if (day < 65)        20 * 10^(-0.07 * log2(day / 65))
    else if (day <= 216) 20
    else                 20 * 10^(0.045 * log2(day / 216))
  }
  q <- if (age < 1) {
    boer(age)
  } else if (age < 2) {
    exp((2 - age) * log(boer(1)) + (age - 1) * log(ekfc(2)))
  } else {
    ekfc(age)
  }
  q / SCR_UMOL_PER_MG_DL
}

#' Assumed serum creatinine for a patient the app knows only by age and sex
#'
#' The normal value for a child's age (`normalCreatinineChild()`) under 18,
#' the adult value (`adultCreatinine()`) from 18.
#'
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @return serum creatinine in mg/dL
#' @keywords internal
assumedCreatinine <- function(age, sex)
{
  if (age < SCR_ADULT_AGE) normalCreatinineChild(age, sex)
  else adultCreatinine(sex)
}

#' The serum creatinine a renal model should use
#'
#' The patient's own if it was entered, otherwise the assumed normal value for
#' the patient's age and sex.
#'
#' @param creatinine the entered serum creatinine in mg/dL, or NULL / NA when
#'   none was entered
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @return serum creatinine in mg/dL
#' @keywords internal
patientCreatinine <- function(creatinine, age, sex)
{
  if (is.null(creatinine) || length(creatinine) != 1 || is.na(creatinine))
    assumedCreatinine(age, sex)
  else
    creatinine
}

#' The serum creatinine an adult renal equation should see
#'
#' `patientCreatinine()`, and for a child that value on the adult scale: over
#' the normal creatinine for the child's age, times the adult's, after the
#' age-adjusted creatinine of Bjork et al. (Kidney Int 2021;99:940-947), which
#' makes the adult equations usable in children; see the file's header.  A
#' blank creatinine is therefore the adult value at any age.
#'
#' @inheritParams patientCreatinine
#' @return serum creatinine in mg/dL, on the adult scale
#' @keywords internal
adultEquivalentCreatinine <- function(creatinine, age, sex)
{
  scr <- patientCreatinine(creatinine, age, sex)
  if (age >= SCR_ADULT_AGE) return(scr)
  scr * adultCreatinine(sex) / normalCreatinineChild(age, sex)
}

#' Cockcroft-Gault creatinine clearance
#'
#' `(140 - age) x weight / (72 x SCr)`, times 0.85 for a woman, in mL/min.
#' This is the estimator the vancomycin (Thomson 2009), cefazolin (Komatsu
#' 2024), sugammadex (Kleijn 2011), gabapentin (Tran 2017) and pregabalin
#' (Chan 2021, from 13 years of age; Schwartz below) models were fitted with.
#'
#' @param weight the weight the calling model scales on, kg
#' @param age age in years
#' @param sex `"male"` or `"female"`
#' @param scr serum creatinine in mg/dL.  The models fitted in adults pass a
#'   child's on the adult scale (`adultEquivalentCreatinine()`).  Defaults to
#'   the assumed adult value, which is what a blank creatinine is on that
#'   scale at any age.
#' @return creatinine clearance in mL/min
#' @keywords internal
creatinineClearanceCG <- function(weight, age, sex, scr = adultCreatinine(sex))
{
  crcl <- (140 - age) * weight / (72 * scr)
  if (sex == SEX_FEMALE) crcl <- crcl * 0.85
  crcl
}

#' Schwartz estimated glomerular filtration rate in children
#'
#' `k x height / SCr`, in mL/min/1.73 m^2, with k 0.55 from one year of age
#' and 0.45 below it.  This is the estimator the pregabalin model (Chan 2021)
#' was fitted with for patients under 13 years; at 13 and over it used
#' Cockcroft-Gault.
#'
#' @param height height in cm
#' @param age age in years
#' @param scr serum creatinine in mg/dL
#' @return eGFR in mL/min/1.73 m^2
#' @keywords internal
egfrSchwartz <- function(height, age, scr)
{
  k <- if (age < 1) 0.45 else 0.55
  k * height / scr
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
#' without the race term, taken to be the version the gentamicin model (Smit
#' 2020) was fitted with: the 2021 equation post-dates its data, and its main
#' text names only "CKD-EPI".  Indexed to 1.73 m^2.
#'
#' @inheritParams creatinineClearanceCG
#' @return eGFR in mL/min/1.73 m^2
#' @keywords internal
egfrCKDEPI2009 <- function(age, sex, scr = adultCreatinine(sex))
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
egfrDeindexed <- function(weight, height, age, sex, scr = adultCreatinine(sex))
{
  egfrCKDEPI2009(age, sex, scr) * bsaDuBois(weight, height) / 1.73
}
