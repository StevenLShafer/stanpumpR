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
# The phase 3 (SUSTAIN 1, 2, 3, 6 and SUSTAIN-Japan; ESM Table S1) analysis in
# people with type 2 diabetes, 0.5 and 1.0 mg once weekly.  One compartment,
# first-order absorption; the full model, ESM Table S3, read from the
# supplement:
#
#     ka   = 0.0286 /h, FIXED from richly sampled clinical pharmacology data
#     CL/F = 0.0478 L/h   for the reference subject
#     V/F  = 12.2 L       (no covariate on V/F)
#
#     CL/F = CLtyp x (weight / 85 kg)^0.774 x 1.04 (men)
#            x 0.988 (age 65-74) or 0.961 (age 75 and over) x ...
#
# The equation is the paper's (Methods): E_weight = (weight / 85 kg)^theta,
# categorical effects theta^indicator.  The reference subject is a white
# non-Hispanic woman under 65, 85 kg, with normal renal function, injected in
# the abdomen with 1.0 mg once weekly.
#
# NOT APPLIED: race and ethnicity (no field), the injection site (the curve
# is the abdomen's; thigh 1.04, upper arm 1.08), the 0.5 mg dose (1.00), and
# renal impairment (mild 0.948, moderate 0.955, severe 0.920), whose eGFR
# categories the paper does not say how it estimated.  All are within 8%.
#
# BODY SIZE: the model has its own weight covariate on CL/F, evaluated at
# the pharmacokinetic weight (size$pkWeight) with the switch on and at total
# body weight with it off.  V/F, which carries no covariate, belongs to the
# typical patient of the phase 3 trials, and with the switch on it is
# anchored at the cohort (SEMAGLUTIDE_COHORT, below; Steven L. Shafer,
# 2026-11-19, after Carbon's second repository audit, A2-F4): V/F x the
# patient's fat-free-mass ratio to the cohort's typical patient.  Scaled from
# the 70 kg reference man, as it was, the cohort's own typical patient
# received a V/F 0.4% above the published 12.2 L (factor 1.004).  A model
# with a size covariate on one parameter is not thereby exempt on the
# others.  The 85 kg reference subject is CL/F's, not V/F's.  With the switch
# off V/F is 12.2 L (legacyVolume = 1).
#
# WHY NOT THE TWO-COMPARTMENT FIT
# ===============================
# Overgaard et al. (Diabetes Ther 2019;10:649-662) fitted intravenous and
# subcutaneous data together: two compartments, F 0.847, CL 0.0348 L/h
# (x 1.12 in type 2 diabetes), Vc 3.59 L, Q 0.304 L/h, Vp 4.10 L at 85 kg,
# and a ka for each study strength of the product (0.0253/h at 1.34 mg/mL,
# x 0.544 in diabetes).  Its reference is the healthy subject: the oral
# paper built on it (Overgaard 2021) reports typical values that follow from
# applying the diabetes factor once.  It is not added as a second
# subcutaneous semaglutide because its absorption is fitted per study
# formulation; it stays in the registry as the disposition model of oral
# semaglutide.  The oral tablets (Rybelsus) are a different product with
# their own bioavailability: semaglutideOral (R/drugs_semaglutideOral.R).
#
# CHECKS (reference subject, switch off)
# ======================================
# Steady-state average concentration F x dose / (CL x 168 h): 1.0 mg weekly
# 124.5 ng/mL = 30.3 nmol/L (paper 29.8 nmol/L, 95% CI 29.4-30.2); 0.5 mg
# 15.1 nmol/L (paper 15.8).  Half-life ln 2 x V / CL = 177 h (about a week).
# The weight effect gives the paper's exposures: 40% higher at 55 kg (paper
# 40%), 27% lower at 127 kg (paper 27%).
#
# References
# ----------
# Carlsson Petri KC et al., Diabetes Ther 2018;9:1533-1547.
#   https://doi.org/10.1007/s13300-018-0458-5
# Overgaard RV et al., Diabetes Ther 2019;10:649-662.
#   https://doi.org/10.1007/s13300-019-0581-y
# -----------------------------------------------------------------------------

SEMAGLUTIDE_WEIGHT_EXPONENT <- 0.774
SEMAGLUTIDE_CL_MALE <- 1.04
SEMAGLUTIDE_CL_AGE_65_74 <- 0.988
SEMAGLUTIDE_CL_AGE_75 <- 0.961

# Carlsson Petri 2018, ESM Table S1 (the supplement's docx), by trial in the
# table's order (SUSTAIN 1, 2, 3, 6 and SUSTAIN-Japan; 1612 in all): mean
# weight, BMI and age and the numbers of men and women of each.  No height is
# printed; each trial's comes from its weight and BMI, the same for both
# sexes, the table giving no means by sex.
# (The article's Table 2, p. 1539, pools 1612 subjects, 927 men and 685 women,
# 86.2 kg, BMI 31.1, and prints no age.)  V/F is anchored at this cohort
# (Steven L. Shafer, 2026-11-19, after Carbon's second repository audit,
# A2-F4; see the header).
SEMAGLUTIDE_COHORT <- list(groups = list(
  list(weight = 93.8, bmi = 33.4, age = 53.2, men = 128, women = 109),
  list(weight = 89.4, bmi = 32.3, age = 55.8, men = 204, women = 213),
  list(weight = 96.2, bmi = 34.1, age = 56.3, men = 193, women = 173),
  list(weight = 88.3, bmi = 32.1, age = 65.4, men = 66, women = 52),
  list(weight = 71.3, bmi = 26.3, age = 58.4, men = 336, women = 138)))

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
  # V/F, with no covariate, is anchored at the phase 3 cohort
  # (SEMAGLUTIDE_COHORT); CL/F carries its own weight term, read at pkWeight,
  # which does not depend on the anchor, and takes no size factor.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1,
                        cohort = do.call(cohortSizeFactors, SEMAGLUTIDE_COHORT))
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  sexFactor <- if (sex == SEX_MALE) SEMAGLUTIDE_CL_MALE else 1
  ageFactor <- if (age >= 75) SEMAGLUTIDE_CL_AGE_75 else
    if (age >= 65) SEMAGLUTIDE_CL_AGE_65_74 else 1

  default <- list(
    v1  = 12.2 * size$volume, v2 = 1, v3 = 1,
    cl1 = 0.0478 * (pkW / 85)^SEMAGLUTIDE_WEIGHT_EXPONENT * sexFactor * ageFactor / 60,
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
      "ka 0.0286/h fixed, CL/F 0.0478 L/h x (weight/85)^0.774 x 1.04 (men) x ",
      "0.988 (65-74 y) or 0.961 (75 y and over), V/F 12.2 L; concentration only. ",
      "https://doi.org/10.1007/s13300-018-0458-5"
    )
  )
}
