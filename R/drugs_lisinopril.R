# -----------------------------------------------------------------------------
# Lisinopril (Zestril, Prinivil, Qbrelis)
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text of Thomson AH, Kelly JG, Whiting B, Br J Clin Pharmacol
# 1989;27:57-65, which he supplied.  Tests:
# tests/testthat/test-drugs-lisinopril.R.
#
# WHAT THE SOURCE IS
# ==================
# A NONMEM analysis of 381 concentrations from 60 hypertensive patients, 40
# of them elderly (over 65) and 20 with renal disease, pooled from two
# multicentre trials.  Steady-state profiles were drawn after at least two
# weeks on a constant daily dose of 2.5 to 40 mg.  Table 3 is transcribed
# into inst/extdata/antihypertensiveParameters.csv with its standard errors.
#
# ONE COMPARTMENT, AND WHY THE TERMINAL SLOPE IS NOT ELIMINATION
# ==============================================================
# The fitted model is one compartment with first-order absorption:
#
#     CL/F = 0.251 x weight x (creatinine/70)^-0.887 x (age/65)^-0.451
#     V/F  = 36.7 L          (no covariate reached significance)
#     ka   = 0.104 /h
#
# At the reference patient CL/F is 17.6 L/h against a V/F of 36.7 L, so the
# elimination rate constant is 0.479 /h and the absorption rate constant is
# 0.104 /h.  ABSORPTION IS THE SLOWER OF THE TWO, by more than fourfold, so
# this drug is FLIP-FLOP: the terminal slope of the plotted curve is the
# absorption rate constant, and the apparent half-life of about 6.7 h is an
# absorption half-life wearing an elimination half-life's clothes.  Anyone
# reading a half-life off this curve is reading the gut, not the kidney.
#
# That is not an artefact of this fit.  Sandra L et al, Br J Clin Pharmacol
# 2023;90:504-515 fitted lisinopril in 13 hypertensive children and obtained
# ka = 0.075 /h, independently and in a different population: the same
# order, from different data, by different authors.  Two studies agreeing
# that absorption is this slow is the reason the value is used as printed
# rather than treated as a transcription error.
#
# THE RENAL TERM, AND ITS UNITS
# =============================
# The creatinine term is a power of the serum creatinine CONCENTRATION, not
# of a creatinine clearance, and the source works in micromoles per litre
# with a reference of 70.  The Patient Profile holds milligrams per decilitre,
# so the field is multiplied by SCR_UMOL_PER_MG_DL before the term is formed; the
# reference of 70 umol/L is 0.79 mg/dL.  The equation was fitted in adults,
# so a child's creatinine reaches it through adultEquivalentCreatinine(),
# which puts it on the adult scale (docs/adding-a-drug.md).  A blank field is
# the assumed normal creatinine for the patient's age and sex.
#
# THE AGE TERM IS A NEGATIVE POWER, SO IT IS FLOORED
# ==================================================
# (age/65)^-0.451 is infinite at age 0, which is a legal entry, so age enters
# it as max(age, ADULT_AGE_TERM_FLOOR) exactly as apixaban's, rivaroxaban's
# and paroxetine's negative age powers do.  The source's youngest patient was
# 21 and its population was elderly and renal, so the floor is a guard rather
# than a modelled paediatric extrapolation: this is an adult model.
#
# WHAT IS DELIBERATELY NOT APPLIED
# ================================
# Cardiac failure multiplied clearance by 0.645 (s.e. 0.112) in the source,
# and it is NOT applied here, because the Patient Profile has no cardiac
# failure field and inventing one from another covariate would be a guess.
# The number is transcribed into the registry so that whoever adds such a
# field has it.  The authors themselves note that every patient with cardiac
# failure was taking a cardiac glycoside, and that the glycoside alone gave a
# smaller likelihood difference than cardiac failure did, which they read as
# the disease rather than the drug being responsible.
#
# EVERY PARAMETER IS APPARENT IN F
# ================================
# CL/F and V/F are apparent: the source fitted oral data only, and the
# introduction puts lisinopril's bioavailability at about 29% from earlier
# work.  So no intravenous unit is offered, bioavailability_PO is 1 by
# construction, and the volume printed on the help page is an apparent one.
#
# NO EFFECT SITE, NO BLOOD PRESSURE, NO BAND
# ==========================================
# The source fitted no pharmacodynamic model.  tPeak, MEAC and the band are
# zero, as they are for every antihypertensive in this library: nothing here
# plots a pressure (docs/antihypertensives.md).
# -----------------------------------------------------------------------------

# Thomson 1989 Table 3.  Equation 5 in the paper prints these rounded to two
# figures; Table 3's values, used here, carry the precision.
LISINOPRIL_CL_PER_KG <- 0.251     # L/h per kg          (s.e. 0.029)
LISINOPRIL_V_F       <- 36.7      # L, no covariate     (s.e. 3.9)
LISINOPRIL_KA_H      <- 0.104     # /h                  (s.e. 0.006)
LISINOPRIL_CR_EXP    <- -0.887    # on creatinine/70    (s.e. 0.108)
LISINOPRIL_AGE_EXP   <- -0.451    # on age/65           (s.e. 0.172)
LISINOPRIL_CHF       <- 0.645     # cardiac failure; NOT applied (s.e. 0.112)
LISINOPRIL_CR_REF    <- 70        # umol/L
LISINOPRIL_AGE_REF   <- 65        # years

#' Lisinopril pharmacokinetics
#'
#' One compartment with first-order absorption, from Thomson 1989.  Every
#' parameter is apparent in bioavailability.  Absorption is slower than
#' elimination, so the curve's terminal slope is the absorption rate
#' constant; see the file header.
#'
#' @param weight Weight in kg.
#' @param height Height in cm.
#' @param age Age in years.
#' @param sex `"male"` or `"female"`.
#' @param adjustToFFM Scale to fat-free mass (default) or reproduce the
#'   published scaling exactly.
#' @param creatinine Serum creatinine in mg/dL, or `NULL` for the assumed
#'   normal value for the patient's age and sex.
#' @return A drug PK list for `getDrugPK()`.
#' @export
lisinopril <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # The source's clearance is LINEAR in weight and its volume carries no size
  # term at all (Table 2 (ii): neither weight nor body surface area improved
  # the volume).  Those are the legacy factors, so the switch off reproduces
  # the published equation for every patient.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1,
                        legacyClearance = weight / FFM_REFERENCE_WEIGHT)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Serum creatinine, on the adult scale the equation was fitted on, in the
  # micromoles per litre the source works in.
  scr   <- adultEquivalentCreatinine(creatinine, age, sex) * SCR_UMOL_PER_MG_DL
  crTerm <- (scr / LISINOPRIL_CR_REF)^LISINOPRIL_CR_EXP

  # A negative power of age, so the age it sees is floored (see the header).
  ageTerm <- (max(age, ADULT_AGE_TERM_FLOOR) /
                LISINOPRIL_AGE_REF)^LISINOPRIL_AGE_EXP

  # CL/F = 0.251 x weight x creatinine term x age term, L/h -> L/min.
  clF <- LISINOPRIL_CL_PER_KG * pkW * crTerm * ageTerm

  default <- list(
    v1  = LISINOPRIL_V_F * size$volume,
    v2  = 1,                          # one compartment
    v3  = 1,
    cl1 = clF / 60,                   # L/h -> L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = LISINOPRIL_KA_H / 60,     # /h -> /min
    # Apparent parameters already carry F: the dose must not be multiplied
    # by it a second time (docs/antihypertensives.md).
    bioavailability_PO = 1,
    tlag_PO = 0                       # none was fitted
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration constant published; plasma only
  MEAC  <- 0

  # No established therapeutic plasma range for lisinopril.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Thomson AH, Kelly JG, Whiting B. Lisinopril population pharmacokinetics ",
    "in elderly and renal disease patients with hypertension. ",
    "Br J Clin Pharmacol 1989;27:57-65. One compartment, apparent in ",
    "bioavailability, from 381 concentrations in 60 hypertensive patients: ",
    "CL/F = 0.251 x weight x (creatinine/70)^-0.887 x (age/65)^-0.451 with ",
    "creatinine in umol/L, V/F 36.7 L, ka 0.104 /h. ABSORPTION IS SLOWER ",
    "THAN ELIMINATION, so the curve's terminal slope is the absorption rate ",
    "constant and its apparent half-life is an absorption half-life. The ",
    "source's cardiac-failure factor of 0.645 is not applied: the Patient ",
    "Profile has no such field. A blank serum creatinine means the assumed ",
    "normal value for the patient's age and sex. ",
    "https://doi.org/10.1111/j.1365-2125.1989.tb05335.x"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
