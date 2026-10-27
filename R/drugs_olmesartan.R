# -----------------------------------------------------------------------------
# Olmesartan (Benicar)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text of Yoshihara K, Gao Y, Shiga H, Wada DR, Hisaoka M,
# Clin Pharmacokinet 2005;44:1329-1342, which he supplied.  Tests:
# tests/testthat/test-drugs-olmesartan.R.
#
# WHAT THE SOURCE IS
# ==================
# A NONMEM analysis of 7911 plasma concentrations from 472 people in 12
# trials: 89 healthy volunteers and 383 hypertensive patients (337 with
# hypertension only, 34 with renal and 12 with hepatic impairment), 355
# Westerners (338 Caucasian, 17 Hispanic) and 117 Japanese, single and
# multiple oral doses of olmesartan medoxomil 2.5 to 80 mg.  Table II counts
# 592 individuals (209 healthy, 475 Western) because the 30 Western men of a
# five-way crossover (trial 5) were "treated as separate individuals" in each
# period (its footnote b): 30 x 4 = 120 more.  (Until 2026-10-27 this header
# said 5119 concentrations from 629 participants; an adversarial review
# caught it, and the counts were settled from the full text.)  Two
# compartments with
# first-order absorption and an absorption lag (ADVAN4/TRANS1), parameterised
# in CL/F, ka, ke, k12, k21 and ALAG.  Table IV is transcribed into
# inst/extdata/antihypertensiveParameters.csv with its standard errors.
#
# THE DOSE IS THE PRODRUG, AND THE PAPER SAYS SO
# ==============================================
# Olmesartan medoxomil is a prodrug ester, hydrolysed completely during
# absorption; the plotted analyte is olmesartan, 20% lighter.  A model fitted
# to olmesartan concentrations could have been given either basis, and getting
# it wrong is a flat 20% error in every curve.  This paper settles it:
# "the observed pharmacokinetic data normalised to a dose of olmesartan
# medoxomil 20mg" (Population Pharmacokinetic Model Evaluation), and the dose
# range it reports throughout is of the medoxomil.  So the apparent parameters
# already carry the molecular-weight ratio along with the bioavailability, and
# A DOSE IS ENTERED AS PRESCRIBED -- as medoxomil, unconverted.  There is no
# `molar` block here and there must not be one.
#
# WHY THE COVARIATES SCALE THE CURVE WITHOUT CHANGING ITS SHAPE
# =============================================================
# The parameterisation is the unusual thing about this model and it has to be
# reproduced, not improved.  The source fits CL/F together with the four rate
# constants ke, k12, k21 and ka, so the central volume is not a parameter at
# all -- it is
#
#     V1/F = (CL/F) / ke
#
# and every covariate sits on CL/F.  A patient with half the clearance
# therefore also has half the central volume and the same ke, so the three
# eigenvalues of the disposition are the same for everybody and the entire
# covariate model acts as a single multiplier on concentration:
#
#     C(t) = (dose / V1) x f(ke, k12, k21, ka, ALAG, t),   V1 proportional to CL
#
# Nothing about the shape -- not the time to peak, not either half-life --
# moves with age, weight, sex, renal function or patient status.  Only the
# height of the curve does, inversely with CL/F.  That is a consequence of the
# published parameterisation and it is why the derived volumes below are
# written as functions of clF rather than pinned at the typical patient.
#
# THE COVARIATE MODEL (EQUATION 5, TABLE IV)
# ==========================================
#     CL/F = 7.32 x (age/50)^-0.117 x (BWt/70)^0.310 x (SCr/80)^-0.344
#                 x exp(0.145 x sex) x exp(-0.239 x pv) x exp(-0.030 x race)
#
# with age in years, bodyweight in kg, SCr in micromoles per litre, sex 1 for
# male and 0 for female, pv (patient status) 1 for a hypertensive patient and
# 0 for a healthy volunteer, race 1 for Japanese and 0 for Westerner.
#
# PATIENT STATUS IS APPLIED, AS HYPERTENSIVE
# ==========================================
# pv = 1.  There is no patient-status field, and there does not need to be
# one: a patient receiving an angiotensin receptor blocker is a hypertensive
# patient, and the source's own headline typical values are the hypertensive
# ones (6.66 L/h male, 5.76 L/h female).  This is the same choice
# ertugliflozin makes with its type 2 diabetes term.  It matters: CL/F is
# 21.3% lower in hypertensive patients, so taking the healthy reference
# would draw every curve 21% too low.
#
# RACE IS NOT APPLIED, AND IT IS THE SMALLEST TERM IN THE MODEL
# ============================================================
# race = 0 (Westerner), the source's reference.  There is no race field in the
# Patient Profile and this library will not invent one.  The cost is bounded
# and tiny: the term was NOT statistically significant (p > 0.05, %CV 131, a
# bootstrap interval spanning zero), and Japanese participants cleared
# olmesartan 3.0% more slowly (exp(-0.030) = 0.970).  The authors kept it only because the paper's purpose
# was to look for a race effect and report that there is none -- which is the
# finding, not the coefficient.
#
# THE AGE TERM IS A NEGATIVE POWER, SO IT IS FLOORED
# ==================================================
# (age/50)^-0.117 is infinite at age 0, a legal entry, so age enters it as
# max(age, ADULT_AGE_TERM_FLOOR) exactly as apixaban's, rivaroxaban's,
# paroxetine's and lisinopril's negative age powers do (R/constants.R).  This
# is an adult model: its participants were 18 to 87.  The term is also very
# weak -- a 75-year-old clears 4.6% less than a 50-year-old -- so the floor is
# a guard against a division by zero rather than a paediatric extrapolation.
#
# THE RENAL TERM, AND ITS UNITS
# =============================
# A power of the serum creatinine CONCENTRATION, not of a clearance, centred
# at 80 micromoles per litre (about 0.9 mg/dL).  The Patient Profile holds
# milligrams per decilitre, so the field is multiplied by SCR_UMOL_PER_MG_DL
# before the term is formed.  The equation was fitted in adults, so a child's
# creatinine reaches it through adultEquivalentCreatinine(), which puts it on
# the adult scale (docs/adding-a-drug.md).  A blank field is the assumed normal
# creatinine for the patient's age and sex.  This is the strongest covariate in
# the model: the source reports clearance 33.8% to 44.8% lower at a creatinine
# of 265 to 450 micromoles per litre.
#
# EVERY PARAMETER IS APPARENT IN F
# ================================
# The analysis had no intravenous arm, so CL/F and the volumes derived from it
# are apparent.  bioavailability_PO is 1 by construction -- the dose must not
# be multiplied by a bioavailability a second time -- and no intravenous unit
# is offered.
#
# NO EFFECT SITE, NO BLOOD PRESSURE, NO BAND
# ==========================================
# The source fitted no pharmacodynamic model.  tPeak, MEAC and the band are
# zero, as they are for every antihypertensive in this library: nothing here
# plots a pressure (docs/antihypertensives.md).
# -----------------------------------------------------------------------------

# Yoshihara 2005 Table IV, fixed effects, with standard errors.
OLMESARTAN_CL_F     <- 7.320    # L/h            (s.e. 0.409)
OLMESARTAN_KE       <- 0.193    # /h             (s.e. 0.004)
OLMESARTAN_K12      <- 0.061    # /h             (s.e. 0.002)
OLMESARTAN_K21      <- 0.079    # /h             (s.e. 0.002)
OLMESARTAN_KA       <- 1.460    # /h             (s.e. 0.092)
OLMESARTAN_ALAG     <- 0.427    # h              (s.e. 0.010)

# Yoshihara 2005 Table IV, effects of covariates on CL/F.
OLMESARTAN_K_AGE    <- -0.117   # on age/50      (s.e. 0.061)
OLMESARTAN_K_WT     <-  0.310   # on weight/70   (s.e. 0.102)
OLMESARTAN_K_SCR    <- -0.344   # on SCr/80      (s.e. 0.050)
OLMESARTAN_K_SEX    <-  0.145   # male = 1       (s.e. 0.038)
OLMESARTAN_K_PV     <- -0.239   # hypertensive=1 (s.e. 0.055); APPLIED as 1
OLMESARTAN_K_RACE   <- -0.030   # Japanese = 1   (s.e. 0.040); NOT applied

# The medians the continuous covariates are centred on (Equation 3).
OLMESARTAN_AGE_REF  <- 50       # years
OLMESARTAN_WT_REF   <- 70       # kg
OLMESARTAN_SCR_REF  <- 80       # umol/L

#' Olmesartan pharmacokinetics
#'
#' Two compartments with first-order absorption and a lag, from Yoshihara
#' 2005.  Every parameter is apparent in bioavailability, and a dose is
#' entered as the medoxomil prodrug exactly as prescribed; see the file
#' header.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or `NULL` for the assumed
#'   normal value for the patient's age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
olmesartan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  # The source's own size term is a power of total bodyweight on CL/F, and
  # since V1/F is CL/F over ke every volume carries the same term.  So the
  # whole size model is that one covariate, evaluated at total bodyweight with
  # the switch off -- which reproduces the published equation exactly -- and at
  # the pharmacokinetic weight with it on (docs/weight-adjustment.md).  The
  # library's own factors are therefore not applied on top of it, which would
  # scale size twice; legacyVolume and legacyClearance are 1 to say so.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # A negative power of age, so the age it sees is floored (see the header).
  ageTerm <- (max(age, ADULT_AGE_TERM_FLOOR) /
                OLMESARTAN_AGE_REF)^OLMESARTAN_K_AGE
  wtTerm  <- (pkW / OLMESARTAN_WT_REF)^OLMESARTAN_K_WT

  # Serum creatinine on the adult scale the equation was fitted on, in the
  # micromoles per litre the source works in.
  scr     <- adultEquivalentCreatinine(creatinine, age, sex) * SCR_UMOL_PER_MG_DL
  scrTerm <- (scr / OLMESARTAN_SCR_REF)^OLMESARTAN_K_SCR

  sexTerm <- exp(OLMESARTAN_K_SEX * (sex == SEX_MALE))
  # Patient status is applied as hypertensive, race as Westerner (header).
  pvTerm  <- exp(OLMESARTAN_K_PV * 1)

  clF <- OLMESARTAN_CL_F * ageTerm * wtTerm * scrTerm * sexTerm * pvTerm

  # The source's parameterisation, reproduced: V1 is not fitted, it is CL/ke,
  # and the peripheral compartment follows from k12 and k21.  All in L/h and
  # litres here; converted to the library's L/min below.
  v1F <- clF / OLMESARTAN_KE
  qF  <- OLMESARTAN_K12 * v1F
  v2F <- qF / OLMESARTAN_K21

  default <- list(
    v1  = v1F,
    v2  = v2F,
    v3  = 1,                            # two compartments
    cl1 = clF / 60,                     # L/h -> L/min
    cl2 = qF / 60,
    cl3 = 0,
    ka_PO = OLMESARTAN_KA / 60,         # /h -> /min
    # Apparent parameters already carry F (and the prodrug's molecular
    # weight): the dose must not be multiplied by either a second time.
    bioavailability_PO = 1,
    tlag_PO = OLMESARTAN_ALAG * 60      # h -> min
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration constant published; plasma only
  MEAC  <- 0

  # No established therapeutic plasma range for olmesartan.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Yoshihara K, Gao Y, Shiga H, Wada DR, Hisaoka M. Population ",
    "pharmacokinetics of olmesartan following oral administration of its ",
    "prodrug, olmesartan medoxomil: in healthy volunteers and hypertensive ",
    "patients. Clin Pharmacokinet 2005;44:1329-42. Two compartments with ",
    "first-order absorption and a 0.427 h lag, apparent in bioavailability, ",
    "from 7911 concentrations in 472 people in 12 trials: ",
    "CL/F = 7.32 x (age/50)^-0.117 x (weight/70)^0.310 x (creatinine/80)",
    "^-0.344 x exp(0.145 if male) x exp(-0.239 if hypertensive) with ",
    "creatinine in umol/L, ke 0.193 /h, k12 0.061 /h, k21 0.079 /h, ",
    "ka 1.46 /h. V1/F is CL/F divided by ke, as the source parameterised it, ",
    "so the covariates scale the whole curve and never change its shape. ",
    "A dose is entered as olmesartan medoxomil, as prescribed: the source ",
    "normalised its data to a 20 mg medoxomil dose, so the apparent ",
    "parameters already carry the prodrug's molecular weight. The patient ",
    "status term is applied as hypertensive; the race term (3.0% lower in ",
    "Japanese, not statistically significant) is not applied, there being no ",
    "such field. A blank serum creatinine means the assumed normal value for ",
    "the patient's age and sex. ",
    "https://doi.org/10.2165/00003088-200544120-00011"
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
