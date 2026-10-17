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
# COVARIATES: Table S1 of the supplement (supplied by Steven L. Shafer) and
# its equations.  The reference patient is a 50-year-old non-smoking,
# non-Asian man, BMI 25 kg/m^2, eGFR 100 mL/min/1.73 m^2 (MDRD), total
# protein 70 g/L, ALT and AST 20, AP 70 and LDH 160 U/L:
#
#     CL/F = 10.6 x (age/50)^-0.241 x (BMI/25)^0.320 x (eGFR/100)^0.333
#            x 0.886 (female) x [smoking, Asian race, total protein, liver
#            enzymes]
#     Vc/F = 3.14 x (age/50)^0.795 x (BMI/25)^1.04 x 1.25 (female) x [...]
#     Vp/F = 70.6 x (age/50)^0.135 x (BMI/25)^0.672 x 0.831 (female) x [...]
#     ka   = 0.196 x (age/50)^0.108 x 1.17 (female) x 1.23 (Asian)
#     Q/F  = 6.34 L/h, no covariate; lag 0.5 h, fixed
#
# The eGFR exponent, 0.333, is the one this model had derived from the
# paper's AUCs at eGFR 60, 30 and 15 before the table was in hand.  Table S1
# writes Vc/F's BMI term as (BMI/20)^1.04 while the equation below it writes
# (BMI/25); the equation, which the other BMI terms share, is followed (Vc/F
# is poorly identified in any case: its interval runs almost to zero).
# Applied: age, sex, BMI and eGFR, the covariates the Patient Profile
# carries.  At their reference values, with no field: smoking, race, total
# protein, ALT, AST, alkaline phosphatase and LDH (each changes exposure by
# less than 25%, the paper's Figure 1).  Age is held at 18 and above (an
# adult model) and BMI within 20.8 to 42.4 kg/m^2, the widest of the 95%
# intervals the paper reports for its populations (Table 2).
#
# RENAL FUNCTION: eGFR is the four-variable MDRD (egfrMDRD(),
# R/renalFunction.R) at the patient's creatinine (a child's on the adult
# scale) or an assumed normal creatinine for age and sex.
#
# BODY SIZE: the model's own BMI terms, with BMI from the pharmacokinetic
# weight with the switch on and from total weight with it off; no other size
# scaling.
#
# References
# ----------
# Baron KT et al., Diabetes Ther 2016;7:455-471.
#   https://doi.org/10.1007/s13300-016-0174-y
# -----------------------------------------------------------------------------

# The exponent of (eGFR / 100) on CL/F, Table S1
EMPAGLIFLOZIN_EGFR_EXPONENT <- 0.333
EMPAGLIFLOZIN_BMI_RANGE <- c(20.8, 42.4)   # kg/m^2; see the header

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
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  bmi  <- pkW / (height / 100)^2
  bmi  <- min(max(bmi, EMPAGLIFLOZIN_BMI_RANGE[1]), EMPAGLIFLOZIN_BMI_RANGE[2])
  a    <- max(age, 18) / 50
  b    <- bmi / 25
  fem  <- sex == SEX_FEMALE
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  renal <- (egfr / 100)^EMPAGLIFLOZIN_EGFR_EXPONENT

  default <- list(
    v1  = 3.14 * a^0.795 * b^1.04 * (if (fem) 1.25 else 1),
    v2  = 70.6 * a^0.135 * b^0.672 * (if (fem) 0.831 else 1),
    v3  = 1,
    cl1 = 10.6 * a^-0.241 * b^0.320 * renal * (if (fem) 0.886 else 1) / 60,
    cl2 = 6.34 / 60, cl3 = 0,
    ka_PO = 0.196 * a^0.108 * (if (fem) 1.17 else 1) / 60,
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
      "L/h x (age/50)^-0.241 x (BMI/25)^0.320 x (MDRD eGFR/100)^0.333 from the ",
      "entered or an assumed normal creatinine x 0.886 in women, Vc/F 3.14 L, ",
      "Q/F 6.34 L/h, Vp/F 70.6 L, ka 0.196/h, lag 0.5 h, with Table S1's age, sex ",
      "and BMI terms on the volumes and ka; smoking, race, protein and liver ",
      "enzymes at reference; concentration only. ",
      "https://doi.org/10.1007/s13300-016-0174-y"
    )
  )
}
