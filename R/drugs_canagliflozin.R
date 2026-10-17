# -----------------------------------------------------------------------------
# Canagliflozin: two compartments, lagged sequential zero- and first-order
# oral absorption (Invokana)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: no glucose, HbA1c or urinary
# glucose excretion is plotted.  Drafted by Claude Code, 2026-10-17, at the
# request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Hoeben E, De Winter W, Neyens M, Devineni D, Vermeulen A, Dunne A.
# Population pharmacokinetic modeling of canagliflozin in healthy volunteers
# and patients with type 2 diabetes mellitus.  Clin Pharmacokinet 2016;55:
# 209-223.  9061 samples from 1616 people in 14 studies (healthy volunteers,
# renal impairment, type 2 diabetes; 25 to 1600 mg).  Two compartments,
# parameterised as rate constants (NONMEM ADVAN4 TRANS1), with a lag, then a
# zero-order release over D1 into the gut, then first-order absorption
# (Fig. 1).  Table 5, the final model; the absorption and distribution were
# fitted to the 245 richly sampled phase I subjects and fixed:
#
#     Vc/F  = 99.3 L (men), 82.6 L (women) x (WT/WT0)^0.583 x (AGE/AGE0)^-0.167
#     ke    = 0.145 /h x (eGFR/eGFR0)^0.261 x (DOSE/DOSE0)^-0.0631
#     ka    = 3.68 /h x (BMI/BMI0)^-1.41
#     Tlag  = 0.147 h x (BMI/BMI0)^1.41     (the marketed, non-encapsulated
#                                            tablet; 0.262 h over-encapsulated)
#     D1    = 0.604 h
#     k23   = 0.101 /h      k32 = 0.0856 /h
#     CL/F  = Vc/F x ke: 14.4 L/h in men and 12.0 L/h in women at the medians
#
# In library terms cl1 = ke Vc, cl2 = k23 Vc and v2 = Vc k23 / k32, so the
# peripheral volume and the clearances follow Vc's covariates, as they do in
# the source's parameterisation.  The zero-order release into the gut runs
# through zeroOrderPO (R/subcutaneous.R), exactly.
#
# THE SIGN OF THE BMI TERM.  Table 5 prints one exponent, 1.41, for "body mass
# index on ka and Tlag".  The text (section 3.2, Table 2) says that people
# with a lower BMI absorb faster (median ka about twice as high below 24.2
# kg/m^2 as above 28.0) and that the lag is about twice as long above 28.0:
# ka falls and Tlag rises with BMI, so the exponent is applied with a minus
# sign to ka and a plus sign to Tlag.
#
# THE REFERENCE VALUES (Eq. 1 divides each covariate by its median, which
# the paper does not print; its supplement, with the demographics, was not
# available):
#
#   * eGFR0 = 87 mL/min/1.73 m^2: the median of the pooled data on which ke's
#     covariates were fitted (section 3.1).  Stated.
#   * AGE0 = 45 years: the median of the phase I data on which Vc/F's were
#     fitted (section 3.1).  Stated.
#   * BMI0 = 26 kg/m^2: ASSUMED.  The phase I tertiles were 24.2 and 28.0
#     (Table 2); 26 is between them.  Within that span ka and Tlag change by
#     at most 11%.
#   * WT0 = 75 kg: ASSUMED, the weight at a BMI of 26 and the phase I
#     population's height (63% men; Japanese, Indian and Western subjects).
#     An error of 5 kg changes Vc/F and CL/F by 4%.
#   * DOSE0: the median total daily dose of the pooled data, not stated.  The
#     dose term is not applied (the model is at its reference dose for every
#     dose); Table 6 puts the exposure at 300 mg a day 7% above that at 100
#     mg (300 / 100)^0.0631 = 1.07).
#
# The supplement (ESM 1, 40262_2015_307_MOESM1_ESM) would replace the
# assumed values.
#
# NOT APPLIED.  UGT1A9*3 carriers (1% of subjects, 26% higher AUC) were not
# kept in the final model; no field exists.  The dose term, above.
#
# RENAL FUNCTION: the MDRD eGFR, as in the paper (egfrMDRD(), without the race
# term), at the patient's creatinine (a child's on the adult scale) or an
# assumed normal creatinine for the patient's age and sex when the field is
# blank.  The data included severe impairment (eGFR 15-29); median
# dose-normalised AUC was 11, 40 and 29% higher in mild, moderate and severe
# impairment (Table 6).
#
# BODY SIZE: the model's own weight covariate on Vc/F (and so on CL/F and the
# distribution), evaluated at the pharmacokinetic weight with the switch on and
# at total weight with it off.  BMI acts on the absorption, not on the size,
# and is the patient's own (total weight / height^2) in both positions.  Age
# is held at 18 or over: every study enrolled adults.
#
# CHECKS
# ======
# At the reference values CL/F is 14.4 L/h in a man and 12.0 L/h in a woman
# (paper: 14.4 and 12.0).  A single 100 mg dose in a man at the reference
# values peaks at 820 ng/mL 1.3 h after the dose (paper: peaks 1-2 h after a
# dose), and the curve matches a numerical integration of Fig. 1's equations
# to 1e-4.  The terminal half-life, 0.693 / beta, is 16.1 h; the label's
# 10.6 and 13.1 h at 100 and 300 mg are non-compartmental, from the first
# days after a dose.  300 mg once daily at steady state, averaged over a
# 60-year-old man and woman of 90 and 78 kg with eGFR 80: AUC over a day
# 23,200 ng.h/mL (paper: 24,941 for its typical patient with type 2
# diabetes).  See tests/testthat/test-drugs-canagliflozin.R.
#
# References
# ----------
# Hoeben E et al., Clin Pharmacokinet 2016;55:209-223.
#   https://doi.org/10.1007/s40262-015-0307-x
# -----------------------------------------------------------------------------

CANAGLIFLOZIN_EGFR_REF   <- 87    # mL/min/1.73 m^2, pooled median (stated)
CANAGLIFLOZIN_AGE_REF    <- 45    # years, phase I median (stated)
CANAGLIFLOZIN_BMI_REF    <- 26    # kg/m^2, assumed; see the header
CANAGLIFLOZIN_WEIGHT_REF <- 75    # kg, assumed; see the header

#' Canagliflozin pharmacokinetics (oral)
#'
#' Two compartments, a lag, then sequential zero- and first-order absorption
#' (Hoeben 2016); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
canagliflozin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL)
{
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  bmi  <- weight / (height / 100)^2
  egfr <- egfrMDRD(age, sex, adultEquivalentCreatinine(creatinine, age, sex))

  vc <- (if (sex == SEX_FEMALE) 82.6 else 99.3) *
    (pkW / CANAGLIFLOZIN_WEIGHT_REF)^0.583 *
    (max(age, 18) / CANAGLIFLOZIN_AGE_REF)^-0.167
  ke <- 0.145 * (egfr / CANAGLIFLOZIN_EGFR_REF)^0.261
  b  <- bmi / CANAGLIFLOZIN_BMI_REF
  k23 <- 0.101
  k32 <- 0.0856

  default <- list(
    v1  = vc, v2 = vc * k23 / k32, v3 = 1,
    cl1 = ke * vc / 60, cl2 = k23 * vc / 60, cl3 = 0,
    ka_PO = 3.68 * b^-1.41 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0.147 * b^1.41 * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # After the lag, each tablet is released into the gut over D1
    zeroOrderPO = list(duration = 0.604 * 60),
    reference = paste0(
      "Hoeben 2016 (1616 healthy volunteers and patients with type 2 diabetes): two ",
      "compartments, Vc/F 99.3 L (men) or 82.6 L (women) x (weight/75)^0.583 x ",
      "(age/45)^-0.167, ke 0.145/h x (MDRD eGFR/87)^0.261 from the entered or an ",
      "assumed normal creatinine, k23 0.101/h, k32 0.0856/h; lag 0.147 h, release ",
      "into the gut over 0.604 h, ka 3.68/h, BMI on ka and lag; reference weight and ",
      "BMI assumed, dose term not applied; concentration only. ",
      "https://doi.org/10.1007/s40262-015-0307-x"
    )
  )
}
