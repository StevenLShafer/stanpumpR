# -----------------------------------------------------------------------------
# Metformin extended release: two compartments, zero-order input (Glumetza,
# Glucophage XR, Fortamet and their generics)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# mg, concentrations in mcg/mL (= mg/L).  Concentration only: no glucose,
# HbA1c or lactate is plotted.  Drafted by Claude Code, 2026-11-19, at the
# request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv (metformin_er).
#
# SOURCE
# ======
# Duong JK, Kumar SS, Kirkpatrick CM, Greenup LC, Arora M, Lee TC, Timmins P,
# Graham GG, Furlong TJ, Greenfield JR, Williams KM, Day RO.  Population
# pharmacokinetics of metformin in healthy subjects and patients with type 2
# diabetes mellitus: simulation of doses according to renal function.  Clin
# Pharmacokinet 2013;52:373-384 (PMID 23475568, read in full with its
# supplement).  305 people in three studies (Table 1): 120 patients with type
# 2 diabetes (creatinine clearance 15-127 mL/min, 13 below 30), 16 healthy
# volunteers given immediate- and extended-release tablets over 35 days, and
# 169 healthy Malaysian volunteers given single immediate-release doses.  Two
# compartments, apparent parameters (Table 2; Eq. 1 and 2):
#
#     CL/F  = 1212 mL/min x (CLcr / 6 L/h)       CLcr by Cockcroft-Gault on
#     V1/F  = 149 L x (TBW / 70 kg)              lean body weight
#     Q/F   = 203 mL/min       V2/F = 182 L
#     XR: zero-order input for D = 9.55 h, relative F 1.26, 1.16, 1.06 and 1
#         at 500, 1000, 1500 and 2000 mg
#     IR: ka 0.35 /h, lag 0.38 h, F 1 (the reference)
#
# CL/F is proportional to creatinine clearance with no non-renal part (the
# regression through the origin, section 3.3).  CLcr is Cockcroft-Gault
# (their ref. 24) with the Janmahasatian lean body weight (refs. 22 and 23),
# normalised to the median, 6 L/h (100 mL/min); TBW to 70 kg.  The XR input
# goes into the central compartment with no lag ("a zero-order input for the
# XR formulation"; the lag is the IR formulation's), and its relative F is by
# the size of each dose: interpolated linearly between the tabulated sizes
# and held at 1.26 below 500 mg and 1 above 2000 mg.  The release is run by
# oralZeroOrder (R/oral-zero-order.R) with its relativeF table.
#
# Only the extended-release tablet is this drug; the immediate-release tablet
# is metformin (Chae 2012), which was not replaced.  Duong's IR parameters
# are recorded in the registry.
#
# RENAL FUNCTION: Cockcroft-Gault on the Janmahasatian lean body weight
# (lbwJanmahasatian(), below) of the patient's total weight and height, at
# the entered creatinine or an assumed normal one for age and sex
# (adultEquivalentCreatinine()).  The creatinine clearance carries the size,
# so CL/F is not scaled again (docs/weight-adjustment.md: a renal term is not
# anchored).  Fitted down to 15 mL/min.
#
# BODY SIZE: the source's own covariates.  V1/F is proportional to total body
# weight, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off; Q/F and V2/F have no covariate and take the
# library's fat-free-mass factors with the switch on (docs/weight-adjustment.md:
# parameters the source left without a size term), the published values with
# it off.  A model with its own size covariate is not anchored at the cohort.
#
# PREGNANCY: metformin's own late-pregnancy factors (R/drugs_metformin.R; Liao
# MZ et al., Drug Metab Dispos 2020;48:264-271): CL/F x 1.27, and both
# apparent volumes x 1.33 (Liao fitted one volume).  The renal term is not
# also raised.  The CL/F factor contains pregnancy's rise in filtration, so it
# is applied only with a blank Serum creatinine: a measured pregnant
# creatinine already raises the Cockcroft-Gault term (the rule of
# pregnancyGfrFactor(), R/pregnancy.R), and the two together would count it
# twice.  The volume factors apply either way.  Measured with the immediate-release tablet; the clearance is
# the drug's, not the tablet's.
#
# CHECKS (test-drugs-metforminER.R)
#   A 70 kg, 170 cm, 40-year-old man with creatinine 0.9 mg/dL: lean body
#   weight 54.5 kg, CLcr 84 mL/min, CL/F 61.1 L/h.  Duong section 3.2: the
#   patient with end-stage kidney disease (CLcr 15 mL/min) on 2000 mg daily
#   had the highest Cav,ss, 7.7 mg/L; the model at CLcr 15 gives CL/F 10.9
#   L/h and 2000 / (10.9 x 24) = 7.6 mg/L.  The simulated steady-state
#   average equals dose x relative F / (CL/F x 24 h).
#
# References
# ----------
# Duong JK et al., Clin Pharmacokinet 2013;52:373-384.
#   https://doi.org/10.1007/s40262-013-0046-9
# Janmahasatian S, Duffull SB, Ash S, Ward LC, Byrne NM, Green B.
#   Quantification of lean bodyweight.  Clin Pharmacokinet 2005;44:1051-1065.
#   https://doi.org/10.2165/00003088-200544100-00004
# -----------------------------------------------------------------------------

METFORMIN_ER_CL       <- 1212 / 1000   # L/min at a CLcr of 100 mL/min
METFORMIN_ER_CLCR_REF <- 100           # mL/min (6 L/h, the median)
METFORMIN_ER_V1       <- 149           # L at 70 kg
METFORMIN_ER_WT_REF   <- 70            # kg
METFORMIN_ER_Q        <- 203 / 1000    # L/min
METFORMIN_ER_V2       <- 182           # L
METFORMIN_ER_D_H      <- 9.55          # h, the zero-order input
METFORMIN_ER_RELATIVE_F <- list(dose = c(500, 1000, 1500, 2000),
                                f    = c(1.26, 1.16, 1.06, 1))

#' Janmahasatian lean body weight
#'
#' Janmahasatian 2005: men 9270 W / (6680 + 216 BMI), women 9270 W / (8780 +
#' 244 BMI), W in kg and BMI in kg/m^2.  Duong 2013 (metforminER) computed
#' creatinine clearance on it.
#'
#' @param weight total body weight, kg
#' @param height height, cm
#' @param sex "male" or "female"
#' @returns lean body weight, kg
#' @keywords internal
lbwJanmahasatian <- function(weight, height, sex)
{
  bmi <- weight / (height / 100)^2
  if (sex == SEX_FEMALE) 9270 * weight / (8780 + 244 * bmi) else
    9270 * weight / (6680 + 216 * bmi)
}

#' Metformin pharmacokinetics (oral, extended release)
#'
#' Two compartments with a zero-order input whose relative bioavailability
#' falls with the dose (Duong 2013); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant when \code{TRUE}, apply metformin's changes of late
#'   pregnancy: both volumes x 1.33, and CL/F x 1.27 only when
#'   \code{creatinine} is blank (see the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
metforminER <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, pregnant = FALSE)
{
  # Q/F and V2/F have no size term: the library's factors with the switch on,
  # the published values (factors of 1) with it off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(lbwJanmahasatian(weight, height, sex), age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))

  # Liao's CL/F factor contains pregnancy's rise in filtration, which a
  # measured (pregnant) creatinine already carries through Cockcroft-Gault:
  # applied only with a blank creatinine, as pregnancyGfrFactor() is
  clPregnancy <- if (isTRUE(pregnant) && is.null(creatinine))
    METFORMIN_PREGNANCY_CL_FACTOR else 1
  vPregnancy  <- if (isTRUE(pregnant)) METFORMIN_PREGNANCY_V_FACTOR else 1

  default <- list(
    v1  = METFORMIN_ER_V1 * pkW / METFORMIN_ER_WT_REF * vPregnancy,
    v2  = METFORMIN_ER_V2 * size$volume * vPregnancy, v3 = 1,
    cl1 = METFORMIN_ER_CL * crcl / METFORMIN_ER_CLCR_REF * clPregnancy,
    cl2 = METFORMIN_ER_Q * size$clearance, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Duong 2013 (305 healthy volunteers and adults with type 2 diabetes, ",
    "creatinine clearance 15-178 mL/min): two compartments, CL/F 1212 mL/min x ",
    "(Cockcroft-Gault on lean body weight / 100 mL/min), V1/F 149 L x (weight/70), ",
    "Q/F 203 mL/min, V2/F 182 L; the extended-release tablet a zero-order input ",
    "over 9.55 h with relative F 1.26, 1.16, 1.06 and 1 at 500, 1000, 1500 and ",
    "2000 mg; concentration only. https://doi.org/10.1007/s40262-013-0046-9"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0(if (is.null(creatinine))
               paste0("CL/F x ", METFORMIN_PREGNANCY_CL_FACTOR, ", ") else
               "CL/F from the measured creatinine (factor not applied), ",
             "V1/F and V2/F x ", METFORMIN_PREGNANCY_V_FACTOR),
      paste0("Liao MZ et al., Drug Metab Dispos 2020;48:264-271. ",
             "https://doi.org/10.1124/dmd.119.088435")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # Each tablet a zero-order input into the circulation (R/oral-zero-order.R)
    oralZeroOrder = list(duration = METFORMIN_ER_D_H * 60, lag = 0,
                         relativeF = METFORMIN_ER_RELATIVE_F),
    reference = reference
  )
}
