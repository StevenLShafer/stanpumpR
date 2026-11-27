# -----------------------------------------------------------------------------
# Ciprofol (HSK3486), intravenous: Liu 2024 for adults, Wang 2026 for children
# -----------------------------------------------------------------------------
# Ciprofol is a 2,6-disubstituted phenol, a structural analogue of propofol
# about four to five times as potent, approved in China for sedation and
# general anaesthesia and for sedation in the ICU; not approved in the US.
# Steven L. Shafer chose the sources on 2026-11-26 ("Yes, build ciprofol that
# way").  Every number below was read on the papers.
#
# ADULTS: Liu L, Wang K, Sun Z, et al. Pharmacokinetics and exposure-safety
# relationship of ciprofol for sedation in mechanically ventilated patients
# in the intensive care unit. CPT Pharmacometrics Syst Pharmacol
# 2024;13:823-836.  doi:10.1002/psp4.13121, PMID 38440939 (card
# papers/digests/PMID_38440939.md; text read in full).  Twelve trials, 462
# adults aged 18-77 (median weight 61.7 kg, 41.6-97.9; height 165 cm; 226
# men, 236 women; 85% Asian): 125 healthy volunteers, 225 elective surgical
# patients and 112 ventilated ICU patients.  Three compartments (Table 2):
#   CL = 80.9 L/h x (WT/62)^0.578 x 1.09 (women) x 0.789 (not ICU)
#        [x 0.823 venous; x 1.26 short-term-infusion studies]
#   V1 = 3.53 L x (age/33)^0.496
#   Q2 73.9 L/h, V2 51.9 L, Q3 28.6 L/h, V3 419 L
#        [short-term-infusion studies: V2 x 0.125, Q3 x 2.23, V3 x 0.198]
# The paper's reference is an ICU patient after a long infusion, sampled
# arterially.  stanpumpR uses the ARTERIAL parameters (Shafer, 2026-11-26:
# AGENTS.md, Arterial pharmacokinetics are preferred), of the LONG-INFUSION,
# NON-ICU studies (FLG = 0, ICU = 1: HSK3486-102, -204 and -306, healthy
# volunteers and the maintenance of general anaesthesia), Shafer's choice of
# 2026-11-26.  Why not the short-infusion terms: they are the single-dose
# studies, whose slow phase after 0.1-0.9 mg/kg probably fell below the
# assay's 5 ng/mL before it could be seen (they sampled to 24 h, so it was
# not the sampling time; Liu 2024 J Clin Anesth Table 1), giving a deep
# volume a fifth of the long infusions'.  A linear model cannot change its
# volumes with the length of an infusion, so the long-infusion set, which
# saw the deep compartment, is taken as the drug's.  The paper's text says
# CL is 35.1% lower in short-infusion subjects than in the ICU reference,
# where Table 2 and the equation give 0.99; the set used here needs neither.
# The ICU set (clearance 27% higher) is not offered: the Patient Profile has
# no ICU field.
#
# EFFECT SITE AND BIS: Liu L, Wang K, Yang Y, et al. Population
# pharmacokinetic/pharmacodynamic modeling and exposure-response analysis of
# ciprofol in the induction and maintenance of general anesthesia in
# patients undergoing elective surgery. J Clin Anesth 2024;92:111317.
# doi:10.1016/j.jclinane.2023.111317, PMID 37976683 (read on the PDF supplied
# by Shafer, 2026-11-26).  The earlier analysis of nine of the twelve trials,
# whose long-infusion parameters agree with the set above (arterial CL
# 50.5 x 1.23 = 62.1 L/h against 63.8; V3 436 against 419 L).  BIS from
# studies 204 and 306 (115 surgical patients, 5640 BIS values): an effect
# compartment, ke0 7.04 /h (IIV 42.5%), and an inhibitory sigmoid Emax with
# Emax fixed at 1 and E0 the patient's own baseline, EC50 582 ng/mL, gamma
# 2.14 (Table 3).  The time to peak effect after a bolus on that paper's own
# long-infusion model (venous, 62 kg, 33-year-old man) is 2.94 min, and on
# the set above with the same ke0 also 2.94 (2.81 on the short-infusion
# set), so tPeak = 2.94 min is given and ke0 is solved from it on the
# patient's own disposition.  The BIS model is the % effect panel
# (inst/extdata/pdEffect.csv).
#
# CHILDREN: Wang S, Li Y, Hu Z, et al. Population pharmacokinetics of a
# single bolus of ciprofol in Chinese pediatric patients. BMC Anesthesiol
# 2026;26:141.  doi:10.1186/s12871-026-03647-9, PMID 41612213 (card
# papers/digests/PMID_41612213.md).  25 children aged 1-9 years (9.8-37 kg),
# 0.6 mg/kg over 30 s, arterial samples for 3 h.  Three compartments, every
# parameter per kg (Table 3): CL 31.2 mL/min/kg, V1 506 mL/kg, Q2 28.2
# mL/min/kg, V2 231 mL/kg, Q3 19.8 mL/min/kg, V3 1360 mL/kg.  V1's BUN term
# (BUN/5 mmol/L)^-0.821 is held at its reference: the Patient Profile has
# no BUN.  The children's time to peak effect is the adults' 2.94 min, no
# paediatric effect-site study existing (Shafer's decision, 2026-11-27).
#
# NO BAND, NO TCI.  Neither paper gives a therapeutic range; the BIS model
# is drawn as % effect instead.  A band from BIS 40-60 (about 450-675
# ng/mL at an assumed baseline BIS of 95) is not drawn (Shafer's decision,
# 2026-11-27).  Target-controlled infusion is not offered.
# -----------------------------------------------------------------------------

#' Time to peak effect of ciprofol after a bolus, minutes
#'
#' From Liu 2024 (J Clin Anesth) ke0 7.04 /h on its own long-infusion model;
#' see `R/ciprofol.R`.
#' @keywords internal
CIPROFOL_TPEAK <- 2.94

#' Liu 2024 (CPT Pharmacometrics Syst Pharmacol) ciprofol parameters
#'
#' Table 2's arterial ICU reference: L and L/h, CL at 62 kg, V1 at 33 years;
#' and the covariate multipliers.
#' @keywords internal
CIPROFOL_LIU <- c(cl = 80.9, v1 = 3.53, q2 = 73.9, v2 = 51.9, q3 = 28.6,
                  v3 = 419, clWeightExponent = 0.578, v1AgeExponent = 0.496,
                  clFemale = 1.09, clNotIcu = 0.789)

#' Liu 2024's cohort, for anchoring the size-free parameters
#' @keywords internal
CIPROFOL_LIU_COHORT <- list(weight = 61.7, age = 38, men = 226, women = 236,
                            height = 165)

#' Wang 2026 ciprofol parameters, per kilogram
#'
#' CL and Q in L/min/kg, volumes in L/kg (Table 3).
#' @keywords internal
CIPROFOL_WANG <- c(cl = 0.0312, v1 = 0.506, q2 = 0.0282, v2 = 0.231,
                   q3 = 0.0198, v3 = 1.36)

ciprofolResult <- function(default, reference)
{
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  list(
    PK = PK,
    tPeak = CIPROFOL_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      reference,
      " Time to peak effect 2.94 min, from the BIS ke0 7.04/h of Liu L et al.,",
      " J Clin Anesth 2024;92:111317. https://doi.org/10.1016/j.jclinane.2023.111317")
  )
}

#' Ciprofol, intravenous, adults (Liu 2024)
#'
#' Arterial, long-infusion, non-ICU parameters; see `R/ciprofol.R`.
#' @inheritParams cefazolin
#' @export
ciprofolLiu <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CIPROFOL_LIU
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, CIPROFOL_LIU_COHORT))
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  # the age term is a positive power, zero at birth: held to the 18 years
  # the source studied (AGENTS.md, Age terms)
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  cl <- p[["cl"]] * (pkW / 62)^p[["clWeightExponent"]] * p[["clNotIcu"]] *
    (if (identical(sex, "female")) p[["clFemale"]] else 1)

  default <- list(
    v1  = p[["v1"]] * (ageTerm / 33)^p[["v1AgeExponent"]] * size$volume,
    v2  = p[["v2"]] * size$volume,
    v3  = p[["v3"]] * size$volume,
    cl1 = cl / 60,
    cl2 = p[["q2"]] / 60 * size$clearance,
    cl3 = p[["q3"]] / 60 * size$clearance
  )
  ciprofolResult(default, paste0(
    "Liu L et al., CPT Pharmacometrics Syst Pharmacol 2024;13:823-836 ",
    "(462 adults; arterial, long-infusion, non-ICU parameters). ",
    "https://doi.org/10.1002/psp4.13121."))
}

#' Ciprofol, intravenous, children (Wang 2026)
#'
#' Three compartments per kilogram in children aged 1-9 years; see
#' `R/ciprofol.R`.
#' @inheritParams cefazolin
#' @export
ciprofolWang <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CIPROFOL_WANG
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  default <- list(
    v1  = p[["v1"]] * pkW,
    v2  = p[["v2"]] * pkW,
    v3  = p[["v3"]] * pkW,
    cl1 = p[["cl"]] * pkW,
    cl2 = p[["q2"]] * pkW,
    cl3 = p[["q3"]] * pkW
  )
  ciprofolResult(default, paste0(
    "Wang S et al., BMC Anesthesiol 2026;26:141 (25 children 1-9 years, ",
    "arterial; BUN held at 5 mmol/L). https://doi.org/10.1186/s12871-026-03647-9."))
}
