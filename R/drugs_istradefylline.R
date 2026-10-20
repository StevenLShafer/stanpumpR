# -----------------------------------------------------------------------------
# Istradefylline (Nourianz): two compartments, apparent oral, with lean body
# mass, age and weight covariates (Knebel 2011, as the FDA reviewed it)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# The sponsor's population model (published as Knebel W, Rao N, Uchimura T, et
# al., J Clin Pharmacol 2011;51:40-52, PMID 20200269, read as the abstract),
# reproduced in full as Table 1 of the FDA's 2007-8 pharmacometrics review of
# the first submission, which is Appendix I of the Clinical Pharmacology
# Review of NDA 022075 (2019), read from the page image (PM review p. 18): 16
# studies, 1449 subjects (1219 with Parkinson's disease), 10909
# concentrations; two compartments with first-order absorption:
#
#     CL/F = 5.76 L/h x (LBM/55.6)^0.106 x (AGE/62)^-0.111
#            x 0.903 (Hispanic/Latino, African American, other) x 1.05 (Asian)
#            x 0.736 (with a CYP3A4 inhibitor) x 1.63 (smoker)
#     V2/F = 198 L x (WT/70)^-0.0521        (central)
#     Q/F  = 21.6 L/h x (LBM/55.6)^1.23
#     V3/F = 307 L x (WT/70)^1.69           (peripheral)
#     Ka   = 0.464 /h;  F1 1.10 with food, 0.929 fasted, 1 unknown
#
# for the reference subject "Caucasian 62 years, 70kg, 55.6kg LBM,
# non-smoker, no CYP3A4 inhibitors, unknown food status" (PM review p. 24).
# The same review: "Smoking would be expected to decrease AUCss by 38% and the
# presence of CYP3A4 inhibitors would increase the AUCss by 35%".  The 2019
# resubmission's model (Table 5 of the same review) is not used: its
# intercompartmental clearance is unidentified (SE 1044%), and its covariate
# forms and reference values are not printed.
#
# WHAT IS INFERRED: LEAN BODY MASS
# ================================
# Neither the review nor the abstract gives the lean body mass equation.  The
# James equation (lbmJames(), R/lbmJames.R), the usual NONMEM covariate of the
# period, gives 55.3 kg for a 70 kg, 170 cm man, close to the reference 55.6;
# Janmahasatian's gives 54.5.  James is used.  Clearance barely depends on it
# (exponent 0.106); Q does (1.23).
#
# WHAT IS HELD FIXED
# ==================
# The Patient Profile has no race, smoking, co-medication or food field.  The
# reference subject is drawn: White, non-smoker, no CYP3A4 inhibitor, unknown
# food status.  A smoker's exposure is 39% lower (the label raises the dose to
# 40 mg at 20 or more cigarettes a day), and a strong CYP3A4 inhibitor's 35%
# higher (the label caps the dose at 20 mg).  The race factors are within 10%.
#
# AGE: (AGE/62)^-0.111 is a negative power of age, so it sees
# max(age, ADULT_AGE_TERM_FLOOR) (R/constants.R).
#
# DOSE BASIS: istradefylline is not a salt; tablets are mg of istradefylline.
#
# BODY SIZE (docs/weight-adjustment.md): the model carries its own size
# covariates (lean body mass on CL and Q, weight on V2 and V3), evaluated at
# the pharmacokinetic weight (with the patient's height) with the fat-free-
# mass switch on and at total body weight with it off.
#
# CHECKS (reference subject): CL/F 5.76 L/h, V2 + V3 505 L (label Vd/F 557 L,
# CL/F 4.6 L/h), terminal half-life 67 h (label 83 h; the review, 70 to 118
# h); time to peak after one tablet about 4 h (label: 4 h fasted).
#
# NO EFFECT SITE, NO BAND: the review's exposure-response is a direct Emax of
# daily OFF time on steady-state AUC (maximum fall 5.79 percentage points,
# AUC50 1690 ng.h/mL; Knebel 2012), which is a dose-level relation, not a
# time course; there is no published concentration range.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 022075 (Nourianz), with the
#   2007-8 pharmacometrics review as Appendix I.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2019/022075Orig1s000ClinPharmR.pdf
# Knebel W et al. J Clin Pharmacol 2011;51:40-52.
#   https://doi.org/10.1177/0091270010363809
# -----------------------------------------------------------------------------

# 2007-8 FDA pharmacometrics review, Table 1 (Knebel 2011's model)
ISTRADEFYLLINE_CL       <- 5.76    # L/h
ISTRADEFYLLINE_CL_LBM   <- 0.106
ISTRADEFYLLINE_CL_AGE   <- -0.111
ISTRADEFYLLINE_V2       <- 198     # L, central
ISTRADEFYLLINE_V2_WT    <- -0.0521
ISTRADEFYLLINE_Q        <- 21.6    # L/h
ISTRADEFYLLINE_Q_LBM    <- 1.23
ISTRADEFYLLINE_V3       <- 307     # L, peripheral
ISTRADEFYLLINE_V3_WT    <- 1.69
ISTRADEFYLLINE_KA       <- 0.464   # /h
ISTRADEFYLLINE_LBM_REF  <- 55.6    # kg
ISTRADEFYLLINE_AGE_REF  <- 62      # years
# Not applied (no Patient Profile field); recorded for the help and tests
ISTRADEFYLLINE_SMOKER   <- 1.63
ISTRADEFYLLINE_CYP3A4_INHIBITOR <- 0.736

#' Istradefylline pharmacokinetics (oral)
#'
#' Two compartments, apparent (/F), from Knebel 2011 as Table 1 of the FDA's
#' pharmacometrics review; see the header of \code{R/drugs_istradefylline.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
istradefylline <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  w <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  lbm <- lbmJames(w, height, sex)
  cl <- ISTRADEFYLLINE_CL * (lbm / ISTRADEFYLLINE_LBM_REF)^ISTRADEFYLLINE_CL_LBM *
    (max(age, ADULT_AGE_TERM_FLOOR) / ISTRADEFYLLINE_AGE_REF)^ISTRADEFYLLINE_CL_AGE
  q  <- ISTRADEFYLLINE_Q * (lbm / ISTRADEFYLLINE_LBM_REF)^ISTRADEFYLLINE_Q_LBM

  default <- list(
    v1  = ISTRADEFYLLINE_V2 * (w / 70)^ISTRADEFYLLINE_V2_WT,
    v2  = ISTRADEFYLLINE_V3 * (w / 70)^ISTRADEFYLLINE_V3_WT,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = q / 60,
    cl3 = 0,
    ka_PO = ISTRADEFYLLINE_KA / 60,
    bioavailability_PO = 1,          # apparent parameters, unknown food status
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Knebel 2011, as Table 1 of the FDA pharmacometrics review in the ",
      "Clinical Pharmacology Review of NDA 022075: two compartments, CL/F ",
      "5.76 L/h x (LBM/55.6)^0.106 x (age/62)^-0.111, V2/F 198 L x ",
      "(weight/70)^-0.0521, Q/F 21.6 L/h x (LBM/55.6)^1.23, V3/F 307 L x ",
      "(weight/70)^1.69, ka 0.464/h; lean body mass by James (not stated in the ",
      "source); White, non-smoker, no CYP3A4 inhibitor; concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2019/022075Orig1s000ClinPharmR.pdf"
    )
  )
}
