# -----------------------------------------------------------------------------
# Methylene blue (methylthioninium chloride), intravenous: two compartments
# derived exactly from the ProvayBlue label and the Proveblue SmPC
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-28, at the request of Steven L. Shafer, who
# supplied every document named below and chose this construction over
# keeping the drug blocked (inst/extdata/vasoactiveRegistry.csv).
#
# THE SOURCES
# ===========
# 1. PROVAYBLUE (methylene blue) injection prescribing information, NDA
#    204630, revised 12/2017 (FDA Reference ID 4201611), section 12.3: after
#    2 mg/kg as a 5-minute intravenous infusion, mean (CV) Cmax 2917 ng/mL
#    (39%) and AUC 13 977 ng h/mL (21%); Vss 255 +/- 58 L; half-life "about
#    24 hours"; plasma protein binding about 94%; blood-to-plasma ratio 5.1 at
#    5 minutes falling to a plateau of 0.6 at 4 hours; about 40% excreted
#    unchanged in urine.  The study's subjects, their number and their weight
#    are not printed.
# 2. Methylthioninium chloride Proveblue, EMA summary of product
#    characteristics, section 5.2: terminal half-life after intravenous
#    administration 24.7 (SD 7.2) h; renal and hepatic impairment exposures
#    (below).
#
# THE DERIVATION
# ==============
# A two-compartment model has four parameters, and the label and SmPC give
# exactly four numbers: the terminal rate constant lambda2 = ln 2 / 24.7 h;
# the clearance, dose / AUC; the mean residence time, Vss / CL; and the Cmax
# at the end of the 5-minute infusion.  With the bolus coefficients A1, A2,
#
#     A1/lambda1   + A2/lambda2   = AUC / dose
#     A1/lambda1^2 + A2/lambda2^2 = MRT AUC / dose
#     Cmax = (dose/T) sum Ai (1 - exp(-lambda_i T)) / lambda_i,  T = 5 min
#
# the first two give A1 and A2 for any lambda1, and the third fixes lambda1
# (methyleneBlueFromLabel() solves it).  At 70 kg (2 mg/kg = 140 mg):
#
#     V1 46.83 L,  V2 208.17 L,  CL 10.02 L/h,  Q 17.77 L/h
#     half-lives 1.07 h and 24.7 h; 30% of the area in the fast phase
#
# Peter 2000 (Eur J Clin Pharmacol 56:247; whole blood, an older HPLC assay)
# fitted three exponentials whose two fast phases have half-lives of 2.9 and
# 10.7 minutes.  With the label's numbers they would need a negative
# coefficient, and scaled to the label's AUC its shape gives a Cmax eleven
# times the label's and a Vss of 45 L, so the label and Peter cannot both
# describe the drug, and the model is two compartments.  There is no fifth
# number in the label to check the derivation; Walter-Sack 2009 is the check.
#
# THE CHECK: WALTER-SACK 2009 (NOT FITTED)
# ========================================
# Walter-Sack I, et al. Eur J Clin Pharmacol 2009;65:179-189 (doi
# 10.1007/s00228-008-0563-x): 16 healthy volunteers (mean 67.1 kg), 50 mg
# over 9.5 minutes, plasma and whole blood by LC-MS/MS to 24 h, Table 1:
# plasma AUC(0-inf) 7639 +/- 3384 ng h/mL, AUC(0-24) 4928, Cmax 748 at a
# Tmax of 0.5 h after the end of the injection, half-life 18.5 h.  This
# model, scaled to 67.1 kg, predicts AUC(0-inf) 5208 (32% low) and AUC(0-24)
# from the end of the injection 3263 (34% low): a clearance of 0.143 L/h/kg
# against Walter-Sack's 0.098.  Clearance is
# the parameter to trust in a comparison (AGENTS.md), and the difference is
# not a sampling artifact: Walter-Sack's 24-hour sampling, with a third of its
# area extrapolated, would overstate its clearance, not understate it.  The
# doses differ (2 mg/kg against 0.75 mg/kg), and whether clearance depends on
# dose is unknown.  The model therefore follows the label, the clinical dose
# (1-2 mg/kg), and may underpredict concentrations by about a third.
#
# THE FIRST HALF HOUR
# ===================
# In Walter-Sack's subjects whole blood peaked 10 minutes after the injection
# and PLASMA 30 minutes after it (range 10 minutes to 1.2 h), and the label's
# blood-to-plasma ratio of 5.1 at 5 minutes says the same: the drug enters red
# cells first and returns to the plasma.  No compartmental model with plasma
# as its central compartment rises after an infusion stops, so the plasma
# curve here is too high at the end of a short infusion and its first half
# hour is not the measured shape.  Total (bound plus unbound) plasma
# methylene blue is plotted; leucomethylene blue and azure B are not.
#
# RENAL AND HEPATIC IMPAIRMENT (NOT APPLIED)
# ==========================================
# SmPC 5.2: after 1 mg/kg, AUC(0-96h) rose 52%, 116% and 192% in mild,
# moderate and severe renal impairment (eGFR 60-89, 30-59, 15-29), with the
# half-life unchanged in mild and moderate impairment and 33 h in severe.  An
# AUC that rises with an unchanged half-life is not a clearance change alone,
# no equation of clearance on eGFR is given, and the subjects' weights are
# not printed, so no renal term is applied, and `creatinine` is not read.
# Hepatic impairment: no consistent effect (the SmPC's own conclusion).
#
# DOSE BASIS
# ==========
# Doses are methylene blue (methylthioninium chloride) as labelled, the basis
# of both the label's dose and its concentrations.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The label doses per kilogram but prints Vss in litres and does not give the
# subjects' weight; 70 kg is assumed, FFM_REFERENCE_WEIGHT.  The derived
# 70-kg values are scaled to fat-free mass with the switch on, and linearly in
# weight with it off (the default legacy factors).  If the label's subjects
# weighed more than 70 kg, the clearance and V1 per kilogram are unchanged
# (both follow from per-kilogram quantities) but Vss per kilogram is lower.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pharmacokinetic study of methylene blue in pregnancy was found (PubMed,
# 2026-10-28).  Pruthi 2011 (Am J Surg 201:70) studied nonpregnant women given
# it for lymphatic mapping and only estimated fetal exposure.  Nothing is
# applied; the registry records pregnancy as unknown.
#
# EFFECT SITE AND BAND
# ====================
# No time to peak effect has been measured for the vasopressor or the
# methaemoglobin-reducing effect, so tPeak is 0 and only the plasma is drawn.
# There is no therapeutic concentration range, so no band.
# -----------------------------------------------------------------------------

# ProvayBlue label, section 12.3 (2 mg/kg as a 5-minute infusion)
METHYLENE_BLUE_DOSE_PER_KG <- 2       # mg/kg
METHYLENE_BLUE_INFUSION    <- 5       # min
METHYLENE_BLUE_CMAX        <- 2917    # ng/mL
METHYLENE_BLUE_AUC         <- 13977   # ng h/mL
METHYLENE_BLUE_VSS         <- 255     # L
# Proveblue SmPC, section 5.2
METHYLENE_BLUE_THALF       <- 24.7    # h, terminal

#' The methylene blue two-compartment model derived from the label
#'
#' Solves the four label and SmPC numbers (Cmax at the end of a 5-minute
#' infusion, AUC, Vss and the terminal half-life) for the two-compartment
#' model they determine, at \code{FFM_REFERENCE_WEIGHT}.  See the header of
#' \code{R/drugs_methyleneBlue.R}.
#'
#' @returns a list: \code{v1}, \code{v2} (L), \code{cl1}, \code{cl2} (L/min),
#'   the exponents \code{lambda} (/min) and bolus coefficients \code{A}
#'   (mg/L per mg)
#' @keywords internal
methyleneBlueFromLabel <- function()
{
  dose <- METHYLENE_BLUE_DOSE_PER_KG * FFM_REFERENCE_WEIGHT       # mg
  auc  <- METHYLENE_BLUE_AUC / 1000 * 60                          # mg min/L
  cl   <- dose / auc                                              # L/min
  mrt  <- METHYLENE_BLUE_VSS / cl                                 # min
  l2   <- log(2) / (METHYLENE_BLUE_THALF * 60)                    # /min
  T    <- METHYLENE_BLUE_INFUSION

  coefficients <- function(l1)
    solve(matrix(c(1 / l1, 1 / l2, 1 / l1^2, 1 / l2^2), 2, byrow = TRUE),
          c(auc, mrt * auc)) / dose
  cmax <- function(l1) {
    A <- coefficients(l1)
    l <- c(l1, l2)
    sum(A * dose / T * (1 - exp(-l * T)) / l) * 1000                # ng/mL
  }
  l1 <- stats::uniroot(function(x) cmax(x) - METHYLENE_BLUE_CMAX,
                       c(1e-3, 1), tol = 1e-12)$root
  A <- coefficients(l1)
  l <- c(l1, l2)

  v1  <- 1 / sum(A)
  k21 <- sum(A * rev(l)) / sum(A)
  k10 <- prod(l) / k21
  k12 <- sum(l) - k21 - k10
  list(v1 = v1, v2 = v1 * k12 / k21, cl1 = k10 * v1, cl2 = k12 * v1,
       lambda = l, A = A)
}

#' Methylene blue, intravenous (ProvayBlue label and Proveblue SmPC)
#'
#' Two compartments derived exactly from the label's Cmax, AUC and Vss and the
#' SmPC's terminal half-life, checked against Walter-Sack 2009; see the header
#' of \code{R/drugs_methyleneBlue.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methyleneBlue <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  p <- methyleneBlueFromLabel()

  default <- list(
    v1  = p$v1  * size$volume,
    v2  = p$v2  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = p$cl1 * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "PROVAYBLUE (methylene blue) prescribing information, NDA 204630, rev. ",
    "12/2017, section 12.3, and the Proveblue EMA SmPC, section 5.2. Two ",
    "compartments derived exactly from the label's Cmax, AUC and Vss after ",
    "2 mg/kg over 5 min and the SmPC's 24.7-h half-life, at an assumed 70 kg: ",
    "V1 46.8 L, V2 208 L, CL 10.0 L/h, Q 17.8 L/h. Plasma methylene blue. ",
    "Walter-Sack I et al., Eur J Clin Pharmacol 2009;65:179-189 (not fitted) ",
    "measured a third lower clearance. ",
    "https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm?event=overview.process&ApplNo=204630"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
