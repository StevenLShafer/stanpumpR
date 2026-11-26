# -----------------------------------------------------------------------------
# Antithrombin alfa (recombinant), ATryn: two compartments, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# units of 10 IU, activity in IU/dL (= % of normal).  Plotted as the patient's
# own antithrombin activity plus the infused activity (R/factorBaselines.R).
# Drafted by Claude Code, 2026-11-26, at the request of Steven L. Shafer, who
# chose (2026-11-26) to add ATryn as a drug of its own beside the plasma-
# derived antithrombin (R/drugs_antithrombin.R); digest card
# papers/digests/PMID_24335249.md.
#
# SOURCE
# ======
# DeJongh J, Frieling J, Lowry S, Drenth HJ.  Pharmacokinetics of recombinant
# human antithrombin in delivery and surgery patients with hereditary
# antithrombin deficiency.  Clin Appl Thromb Hemost 2014;20:355-364.  Read on
# the PDF supplied by Shafer on 2026-11-26.  A two-compartment model fitted to
# a single-dose study (AT III 009-00) of 15 adults with hereditary
# deficiency not in a high-risk situation (13 women, 2 men; 50 IU/kg in 9,
# 100 IU/kg in 6), then, with every parameter but the baseline fixed,
# checked against two phase 3 trials of 32 patients in surgery (11) or at
# delivery (21) given a 15-minute loading dose and a continuous infusion; the
# delivery patients were over-predicted, and their clearance and Vss were
# re-estimated.  Activity by a functional assay, % of normal, added to a
# baseline estimated as a parameter.  NONMEM, FOCE-I.
#
# Table 3 (final model):
#
#                    surgery    delivery
#     CL (L/h)        0.665      1.38
#     Vss (L)         7.72       14.3
#     Vr = Vss/Vcentral - 1 = 1.51 (both)
#     Q (L/h)         0.613 (both)
#     ATBL (%)        44.7 (both)
#
# So Vcentral = Vss/2.51 (3.08 L in surgery, 5.70 at delivery) and Vperiph =
# Vss - Vcentral.  The text calls the delivery values "lower 95% confidence
# boundaries" in one sentence; Table 3 and the ATryn label (12.3: 1.38 L/h
# and 14.3 L against 0.67 and 7.7) give them as the estimates, which they are
# taken to be.  The text says Q was expressed as a multiple of CL during
# model building; Table 3 prints one Q for both groups, which is used.
#
# The surgery values are the non-pregnant patient; under the Patient
# Profile's Pregnant (third trimester, term), the delivery values (see
# PREGNANCY).  No covariate was retained: age, sex, weight, study centre and
# concomitant drugs were tested against the residuals and did not matter.
#
# CHECKS: the ATryn label (12.3, the single-dose study, baseline-corrected,
# non-compartmental): CL 9.6 and 7.2 mL/h/kg, Vss 126 and 156 mL/kg,
# half-life 11.6 and 17.7 h, incremental recovery 2.24 and 1.94 IU/dL per
# IU/kg after 50 and 100 IU/kg.  This model at 70 kg: CL 9.5 mL/h/kg, Vss
# 110 mL/kg, terminal half-life 11.9 h (15.1 h at delivery), and 2.27 IU/dL per IU/kg at the end
# of a bolus into the central volume (70/30.8).  The label also puts
# ATryn's clearance at about seven times, and half-life at about a ninth of,
# the plasma-derived product's.
#
# THE BASELINE: the Patient Profile's Antithrombin baseline (IU/dL), the
# same field the plasma-derived antithrombin reads; here it is only the
# plotted baseline (the model has no baseline covariate).  Blank, ATBL,
# 44.7 IU/dL, the source population's (hereditary deficiency).
#
# BODY SIZE ANCHOR (docs/weight-adjustment.md, Anchoring at the source's
# cohort): the parameters carry no size covariate.  DeJongh prints no weight,
# height or age for any of its cohorts (the 76 kg it simulated is an assumed
# typical patient, not the cohort's, and the label gives only "18 years or
# older"), so the fixed values stay anchored at the 70 kg reference man and
# are scaled by fat-free mass (volumes by the volume factor, clearances by
# the clearance factor); with the switch off they are the published values.
#
# PREGNANCY: applied.  The delivery group's CL and Vss (Table 3) under the
# Pregnant flag: CL x 1.38/0.665 = 2.075, Vss (so Vcentral and Vperiph) x
# 14.3/7.72 = 1.852, Q unchanged.  These are measured in the target
# population, hereditary deficiency at delivery (the label's peri-partum
# indication), with a separate dosing rule in the label.
#
# BAND: the label's target, 80 to 120% of normal (IU/dL), typical 100
# (ATryn prescribing information, 2.2).
#
# References
# ----------
# DeJongh J et al., Clin Appl Thromb Hemost 2014;20:355-364.
#   https://doi.org/10.1177/1076029613516188
# ATryn prescribing information, BL 125284, GTC Biotherapeutics, 3 Feb 2009
#   (https://www.fda.gov/media/75529/download).
# -----------------------------------------------------------------------------

ANTITHROMBIN_ALFA_CL  <- 0.665   # L/h, surgery (non-pregnant)
ANTITHROMBIN_ALFA_VSS <- 7.72    # L, surgery
ANTITHROMBIN_ALFA_VR  <- 1.51    # Vss/Vcentral - 1, both groups
ANTITHROMBIN_ALFA_Q   <- 0.613   # L/h, both groups
# Delivery (Table 3): the pregnancy factors
ANTITHROMBIN_ALFA_PREGNANT_CL  <- 1.38 / 0.665
ANTITHROMBIN_ALFA_PREGNANT_VSS <- 14.3 / 7.72
# Blank: the source population's baseline, ATBL (Table 3)
ANTITHROMBIN_ALFA_BASELINE <- 44.7   # IU/dL

#' Antithrombin alfa (recombinant), ATryn (DeJongh 2014)
#'
#' Two compartments, plotted as the patient's own antithrombin activity plus
#' the infused, in IU/dL; the delivery parameters under the Pregnant flag.
#' See the header of \code{R/drugs_antithrombinAlfa.R}.
#'
#' @inheritParams cefazolin
#' @param antithrombinBaseline the patient's own antithrombin activity in
#'   IU/dL, or NULL for 44.7 IU/dL, the source population's (hereditary
#'   deficiency)
#' @param pregnant TRUE for the third trimester or term: the source's
#'   delivery patients
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
antithrombinAlfa <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             antithrombinBaseline = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  preg <- isTRUE(pregnant)
  cl  <- ANTITHROMBIN_ALFA_CL * (if (preg) ANTITHROMBIN_ALFA_PREGNANT_CL else 1)
  vss <- ANTITHROMBIN_ALFA_VSS * (if (preg) ANTITHROMBIN_ALFA_PREGNANT_VSS else 1)
  vc  <- vss / (1 + ANTITHROMBIN_ALFA_VR)

  default <- list(
    v1  = vc * size$volume, v2 = (vss - vc) * size$volume, v3 = 1,
    cl1 = cl * size$clearance / 60, cl2 = ANTITHROMBIN_ALFA_Q * size$clearance / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  baseline <- if (is.null(antithrombinBaseline)) ANTITHROMBIN_ALFA_BASELINE else
    antithrombinBaseline

  reference <- paste0(
    "DeJongh 2014 (47 adults with hereditary antithrombin deficiency, ATryn): ",
    "two compartments, CL 0.665 L/h, Vss 7.72 L, Vss/Vcentral 2.51, Q 0.613 L/h",
    if (preg) "; at delivery (Pregnant) CL 1.38 L/h, Vss 14.3 L" else "",
    ". Plotted as the patient's antithrombin activity plus the infused: baseline ",
    signif(baseline, 3), " IU/dL",
    if (is.null(antithrombinBaseline)) " (blank: the source population's)" else "",
    ". Band: the label's target, 80-120 IU/dL. https://doi.org/10.1177/1076029613516188")
  if (preg)
    reference <- paste0(reference, "; ", pregnancyReference(
      "CL x 2.08, Vss x 1.85, Q unchanged",
      "DeJongh 2014, Table 3, delivery against surgery"))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 120, lowerTypical = 80,
    reference = reference,
    endogenousActivity = baseline
  )
}
