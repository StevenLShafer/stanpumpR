# -----------------------------------------------------------------------------
# Antithrombin III (human), Thrombate III: one compartment, intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# units of 10 IU, activity in IU/dL (= % of normal).  Plotted as the patient's
# own antithrombin activity plus the infused activity (R/factorBaselines.R;
# Steven L. Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-26, at the
# request of Steven L. Shafer (docs/library-expansion/plan.csv, Biologics
# (sepsis)); digest card papers/digests/PMID_28664670.md.
#
# WHY THIS SOURCE (Shafer, 2026-11-26)
# ====================================
# DeJongh 2014 (PMID 24335249), the adult model he had chosen to wait for,
# turned out on reading to be a model of ATryn, recombinant antithrombin alfa,
# whose half-life (12 to 18 h) is a fraction of the plasma-derived
# concentrate's (2.5 days by the paper's own comparison), and ATryn is no
# longer listed on DailyMed.  Thrombate III, plasma-derived, is the US
# antithrombin.  Shafer chose (2026-11-26) to build this drug, Thrombate III,
# from Moffett 2017, the only population model of a plasma-derived
# concentrate in the archive, and to add ATryn as a drug of its own
# (antithrombinAlfa) from DeJongh.
#
# SOURCE
# ======
# Moffett BS, Diaz R, Galati M, Mahoney D, Teruya J, Yee DL.  Population
# pharmacokinetics of human antithrombin concentrate in paediatric patients.
# Br J Clin Pharmacol 2017;83:2450-2457.  Read on the PDF; Table 3 confirmed
# on the page image.  A retrospective review of 184 inpatients under 19 years
# (median 0.35 years, IQR 0.07-3.9; median 5.2 kg, IQR 3.4-14.8; 46.7% boys;
# 50 neonates, 85 infants to 2 years, 30 children, 19 adolescents) given
# Thrombate (Grifols) over 15 minutes, 46.3 +/- 13.6 units/kg a dose, most
# (87.5%) for a low antithrombin activity during a heparin infusion; baseline
# activity 59 +/- 17 IU/dL (Table 2).  Activity by a chromogenic functional
# assay (STA-Stachrom AT).  NONMEM 7.2, FOCE-I; validated in 30 more patients
# (median prediction error 1.75%).
#
# Table 3 (final model), CL in dL/h, VD in dL:
#
#     CL = 0.917 x (WT/70)^0.75 + 0.129 x (UFH/173)
#     VD = 67.9 x (WT/70) x (BASE/60)^-0.389
#
# with UFH the continuous heparin infusion in units/h and BASE the predose
# antithrombin activity in IU/dL.  One compartment; the allometric exponents
# are fixed.  The baseline entered the model as a covariate and through the
# error model (Dansirikul's B3 method); the infused activity is added to it.
#
# HELD: the heparin term at 0 (Shafer, 2026-11-25, accepting the
# recommendation, "the heparin term (if the source has one) as a first step
# held at 0 and said").  UFH is the heparin rate in units/h, which the dose
# table holds, but no model reads another drug's doses.  With heparin running
# the source's median child (5.2 kg, 34 units/kg/h) clears antithrombin twice
# as fast: CL 0.262 against 0.131 dL/h, half-life 13 against 27 hours.  So the
# curve is that of a patient given no heparin, and with heparin the activity
# falls faster than drawn.  The help page says so.
#
# THE BASELINE: the Patient Profile's Antithrombin baseline (IU/dL), both the
# plotted baseline and BASE in the volume (Shafer, 2026-11-25).  Blank, 60
# IU/dL, the value the source normalises BASE to and the baseline of the
# median patient it simulates (Results); the cohort's mean was 59.  In the
# volume, BASE is held within 25 to 93 IU/dL, the cohort's mean +/- 2 SD
# (59 +/- 2 x 17), because the paper prints no range and the power of a
# baseline of 0 would be an infinite volume.  [decided] by Shafer, 2026-11-26:
# those bounds; a patient below 25 IU/dL is given the volume at 25.  The plotted
# baseline is the value typed, unheld.
#
# WHO IT DESCRIBES: children under 19 years, mostly infants, critically ill,
# on heparin.  It is used for adults as the only model of a plasma-derived
# concentrate (Shafer, 2026-11-26).  At 70 kg and a baseline of 60 the
# half-life is 51 hours (2.1 days) and the incremental recovery 1.03 IU/dL
# per IU/kg.  The Thrombate III label (12.3), 8 asymptomatic patients with
# hereditary deficiency, functional assay: recovery 1.4 +/- 0.1 IU/dL per
# IU/kg, half-life 3.8 +/- 1.8 days (2.5 +/- 1.5 by the immunological assay).
# So in an adult with hereditary deficiency the model draws a smaller rise
# and a faster fall than the label; the source's children were consuming
# antithrombin, which the authors offer as the reason for a larger volume
# at a lower baseline.  The help page says so.
#
# CHECKS (Table 5, 5.2 kg, baseline 60, means of 10 000 simulated patients
# with the interindividual variability): CL 2.8 mL/kg/h without heparin, 5.7
# with 34 units/kg/h; VD 101 mL/kg.  The typical values here are 2.51 and
# 5.04 mL/kg/h and 97.0 mL/kg; with Table 3's variability (47.5% and 28.5%,
# log-normal) the means are 2.81, 5.64 and 101.  The text's "~22 h" half-life
# with heparin does not follow from its own equations (13.3 h at the typical
# values; Table 5's mean is 15.5 h); its "~27 h" without heparin does (26.8).
#
# BAND: the label's target, 80 to 120% of normal (IU/dL), typical 100
# (Thrombate III prescribing information, 2.1, Table 1).
#
# BODY SIZE: both parameters carry the source's own weight term, kept as
# fitted, evaluated at the pharmacokinetic weight with the switch on and at
# total weight with it off.
#
# PREGNANCY: no pregnancy-related change in the plasma-derived concentrate's
# clearance or volume has been quantified against a non-pregnant comparison
# (Weiner 1990, PMID 2353344, abstract: half-life 29.4 h in 5 healthy pregnant
# women; DeJongh 2014 found larger CL and Vss at delivery for the recombinant
# product); nothing is applied (inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Moffett BS et al., Br J Clin Pharmacol 2017;83:2450-2457.
#   https://doi.org/10.1111/bcp.13359
# THROMBATE III prescribing information, BLA 103196, revised 11/2026.
# -----------------------------------------------------------------------------

ANTITHROMBIN_CL   <- 0.917   # dL/h at 70 kg, no heparin
ANTITHROMBIN_UFH  <- 0.129   # dL/h per 173 units/h of heparin (held at 0)
ANTITHROMBIN_UFH_REF <- 173  # units/h
ANTITHROMBIN_VD   <- 67.9    # dL at 70 kg and a baseline of 60 IU/dL
ANTITHROMBIN_VD_BASE_EXPONENT <- -0.389
ANTITHROMBIN_BASE_REF <- 60  # IU/dL
# The heparin rate the model is run at (units/h): held at 0, see the header
ANTITHROMBIN_HEPARIN_HELD <- 0
# The baseline the volume reads is held to the cohort's mean +/- 2 SD,
# 59 +/- 2 x 17 IU/dL (Table 2; no range printed): decided by Shafer, 2026-11-26
ANTITHROMBIN_BASE_MIN <- 25
ANTITHROMBIN_BASE_MAX <- 93
# Blank: the source's reference baseline
ANTITHROMBIN_BASELINE <- 60  # IU/dL

#' Antithrombin III (human), Thrombate III (Moffett 2017)
#'
#' One compartment, allometric in weight, plotted as the patient's own
#' antithrombin activity plus the infused, in IU/dL; the heparin term is held
#' at 0. See the header of \code{R/drugs_antithrombin.R}.
#'
#' @inheritParams cefazolin
#' @param antithrombinBaseline the patient's own antithrombin activity in
#'   IU/dL, or NULL for 60 IU/dL, the source's reference; also the baseline
#'   on which the volume depends (held within 25 to 93 IU/dL there)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
antithrombin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         antithrombinBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / 70

  baseline <- if (is.null(antithrombinBaseline)) ANTITHROMBIN_BASELINE else
    antithrombinBaseline
  base <- min(max(baseline, ANTITHROMBIN_BASE_MIN), ANTITHROMBIN_BASE_MAX)

  cl <- ANTITHROMBIN_CL * w^0.75 +
    ANTITHROMBIN_UFH * ANTITHROMBIN_HEPARIN_HELD / ANTITHROMBIN_UFH_REF   # dL/h
  vd <- ANTITHROMBIN_VD * w *
    (base / ANTITHROMBIN_BASE_REF)^ANTITHROMBIN_VD_BASE_EXPONENT          # dL

  default <- list(
    v1 = vd / 10, v2 = 1, v3 = 1,
    cl1 = cl / 10 / 60, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Moffett 2017 (184 children under 19 years, median 5.2 kg, given Thrombate): ",
    "one compartment, CL 0.917 dL/h x (weight/70)^0.75 + 0.129 x heparin units/h / 173, ",
    "VD 67.9 dL x weight/70 x (baseline/60)^-0.389; the heparin term held at 0 ",
    "(no heparin), used beyond the children it was fitted in. Plotted as the patient's ",
    "antithrombin activity plus the infused: baseline ", signif(baseline, 3), " IU/dL",
    if (is.null(antithrombinBaseline)) " (blank: the source's reference)" else "",
    ". Band: the label's target, 80-120 IU/dL. https://doi.org/10.1111/bcp.13359")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 120, lowerTypical = 80,
    reference = reference,
    endogenousActivity = baseline
  )
}
