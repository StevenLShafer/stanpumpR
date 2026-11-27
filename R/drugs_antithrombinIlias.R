# -----------------------------------------------------------------------------
# Antithrombin III (human), plasma-derived, in adults with severe sepsis
# (Ilias 2000): one compartment, intravenous; a candidate of the antithrombin
# group
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in
# units of 10 IU, activity in IU/dL (= % of normal).  Plotted as the patient's
# own antithrombin activity plus the infused activity (R/factorBaselines.R;
# Steven L. Shafer, 2026-11-25).  Drafted by Claude Code, 2026-11-26, at the
# request of Steven L. Shafer (docs/library-expansion/plan.csv, Biologics
# (sepsis)); digest card papers/digests/PMID_10945387.md.
#
# WHY THIS SOURCE (Shafer, 2026-11-26)
# ====================================
# The antithrombin drug (Thrombate III) ran on Moffett 2017, a model of
# children under 19 years, at every age.  Ilias 2000 is an adult study of a
# plasma-derived concentrate.  Shafer chose (2026-11-26) to make antithrombin
# a model group (R/modelSelection.R): this model for adults, Moffett for
# children, the baseline only plotted here, with no covariate on the volume.
#
# SOURCE
# ======
# Ilias W, List W, Decruyenaere J, Lignian H, Knaub S, Schindel F,
# Keinecke H-O, Heinrichs H, Thijs LG.  Antithrombin III in patients with
# severe sepsis: a pharmacokinetic study.  Intensive Care Med
# 2000;26:704-715.  Read on the PDF.  A randomised, open, multicentre phase
# II study: 33 adults with severe sepsis (median age 64 years, range 18-85;
# 23 men, 10 women; 82% surgical; median APACHE II 17), 32 of them in the
# pharmacokinetic analysis.  Kybernin P (Centeon, plasma-derived), 6000 IU
# over a bolus infusion, then 1000 IU every 4 hours or 250 IU/h for 4 days,
# 30 000 IU in all.  Activity by a chromogenic functional assay; antigen and
# activity agreed (Figure 1).  Each patient fitted to one compartment with
# zero-order input after the baseline was subtracted (HOEREP-PC); the
# individual parameters are summarised, not a population model.  Baseline
# activity median 63% (range 20-100, Table 1).
#
# Table 3, all 32 patients, volume and clearance "standardized to 70 kg body
# weight":
#
#                      median   mean    range
#     half-life (h)     18.6    18.9    5.1-37.4
#     Vss (L)           4.5     4.5     2.4-6.5
#     clearance (mL/min) 2.7    3.4     1.4-10.1
#     response (%/IU/kg) 1.70   1.75    1.14-2.80
#
# The medians are used: the clearance is skewed (CV 59%, mean 3.4 against a
# median 2.7), and the medians agree with each other, 0.693 x 4.5 L / 2.7
# mL/min = 19.3 h against the median half-life of 18.6 (the means give 15.3
# against 18.9, a mean of ratios not being a ratio of means).
#
#     V  = 4.5 L x W/70
#     CL = 2.7 mL/min x W/70
#
# THE BASELINE: the Patient Profile's Antithrombin baseline (IU/dL), plotted
# only: the source subtracted each patient's baseline before fitting and has
# no covariate (Shafer, 2026-11-26).  Blank, 63 IU/dL, the cohort's median.
#
# HEPARIN: eight patients took heparin with no effect on the kinetics (the
# paper's Results); the protocol allowed low-dose heparin for prophylaxis.
# Nothing is held.
#
# WHO IT DESCRIBES: adults with severe sepsis, consuming antithrombin, so the
# half-life (19 h) is far shorter than in hereditary deficiency (the Thrombate
# III label: 3.8 days by the functional assay, 8 patients); in a patient who
# is not septic the activity will fall more slowly than drawn.  The product
# is Kybernin P, not Thrombate III; both are plasma-derived human
# antithrombin III, dosed in the same international units.
#
# CHECKS: at 70 kg the model gives a half-life of 19.3 h and a rise of 1.56
# IU/dL for each IU/kg (70 IU into 45 dL); the paper's median response is
# 1.70 (Table 3; the abstract's 1.75 is the mean), measured as the largest
# rise within 2 hours of the loading dose, and
# its median half-life 18.6 h.  Its last samples sometimes exceeded the fit,
# which the authors read as recovering synthesis (not modelled).
#
# BAND: the label's target, 80 to 120% of normal (IU/dL), typical 100
# (Thrombate III prescribing information, 2.1, Table 1).
#
# BODY SIZE: the source standardised both parameters to 70 kg in proportion
# to weight, its own size term, kept: evaluated at the pharmacokinetic weight
# with the switch on and at total weight with it off.
#
# PREGNANCY: none studied; nothing applied (inst/extdata/pregnancyRegistry.csv).
#
# References
# ----------
# Ilias W et al., Intensive Care Med 2000;26:704-715.
#   https://doi.org/10.1007/s001340051236
# THROMBATE III prescribing information, BLA 103196, revised 11/2026.
# -----------------------------------------------------------------------------

ANTITHROMBIN_ILIAS_V  <- 4.5    # L at 70 kg (Table 3, median Vss)
ANTITHROMBIN_ILIAS_CL <- 2.7    # mL/min at 70 kg (Table 3, median)
# Blank: the cohort's median baseline (Table 1)
ANTITHROMBIN_ILIAS_BASELINE <- 63  # IU/dL

#' Antithrombin III (human), plasma-derived, in adults with sepsis (Ilias 2000)
#'
#' One compartment, in proportion to weight, plotted as the patient's own
#' antithrombin activity plus the infused, in IU/dL. A candidate of the
#' \code{antithrombin} group. See the header of
#' \code{R/drugs_antithrombinIlias.R}.
#'
#' @inheritParams cefazolin
#' @param antithrombinBaseline the patient's own antithrombin activity in
#'   IU/dL, or NULL for 63 IU/dL, the source cohort's median; plotted only
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
antithrombinIlias <- function(weight, height, age, sex, adjustToFFM = TRUE,
                              antithrombinBaseline = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / 70

  baseline <- if (is.null(antithrombinBaseline)) ANTITHROMBIN_ILIAS_BASELINE else
    antithrombinBaseline

  default <- list(
    v1 = ANTITHROMBIN_ILIAS_V * w, v2 = 1, v3 = 1,
    cl1 = ANTITHROMBIN_ILIAS_CL / 1000 * w, cl2 = 0, cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Ilias 2000 (32 adults with severe sepsis, 18-85 years, given Kybernin P): ",
    "one compartment, V 4.5 L and CL 2.7 mL/min at 70 kg, in proportion to weight ",
    "(medians of the individual fits; half-life 19 h). Plotted as the patient's ",
    "antithrombin activity plus the infused: baseline ", signif(baseline, 3), " IU/dL",
    if (is.null(antithrombinBaseline)) " (blank: the source's median)" else "",
    ". Band: the label's target, 80-120 IU/dL. https://doi.org/10.1007/s001340051236")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 100, upperTypical = 120, lowerTypical = 80,
    reference = reference,
    endogenousActivity = baseline
  )
}
