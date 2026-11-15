# -----------------------------------------------------------------------------
# Selexipag (Uptravi): two compartments, oral, forming its active metabolite
# ACT-333679, from Krause 2017's population model of the GRIPHON trial
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentrations are ng/mL (= mcg/L), total plasma.  The
# metabolite is its own drug, ACT333679 (R/drugs_ACT333679.R).
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer, who
# asked for the drugs of pulmonary arterial hypertension.  Source, read in
# full (PMC5529739): Krause A, Machacek M, Lott D, Hurst N, Bruderer S,
# Dingemanse J.  Population modeling of selexipag pharmacokinetics and
# clinical response parameters in patients with pulmonary arterial
# hypertension.  CPT Pharmacometrics Syst Pharmacol 2017;6:477-485.
# doi 10.1002/psp4.12202.
#
# THE SOURCE
# ==========
# GRIPHON, 512 patients with pulmonary arterial hypertension on 200 to 1,600
# mcg twice a day (sparse sampling), Monolix.  Two compartments for selexipag
# with first-order absorption after a lag, first-order conversion to
# ACT-333679, and two compartments for the metabolite.  Table 1 and Table 2:
#
#     tlag 0.67 h (fixed: "0.668 h" from the healthy-subject model)
#     ka 0.71 /h
#     Vp/F 12.90 L x (bw/70)^1.2
#     CL/F 19.10 L/h x (bw/70)^0.61 x (bilirubin/10 umol/L)^-0.40
#     k12 0.09 /h      k21 0.06 /h
#     kmet 0.67 /h     (selexipag to ACT-333679)
#     Vm/F 4.65 L x (bw/70)^0.88
#     k34 1.04 /h      k43 0.18 /h
#     km 0.49 /h; male 0.57 /h ("Sex on km" 0.15: 0.49 exp(0.15))
#
# "The reference group was female for sex ... naive for PAH-specific
# comedication", bilirubin 10 umol/L.  The patient is held at no endothelin
# receptor antagonist or PDE5 inhibitor and a bilirubin of 10 umol/L; sex is
# applied.
#
# CL/F is the clearance OTHER than conversion: Axelsen 2024's re-estimate of
# the same model (CPT Pharmacometrics Syst Pharmacol, doi 10.1002/psp4.13231,
# Table 1) defines it as "Apparent clearance of parent (not including
# kmet)".  The library's metabolite route adds formation as a parameter of its
# own, not taken out of the parent's clearance (metaboliteCoefficients()), so
# the parent's total clearance here is CL/F + kmet Vp/F = 19.1 + 0.67 x 12.9
# = 27.7 L/h at the reference, and kmet is the metabolite's kFormation.
#
# The source's doses and both analytes are in mass (mcg; ng/mL), and no
# molar conversion appears in it, so mwRatio is 1 (the source's amounts are
# already on the metabolite's scale).  The steady-state ratio of the AUCs is
# then kmet Vp / (km Vm) = 3.8, the label's "approximately 3- to 4-fold".
#
# CHECKS (not used in the fit)
# ============================
# Kaufmann P et al., Am J Cardiovasc Drugs 2015 (doi 10.1007/s40256-015-
# 0117-4, PMC4452035, Table 3), healthy men, 400 mcg once, fasted, means:
# selexipag Cmax 5.98 ng/mL, AUC 12.35 ng.h/mL; ACT-333679 Cmax 8.18 ng/mL,
# AUC 53.65 ng.h/mL.  This model, at a 70-kg man: selexipag 5.75 ng/mL and
# 14.4 ng.h/mL; ACT-333679 5.55 ng/mL and 46.4 ng.h/mL.  The
# label: selexipag terminal half-life 0.8-2.5 h, ACT-333679 6.2-13.5 h with
# an effective half-life of 3-4 h; Vss 11.7 L and total clearance 17.9 L/h
# after intravenous doses (absolute; this model's are apparent, with oral F
# "approximately 49%").
#
# ROUTES
# ======
# Oral only.  Uptravi for injection exists, but the oral model cannot carry
# it: after intravenous doses "exposure to the active metabolite" is similar
# while "exposure to selexipag is approximately twice as high" (label), so
# much of the oral metabolite is formed before selexipag reaches the
# circulation, which this model's systemic conversion does not represent.
# Recorded as a decision in the PR (2026-11-15).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight terms, evaluated at the pharmacokinetic weight with
# the switch on and at total weight with it off.
#
# DOSE
# ====
# Uptravi tablets, 200 to 1,600 mcg of selexipag (no salt), twice a day.
#
# PREGNANCY AND THE BAND
# ======================
# No pregnancy pharmacokinetics (registry row).  No therapeutic range.
# -----------------------------------------------------------------------------

# Krause 2017, Table 1
SELEXIPAG_TLAG  <- 0.67    # h
SELEXIPAG_KA    <- 0.71    # /h
SELEXIPAG_VP    <- 12.90   # L at 70 kg
SELEXIPAG_VP_WT <- 1.2
SELEXIPAG_CL    <- 19.10   # L/h at 70 kg, bilirubin 10 umol/L, excluding kmet
SELEXIPAG_CL_WT <- 0.61
SELEXIPAG_K12   <- 0.09    # /h
SELEXIPAG_K21   <- 0.06    # /h
SELEXIPAG_KMET  <- 0.67    # /h
SELEXIPAG_WT_REF <- 70     # kg

#' Selexipag (Krause 2017)
#'
#' Two compartments, oral, forming the active metabolite ACT-333679; see the
#' header of \code{R/drugs_selexipag.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
selexipag <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  wt <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / SELEXIPAG_WT_REF
  vp <- SELEXIPAG_VP * wt^SELEXIPAG_VP_WT
  cl <- SELEXIPAG_CL * wt^SELEXIPAG_CL_WT + SELEXIPAG_KMET * vp   # L/h, total

  default <- list(
    v1  = vp,
    v2  = vp * SELEXIPAG_K12 / SELEXIPAG_K21,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = vp * SELEXIPAG_K12 / 60,
    cl3 = 0,
    ka_PO = SELEXIPAG_KA / 60,
    bioavailability_PO = 1,      # apparent parameters
    tlag_PO = SELEXIPAG_TLAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    metabolite = list(
      name = "ACT333679",
      kFormation = SELEXIPAG_KMET / 60,
      mwRatio = 1          # the source's amounts are mass on both sides
    ),
    reference = SELEXIPAG_REFERENCE
  )
}

SELEXIPAG_REFERENCE <- paste0(
  "Krause A, Machacek M, Lott D, Hurst N, Bruderer S, Dingemanse J, CPT ",
  "Pharmacometrics Syst Pharmacol 2017;6:477-485, Table 1: selexipag two compartments ",
  "(lag 0.67 h, ka 0.71 /h, Vp/F 12.9 L x (bw/70)^1.2, CL/F 19.1 L/h x (bw/70)^0.61 ",
  "excluding conversion, k12 0.09 /h, k21 0.06 /h), conversion kmet 0.67 /h, ",
  "ACT-333679 two compartments (Vm/F 4.65 L x (bw/70)^0.88, k34 1.04 /h, k43 0.18 /h, ",
  "km 0.49 /h, x exp(0.15) in men); bilirubin 10 umol/L, no PAH comedication. ",
  "https://doi.org/10.1002/psp4.12202")
