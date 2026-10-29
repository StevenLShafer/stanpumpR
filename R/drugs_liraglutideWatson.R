# -----------------------------------------------------------------------------
# Liraglutide (Watson 2010): one compartment, sequential zero- then
# first-order subcutaneous absorption (Victoza)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (1 nmol/L = 3.751 ng/mL; molecular weight
# 3751.2 g/mol).  Concentration only.  A second model of the same product as
# `liraglutide` (Roge 2015), from a different and larger analysis; the two are
# not mixed.  Drafted by Claude Code, 2026-10-17, at the request of Steven L.
# Shafer; see the antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Watson E, Jonker DM, Jacobsen LV, Ingwersen SH.  Population pharmacokinetics
# of liraglutide, a once-daily human glucagon-like peptide-1 analog, in
# healthy volunteers and subjects with type 2 diabetes, and comparison to
# twice-daily exenatide.  J Clin Pharmacol 2010;50:886-894.  Four phase 1
# studies (A-D): 74 healthy volunteers (1.25-20 mcg/kg single doses, with an
# intravenous arm) and 24 people with type 2 diabetes; validated on once-daily
# dosing in 168 people with type 2 diabetes (study E).  Table II, all
# studies:
#
#     CL/F = 0.013 L/h/kg      V/F = 0.16 L/kg
#     type 2 diabetes:   T0 = 8.0 h,  kA = 0.154 /h
#     healthy volunteers: T0 = 6.0 h, kA = 0.104 /h
#
# ABSORPTION, "sequential dual absorption" (Equations 1-2):
#
#     dA1/dt = -D F kA / (1 + kA T0)    for t <= T0
#     dA1/dt = -kA A1                   for t >  T0
#
# The depot empties at a constant rate for T0, so a share kA T0 / (1 + kA T0)
# of the dose (55% in diabetes) enters the circulation at a constant rate
# over T0, and what is left, D F / (1 + kA T0), is absorbed first order from
# T0 on (continuous at T0, as the paper's parameterisation ensures).  That is
# exactly a zero-order input into the central compartment (zeroOrderSC, into
# "central") beside a first-order depot lagged by T0, which the linear engine
# runs.  The type 2 diabetes values are used.  The absolute bioavailability,
# 0.51 in study A, is inside the apparent CL/F and V/F, so F is 1 here.
#
# BODY SIZE: per kilogram, as the source (doses were per kilogram in studies
# A-D), at the pharmacokinetic weight with the switch on and at total weight
# with it off.
#
# CHECKS (70 kg): elimination half-life 0.693 x 0.16 / 0.013 = 8.5 h (paper:
# "the mean elimination half-life was 12 hours" over 24-48 h after a dose,
# which includes the tail of absorption).  1.2 mg at 70 kg peaks at 50
# ng/mL (13 nmol/L) 11.8 h after the dose: the paper's Figure 1 shows 15 and
# 17.5 mcg/kg (about 1.1 and 1.2 mg) peaking near 11 to 13 nmol/L at about
# 12 h in healthy volunteers.  See
# tests/testthat/test-drugs-liraglutideWatson.R.
#
# References
# ----------
# Watson E et al., J Clin Pharmacol 2010;50:886-894.
#   https://doi.org/10.1177/0091270009354996
# -----------------------------------------------------------------------------

LIRAGLUTIDE_WATSON_T0 <- 8.0      # h, type 2 diabetes
LIRAGLUTIDE_WATSON_KA <- 0.154    # /h, type 2 diabetes

#' Liraglutide pharmacokinetics (Watson 2010, subcutaneous)
#'
#' One compartment with sequential zero- then first-order absorption; see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
liraglutideWatson <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  kaT0 <- LIRAGLUTIDE_WATSON_KA * LIRAGLUTIDE_WATSON_T0

  default <- list(
    v1  = 0.16 * pkW, v2 = 1, v3 = 1,
    cl1 = 0.013 * pkW / 60, cl2 = 0, cl3 = 0,
    ka_SC = LIRAGLUTIDE_WATSON_KA / 60,
    bioavailability_SC = 1,                   # apparent CL/F and V/F
    tlag_SC = LIRAGLUTIDE_WATSON_T0 * 60      # the first-order phase starts at T0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # the zero-order share, kA T0 / (1 + kA T0), over T0
    zeroOrderSC = list(fraction = kaT0 / (1 + kaT0),
                       duration = LIRAGLUTIDE_WATSON_T0 * 60,
                       into = "central"),
    reference = paste0(
      "Watson 2010 (healthy volunteers and type 2 diabetes): one compartment, CL/F ",
      "0.013 L/h/kg, V/F 0.16 L/kg; sequential absorption, zero order for T0 8.0 h ",
      "then first order at 0.154/h (type 2 diabetes values); concentration only. ",
      "https://doi.org/10.1177/0091270009354996"
    )
  )
}
