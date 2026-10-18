# -----------------------------------------------------------------------------
# Dulaglutide: two compartments, subcutaneous once weekly (Trulicity)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: the FDA's fasting glucose and
# HbA1c disease model is not plotted.  Drafted by Claude Code, 2026-10-17, at
# the request of Steven L. Shafer; see the antidiabetic registry,
# inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Geiser JS, Heathman MA, Cui X, et al.  Clinical pharmacokinetics of
# dulaglutide in patients with type 2 diabetes: analyses of data from
# clinical trials.  Clin Pharmacokinet 2016;55:625-634.  The phase 3 combined
# analysis (1067 patients with type 2 diabetes, 0.75 and 1.5 mg once weekly),
# "a two-compartment model with first-order absorption and elimination".
# Table 6, true scale (an absolute bioavailability study fixed F):
#
#     F1 (1.5 mg) = 0.470 FIXED x exp(-0.00877 x (WT - 92.5 kg))
#     ka = 0.00769 /h      CL = 0.0593 L/h      Q  = 0.0201 L/h
#     Vc = 2.25 L          Vp = 3.75 L
#
# These are the values of the FDA review's Table 5 that the specification
# cited.  Body weight is the only covariate; it acts on bioavailability
# because it had "similar effects on both clearance and the volume of
# distribution".  The apparent clearance at 92.5 kg, 0.0593 / 0.47 = 0.126
# L/h, is the abstract's.
#
# THE DOSE.  The phase 2 analysis made bioavailability fall with the dose (an
# Emax: maximum reduction 59.7%, half at 0.19 mg); the phase 3 table gives F
# for 1.5 mg only.  The model uses 0.47 for every dose, the final model's
# value: the paper's noncompartmental steady state is close to dose
# proportional between 0.75 and 1.5 mg (CL/F 0.111 and 0.107 L/h, Table 4),
# while the phase 2 Emax would make 0.75 mg 11% more bioavailable.  Doses
# above 1.5 mg (the 3 and 4.5 mg strengths) are outside the fitted range.
#
# BODY SIZE: the model's own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off,
# with F held at or below 1 (a weight below about 6 kg would otherwise give
# more than the dose); the disposition parameters carry no size term.
#
# CHECKS
# ======
# 1.5 mg weekly at 83 kg (the phase 1 population, switch off): steady-state
# AUC over the week F x dose / CL = 12.9 mg.h/L against 14.0 (CV 30%)
# observed, and peak 92 ng/mL against 114 (CV 35%) (Table 4).  A single dose
# peaks at 58 h (paper: median 48 h).  The model's slowest phase has a
# half-life of 7.5 days; the paper's 4.7 days is a noncompartmental estimate
# within one weekly interval, which mixes it with the absorption (3.8 days)
# and distribution phases.
#
# References
# ----------
# Geiser JS et al., Clin Pharmacokinet 2016;55:625-634.
#   https://doi.org/10.1007/s40262-015-0338-3
# -----------------------------------------------------------------------------

DULAGLUTIDE_F_REFERENCE <- 0.470
DULAGLUTIDE_F_WEIGHT <- -0.00877   # per kg, about 92.5 kg
DULAGLUTIDE_REFERENCE_WEIGHT <- 92.5

#' Dulaglutide pharmacokinetics (subcutaneous, once weekly)
#'
#' Two compartments with first-order absorption (Geiser 2016); see the header
#' of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dulaglutide <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  f <- min(1, DULAGLUTIDE_F_REFERENCE *
             exp(DULAGLUTIDE_F_WEIGHT * (pkW - DULAGLUTIDE_REFERENCE_WEIGHT)))

  default <- list(
    v1  = 2.25, v2 = 3.75, v3 = 1,
    cl1 = 0.0593 / 60, cl2 = 0.0201 / 60, cl3 = 0,
    ka_SC = 0.00769 / 60,
    bioavailability_SC = f,
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Geiser 2016 (phase 3, type 2 diabetes): two compartments, ka 0.00769/h, CL ",
      "0.0593 L/h, Q 0.0201 L/h, Vc 2.25 L, Vp 3.75 L, F 0.47 x exp(-0.00877 x ",
      "(weight - 92.5)); concentration only. https://doi.org/10.1007/s40262-015-0338-3"
    )
  )
}
