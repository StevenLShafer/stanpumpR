# -----------------------------------------------------------------------------
# Norepinephrine (Li 2024): plasma norepinephrine in healthy volunteers
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article and its supplement, which
# he supplied (the PubMed Central copy has its tables and equations
# stripped): Table 3, Equations 1-4 and the Results.
#
# THE SOURCE
# ==========
# Li Y, Koomen JV, Eleveld DJ, van den Berg JP, Vos JJ, de Keijzer IN,
# Struys MMRF, Colin PJ.  Population pharmacokinetic modelling of
# norepinephrine in healthy volunteers prior to and during general
# anesthesia.  Clin Pharmacokinet 2024;63:1597-1608
# (doi 10.1007/s40262-024-01430-y).  Thirty-six healthy volunteers, 18-70
# years, six men and six women in each of three age bands, median weights
# 67-72 kg.  Each was given 5 mcg and then 0.04, 0.08, 0.12, 0.16 and
# 0.20 mcg/kg/min for 15 minutes each, awake and again under propofol and
# remifentanil; 1219 radial-arterial plasma samples, NONMEM 7.5, FOCE-I.
# Final model, Table 3:
#
#     CL    2.1 L/min      x (BW/70)^0.75 x exp(-0.344/100 x (AGE - 35))
#                          x exp(-3.57/100 x propofol concentration, mcg/mL)
#     Vc    2.4 L          x BW/70
#     Q     0.6 L/min      x (BW/70)^0.75
#     Vp    3.6 L          x BW/70
#     Prod  497.7 ng/min   x (BW/70)^0.75    (endogenous production)
#     lag   13.7 s
#
# Two readings had to be made, and both are checked:
#
#   The covariate coefficients are per 100.  Equation 2 shows the age term
#   divided by 100; Equation 4, for propofol, does not.  The text settles it:
#   "patients 10 years older have about 3% lower CL" is exp(-0.00344 x 10) =
#   0.966, and "an expected decrease in CL of 11.8%" at the median propofol
#   concentration of 3.53 mcg/mL is exp(-0.0357 x 3.53) = 0.882.  Without the
#   division the propofol term would cut clearance by 99.9997%.
#
#   The allometric reference weight is not printed.  The paper cites
#   Eleveld's allometric scaling (Anesthesiology 2022;136:609), which uses
#   70 kg, and the three age bands' median weights are 66.9, 72.3 and
#   69.6 kg, so any reference near the study's own makes no material
#   difference.  70 kg is used.
#
# WHAT IS PLOTTED: EXOGENOUS NOREPINEPHRINE
# =========================================
# Norepinephrine is endogenous: the model's baseline is Prod / CL (Equation 1),
# 497.7 / 2.1 = 237 ng/L, which is 1.40 nmol/L -- exactly the median baseline
# Table 2 reports for the 18-34-year-olds, 1.40 nmol/L, an internal check the
# test repeats.  The engine has no constant input, so, as for epinephrine
# (R/drugs_epinephrine.R), the curve is the norepinephrine given, above the
# patient's own; add about 0.24 ng/mL to read a total.  At 0.1 mcg/kg/min the
# baseline is about 7% of the total.
#
# DOSE BASIS AND UNITS
# ====================
# The study used norepinephrine tartrate diluted to 20 mcg/mL and reports
# doses in mcg/kg/min without naming the basis.  Dutch and US labels state
# norepinephrine as the base, and the model's clearance (2.1 L/min) is the
# literature's metabolic clearance of norepinephrine only if the doses are
# the base; read as the tartrate, the true clearance would be half that.  So
# a dose here is the base, as labelled.  Concentrations, measured in nmol/L,
# are plotted in ng/mL at 169.18 g/mol.
#
# PROPOFOL, AGE, THE LAG
# ======================
# The model is evaluated AWAKE, with no propofol, because the library has no
# way to feed a simulated propofol concentration into another drug's
# clearance as a time-varying covariate.  Under general anaesthesia at the
# study's median 3.53 mcg/mL, clearance is 11.8% lower and every
# concentration about 13% higher than plotted.  Age is applied as published.
# The 13.7-second lag (the transit from the infusion arm to the sampling arm)
# is not represented; it is a fifth of a minute.
#
# CHECKED AGAINST THE PAPER
# =========================
# The paper's own simulation (Figure 5: 0.12 mcg/kg/min for 15 minutes, no
# propofol) reaches about 24 nmol/L total at 15 minutes; this model gives 25.
# Table 2's OBSERVED medians at the end of each awake step run 10 to 22%
# above the typical-value curve, the most at the highest rates and in the
# youngest band; Figure 3 shows the same slight underprediction at high
# concentrations, so it belongs to the fit, not the transcription.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own allometric weight covariate on every parameter,
# so it is evaluated at the pharmacokinetic weight with the fat-free-mass
# switch on (size$pkWeight) and at total body weight with it off, which
# reproduces the published equations exactly.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only; the pressure response was reported separately (de Keijzer,
# Br J Anaesth) and is not modelled here.
# -----------------------------------------------------------------------------

# Li 2024, Table 3
NOREPINEPHRINE_CL      <- 2.1      # L/min at 70 kg, age 35, awake
NOREPINEPHRINE_VC      <- 2.4      # L at 70 kg
NOREPINEPHRINE_Q       <- 0.6      # L/min at 70 kg
NOREPINEPHRINE_VP      <- 3.6      # L at 70 kg
NOREPINEPHRINE_PROD    <- 497.7    # ng/min at 70 kg, endogenous (not plotted)
NOREPINEPHRINE_AGE     <- -0.344   # per 100, per year from 35, on CL
NOREPINEPHRINE_PROPOFOL <- -3.57   # per 100, per mcg/mL of propofol, on CL
NOREPINEPHRINE_LAG     <- 13.7     # s, not represented
NOREPINEPHRINE_MW      <- 169.18   # g/mol, the base

#' Norepinephrine (Li 2024)
#'
#' Two compartments in healthy volunteers, awake; see the header of
#' \code{R/drugs_norepinephrine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
norepinephrine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w    <- pkW / 70
  propofol <- 0   # awake: see the header

  cl <- NOREPINEPHRINE_CL * w^0.75 *
    exp(NOREPINEPHRINE_AGE / 100 * (age - 35)) *
    exp(NOREPINEPHRINE_PROPOFOL / 100 * propofol)

  default <- list(
    v1  = NOREPINEPHRINE_VC * w,
    v2  = NOREPINEPHRINE_VP * w,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = cl,
    cl2 = NOREPINEPHRINE_Q * w^0.75,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Li Y et al., Clin Pharmacokinet 2024;63:1597-1608. ",
    "Two compartments in 36 healthy volunteers, 18-70 y: CL 2.1 L/min ",
    "x (BW/70)^0.75 x exp(-0.00344 (age - 35)), Vc 2.4 L, Q 0.6 L/min, ",
    "Vp 3.6 L.  Awake (propofol lowers CL 11.8% at 3.53 mcg/mL); exogenous ",
    "norepinephrine plotted above the endogenous baseline (about 0.24 ng/mL). ",
    "https://doi.org/10.1007/s40262-024-01430-y"
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
