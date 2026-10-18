# -----------------------------------------------------------------------------
# Epinephrine (Abboud 2009): plasma epinephrine in adult septic shock
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the full text of the source in PubMed Central
# (PMC2750169), Table 3 and the Results.
#
# THE SOURCE
# ==========
# Abboud I, Lerolle N, Urien S, Tadie JM, Leviel F, Fagon JY, Faisy C.
# Pharmacokinetics of epinephrine in patients with septic shock: modelization
# and interaction with endogenous neurohormonal status.  Crit Care
# 2009;13:R120 (doi 10.1186/cc7972).  Thirty-eight consecutive adults in a
# medical ICU (age 64 +/- 15 years, weight 68 +/- 19 kg, SAPS II 64 +/- 23),
# epinephrine the only catecholamine, started at 0.15 mcg/kg/min and titrated
# to a mean arterial pressure of 65 to 75 mmHg (0.026 to 1.7 mcg/kg/min at
# sampling).  One sample before the infusion and one at steady state, when the
# cumulative dose reached 0.15 mg/kg: 73 concentrations, analysed in NONMEM VI
# (FOCE) as a one-compartment model with first-order elimination and a
# baseline (endogenous) input rate R0.  Michaelis-Menten elimination did not
# improve the fit, so the model is linear by the source's own test.  Final
# model, Table 3:
#
#     CL (L/h) = 127 x (BW/70)^0.60 x (SAPS II/50)^-0.67
#     V        = 7.9 L
#     R0       = 43.5 nmol/h     (endogenous epinephrine input)
#
# DOSE BASIS AND UNITS
# ====================
# The paper's dose rates are epinephrine base: its 0.1 and 7 mg/h correspond
# to 545 and 38 208 nmol/h, which is 183.2 g/mol, the molecular weight of the
# base.  US epinephrine injection is labelled as the base too, so a dose is
# entered as labelled.  The paper reports concentrations in nmol/L; the
# library plots ng/mL, which is nmol/L x 0.1832.  No conversion is needed in
# the model itself, because a clearance and a volume do not depend on the
# mass unit.
#
# WHAT IS PLOTTED: EXOGENOUS EPINEPHRINE
# ======================================
# The curve is the epinephrine the user gives, above the patient's own.  The
# endogenous input R0 gives a baseline of R0 / CL = 43.5 / 127 = 0.34 nmol/L
# (0.063 ng/mL) at the reference patient, which is exactly the median
# pre-infusion concentration the paper measured (0.34 nmol/L, Table 2) -- an
# internal check the test repeats.  It is under 1% of the median steady-state
# concentration during infusion (95.8 nmol/L, 17.5 ng/mL), so leaving it out
# changes nothing a user would see, and it is not drawn.
#
# SEVERITY OF ILLNESS
# ===================
# SAPS II is not a Patient Profile field, so clearance is evaluated at the
# model's reference score of 50.  The study's own patients averaged 64, at
# which clearance is (64/50)^-0.67 = 0.85 of the plotted value: a sicker
# patient than the reference has a higher concentration for the same rate.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance carries its own weight covariate, (BW/70)^0.60, so it is evaluated
# at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, which reproduces the
# published equation exactly.  The volume was fitted as a fixed 7.9 L with no
# size term, so it is scaled by the library's fat-free-mass factor with the
# switch on and left at 7.9 L with it off (legacyVolume = 1).
#
# WHAT THIS MODEL IS NOT
# ======================
# The data are steady-state concentrations (one per patient during infusion),
# so the clearance is well determined (SE 6%) and the volume is not (SE 36%,
# bootstrap 90% interval 1 to 13 L).  The plateau for a given rate is
# reliable; how fast the concentration gets there after a rate change is
# fixed by V/CL, a half-life of 2.6 minutes here, and is only approximately
# known.  A bolus (the 1 mg of cardiac arrest) is far outside the infusion
# data the model was fitted to.  There is no effect site and no band: the
# study fitted no concentration-effect relation, and the pressure a given
# concentration produces in septic shock varies enormously, which is the
# paper's own starting point.
# -----------------------------------------------------------------------------

# Abboud 2009, Table 3
EPINEPHRINE_CL       <- 127     # L/h at 70 kg and SAPS II 50
EPINEPHRINE_CL_BW    <- 0.60    # exponent of BW/70
EPINEPHRINE_CL_SAPS  <- -0.67   # exponent of SAPS II/50
EPINEPHRINE_V        <- 7.9     # L
EPINEPHRINE_R0       <- 43.5    # nmol/h, endogenous input (not plotted)
EPINEPHRINE_SAPS_REF <- 50      # the reference score, used for every patient
EPINEPHRINE_MW       <- 183.2   # g/mol, the base

#' Epinephrine (Abboud 2009)
#'
#' One compartment in adult septic shock; see the header of
#' \code{R/drugs_epinephrine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
epinephrine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  cl <- EPINEPHRINE_CL / 60 * (pkW / 70)^EPINEPHRINE_CL_BW *
    (EPINEPHRINE_SAPS_REF / 50)^EPINEPHRINE_CL_SAPS

  default <- list(
    v1  = EPINEPHRINE_V * size$volume,
    v2  = 1,                        # one compartment
    v3  = 1,
    cl1 = cl,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Abboud I et al., Crit Care 2009;13:R120. ",
    "One-compartment model in 38 adults with septic shock, epinephrine ",
    "0.026-1.7 mcg/kg/min: CL 127 L/h x (BW/70)^0.60 x (SAPS II/50)^-0.67, ",
    "V 7.9 L.  SAPS II is held at the reference 50.  Exogenous epinephrine ",
    "is plotted; the endogenous baseline (about 0.06 ng/mL) is not. ",
    "https://doi.org/10.1186/cc7972"
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
