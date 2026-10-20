# -----------------------------------------------------------------------------
# Dopamine (MacGregor 2000): plasma dopamine in healthy men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published article, which he supplied:
# Tables 1-4 and Equations 1-3.
#
# THE SOURCE
# ==========
# MacGregor DA, Smith TE, Prielipp RC, Butterworth JF, James RL, Scuderi PE.
# Pharmacokinetics of dopamine in healthy male subjects.  Anesthesiology
# 2000;92:338-346 (doi 10.1097/00000542-200002000-00013).  Nine healthy men,
# 23-45 years, 73-116 kg (mean 90), given 10 mcg/kg/min for 10 minutes and,
# after 30 minutes' washout, 3 mcg/kg/min for 90 minutes; 32 radial arterial
# plasma samples each, HPLC with electrochemical detection; NONMEM.  The best
# model (Table 3, model D) is two compartments with a 0.5-minute lag and V1
# per kilogram; Table 4 gives
#
#     k10 1.10 /min, k12 0.244 /min, k21 0.070 /min, V1 0.241 L/kg, lag 0.508 min
#
# and, derived from those by Equation 1, CL 0.27 L/kg/min, Q 0.059 L/kg/min,
# V2 0.840 L/kg, alpha 1.36 and beta 0.0567 /min (half-lives 0.511 and
# 12.3 min).  The micro constants reproduce every derived value to its printed
# precision, and the steady-state constant of Equation 3, 3.77 min kg/L, is
# 1 / (V1 k10) -- the internal checks the test repeats.
#
# ONLY V1 CARRIES WEIGHT
# =======================
# Only V1 carries weight; the rate constants do not, so every volume and
# clearance is proportional to weight.  As for any model with its own weight
# covariate, it is evaluated at the pharmacokinetic weight with the
# fat-free-mass switch on (size$pkWeight) and at total body weight with it
# off.  The study's men were heavy (BMI 25.6-35.4), and weight did not
# predict their concentrations well.
#
# WHAT IS NOT REPRESENTED
# =======================
# The 0.5-minute lag, the transit from the infusing arm to the sampling
# artery.  The endogenous baseline: the model has no production term, and the
# measured baselines ranged from 3 to 8023 ng/L, against 11 300 ng/L at
# steady state on 3 mcg/kg/min.  Dose-dependence: the model overpredicted the
# 10 mcg/kg/min concentrations (Figure 1), and Le Corre 1993 found the volume
# and half-life roughly double from 3 to 6 mcg/kg/min in patients.
#
# HOW FAR TO TRUST IT
# ===================
# The authors' own conclusion is that dosing by weight "does not yield
# predictable blood concentrations": concentrations at the same rate varied
# 10- to 75-fold, half the measurements differed from the prediction by more
# than 59%, and 5% by more than 514% (APE50 and APE95, Table 3).  The curve is
# the typical healthy man, not a prediction for any patient.
#
# DOSE BASIS
# ==========
# Not stated; dopamine is labelled as the hydrochloride, so a dose is entered
# as labelled.  If the source's rates were the base, every concentration here
# is 24% high (189.64 / 153.18).
#
# NO EFFECT SITE, NO BAND
# =======================
# Heart rate rose at 10 mcg/kg/min but no concentration-effect relation was
# fitted.
# -----------------------------------------------------------------------------

# MacGregor 2000, Table 4
DOPAMINE_K10 <- 1.10    # /min
DOPAMINE_K12 <- 0.244   # /min
DOPAMINE_K21 <- 0.070   # /min
DOPAMINE_V1  <- 0.241   # L/kg
DOPAMINE_LAG <- 0.508   # min, not represented

#' Dopamine (MacGregor 2000)
#'
#' Two compartments per kilogram in healthy men; see the header of
#' \code{R/drugs_dopamine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
dopamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1 <- DOPAMINE_V1 * pkW
  default <- list(
    v1  = v1,
    v2  = DOPAMINE_K12 * v1 / DOPAMINE_K21,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = DOPAMINE_K10 * v1,
    cl2 = DOPAMINE_K12 * v1,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "MacGregor DA et al., Anesthesiology 2000;92:338-346. ",
    "Two compartments in 9 healthy men: k10 1.10, k12 0.244, k21 0.070 /min, ",
    "V1 0.241 L/kg (CL 0.265 L/kg/min, V2 0.840 L/kg).  Concentrations at the ",
    "same rate varied 10- to 75-fold; the endogenous baseline and the 0.5 min ",
    "lag are not represented. https://doi.org/10.1097/00000542-200002000-00013"
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
