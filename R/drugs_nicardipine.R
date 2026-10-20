# -----------------------------------------------------------------------------
# Nicardipine, intravenous (Modi 1993): healthy men
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv), from
# the full text, which he supplied.
#
# THE SOURCE
# ==========
# Modi NB, Veng-Pedersen P, Graham DJ, Dow RJ.  Application of a system
# analysis approach to population pharmacokinetics and pharmacodynamics of
# nicardipine hydrochloride in healthy males.  J Pharm Sci 1993;82:705-713
# (doi 10.1002/jps.2600820707).  Seven healthy men (19-28 years, 58.8-87.5
# kg) given 30-minute infusions of 70, 140, 210 and 280 mcg/kg nicardipine
# hydrochloride, a week or more apart, venous plasma by HPLC for 24.5 hours;
# disposition linear over the doses.  Each man's unit impulse response was
# biexponential, and Table I gives, per subject and as means, the
# disposition parameters it implies:
#
#     CL  0.920 L/h/kg     V (central) 0.291 L/kg     Vss 1.13 L/kg
#     "C_d" 0.826 L/h/kg   mean residence times 1.27 h (body), 0.324 h
#     (sampling compartment), 0.950 h (periphery); distribution time 0.457 h
#
# The row C_d is the intercompartmental (distribution) clearance: in every
# one of the seven subjects, the two-compartment model built from CL, V,
# Vss - V and C_d reproduces that subject's printed distribution time (the
# peak of the peripheral amount after a bolus) to the third decimal, which
# the row printed above it (1.75 L/h/kg) does not.  The subjects also obey
# V = CL x MRT(sampling) and Vss = CL x MRT(body) to rounding, except
# subject 3, whose clearance is printed 0.833 L/h/kg: at 0.883 that subject
# satisfies both identities and the distribution time (0.157 h), and the
# seven clearances average the printed 0.920 (0.833 gives 0.914), so 0.833 is
# a transposition.  The model uses the printed means, which already reflect
# the corrected value.
#
# THE TYPICAL PATIENT
# ===================
# The printed means of the four parameters: CL 0.920, V1 0.291, V2 = 1.13 -
# 0.291 = 0.839 L/kg, Q 0.826 L/h/kg, the mean-of-individuals approach of
# verapamil and phenylephrine.  Half-lives 6.4 minutes and 1.45 hours, and a
# distribution time of 0.43 h against the subjects' mean 0.457.
# The doses are the hydrochloride, as the injection is labelled.
#
# HOW FAR TO TRUST IT
# ===================
# Seven young men.  The authors note that other studies found clearances of
# 0.31 to 0.91 L/h/kg (Graham 1985, 0.494), so this model sits at the fast
# end: a patient may well reach twice the concentration plotted.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Per kilogram, as published: total weight with the switch off, the
# pharmacokinetic weight with it on.
#
# NO EFFECT SITE, NO BAND
# =======================
# The source modelled heart rate and mean arterial pressure through
# conduction functions, not an effect site, and reports no time to peak
# effect after a bolus; plasma only.
# -----------------------------------------------------------------------------

# Modi 1993, Table I, means
NICARDIPINE_CL  <- 0.920   # L/h/kg
NICARDIPINE_V1  <- 0.291   # L/kg
NICARDIPINE_VSS <- 1.13    # L/kg
NICARDIPINE_Q   <- 0.826   # L/h/kg

#' Nicardipine, intravenous (Modi 1993)
#'
#' Two compartments per kilogram from the mean disposition parameters of
#' seven healthy men; see the header of \code{R/drugs_nicardipine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
nicardipine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = NICARDIPINE_V1 * pkW,
    v2  = (NICARDIPINE_VSS - NICARDIPINE_V1) * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = NICARDIPINE_CL / 60 * pkW,
    cl2 = NICARDIPINE_Q / 60 * pkW,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Modi NB, Veng-Pedersen P, Graham DJ, Dow RJ, J Pharm Sci 1993;82:705-713. ",
    "Two compartments from Table I's means in 7 healthy men: CL 0.920 L/h/kg, ",
    "V1 0.291 L/kg, Vss 1.13 L/kg, distribution clearance 0.826 L/h/kg; doses ",
    "as nicardipine hydrochloride. https://doi.org/10.1002/jps.2600820707"
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
