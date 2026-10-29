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
# EFFECT SITE: tPeak 1.2 MINUTES (Cheung 1999); NO BAND
# =====================================================
# Modi modelled heart rate and mean arterial pressure through conduction
# functions, not an effect site, and reports no time to peak.  Cheung AT,
# Guvakov DV, Weiss SJ, et al., Anesth Analg 1999;89:1116-1123 (read in full;
# its pharmacokinetics are unusable, see the registry) gave 0.25 to 2 mg over
# 5 seconds into the right atrium of 40 anaesthetised cardiac surgical
# patients and read the radial arterial pressure every 15 seconds: the
# largest fall in MAP came 66, 63, 56 and 101 s after the four doses, a mean
# of 72 s, which is the tPeak here (ke0 2.9 /min on this plasma model).
# Cheung DG, Gasster JL, Neutel JM, Weber MA, Am Heart J 1990;119:438-442
# (doi 10.1016/s0002-8703(05)80065-1, read in full), in 11 awake
# hypertensive patients given 0.125 to 7 mg over 2 minutes, put the largest
# fall 2.5 +/- 0.6 minutes after the end of the injection; with this ke0 a
# 2-minute injection peaks 0.6 minutes after its end.  Cheung 1990 read the
# pressure by cuff at 1-minute intervals, so its peak is the less precise of
# the two, and is kept as evidence the peak may lie later in awake patients
# (on this plasma model it implies a bolus tPeak of about 3 minutes).
# -----------------------------------------------------------------------------

# Modi 1993, Table I, means
NICARDIPINE_CL  <- 0.920   # L/h/kg
NICARDIPINE_V1  <- 0.291   # L/kg
NICARDIPINE_VSS <- 1.13    # L/kg
NICARDIPINE_Q   <- 0.826   # L/h/kg
NICARDIPINE_TPEAK <- 1.2   # min, Cheung 1999 (72 s to the largest fall in MAP)

#' Nicardipine, intravenous (Modi 1993)
#'
#' Two compartments per kilogram from the mean disposition parameters of
#' seven healthy men, with the time to peak effect from Cheung 1999; see the
#' header of \code{R/drugs_nicardipine.R}.
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
    "as nicardipine hydrochloride.  Time to peak effect 1.2 min (72 s to the ",
    "largest fall in MAP) from Cheung AT et al., Anesth Analg 1999;89:1116-1123. ",
    "https://doi.org/10.1002/jps.2600820707"
  )

  list(
    PK = PK,
    tPeak = NICARDIPINE_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
