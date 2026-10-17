# -----------------------------------------------------------------------------
# Azilsartan, the active moiety of azilsartan medoxomil (Edarbi)
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# the full text of Tsai MC, Wu J, Kupfer S, Vakilynejad M, J Clin Pharmacol
# 2016;56:988-998, supplied by him after docs/antihypertensives.md section 9
# named it the single highest-value source still missing.  Tests:
# tests/testthat/test-drugs-azilsartan.R.
#
# WHAT THE SOURCE IS
# ==================
# A population analysis of 1481 subjects in an eight-week phase 3 factorial
# trial of azilsartan medoxomil and chlorthalidone in moderate to severe
# essential hypertension, 1201 of them contributing 5432 azilsartan
# concentrations.  Two compartments, first-order absorption and elimination,
# fitted simultaneously with a one-compartment model for the inactive
# metabolite M-II.  Table 1, every number of which is transcribed into
# inst/extdata/antihypertensiveParameters.csv with its precision.
#
# THE DOSE BASIS, WHICH THE PAPER NEVER STATES
# ============================================
# Azilsartan medoxomil is a prodrug, hydrolysed during absorption to
# azilsartan.  The model is of azilsartan, and prescriptions are written in
# milligrams of the medoxomil.  Nothing in the paper says which mass the
# model's doses are in -- and the difference is 20%, which is not a rounding.
#
# Its own simulated exposures settle it.  The paper prints mean steady-state
# AUC values of 10919, 21675 and 42606 ng*h/mL for 20, 40 and 80 mg.  For a
# linear model AUC is dose over clearance, so the dose the model received is
# AUC x CL/F:
#
#     20 mg AZL-M  ->  10919 x 1.47 = 16.05 mg entered the model
#     20 x 456.45/568.52                 = 16.06 mg of azilsartan
#
# a match to 0.04%.  The doses are therefore in AZILSARTAN, converted from the
# medoxomil by molecular weight.  The check also tells us WHICH medoxomil:
# the marketed salt is azilsartan medoxomil potassium, and had the strengths
# been of the salt the ratio would be 456.45/606.62 = 0.752, which does not
# fit.  The 40 and 80 mg points imply 0.797 and 0.783 rather than 0.803,
# drift that is expected because those AUCs are population summaries over the
# covariate distribution rather than the typical-value AUC; the 20 mg point,
# where the drifting term is smallest, is the one that pins the constant.
#
# A dose here is entered as the medoxomil, the way it is prescribed.  So every
# apparent volume and clearance below is DIVIDED by that ratio, which puts
# them on a per-milligram-of-medoxomil basis and leaves every rate constant,
# half-life and time to peak exactly as published.  The printed numbers
# therefore differ from Table 1 by design; the registry holds Table 1's own
# values, and the test pins both.
#
# THE METABOLITE IS NOT SIMULATED
# ===============================
# M-II is fitted in the same model (CL/F 1.86 L/h, V/F 14.6 L, Fm fixed at
# 0.25) and the paper calls it virtually inactive, then pharmacologically
# inactive.  Its parameters are transcribed into the registry and deliberately
# not plotted: an inactive metabolite adds a curve that means nothing at the
# effect site, and the library's metabolite engine exists for active ones.
#
# WHERE THIS MODEL IS STRONG, AND WHERE IT IS AN EXTRAPOLATION
# ============================================================
# Checked against the label rather than against the paper, this model gives a
# time to peak of 3.0 h, at the top of the labelled 1.5 to 3 h, and a terminal
# half-life of 26 h against a labelled 11 to 12 h -- more than twice as long.
#
# That is not a transcription error, it is the shape of the data.  Sampling
# was sparse and inside one dosing interval: troughs at weeks 4 and 6, one
# sample 1 to 4 h after a dose and two more 4 to 12 h after.  Nothing in the
# dataset observes the drug beyond 24 h, which is why the authors FIXED Q and
# V2/F rather than estimating them -- and those two fixed numbers are exactly
# what sets the terminal slope.  The 26 h is therefore an extrapolation from
# fixed parameters, not a fitted half-life.
#
# So the curve is trustworthy where it was fitted, over a steady-state dosing
# interval, which is where it reproduces all of the paper's own exposures; its
# tail is not evidence about azilsartan's terminal phase.  Read accumulation
# and washout beyond a day with that in mind.
#
# WHAT IS NOT MODELLED
# ====================
# No blood pressure.  The same paper fits an Emax model of ambulatory systolic
# pressure against AUC, and that record is in the registry as a catalogued
# endpoint, not a curve: it is driven by steady-state AUC rather than by a
# concentration, its Emax differs between black and non-black subjects, and
# its EC50 could not be estimated with precision (the 95% interval printed for
# azilsartan is 558 to 6570 ng*h/mL).  No antihypertensive in this library
# plots a pressure; see docs/antihypertensives.md.
# -----------------------------------------------------------------------------

# Tsai 2016, Table 1, as published: per milligram of AZILSARTAN
AZILSARTAN_CL_F      <- 1.47     # L/h, apparent oral clearance
AZILSARTAN_V1_F      <- 3.98     # L, apparent central volume
AZILSARTAN_V2_F      <- 15.5     # L, apparent peripheral volume (fixed)
AZILSARTAN_Q_F       <- 0.591    # L/h, apparent intercompartmental (fixed)
AZILSARTAN_KA        <- 0.195    # 1/h, first-order absorption
AZILSARTAN_AGE_CL    <- -0.0149  # L/h per year, about age 58
AZILSARTAN_WT_CL     <- 0.267    # exponent on weight / 86 kg
AZILSARTAN_AGE_REF   <- 58       # years, population median
AZILSARTAN_WEIGHT_REF <- 86      # kg, population median

# Molecular weights, for the dose basis derived in the header
AZILSARTAN_MW           <- 456.45   # g/mol, azilsartan
AZILSARTAN_MEDOXOMIL_MW <- 568.52   # g/mol, azilsartan medoxomil
AZILSARTAN_MOIETY <- AZILSARTAN_MW / AZILSARTAN_MEDOXOMIL_MW

#' Azilsartan pharmacokinetics (oral, apparent, dosed as the medoxomil)
#'
#' Tsai et al. (2016), two compartments with first-order absorption in 1481
#' hypertensive subjects; see the header for the dose basis, which the paper
#' does not state and which its own simulated exposures settle.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
azilsartan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source's own size term is a power of total weight on clearance only,
  # with no weight term on either volume; it is reproduced exactly with the
  # switch off (docs/weight-adjustment.md).
  size <- pkSizeFactors(
    weight, height, age, sex, adjustToFFM,
    legacyVolume    = 1,
    legacyClearance = (weight / AZILSARTAN_WEIGHT_REF)^AZILSARTAN_WT_CL)

  # CL/F = (1.47 - 0.0149 x [Age - 58]) x (Weight/86)^0.267, Tsai 2016 p991.
  # The age term is linear and the paper fitted it between 22 and 87 years;
  # it stays positive to well past any human age, so it needs no floor.
  clF <- AZILSARTAN_CL_F + AZILSARTAN_AGE_CL * (age - AZILSARTAN_AGE_REF)

  # Per milligram of azilsartan medoxomil as dosed (see the header).  Rate
  # constants are unchanged by this, since every volume and clearance moves
  # together.
  v1  <- AZILSARTAN_V1_F / AZILSARTAN_MOIETY * size$volume
  v2  <- AZILSARTAN_V2_F / AZILSARTAN_MOIETY * size$volume
  cl1 <- clF            / AZILSARTAN_MOIETY * size$clearance / 60   # L/min
  cl2 <- AZILSARTAN_Q_F / AZILSARTAN_MOIETY * size$clearance / 60

  default <- list(
    v1  = v1,
    v2  = v2,
    v3  = 1,                     # two compartments
    cl1 = cl1,
    cl2 = cl2,
    cl3 = 0,
    ka_PO = AZILSARTAN_KA / 60,  # 1/min
    # Apparent parameters already carry F: the dose must not be multiplied by
    # it a second time (docs/antihypertensives.md).
    bioavailability_PO = 1,
    tlag_PO = 0                  # no lag was fitted
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no effect-site model published; plasma only
  MEAC  <- 0

  # No therapeutic plasma range: the source's exposure-response is driven by
  # steady-state AUC, not by a concentration, so no band is defensible.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Tsai MC et al., J Clin Pharmacol 2016;56:988-998. ",
    "Two-compartment apparent oral model of AZILSARTAN in 1481 subjects ",
    "with stage 2 hypertension: CL/F 1.47 L/h, V1/F 3.98 L, with Q/F 0.591 ",
    "L/h and V2/F 15.5 L fixed, and first-order absorption at 0.195 /h. ",
    "CL/F = (1.47 - 0.0149 x [age - 58]) x (weight/86)^0.267. Doses are ",
    "entered as azilsartan MEDOXOMIL, the prodrug as prescribed, and ",
    "converted to azilsartan by molecular weight (456.45/568.52); the ",
    "paper does not state its dose basis, but its own simulated exposures ",
    "do. The inactive metabolite M-II is fitted in the source and is not ",
    "plotted. No blood pressure is predicted. ",
    "https://doi.org/10.1002/jcph.684"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
