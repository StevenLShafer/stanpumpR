# -----------------------------------------------------------------------------
# Valsartan (Diovan)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, from
# the full text and supplementary material of Heo YA, Holford N, Kim Y, Son M,
# Park K, Br J Clin Pharmacol 2016;82:1557-1567, which he supplied.  Tests:
# tests/testthat/test-drugs-valsartan.R.
#
# WHAT THE SOURCE IS
# ==================
# A population analysis of 624 valsartan concentrations from 48 healthy Korean
# subjects (age 29 +/- 6 y, weight 68.7 +/- 7.6 kg) given a single fixed-dose
# combination tablet of valsartan 160 mg with amlodipine 10 mg, in a crossover
# of two formulations with a 14-day washout, sampled to 48 hours.  The
# pharmacokinetic half of a paper whose purpose was the blood-pressure
# interaction between the two drugs.  Supplementary Table S4 is transcribed
# into inst/extdata/antihypertensiveParameters.csv with its relative standard
# errors.
#
# ZERO-ORDER ABSORPTION, FITTED AGAINST FIRST ORDER AND WINNING
# =============================================================
# Two compartments with first-order elimination and a ZERO-ORDER input of
# duration D1, which the authors chose on the data rather than by habit: the
# objective function was 7712.434 for zero-order absorption against 7874.858
# for first-order, a difference of 162 on the same number of parameters.  So
# the absorption is declared as an `oralZeroOrder` release (R/oral-zero-order.R,
# built for carvedilol, whose source rejected first-order absorption for the
# same kind of reason) and each oral dose is rewritten into the constant-rate
# rows that deliver it over D1.  There is no ka and no bioavailability term:
# both are inside the apparent scale.
#
# The practical consequence is that the peak is at the END of the release, at
# exactly 4.39 hours, and not at a time traded off between absorption and
# distribution.  The label's 2 to 4 hours is a little earlier, and the authors
# say themselves that "the median peak concentration of both drugs was somewhat
# underpredicted" by this model.  The curve is therefore a fair description of
# valsartan's exposure and a slightly blunt one of its peak.
#
# WEIGHT IS THE ONLY COVARIATE, AND THE AUTHORS LOOKED FOR OTHERS
# ===============================================================
# The paper screened age, serum creatinine, smoking and drinking status by
# stepwise covariate modelling and reports that "apart from body weight, no
# other covariate factors were found to be significant (P > 0.001)".  So this
# model has NO renal term, which is worth knowing for a drug given to patients
# whose kidneys are the reason they are taking it: the population was 48
# healthy young adults with a mean creatinine of 0.81 mg/dL, and it contains no
# information about renal impairment at all.  A creatinine entered in the
# Patient Profile does not reach this drug, and the signature deliberately does
# not name it.
#
# Size is theory-based allometry at a 70 kg standard (the paper's Equations 1
# and 2): weight^0.75 on CL/F and Q/F, weight^1 on V1/F and V2/F.  That is
# exactly the library's own scaling, so the switch off reproduces the published
# model and the switch on applies the same exponents to fat-free mass
# (docs/weight-adjustment.md).  D1 is NOT scaled: the paper's two size
# equations name clearance and volume only, and a release duration is not a
# size-dependent quantity.
#
# EVERY PARAMETER IS APPARENT IN F
# ================================
# Oral data only, so CL/F, V1/F, Q/F and V2/F are apparent.  No intravenous
# unit is offered.
#
# WHY NOT YANG 2022
# =================
# Yang et al. (Clin Pharmacol Drug Dev 2022;11:1181) fitted valsartan in
# healthy Chinese subjects and is the more recent paper, with a larger
# population.  It is NOT used, and the reason is specific rather than a
# preference: its printed parameters do not reproduce its own figures.
# Simulating the published model gives a time to peak of 7.49 h and a peak of
# 0.99 mcg/mL against the 2 to 2.5 h and roughly 2.8 mcg/mL of its own
# Figure 1 visual predictive check; the area under the curve, 20.3, does match
# the figure's 20, so the clearance is sound and the shape is not.  Moving the
# decimal point in its Ka (1.9 for 0.19) fixes the time to peak exactly, at
# 1.96 h, and still leaves the peak 1.6-fold low, so at least two printed
# parameters are irreconcilable and nothing in the paper says which.  Checked
# with the closed-form solution and again with an independent Runge-Kutta
# integration.  The paper also prints a V2/F of 0.51 L with a 26-fold
# confidence interval on CL2/F, describes a BMI allometry its parameter table
# does not contain, and studied sacubitril/valsartan rather than conventional
# valsartan.  Recorded in the registry as `requires_parameter_fit`.
#
# NO EFFECT SITE, NO BLOOD PRESSURE, NO BAND
# ==========================================
# The source DID fit a blood-pressure model -- an effect compartment with an
# Imax of 0.164 -- and this library still does not plot a pressure
# (docs/antihypertensives.md).  The relation is recorded in the registry as an
# endpoint record and reachable through antihypertensiveEndpoint(), which
# returns the change the drug term predicts and never an absolute pressure.
# tPeak, MEAC and the band are zero here.
# -----------------------------------------------------------------------------

# Heo 2016 Supplementary Table S4, valsartan column, with relative standard
# errors.  All four disposition parameters are at the paper's 70 kg standard.
VALSARTAN_CL_F  <- 6.18    # L/h  per 70 kg   (5.76% RSE)
VALSARTAN_V1_F  <- 25.9    # L    per 70 kg   (9.73% RSE)
VALSARTAN_Q_F   <- 2.01    # L/h  per 70 kg   (14.5% RSE)
VALSARTAN_V2_F  <- 17.4    # L    per 70 kg   (16.0% RSE)
VALSARTAN_D1_H  <- 4.39    # h, zero-order release duration (5.83% RSE)

# The paper's own allometric exponents (Equations 1 and 2), which are also the
# library's; named here so the drug does not carry a bare 0.75.
VALSARTAN_CL_EXP <- 0.75

#' Valsartan pharmacokinetics (oral, apparent, zero-order release)
#'
#' Heo et al. (2016), two compartments with a zero-order oral release over
#' 4.39 h in 48 healthy subjects, with body weight as the only covariate; see
#' the file header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
valsartan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source's own size model is theory-based allometry at a 70 kg standard,
  # which is the library's: weight^0.75 on the clearances and weight on the
  # volumes.  So the legacy factors are exactly those, and the switch
  # reproduces the published model when it is off (docs/weight-adjustment.md).
  size <- pkSizeFactors(
    weight, height, age, sex, adjustToFFM,
    legacyVolume    = weight / FFM_REFERENCE_WEIGHT,
    legacyClearance = (weight / FFM_REFERENCE_WEIGHT)^VALSARTAN_CL_EXP)

  default <- list(
    v1  = VALSARTAN_V1_F * size$volume,
    v2  = VALSARTAN_V2_F * size$volume,
    v3  = 1,                                      # two compartments
    cl1 = VALSARTAN_CL_F / 60 * size$clearance,   # L/h -> L/min
    cl2 = VALSARTAN_Q_F  / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # the source's effect compartment is for blood pressure, which
  MEAC  <- 0    # this library does not plot; see the header

  # No established therapeutic plasma range for valsartan.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Heo YA, Holford N, Kim Y, Son M, Park K. Quantitative model for the ",
    "blood pressure-lowering interaction of valsartan and amlodipine. ",
    "Br J Clin Pharmacol 2016;82:1557-67. Two compartments, apparent in ",
    "bioavailability, from 624 concentrations in 48 healthy subjects given a ",
    "single 160 mg valsartan with 10 mg amlodipine: CL/F 6.18 L/h, V1/F ",
    "25.9 L, Q/F 2.01 L/h, V2/F 17.4 L at 70 kg, with theory-based allometry ",
    "(weight^0.75 on the clearances, weight on the volumes). ABSORPTION IS ",
    "ZERO ORDER over 4.39 h, which the source chose over first order on an ",
    "objective function 162 lower, so the peak is at the end of the release. ",
    "Body weight was the ONLY significant covariate: age, serum creatinine, ",
    "smoking and drinking were screened and rejected, so this model carries ",
    "no renal term and its population of 48 healthy young adults contains no ",
    "information about renal impairment. The authors report that the model ",
    "somewhat underpredicts the median peak concentration. ",
    "https://doi.org/10.1111/bcp.13069"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      # The release the engine delivers each oral dose through
      # (R/oral-zero-order.R), in minutes.  No lag was fitted.
      oralZeroOrder = list(duration = VALSARTAN_D1_H * 60, lag = 0),
      reference = reference
    )
  )
}
