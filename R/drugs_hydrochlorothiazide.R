# -----------------------------------------------------------------------------
# Hydrochlorothiazide (Van Wart 2013): apparent oral, plasma, renal covariate
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, from
# the full text and supplementary material of Van Wart SA, Shoaf SE,
# Mallikaarjun S, Mager DE, Population-based meta-analysis of
# hydrochlorothiazide pharmacokinetics, Biopharm Drug Dispos 2013;34:527-539
# (doi 10.1002/bdd.1863), which he supplied.  Tests:
# tests/testthat/test-drugs-hydrochlorothiazide.R.  See
# docs/antihypertensives.md sections 10.4 and 12.
#
# WHAT IS PLOTTED
# ===============
# Hydrochlorothiazide in PLASMA, ng/mL.  No blood pressure and no diuresis:
# this is the thiazide that the antihypertensive registry's own rule covers --
# no drug in this library plots a pressure -- and the source fitted no
# pharmacodynamic model at all.
#
# NOT PLOTTED, THOUGH THE SOURCE MODELS IT: the amount of drug excreted in
# urine.  Van Wart fits plasma and urine jointly (137 urinary amounts beside
# 823 plasma concentrations), and the paper's point is that the model predicts
# the rate of urinary excretion from dosing history and creatinine clearance.
# That is the excretion of the DRUG, not diuresis, and the library has no
# urinary-amount output to draw it in.  Leaving it out changes no plasma
# concentration: the urine compartment is an accumulator driven by renal
# clearance, a sub-partition of the elimination already in cl1, not a
# competing pathway.  A reader wanting urinary recovery has to integrate
# CL_R x Cp themselves.
#
# THE SOURCE
# ==========
# A population meta-analysis of 26 published studies (Table 1) covering oral
# doses from 10 to 500 mg and the whole range of renal function: normal
# volunteers, hypertension, heart failure, and mild, moderate and severe
# renal impairment.  Two compartments, apparent (there is no bioavailability
# term in the model: the supplementary NONMEM control stream confirms it, so
# V1, V2, Q and both clearances are all V/F and CL/F), and a WEIBULL oral
# input.
#
# ITS UNIT OF ANALYSIS, which is unusual and worth stating: 78.6% of the
# digitized profiles are AGGREGATE plasma or urine curves, each entering as
# one "meta-individual" carrying the number of subjects behind it.  The fit
# rests on 823 plasma concentrations and 137 urinary amounts from 43
# individuals OR meta-individuals -- not from 43 people.  The reported
# between-subject variability is therefore between studies as much as between
# patients, and the typical curve is better determined than that variability
# would suggest.  Mean age 40.6 (17.9) y, weight 73.9 (6.9) kg, creatinine
# clearance 101 (31.5) mL/min.
#
# Table 2, every number verified from the full text and cross-checked against
# the control stream, and transcribed with its precision into
# inst/extdata/antihypertensiveParameters.csv:
#
#     V1/F        97.3 L     (SEM 10.5%)
#     V2/F        178 L      (SEM 41.1%)
#     Q/F         17.2 L/h   (SEM 2.79%)
#     CL_NR/F     2.44 L/h   (SEM 22.2%)
#     CL_R/F      18.3 x (CL_CR/120)^1.38 L/h  (SEM 12.1% and 8.65%)
#     ka          1.34 /h    (SEM 17%)   -- a Weibull SCALE, not a rate
#     gamma       2.84       (SEM 15.9%) -- the Weibull shape
#
# The micro constants from these reproduce the paper's own reported half-lives
# of 1.6 and 14.8 h to three figures, which is an internal consistency check
# the transcription had to pass.  One discrepancy is worth knowing: Table 2
# prints V1 = 97.3 L while the control stream's $THETA prints 97.5, with its
# other seven thetas matching Table 2 exactly.  The table is taken as the
# final estimate; 0.2% moves no curve.
#
# THE INPUT SHAPE, AND WHY IT IS DISCRETISED
# ==========================================
# The depot does not empty first order.  The source's input is
#
#     dA/dt = -(1 - exp(-(ka t)^gamma)) A,     gamma = 2.84
#
# so the depot's rate constant is TIME-VARYING, rising from zero as the dose
# sits in the gut.  Nothing in this package's extravascular path can represent
# that: `ka_PO` is a constant.  This is not a missing parameter, it is a
# missing input SHAPE, and the library's answer to that -- set by MDMA's
# published zero-order depot -- is to DISCRETISE the published shape and
# measure the cost, not to substitute a different one.
#
# So each oral dose is given as PULSES of the default oral form
# (`oralPulses`, R/oral-pulses.R, using the reserved default-form key this
# drug is the first to need), with the depot rate constant fixed at
#
#     ka_PO = 1.0 /h
#
# which is not a fitted number and not a round one chosen for tidiness.  The
# bracket multiplying A above is dimensionless and tends to 1 as t grows, and
# the source writes the equation in HOURS (its ka is 1.34 /h), so the depot's
# rate constant tends to exactly 1 /h: ONE PER HOUR IS THE WEIBULL'S OWN
# ASYMPTOTIC RATE CONSTANT, a property of the published model rather than of
# any fit to it.
#
# Fitting it freely instead gives 0.982, 0.993 and 0.996 /h at two, three and
# four pulses, converging there, which is the evidence that fixing it is not
# forcing anything.  What fixing it costs, at four pulses: the worst point on
# the curve moves from 0.92% to 1.02% of the peak and the peak itself from
# -0.04% to +0.06%.  A tenth of a percent is a good price for replacing a
# fitted number with a published one, so it is fixed and only the fractions
# and delays are fitted.
#
# The fractions and delays below were fitted to the PLASMA curve this package
# plots, pooled over the four renal functions the paper itself tabulates
# (creatinine clearances of 120, 80, 50 and 30 mL/min), each normalised by its
# own peak, against a numerical integration of the exact Weibull system.
# Pooling matters: one pulse set has to serve every patient, so it is fitted
# to all of them rather than to a typical one.  The resulting error is
# essentially FLAT across renal function, which is the evidence that the
# discretisation describes the release and has not absorbed a disposition
# difference.  The measured cost is in the test and in
# docs/antihypertensives.md section 12.
#
# What a reader should take from this: the pulses are not a product.
# Hydrochlorothiazide is an ordinary immediate-release tablet, and these
# fractions describe nothing about how it is made.  They are a numerical
# stand-in for a published curve, accurate to a fraction of a percent of the
# peak, and the dose you enter is the dose you would prescribe.
#
# THE RENAL COVARIATE
# ===================
# Renal clearance carries the whole covariate model, as a power of
# Cockcroft-Gault creatinine clearance over 120 mL/min, which is the
# estimator the source used (docs/adding-a-drug.md; R/renalFunction.R).  The
# Patient Profile's serum creatinine reaches this model because the signature
# names `creatinine`, put on the adult scale by adultEquivalentCreatinine()
# since Cockcroft-Gault was fitted in adults.  A BLANK creatinine field is an
# assumed normal value for the patient's age and sex, so renal impairment is
# then NOT represented -- and for this drug that matters more than for most,
# because renal function is the only covariate it has: the paper's RENAL
# clearance ladder runs 18.3, 10.5, 5.47 and 2.70 L/h at creatinine clearances
# of 120, 80, 50 and 30 mL/min, an almost sevenfold range.  TOTAL clearance
# over that same range falls about fourfold only, 20.7 to 5.14 L/h, because
# the non-renal 2.44 L/h does not move -- the two ratios are easy to confuse
# and only the second sets the half-life, which rises from 14.8 h to 29 and
# 42 h in moderate and severe impairment.
#
# Non-renal clearance is a constant with no covariate, and is the floor: even
# at an anuric creatinine clearance the drug still clears at 2.44 L/h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source fitted NO body-size covariate -- not weight, not body surface
# area -- so the 70 kg reference parameters are scaled by pkSizeFactors() with
# legacy factors of 1, which reproduces the published values exactly with the
# fat-free-mass switch off.  Weight does reach the model with the switch
# either way, through Cockcroft-Gault, which contains it; with the switch on
# the creatinine clearance is evaluated at the pharmacokinetic weight, as
# every other renal model in the library does it.
#
# NO EFFECT SITE, NO BAND, NO MEAC
# ================================
# tPeak = 0, so only the plasma concentration is drawn.  There is no
# equilibration constant in the source, no therapeutic plasma range for a
# thiazide, and no concentration-to-pressure relation anywhere in the paper.
#
# PREGNANCY
# =========
# No study of hydrochlorothiazide's pharmacokinetics in pregnancy was found
# (PubMed, searched 2026-10-19).  The drug is cleared mostly unchanged in the
# urine, by filtration and tubular secretion, and Van Wart's clearance is
# that renal clearance plus a small non-renal one.  With the Patient
# Profile's Pregnant field on and the serum creatinine left blank, the RENAL
# CLEARANCE itself, CL_R, is multiplied by PREGNANCY_GFR_FACTOR = 1.5
# (pregnancyGfrFactor(), R/pregnancy.R), the rise in glomerular filtration of
# pregnancy; the non-renal clearance, the volumes, Q and the absorption are
# unchanged.  At the reference creatinine clearance of 120 mL/min the total
# clearance rises by (18.3 x 1.5 + 2.44) / (18.3 + 2.44) = 1.44.  The factor
# multiplies CL_R and not the creatinine clearance it reads, because the
# power 1.38 describes the loss of renal function in renal disease, not the
# rise in filtration of pregnancy: 1.5 on the creatinine clearance would
# multiply CL_R by 1.5^1.38 = 1.75.  A measured creatinine is not multiplied:
# a pregnant woman's creatinine is already low, and the Cockcroft-Gault
# estimate made from it already carries the rise.  This is a physiological
# extrapolation with no hydrochlorothiazide data behind it.  The absorption
# pulses were fitted at creatinine clearances of 30 to 120 mL/min; at the
# faster clearance of a pregnant woman of 30 years and 70 kg (renal CL/F
# 25.5 L/h) they still meet the documented standard against the exact
# Weibull system, 0.40% over the curve and 0.008% at the peak, which the
# test file checks.  Late pregnancy is the reference state.  (Claude Code,
# 2026-10-19.)
#
# References
# ----------
# Van Wart SA, Shoaf SE, Mallikaarjun S, Mager DE.  Biopharm Drug Dispos
#   2013;34:527-539.  https://doi.org/10.1002/bdd.1863
# Ansari J, Carvalho B, Shafer SL, Flood P.  Anesth Analg 2016;122:786-804.
#   https://doi.org/10.1213/ANE.0000000000001143 (pregnancy: the rise in
#   glomerular filtration)
# -----------------------------------------------------------------------------

# Van Wart 2013, Table 2, as published.  Apparent (/F) throughout.
HCTZ_V1       <- 97.3    # L, apparent central volume
HCTZ_V2       <- 178     # L, apparent peripheral volume
HCTZ_Q        <- 17.2    # L/h, apparent distribution clearance
HCTZ_CL_NR    <- 2.44    # L/h, apparent non-renal clearance, no covariate
HCTZ_CL_R     <- 18.3    # L/h, apparent renal clearance at the reference
HCTZ_CL_R_EXP <- 1.38    # power on creatinine clearance / 120
HCTZ_CRCL_REF <- 120     # mL/min, Cockcroft-Gault, the reference

# The published Weibull input.  Kept here although the engine cannot use it
# directly, because the discretisation below has to be checked against it and
# a later reader must be able to find the shape it stands for.
HCTZ_WEIBULL_SCALE <- 1.34   # 1/h, the Weibull's ka: NOT a rate constant
HCTZ_WEIBULL_SHAPE <- 2.84   # gamma

# The discretisation (see the header).  ka is the Weibull's asymptotic rate
# constant, exactly; the fractions and delays are fitted to the plasma curve
# pooled over the paper's four renal functions.
HCTZ_KA <- 1.0           # 1/h, exact: the limit of 1 - exp(-(ka t)^gamma)

# Eight fractions, fitted MINIMAX: the objective is the WORST point of the
# plotted curve, not the sum of squares, because the worst point is what the
# standard limits.  Two things had to be right about the measurement, and the
# first attempt got both wrong (PR #532, found in review):
#
#   - the error peaks EXACTLY AT A PULSE TIME, where a lagged depot starts and
#     the approximation kinks.  A uniform grid steps over the kinks: a 0.02 h
#     grid reported 0.45% where the truth was 0.64%.  simulationTimeGrid() puts
#     a point at every dose time, so the app evaluates precisely there -- the
#     figure a user can see was the one not being measured.
#   - the delays MOVE during the fit, so no fixed grid is safe.  The objective
#     rebuilds its grid per candidate: a uniform backbone plus a neighbourhood
#     of that candidate's own pulse times, with the exact system integrated
#     once at h = 1/4000 and interpolated (it is smooth -- only the
#     approximation kinks).
#
# Both of MDMA's figures are held, each normalised by its own limit so the fit
# cannot buy one with the other: 0.53% over the curve and 0.01% at the peak.
# Six pulses cannot do it (0.59% on the curve even fitted optimally) and seven
# only just (0.52%, 3% of headroom).  Eight reaches 0.341% and 0.0064%, each
# about 64% of its limit.  They are a numerical stand-in for a published shape
# and describe nothing about how the tablet is made.
HCTZ_PULSE_FRACTION <- c(0.084971, 0.170064, 0.155185, 0.085600,
                         0.189206, 0.145831, 0.112379, 0.056764)
HCTZ_PULSE_DELAY    <- c(12.4524, 23.7416, 31.7669, 35.8427,
                         40.5300, 49.3181, 55.1718, 69.7187)   # minutes

#' Hydrochlorothiazide pharmacokinetics (apparent oral, plasma)
#'
#' Van Wart et al. (2013), a population meta-analysis across renal function.
#' Two compartments with a Weibull oral input, which this package discretises
#' into pulses of its default oral form; see the header of
#' \code{R/drugs_hydrochlorothiazide.R} for the shape it stands for and what
#' the stand-in costs.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM when \code{TRUE}, evaluate the creatinine-clearance term
#'   at the pharmacokinetic weight and scale the size-free parameters to
#'   fat-free mass
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex, which represents NO renal impairment
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   left blank, multiplies the renal clearance by 1.5, the rise in glomerular
#'   filtration of pregnancy (see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
hydrochlorothiazide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                creatinine = NULL, pregnant = FALSE)
{
  # No body-size covariate was fitted, so the published values stand with the
  # switch off and the package's own scaling applies with it on.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Cockcroft-Gault, the estimator the source used.  It already contains
  # weight, so the renal term carries no further size factor.
  crcl <- creatinineClearanceCG(pkW, age, sex,                      # mL/min
                                adultEquivalentCreatinine(creatinine, age, sex))
  # Pregnancy (see the header): the renal clearance itself, not the
  # creatinine clearance it reads.
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  clR <- HCTZ_CL_R * (crcl / HCTZ_CRCL_REF)^HCTZ_CL_R_EXP *         # L/h
    gfrPregnancy
  cl  <- clR + HCTZ_CL_NR * size$clearance                          # L/h

  default <- list(
    v1  = HCTZ_V1 * size$volume,
    v2  = HCTZ_V2 * size$volume,
    v3  = 1,                            # two compartments
    cl1 = cl / 60,                      # L/min
    cl2 = HCTZ_Q / 60 * size$clearance,
    cl3 = 0,
    # The depot each pulse is absorbed through.  Apparent parameters already
    # carry F, so the dose must not be multiplied by it a second time, and the
    # lag is carried by the pulse delays rather than by tlag_PO.
    ka_PO = HCTZ_KA / 60,               # 1/min
    bioavailability_PO = 1,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0      # no effect-site model published; plasma only
  MEAC  <- 0

  # No therapeutic plasma range exists for a thiazide, and the source fits no
  # pharmacodynamic model of any kind.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Van Wart SA et al., Biopharm Drug Dispos 2013;34:527-539. ",
    "Two-compartment apparent oral meta-analysis pooling 43 individuals and ",
    "meta-individuals across renal function: V1/F 97.3 L, V2/F 178 L, Q/F ",
    "17.2 L/h, non-renal CL/F 2.44 L/h and renal CL/F 18.3 x ",
    "(CLcr/120)^1.38 L/h, with Cockcroft-Gault creatinine clearance from the ",
    "entered serum creatinine; a blank field is an assumed normal value for ",
    "age and sex, which represents no renal impairment. The source's oral ",
    "input is a WEIBULL function (scale 1.34 /h, shape 2.84), a shape this ",
    "package's first-order depot cannot represent; it is discretised into ",
    "eight",
    " lagged fractions absorbed at 1.0 /h, the Weibull's own asymptotic rate ",
    "constant, which reproduces a numerical integration of the exact system ",
    "to 0.34% of the peak, and 0.006% at the peak itself, at every renal function the paper ",
    "tabulates. Plasma. No blood pressure, no diuresis and no therapeutic ",
    "range are predicted. https://doi.org/10.1002/bdd.1863"
  )
  if (gfrPregnancy != 1) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("renal clearance x ", PREGNANCY_GFR_FACTOR,
             ", the rise in glomerular filtration; no hydrochlorothiazide data")))
  }

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Each "mg PO" dose becomes these fractions at these delays, each
      # absorbed through the depot above (R/oral-pulses.R).  The reserved
      # default-form key: hydrochlorothiazide has one oral product and it
      # names no formulation.
      oralPulses = stats::setNames(
        list(list(fraction = HCTZ_PULSE_FRACTION,
                  delay    = HCTZ_PULSE_DELAY)),
        ORAL_PULSE_DEFAULT_FORM)
    )
  )
}
