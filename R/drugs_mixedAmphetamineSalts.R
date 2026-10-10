# -----------------------------------------------------------------------------
# Mixed amphetamine salts (Adderall IR and Adderall XR): d-amphetamine in
# children
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
#
# WHAT IS PLOTTED
# ===============
# Plasma d-AMPHETAMINE, in ng/mL, after an oral dose of Adderall entered as the
# labelled strength in mg of mixed amphetamine salts: "mg PO" for the
# immediate-release tablet, "mg PO XR" for the extended-release capsule.
# l-amphetamine is not plotted.  It is about a quarter of the base, its
# concentrations tracked d-amphetamine's almost exactly in the source
# (r = 0.997), and no behavioural measure has been shown to depend on it
# separately; its values are given below and in the help page.
#
# NO PUBLISHED POPULATION MODEL: THIS ONE IS FITTED HERE
# ======================================================
# No population pharmacokinetic model of Adderall itself has been published
# with its parameters.  The FDA's Mydayis (SHP465) analysis is a different
# product, and its input model does not transfer.  This model is a typical-
# patient curve fitted here to published group means, from three sources:
#
#   McGough JJ et al., J Am Acad Child Adolesc Psychiatry 2003;42:684-691
#     (doi 10.1097/01.CHI.0000046850.56865.CB).  51 children with ADHD, 6-12 y,
#     mean 37.8 kg (21.7-73.0), 86% boys.  Single 20 mg Adderall XR, sampled
#     0.5-24 h, d-amphetamine by enantioselective LC-MS/MS, non-compartmental:
#     Cmax 48.8 ng/mL, Tmax 6.8 h, AUC0-24 703.9, AUC0-inf 936.7 ng.h/mL.
#     Then one week once-daily: steady state on Adderall 10 mg (n = 9) and
#     Adderall XR 10, 20 and 30 mg (n = 8, 9, 6), Table 3.  Errors are SEM.
#   Adderall XR label (NDA 021303, s026): base equivalence per strength;
#     d-amphetamine half-life 9 h in children 6-12 (10 h adults, 11 h
#     adolescents); XR 20 mg once daily comparable to Adderall 10 mg twice,
#     4 h apart; a high-fat meal delays Tmax 2.5 h without changing extent.
#   Adderall label (NDA 011522, s043): base equivalence per tablet; IR peak
#     about 3 h for both isomers; dose-proportional 10 to 30 mg.
#
# DOSE BASIS: d-AMPHETAMINE BASE IN THE MIXED SALTS
# =================================================
# Each strength is equal masses of four salts: dextroamphetamine saccharate,
# amphetamine aspartate monohydrate (racemic), dextroamphetamine sulfate, and
# amphetamine sulfate (racemic).  From their formula weights (below), 20 mg
# contains 12.51 mg of amphetamine base, the "total amphetamine base
# equivalence" of 12.5 mg the XR label prints (12.6 mg on the IR label), of
# which 9.50 mg is d and 3.02 mg l (d:l 3.15, the labels' "3:1").  The
# d-amphetamine fraction, 0.4749 mg per mg of salts, is applied exactly once,
# as bioavailability_PO.  It is a dose basis, not an oral bioavailability: the
# apparent scale (CL/F, V/F) already carries that.
#
# DISPOSITION
# ===========
# One compartment.  At McGough's mean weight of 37.8 kg:
#   CL/F = 9.498 mg / 936.7 ng.h/mL = 10.14 L/h, McGough's single-dose
#          AUC0-inf.  The steady-state groups agree (dose / AUCtau:
#          IR 10 mg 11.2, XR 10 mg 11.0, XR 20 mg 12.2, XR 30 mg 10.4 L/h).
#   V/F  = CL/F x 9 h / ln 2 = 131.7 L, from the label's 9 h half-life in
#          children 6-12.  (A free fit to McGough's XR summary gives 150 L;
#          Tsuda's lisdexamfetamine model gives 144 L at this weight.)
#
# Weight: McGough fitted no weight model (race, age and BMI predicted AUC in a
# regression, not a covariate model).  The weight exponents are BORROWED from
# Tsuda 2020's pediatric d-amphetamine model (R/drugs_lisdexamfetamine.R),
# CL/F x (W/37.8)^0.600 and V/F x (W/37.8)^0.776, on the reasoning that
# disposition belongs to d-amphetamine, not to the product.  That is an
# assumption, and it is the weakest part of this file outside the 6-12 y,
# 22-73 kg range McGough studied.  As for the other self-scaled models
# (docs/weight-adjustment.md), the weight is the pharmacokinetic weight with
# the switch on and total body weight with it off.
#
# ABSORPTION AND RELEASE
# ======================
# Immediate release: first-order absorption, ka = 0.689 /h, no lag, fitted so
# that the steady-state peak after Adderall 10 mg once daily falls at McGough's
# 3.3 h.  (The label gives about 3 h after a single dose in adults.)
#
# Extended release: the XR capsule holds two bead populations, about half
# released at once and half later.  It is represented as two pulses, half the
# dose at the time given and half 4 h later, each absorbed as the immediate-
# release tablet (oralPulses, R/oral-pulses.R).  The 4 h was not imposed: a
# delay fitted to McGough's XR Tmax of 6.8 h came out at 4.02 h, matching the
# label's "two doses 4 h apart" comparison, and 4 h is used.  The real release
# is not two instantaneous pulses (Watanalumlerd 2007), so the first hour or
# so after each pulse is the least reliable part of the XR curve.
#
# CHECKS AGAINST McGOUGH (model / observed, group means)
#   XR 20 mg single dose: Cmax 50.5 / 48.8 ng/mL, Tmax 6.8 / 6.8 h,
#     AUC0-24 741 / 704 ng.h/mL
#   IR 10 mg steady state: Cmax 33.2 / 33.8, Tmax 3.3 / 3.3
#   XR 30 mg steady state: Cmax 91.8 / 89.0
# The steady-state groups are 6 to 9 children each, not the 48 of the
# single-dose analysis, and a typical-patient curve is not a mean of
# individual peaks; tests/testthat/test-drugs-mixedAmphetamineSalts.R holds
# these within 10%.
#
# l-AMPHETAMINE (NOT PLOTTED)
# ===========================
# 3.015 mg per 20 mg; McGough AUC0-inf 309.0 ng.h/mL gives CL/F 9.76 L/h, and
# the label's 11 h half-life in children V/F 155 L.  Its curve would be about
# a third of d-amphetamine's, peaking a little later.
#
# NOT REPRESENTED
# ===============
# Food (a high-fat meal delays the XR peak about 2.5 h; McGough's subjects were
# dosed at 7:30 with the laboratory's schedule of meals), urinary pH (alkaline
# urine slows and acid urine speeds renal elimination of amphetamine; Wan
# 1978), and CYP2D6.  Interindividual variability is large (McGough: CV 28-56%
# across parameters) and is not simulated; stanpumpR plots the typical
# patient.  Children 6-12 only: adolescents and adults are extrapolations of
# the borrowed weight equations, and the label gives longer half-lives in both.
#
# NO EFFECT SITE, NO THERAPEUTIC BAND
# ===================================
# McGough found no clear relationship between concentration and behavioural
# response, and no calibrated concentration-effect model exists for a
# classroom or weekly endpoint.  tPeak, MEAC and the band are zero.  A UK
# regulatory summary mentions a d-amphetamine EC50 of 24-28 ng/mL from an
# incomplete sponsor model; it is not used.
# -----------------------------------------------------------------------------

# Formula weights (g/mol) of the four salts and of amphetamine base.
AMPHETAMINE_MW <- 135.21
AMPHETAMINE_SALTS <- list(
  # name = c(formula weight, amphetamine molecules per formula, d fraction)
  dextroamphetamineSaccharate = c(2 * AMPHETAMINE_MW + 210.14, 2, 1),
  amphetamineAspartateH2O     = c(AMPHETAMINE_MW + 133.10 + 18.02, 1, 0.5),
  dextroamphetamineSulfate    = c(2 * AMPHETAMINE_MW + 98.08, 2, 1),
  amphetamineSulfate          = c(2 * AMPHETAMINE_MW + 98.08, 2, 0.5)
)

# Base per mg of mixed salts (equal masses of the four), by isomer.
amphetamineBaseFraction <- function(isomer = c("d", "l"))
{
  isomer <- match.arg(isomer)
  sum(vapply(AMPHETAMINE_SALTS, function(s) {
    base <- s[2] * AMPHETAMINE_MW / s[1]
    base * if (isomer == "d") s[3] else 1 - s[3]
  }, numeric(1))) / length(AMPHETAMINE_SALTS)
}

# The fitted and source values, at McGough's mean weight (see the header).
MAS_REFERENCE_WEIGHT <- 37.8                       # kg
MAS_CL_REF  <- 20 * amphetamineBaseFraction("d") / 936.7 * 1000   # L/h
MAS_V_REF   <- MAS_CL_REF * 9 / log(2)             # L, label t1/2 9 h
MAS_KA      <- 0.689                               # /h, IR steady-state Tmax 3.3 h
MAS_XR_DELAY <- 4                                  # h, second bead population

#' Mixed amphetamine salts (Adderall) pharmacokinetics
#'
#' Plasma d-amphetamine after oral Adderall (immediate release, "mg PO") or
#' Adderall XR ("mg PO XR") in children, entered as mg of mixed salts. Fitted
#' to McGough 2003 and the Adderall labels; oral only.
#'
#' @param weight weight in kg
#' @param height height in cm (used only for fat-free mass)
#' @param age age in years (used only for fat-free mass)
#' @param sex sex as a string (used only for fat-free mass)
#' @param adjustToFFM evaluate the weight equations at the pharmacokinetic
#'   weight (TRUE) or at total body weight (FALSE)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mixedAmphetamineSalts <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Borrowed weight equations (header): pharmacokinetic weight with the switch
  # on, total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1  <- MAS_V_REF  * (pkW / MAS_REFERENCE_WEIGHT)^0.776        # L, V/F
  cl1 <- MAS_CL_REF * (pkW / MAS_REFERENCE_WEIGHT)^0.600 / 60   # L/min, CL/F
  v2  <- 1                                   # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO   <- MAS_KA / 60                     # 1/min
  tlag_PO <- 0
  # The dose basis conversion, not an oral bioavailability (header)
  bioavailability_PO <- amphetamineBaseFraction("d")

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Fitted here (typical patient) to McGough JJ et al., J Am Acad Child ",
    "Adolesc Psychiatry 2003;42:684-691, ",
    "https://doi.org/10.1097/01.CHI.0000046850.56865.CB, and the Adderall XR ",
    "(NDA 021303) and Adderall (NDA 011522) labels: d-amphetamine in children ",
    "6-12; dose in mg of mixed salts converted to d-amphetamine base ",
    "(0.4749 mg/mg); XR as two half-doses 4 h apart; weight exponents borrowed ",
    "from Tsuda 2020"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,     # no calibrated effect model (header)
      MEAC = 0,
      typical = 0,   # no therapeutic band (header)
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference,
      # Adderall XR: half at once, half 4 h later, each absorbed as the tablet
      oralPulses = list(XR = list(fraction = c(0.5, 0.5),
                                  delay = c(0, MAS_XR_DELAY * 60)))
    )
  )
}
