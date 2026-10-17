# -----------------------------------------------------------------------------
# Carvedilol (Hwang 2023): apparent oral plasma concentration, with zero-order
# release
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer.  The
# parameters are read from the paper's Table 2 (full text from PubMed Central,
# PMC10244018), not from a summary.  Verified by
# tests/testthat/test-drugs-carvedilol.R.  See docs/antihypertensives.md.
#
# THE SOURCE
# ==========
# Hwang S, Lee S, Yoon J, Chung JY.  Population pharmacokinetic-pharmacodynamic
# modeling of carvedilol to evaluate the effect of cytochrome P450 2D6 genotype
# on the heart rate reduction.  J Korean Med Sci 2023;38(22):e173
# (doi 10.3346/jkms.2023.38.e173).  Twenty-one healthy Korean men, mean age
# 27.3 years and weight 70.4 kg, given a single 12.5 mg dose and then
# once-daily 25 mg; 450 plasma observations, NONMEM 7.4.4, first-order
# conditional estimation with interaction.
#
# Table 2, with relative standard errors as the paper labels them:
#
#     D1    (zero-order release duration)  0.383 h   (RSE 70%)
#     Tlag  (absorption lag)               0.215 h   (RSE 0.6%)
#     CL/F                                 153 L/h   (RSE 0.7%)
#     V1/F                                 440 L     (RSE 35%)
#     V2/F                                 754 L     (RSE 2.3%)
#     Q/F                                  41.3 L/h  (RSE 5.1%)
#
# Every parameter is APPARENT, divided by the oral bioavailability: the study
# had no intravenous arm.  So the dose enters unmultiplied by F, which the
# apparent volume and clearance already contain, and no intravenous unit is
# offered -- an apparent parameter set predicts an intravenous concentration
# wrong by a factor of 1/F.
#
# ZERO-ORDER ABSORPTION, WHICH IS WHY THIS DRUG NEEDED AN ENGINE ADDITION
# =======================================================================
# The paper tried first-order absorption, first-order with a lag, and dual
# absorption, and reports that none described the data; it selected zero-order
# absorption with a lag.  Its sampling was dense during absorption (0.25, 0.5,
# 1, 2, 3 h), which is what makes the rectangular input identifiable, and the
# authors say so.  The dose therefore enters at a constant 1/D1 of itself per
# unit time from Tlag to Tlag + D1, and this package grew a zero-order oral
# release mode to carry it (R/oral-zero-order.R): the release becomes a finite
# infusion on the apparent oral parameters, which the closed-form engine
# already solves.  A first-order approximation is not used anywhere here.
#
# The release duration is the one weak parameter in the fit, at 70% relative
# standard error, and the paper flags it: "The large value of RSE of PK
# parameter associated with the absorption, duration of zero-order absorption
# (D1), from the final PK model suggested the parameter uncertainty."  Its
# bootstrap interval is 0.330 to 0.515 h against the estimate of 0.383.  The
# shape of the first half hour after a dose is therefore the least certain part
# of this curve, and the test pins the bootstrap interval alongside the
# estimate so that is not forgotten.
#
# CYP2D6: MEASURED, AND DELIBERATELY NOT APPLIED
# ==============================================
# The source fitted a CYP2D6 effect on clearance, CL/F = 153 x (1 - 0.328 x GT)
# where GT is 1 for its intermediate-metaboliser-2 group and 0 for the
# extensive and intermediate-metaboliser-1 groups, so that group's clearance is
# 0.672 of the rest.  That covariate is NOT implemented, and the reason is a
# mismatch of categories rather than of evidence.  The paper splits
# intermediate metabolisers into TWO groups and finds reduced clearance in only
# one of them; the Patient Profile's cyp2d6 field has a single "intermediate"
# value, which cannot tell them apart.  Applying 0.672 to every intermediate
# metaboliser would impose a third less clearance on the paper's own IM-1
# group, in whom it measured none.  The model therefore carries no cyp2d6 term
# and plots the typical patient, the extensive and IM-1 value; the fitted
# effect is recorded in inst/extdata/antihypertensiveParameters.csv.
#
# The paper also reports the effect as 32.8% in its results and abstract and
# 32.7% in its discussion.  The table's fitted value is 0.328.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# None in the source: the paper screened age, height and weight and reports
# that none was significant, in a cohort whose weight range was narrow (70.4
# plus or minus 9.6 kg).  The parameters are therefore the library's
# fixed-parameter case, scaled by pkSizeFactors() with legacyVolume = 1, so
# with the fat-free-mass switch off the published values are used exactly as
# printed, for every patient.
#
# NO EFFECT SITE, NO BLOOD PRESSURE, NO BAND
# ==========================================
# The paper's pharmacodynamic endpoint is the heart-rate rise caused by an
# injection of isoproterenol, modelled as competitive antagonism, with a
# half-maximal inhibitory concentration of 16.5 ng/mL (RSE 34.4%).  That is
# neither a blood pressure nor a resting heart rate: it is the degree to which
# carvedilol blocks a beta-agonist challenge.  It is recorded in the parameter
# audit and is not plotted, and no antihypertensive in this library plots a
# blood pressure (inst/help/models/antihypertensives.md).  tPeak = 0, so ke0
# stays zero; MEAC = 0; and the band is zero, because no therapeutic plasma
# range for carvedilol is established and an inhibitory concentration for a
# drug challenge is not one.
#
# The active metabolite 4'-hydroxyphenyl carvedilol is thirteen times more
# potent than the parent but reaches about a tenth of its concentration; the
# source measured metabolic ratios below 0.1 and modelled the parent alone, as
# it says the other carvedilol models do.  This model follows it, so the
# plotted curve is racemic parent carvedilol only.  Carvedilol is a racemate
# whose beta-blocking activity is the S enantiomer's, and the assay here was
# of the racemate; an S-enantiomer EC50 from another study must not be read
# against this curve.
# -----------------------------------------------------------------------------

# Hwang 2023, Table 2, as published
CARVEDILOL_CL_F   <- 153      # L/h, apparent
CARVEDILOL_V1_F   <- 440      # L, apparent central
CARVEDILOL_V2_F   <- 754      # L, apparent peripheral
CARVEDILOL_Q_F    <- 41.3     # L/h, apparent intercompartmental
CARVEDILOL_D1_H   <- 0.383    # h, zero-order release duration
CARVEDILOL_TLAG_H <- 0.215    # h, absorption lag

#' Carvedilol pharmacokinetics (oral, apparent, zero-order release)
#'
#' Hwang et al. (2023), two compartments with zero-order absorption and a lag
#' in 21 healthy Korean men; see the header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carvedilol <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters, with no covariate in the source: unscaled with
  # the switch off (docs/weight-adjustment.md).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1)

  default <- list(
    v1  = CARVEDILOL_V1_F * size$volume,
    v2  = CARVEDILOL_V2_F * size$volume,
    v3  = 1,                                   # two compartments
    cl1 = CARVEDILOL_CL_F / 60 * size$clearance,
    cl2 = CARVEDILOL_Q_F  / 60 * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no equilibration constant published; plasma only
  MEAC  <- 0

  # No established therapeutic plasma range; the source's inhibitory
  # concentration is for an isoproterenol challenge, not a target.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Hwang S et al., J Korean Med Sci 2023;38:e173. ",
    "Two-compartment apparent oral model in 21 healthy Korean men, with ",
    "ZERO-ORDER absorption over 0.383 h after a 0.215 h lag (the paper ",
    "reports that first-order and dual absorption did not describe the ",
    "data): CL/F 153 L/h, V1/F 440 L, V2/F 754 L, Q/F 41.3 L/h. The fitted ",
    "32.8% lower clearance in its second intermediate-metaboliser group is ",
    "not applied, because the Patient Profile's single intermediate CYP2D6 ",
    "category cannot separate that group from the one in which the paper ",
    "measured no reduction. ",
    "https://doi.org/10.3346/jkms.2023.38.e173"
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
      # (R/oral-zero-order.R), in minutes.
      oralZeroOrder = list(duration = CARVEDILOL_D1_H * 60,
                           lag      = CARVEDILOL_TLAG_H * 60),
      reference = reference
    )
  )
}
