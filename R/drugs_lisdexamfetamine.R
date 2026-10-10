# -----------------------------------------------------------------------------
# Lisdexamfetamine (Vyvanse): d-amphetamine in children and adolescents
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# WHAT IS PLOTTED
# ===============
# Plasma d-AMPHETAMINE, in ng/mL, after an oral dose of lisdexamfetamine
# dimesylate entered as the capsule strength (mg PO).  The prodrug itself is
# not plotted and not modelled: the source has no lisdexamfetamine or red-cell
# conversion compartment.  Its empirical first-order input with a lag subsumes
# absorption and hydrolysis together.
#
# SOURCE
# ======
# Tsuda Y et al., Drug Metab Pharmacokinet 2020;35:548-554
# (doi 10.1016/j.dmpk.2020.08.005): 1365 d-amphetamine concentrations from 194
# children and adolescents with ADHD, 6-17 years, in Japanese and US studies.
# One compartment, first-order absorption with a lag.  Final model, reference
# 34.1 kg Japanese patient:
#
#     CL/F = 8.96 x (W/34.1)^0.600 x 1.26^I_nonJP   L/h
#     V/F  = 133  x (W/34.1)^0.776                  L
#     ka   = 0.480 /h,   Tlag = 0.435 h
#
# DOSE BASIS: d-AMPHETAMINE BASE EQUIVALENT
# =========================================
# The capsule strength is lisdexamfetamine DIMESYLATE mass.  Hydrolysis yields
# one d-amphetamine per lisdexamfetamine, so the d-amphetamine base equivalent
# is the molar ratio 135.21 / 455.60 = 0.29677 (d-amphetamine 135.21 g/mol,
# lisdexamfetamine dimesylate 455.60 g/mol): 8.90, 14.84 and 20.77 mg for the
# 30, 50 and 70 mg capsules, the 8.9 / 14.8 / 20.8 mg in the Australian
# regulatory product information.  That conversion is applied exactly once, as
# bioavailability_PO; the apparent scale (CL/F, V/F) already carries any
# incomplete absorption or conversion, so no further F is applied.
#
# Why this basis and not the capsule mass.  Tsuda's paper does not print its
# dose record, so it was checked against an independent study.  Boellner SW et
# al., Clin Ther 2010;32:252-264, gave single 30, 50 and 70 mg doses to 18
# children (mean 36.0 kg, US): mean d-amphetamine Cmax 53.2, 93.3 and 134.0
# ng/mL.  At 36 kg, non-Japanese, this model predicts 44.3, 73.9 and 103.5 on
# the base-equivalent basis, 17 to 23% low, and 149, 249 and 349 on the
# capsule-mass basis, 2.6 to 2.8 times high.  Only the base-equivalent basis
# is plausible.
#
# The remaining 17-23% is NOT explained by variability: simulating Tsuda's
# interindividual variability gives a mean individual Cmax of 43.9 at 30 mg,
# no closer.  The model also peaks later than Boellner observed (4.8 h after
# the dose, against about 3.5 h).  Both are reported here rather than tuned
# away; the parameters are Tsuda's as published.
#
# ORAL ONLY
# =========
# Lisdexamfetamine is an oral product and the parameters are apparent.
#
# COVARIATES
# ==========
# Body size: the model carries its own weight covariate, so as for the other
# self-scaled models (docs/weight-adjustment.md) it is evaluated at the
# pharmacokinetic weight with the switch on (size$pkWeight, 70 kg x FFM /
# FFMref) and at total body weight with it off.  Nothing else is size-scaled.
#
# Cohort: Tsuda estimated clearance 1.26 times higher in the non-Japanese
# (US) cohort.  The patient profile has no such field; the non-Japanese value
# is used, because it is the cohort that matches the US validation data above.
# The term is a study-cohort effect with possible confounding (sites,
# sampling, assays), not a property of a patient's ethnicity, and should not
# be read as a reason to dose anyone differently.
#
# POPULATION AND VARIABILITY
# ==========================
# Children and adolescents 6-17 years, 34.1 kg reference.  Adults are an
# extrapolation of the weight equations well beyond the fitted range.  The
# interindividual variability (CL/F 18.4%, V/F 7.9%, ka 55.7%, Tlag 2.1% CV,
# independent) and the 25.4% proportional residual error are not simulated:
# stanpumpR plots the typical patient only.
#
# NO EFFECT SITE, NO THERAPEUTIC BAND
# ===================================
# Tsuda found no clear exposure-dependent reduction in ADHD-RS-IV, and no
# calibrated concentration-effect model for lisdexamfetamine exists for a
# within-day or weekly endpoint.  Group time windows of benefit (Wigal 2009,
# 1.5-13 h) are trial observations, not a concentration threshold.  tPeak,
# MEAC and the band are therefore zero.
# -----------------------------------------------------------------------------

# d-amphetamine base per mg of lisdexamfetamine dimesylate (molar, 1:1).
LISDEXAMFETAMINE_MW <- 455.60       # g/mol, dimesylate salt
DEXTROAMPHETAMINE_MW <- 135.21      # g/mol, free base
LISDEXAMFETAMINE_D_AMPHETAMINE_FRACTION <- DEXTROAMPHETAMINE_MW / LISDEXAMFETAMINE_MW

# Tsuda's non-Japanese clearance multiplier, applied (see the header).
LISDEXAMFETAMINE_NON_JAPANESE_CL <- 1.26

#' Lisdexamfetamine (d-amphetamine) pharmacokinetics
#'
#' Plasma d-amphetamine after oral lisdexamfetamine dimesylate in children and
#' adolescents (Tsuda 2020). The capsule strength is converted to its
#' d-amphetamine base equivalent. Oral only: the parameters are apparent.
#'
#' @param weight weight in kg
#' @param height height in cm (used only for fat-free mass)
#' @param age age in years (used only for fat-free mass)
#' @param sex sex as a string (used only for fat-free mass)
#' @param adjustToFFM evaluate the weight covariates at the pharmacokinetic
#'   weight (TRUE) or at total body weight (FALSE)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lisdexamfetamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Own weight covariate (header): pharmacokinetic weight with the switch on,
  # total weight with it off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  v1  <- 133 * (pkW / 34.1)^0.776                                    # L, V/F
  cl1 <- 8.96 * (pkW / 34.1)^0.600 * LISDEXAMFETAMINE_NON_JAPANESE_CL / 60  # L/min
  v2  <- 1                                   # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  ka_PO   <- 0.480 / 60                      # 1/min
  tlag_PO <- 0.435 * 60                      # min
  # The dose basis conversion, not an oral bioavailability (header)
  bioavailability_PO <- LISDEXAMFETAMINE_D_AMPHETAMINE_FRACTION

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
    "Tsuda Y et al., Drug Metab Pharmacokinet 2020;35:548-554. ",
    "https://doi.org/10.1016/j.dmpk.2020.08.005 (d-amphetamine after ",
    "lisdexamfetamine in pediatric ADHD; non-Japanese clearance; dose converted ",
    "to d-amphetamine base, 0.2968 mg/mg, checked against Boellner SW et al., ",
    "Clin Ther 2010;32:252-264, https://doi.org/10.1016/j.clinthera.2010.02.011)"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,     # no calibrated effect model (header)
      MEAC = 0,
      typical = 0,   # no therapeutic band (header)
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference
    )
  )
}
