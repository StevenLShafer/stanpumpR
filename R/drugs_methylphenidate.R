# -----------------------------------------------------------------------------
# Methylphenidate (Ritalin immediate release): d-methylphenidate in adults
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# WHAT IS PLOTTED
# ===============
# Plasma d-methylphenidate (d-MPH), the active enantiomer, in ng/mL, after an
# oral dose of racemic immediate-release methylphenidate entered as the tablet
# strength (mg PO).  l-MPH is not modelled: it is largely cleared before it
# reaches the circulation and is not what the source measured.
#
# SOURCE
# ======
# Lyauk YK et al., Clin Transl Sci 2016;9:337-345 (PMC5351003): 503 d-MPH
# concentrations from 122 healthy Danish adults after a single 10 mg Ritalin IR
# tablet, NONMEM FOCE-I.  Three transit compartments (N fixed at 3,
# ktr = (N + 1) / MTT) into an absorption depot, then two disposition
# compartments.  Table 1, at 70 kg:
#
#     MTT 0.505 h, ka 0.418 /h, CL/F 233 L/h, Vc/F 97.6 L, Q/F 70.1 L/h,
#     Vp/F 252 L.  Female sex on MTT: x (1 + 0.925).
#     Allometry fixed: CL/F and Q/F x (W/70)^0.75, volumes x W/70.
#
# DOSE BASIS: HALF THE TABLET IS d-MPH
# ====================================
# The parameters are APPARENT d-MPH parameters, and the paper does not print
# whether the dose record was the 10 mg racemic tablet or its 5 mg of d-MPH.
# The two differ by a factor of two in every concentration, so it was checked
# against the observed exposure in the same subjects rather than assumed.
# Stage C et al., Br J Clin Pharmacol 2017;83:1506-1514 (PMC5465325) is
# Lyauk's Study I: median d-MPH AUC 21.4 ng.h/mL (range 15.7-34.9) in the
# CES1 control group after 10 mg racemic.  AUC = D / (CL/F), so
# D = 21.4 x 233 / 1000 = 4.99 mg.  The fit was on the d-MPH content.
#
# That conversion is applied exactly once, as bioavailability_PO = 0.5: the
# d-enantiomer's share of the racemic tablet mass.  It is NOT an oral
# bioavailability; the apparent scale (CL/F, V/F) already carries that, so
# no further F is applied anywhere.
#
# ORAL ONLY
# =========
# Apparent parameters predict oral concentrations and would predict
# intravenous ones wrong by 1/F (docs/adding-a-drug.md), so only oral units
# are offered.  Nor do the parameters describe any extended-release product:
# Concerta's osmotic input, Ritalin LA's beads and Aptensio's layers are each
# product-specific, and none has a published numerical input model that this
# engine could carry.  Entering a Concerta strength here would simulate an
# immediate-release tablet of that size.
#
# REDUCTION: TRANSIT CHAIN TO A LAG TIME
# ======================================
# The closed-form engine absorbs first-order after a lag; it cannot carry a
# chain of transit compartments.  The transit chain plus depot is replaced by
# tlag_PO and ka_PO fitted by least squares to the published model's typical
# d-MPH curve over 0-24 h (5 mg d-MPH, 70 kg, published disposition held
# unchanged), separately for each sex because sex moves MTT:
#
#               lag (h)    ka (/h)    Cmax       Tmax       worst point
#     male      0.3487     0.3978     -0.5%      1.14 vs 1.26 h   -21% at 0.34 h
#     female    0.6293     0.3661     +0.5%      1.46 vs 1.76 h   -30% at 0.62 h
#
# What is exact: the disposition, and therefore AUC, clearance and the
# terminal phase.  What is approximate: the first hour or so, where a lag
# time switches absorption on abruptly while the transit chain ramps it up;
# the reduction is low just after the lag and the peak comes slightly early.
# From 4 h after the dose the two curves agree within 3% of the peak.
# test-drugs-methylphenidate.R recomputes the published transit model by
# matrix exponential and checks the reduction against it.
#
# COVARIATES
# ==========
# Body size: the source carries fixed allometry on total weight.  As every
# drug here, it is scaled to fat-free mass with the switch on (pkSizeFactors),
# and with the switch off it reproduces the published allometry exactly:
# legacyVolume = W/70, legacyClearance = (W/70)^0.75.
#
# Sex: female MTT x 1.925, carried by the per-sex reduction above.
#
# Not represented: the CES1 effects on CL/F (rs71647871 G143E heterozygote
# x 0.413, rs115629050 x 0.597, CES1A2 one copy x 0.818, two copies x 0.590).
# The patient profile has no CES1 field, so every patient here is the
# wild-type reference.  Lyauk's missing-genotype terms are estimation devices,
# not patient covariates, and are not used either.
#
# POPULATION AND VARIABILITY
# ==========================
# Healthy adults, 10 mg single dose.  Children are an extrapolation: the
# pediatric population model of Shader et al. (J Clin Pharmacol 1999;39:
# 775-785) is a one-compartment model of total (racemic) MPH whose published
# summary gives CL/F 90.7 mL/min/kg and a 4.5 h half-life but no absorption
# rate, so it cannot produce a curve without an invented ka and is not
# offered.  Interindividual variability (Lyauk: MTT 62.1%, CL/F 21.6%,
# Vc/F 90.1% CV, full covariance whose off-diagonals are not printed) and the
# 18.4% proportional residual error are not simulated: stanpumpR plots the
# typical patient only.  The variability is large: a 90% CV on Vc/F moves the
# individual peak a long way from the curve shown.
#
# NO EFFECT SITE, NO THERAPEUTIC BAND
# ===================================
# There is no calibrated concentration-effect model for any methylphenidate
# endpoint that this file could carry (within-day SKAMP or PERMP, weekly
# ADHD-RS-IV), and no concentration is an established therapeutic cutoff.
# tPeak, MEAC and the band are therefore zero.  The Teuscher 2015 ADHD-RS-IV
# Emax relationship was fitted to Aptensio XR exposures, a different product,
# and is not used.
# -----------------------------------------------------------------------------

# Share of racemic methylphenidate tablet mass that is d-MPH.  The dose basis
# conversion, applied once; see the header.
METHYLPHENIDATE_D_FRACTION <- 0.5

# Lag-time reduction of Lyauk's transit absorption, per sex (header).
# Hours and per hour, as fitted; converted to minutes below.
METHYLPHENIDATE_ABSORPTION <- list(
  male   = list(tlag = 0.34871, ka = 0.39782),
  female = list(tlag = 0.62925, ka = 0.36607)
)

#' Methylphenidate (d-methylphenidate) pharmacokinetics
#'
#' Plasma d-methylphenidate after oral racemic immediate-release
#' methylphenidate in adults (Lyauk 2016), with the transit absorption reduced
#' to a lag time and first-order absorption. Oral only: the parameters are
#' apparent.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years
#' @param sex sex as a string; female sex lengthens absorption (Lyauk 2016)
#' @param adjustToFFM scale to fat-free mass (TRUE) or use the published
#'   allometry on total weight (FALSE)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
methylphenidate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Lyauk's allometry on total weight is what the switch-off position
  # reproduces (header).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = weight / 70,
                        legacyClearance = (weight / 70)^0.75)

  v1  <- 97.6        * size$volume      # L,     Vc/F
  v2  <- 252         * size$volume      # L,     Vp/F
  v3  <- 1                              # two compartments
  cl1 <- 233  / 60   * size$clearance   # L/min, CL/F
  cl2 <- 70.1 / 60   * size$clearance   # L/min, Q/F
  cl3 <- 0

  absorption <- if (sex == SEX_FEMALE) {
    METHYLPHENIDATE_ABSORPTION$female
  } else {
    METHYLPHENIDATE_ABSORPTION$male
  }
  ka_PO              <- absorption$ka / 60     # 1/min
  tlag_PO            <- absorption$tlag * 60   # min
  # The dose basis conversion, not an oral bioavailability (header)
  bioavailability_PO <- METHYLPHENIDATE_D_FRACTION

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
    "Lyauk YK et al., Clin Transl Sci 2016;9:337-345. ",
    "https://doi.org/10.1111/cts.12423 (d-methylphenidate after racemic ",
    "Ritalin IR in healthy adults; transit absorption reduced to a lag time; ",
    "dose basis 50% d-MPH, checked against Stage C et al., Br J Clin ",
    "Pharmacol 2017;83:1506-1514, https://doi.org/10.1111/bcp.13237)"
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
