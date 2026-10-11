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
# are offered.
#
# CONCERTA ("mg PO XR")
# =====================
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Concerta (OROS methylphenidate) is entered as "mg PO XR".  No published
# model gives its input numerically (Gomeni 2017 fitted a double Weibull to
# mean curves without printing a usable parameter set), so its input is
# FITTED HERE, to the shape of the mean curve in Childress AC et al., Clin
# Pharmacol Drug Dev 2025;14:829-835 (doi 10.1002/cpdd.1577), the OROS
# reference arms of two bioequivalence trials: healthy adults, fasted, 54 mg
# (n = 67) and 2 x 36 mg (n = 111), sampled every 30 min to 10 h then to 36 h.
#
# The input, given to the disposition above through oralPulses
# (R/oral-pulses.R):
#   - 22% at the time of the dose: the drug overcoat, the label's content
#     fraction, absorbed as the immediate-release tablet;
#   - 78%, the osmotic core, delivered from 2 h to 15 h at a rate falling
#     linearly to zero, given as 52 pulses 15 min apart.
# The 2 h and 15 h were fitted (1.94 and 15.18 h, rounded) to four shape
# targets that do not depend on the level of the curve, the fractions of
# AUC0-inf in 0-3, 3-7, 7-12 and after 12 h, and to Tmax:
#
#                 0-3 h   3-7 h   7-12 h   >12 h   Tmax
#     model       0.111   0.268   0.350    0.274   6.8 h
#     54 mg       0.103   0.257   0.329    0.311   7.0 h (median)
#     2 x 36 mg   0.113   0.298   0.340    0.249   6.5 h (median)
#
# This is an in-vivo input rate, not the tablet's in-vitro release: the
# osmotic pump delivers at a steady or rising rate, but the drug it delivers
# late, in the colon, is absorbed less well, and the fitted rate falls.
#
# THE LEVEL: THIS MODEL IS LOW AGAINST CHILDRESS
# ==============================================
# The disposition is Lyauk's, unchanged, so the AUC of any dose is fixed by
# the dose basis and CL/F above.  Against Childress the model is low: AUC
# 116 against 174 ng.h/mL after 54 mg (-33%), 154 against 211 after 72 mg
# (-27%), and Cmax 8.9 against 14.6 after 54 mg (-39%).  The difference is
# between studies, not between formulations: Lyauk's clearance reproduces
# Stage 2017's d-MPH AUC after the immediate-release tablet.  Candidate
# reasons: Childress assayed total methylphenidate, so l-MPH is included;
# different populations and laboratories.  It is reported, not tuned away.
#
# Ritalin LA, Aptensio XR, Metadate CD, Quillichew and the generic
# "extended-release" methylphenidates are different inputs: "mg PO XR" here
# means Concerta only.
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
# Concerta's input (see the header): 22% at once, then 78% over 2 to 15 h at a
# rate falling linearly to zero, as pulses every 15 min.
CONCERTA_OVERCOAT <- 0.22
CONCERTA_RELEASE_START <- 2    # h
CONCERTA_RELEASE_END   <- 15   # h
CONCERTA_PULSE_STEP    <- 15   # min

concertaPulses <- function()
{
  n <- (CONCERTA_RELEASE_END - CONCERTA_RELEASE_START) * 60 / CONCERTA_PULSE_STEP
  mid <- CONCERTA_RELEASE_START * 60 + (seq_len(n) - 0.5) * CONCERTA_PULSE_STEP  # min
  w <- (CONCERTA_RELEASE_END * 60 - mid)       # falls linearly to zero
  w <- w / sum(w)
  list(fraction = c(CONCERTA_OVERCOAT, (1 - CONCERTA_OVERCOAT) * w),
       delay    = c(0, mid))
}

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
  # Sex selects the absorption below, so an unrecognised value must not fall
  # through to the male parameters.  getDrugPK() checks this too; this guards
  # direct calls to the exported model.
  if (length(sex) != 1 || !sex %in% SEX_VALUES) {
    stop("Invalid sex: ", paste(sex, collapse = ", "),
         ". Must be one of: ", paste(SEX_VALUES, collapse = ", "))
  }

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
    "Pharmacol 2017;83:1506-1514, https://doi.org/10.1111/bcp.13237). ",
    "Concerta (mg PO XR): 22% at once, 78% over 2-15 h at a falling rate, ",
    "fitted here to the curve shape of Childress AC et al., Clin Pharmacol ",
    "Drug Dev 2025;14:829-835, https://doi.org/10.1002/cpdd.1577"
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
      # Concerta ("mg PO XR"): input fitted to Childress 2025 (header)
      oralPulses = list(XR = concertaPulses())
    )
  )
}
