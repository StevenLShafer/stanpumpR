# -----------------------------------------------------------------------------
# Buprenorphine: Bjornsson 2023 three-compartment disposition (absolute scale),
# first-order sublingual and intranasal routes, ke0 from Yassen 2006
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma buprenorphine (norbuprenorphine is not
# modelled).
#
# WHICH PUBLISHED MODEL, AND WHY
# ==============================
# Two population models of buprenorphine are large enough to anchor on.  Jones
# et al. 2021 (SUBLOCADE + sublingual, 570 people) was fitted WITHOUT any
# intravenous data, so its parameters are apparent, relative to the unknown
# bioavailability of the depot (CL/F_XR 52.2 L/h, V/F_XR 432 L).  They cannot
# be used for intravenous dosing.  Bjornsson et al. 2023 (252 people, 10,658
# concentrations, four studies) included intravenous buprenorphine, so its
# disposition is on the absolute scale, and that is the one carried here:
#
#     CL = 52.1 x (AGE/35)^-0.233 x (WT/72.4)^0.413   L/h
#     Vc = 64.3 L   (healthy volunteers; 237 L in the OUD participants)
#     Q2 = 186 L/h   V2 = 130 L
#     Q3 = 60.3 L/h  V3 = 1580 L
#
# The covariate equation is checked against the paper's own worked examples
# (60.8 L/h at 18 y, 45.1 at 65 y, 44.7 at 50 kg, 59.5 at 100 kg).  Vc is the
# healthy-volunteer value because Vc was identified from the intravenous arm,
# which only the healthy volunteers received; the authors attribute the OUD
# value to the absence of intravenous data in that group.  The eigenvalue
# half-lives are 7.4 min, 90 min and 40.6 h.
#
# SUBLINGUAL: ONE FIRST-ORDER INPUT, FITTED TO THE PUBLISHED TWO-PATHWAY ONE
# ==========================================================================
# Bjornsson's sublingual input has two parallel pathways: 75.9% of the
# bioavailable dose enters after a 0.171 h lag as a 0.419 h zero-order input
# into a depot absorbed at 1.72 /h, and 24.1% is absorbed first-order at
# 0.0875 /h.  stanpumpR carries every extravascular route as a single
# first-order input (ka, F, lag), so that structure was simulated exactly on
# the disposition above for a 16 mg tablet (ODE, RK4, 1.8 s step), and a
# single ka and lag were fitted to it by least squares over 15 min to 24 h:
#
#     ka_SL = 0.01719 /min (1.031 /h), tlag_SL = 17.59 min (0.293 h)
#
#                       published       one ka + lag    one ka, no lag
#                       two-pathway     (used)          (until 2026-10-10)
#     time to peak        52 min          56 min          50 min
#     Cmax, 16 mg         6.09 ng/mL      5.86 ng/mL      4.93 ng/mL
#     C at 8 h            0.74            0.69            0.73
#     C at 24 h           0.338           0.284           0.284
#     AUC 0-24 h          25.1            26.4            25.7 ng.h/mL
#
# (All at F 0.14; with the dose-dependent fraction below, 16 mg absorbs
# 0.1384, so its concentrations are 1.1% lower: Cmax 5.79 ng/mL.)
#
# The lag stands in for the tablet dissolving under the tongue, and with it the
# single input reaches the published peak within about 5%; without it the peak
# was about 19% low.  Neither form reproduces the slow mucosal tail, so a daily
# trough is about 16% low either way.  The cost of the lag: the engine has no
# state for a dose until its lag has passed, so for 17.6 min after every
# sublingual dose the time until threshold reads "not yet absorbed" rather
# than a time (R/recoveryStates.R).  Chosen by Steven L. Shafer, 2026-10-10,
# because the peak matters more for this drug than a short gap in a readout
# measured in hours.
#
# DOSE-DEPENDENT SUBLINGUAL BIOAVAILABILITY
# =========================================
# Bioavailability falls with the dose in Bjornsson's fit,
# F = 0.14 x (dose/16 mg)^-0.371: 18.1% at 8 mg, 14.0% at 16 mg, 12.0% at
# 24 mg.  The library applies a dose-dependent fraction the way it does for
# oral gabapentin, scaling each sublingual dose by
#
#     F(D) = bioavailability_SL x (1 - Imax x D / (ID50 + D))
#
# (a sublingualSaturation block; oralSaturationFraction() in R/routes.R).  The
# power law is not of that form, so the three constants were fitted to it by
# least squares on log F over 2-32 mg, the range of the marketed tablets and
# films: bioavailability_SL 0.4227, Imax 0.8165, ID50 3.427 mg.  Within that
# range the two agree to 2.5%:
#
#     dose (mg)        2      4      8      16     24     32
#     power law      0.303  0.234  0.181  0.140  0.120  0.108
#     this form      0.295  0.237  0.181  0.138  0.121  0.111
#
# Below 2 mg the power law rises without limit (0.71 at 0.2 mg); this form
# levels off at 0.42, which is also nearer the 51% Kuhlman 1996 measured at
# 4 mg in six men.  Analgesic sublingual doses (0.2-0.4 mg; 0.39-0.40 here)
# lie outside the fitted range and remain an extrapolation.  As for
# gabapentin, two rows entered at the same time are scaled separately.
#
# INTRANASAL: RESEARCH ROUTE
# ==========================
# Eriksen 1989, nine volunteers, 0.3 mg by nasal spray against 0.3 mg
# intravenously: bioavailability 48.2% (s.e.m. 8.4%), mean time to peak
# 30.6 min.  ka is set so the plasma peak falls at 30.6 min on this
# disposition.  The predicted peak after 0.3 mg is 0.45 ng/mL against the
# 1.77 ng/mL Eriksen reported by radioimmunoassay; that gap belongs to the
# early volume of distribution (and the assay), not to the absorption
# constant, and is unresolved.  No intranasal product is approved.
#
# INTRAMUSCULAR AND SWALLOWED ORAL ARE NOT OFFERED
# ================================================
# No human intramuscular study reporting bioavailability and time to peak was
# found that could be used, and no swallowed product exists (oral
# bioavailability is low, with heavy first-pass formation of
# norbuprenorphine).  Neither is borrowed from another route.
#
# DEPOTS, PATCHES AND IMPLANTS ARE NOT OFFERED
# ============================================
# CAM2038 (Brixadi/Buvidal) weekly and monthly, SUBLOCADE, the transdermal
# patches and the implant all release through two parallel pathways, a
# zero-order phase or a removal event.  None of these is a single first-order
# input, and the engine has no other kind, so none is offered.
#
# EFFECT SITE
# ===========
# ke0 = 0.00447 /min (equilibration half-time 155 min) from Yassen 2006, who
# fitted the antinociceptive effect of 0.05-0.6 mg/70 kg intravenously in
# healthy volunteers with a combined biophase-equilibration and receptor
# association/dissociation model.  Their k_off (0.0785 /min, 8.8 min) is
# fast beside ke0, and they conclude that biophase distribution, not receptor
# kinetics, is rate-limiting, which is what lets the engine's single effect
# compartment stand in for it.  ke0 is supplied directly rather than solved
# from a tPeak because it was estimated, not a peak time.  Against this
# disposition the effect site peaks 134 min after an intravenous bolus, close
# to the 120 min maximum antinociception Escher 2007 observed after 0.15 mg.
#
# MEAC AND THE BAND
# =================
# No minimum effective analgesic concentration is established for
# buprenorphine, and as a high-affinity partial agonist it does not add to
# full agonists the way the MEAC total assumes, so MEAC is 0 and it is not on
# the MEAC panel.  The band is the opioid-use-disorder range: 1.25 ng/mL for
# withdrawal suppression (also the time-until-threshold level), 2.2 ng/mL for
# 70% mu-receptor occupancy by Nasser 2014's Emax model
# (91.4 x C / (0.67 + C)), and 3 ng/mL, the upper end of the 2-3 ng/mL quoted
# for blockade of opioid reinforcement.  These are plasma targets from
# maintenance treatment; they are not analgesic targets.
#
# AGE
# ===
# Bjornsson's participants were adults, and the age covariate is a power of
# age, which is infinite at age 0 and implausible in childhood (119 L/h at one
# year).  The age term is therefore evaluated at no younger than 18 years
# (BUPRENORPHINE_MIN_AGE): a child gets the clearance of an 18-year-old of the
# same size.  The model is not validated in children.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Clearance carries its own weight and age covariates.  With the switch on it
# sees the pharmacokinetic weight (size$pkWeight) and the size-free volumes
# and intercompartmental clearances take the library's fat-free-mass factors;
# with it off it sees total body weight and the other parameters are the
# published fixed values, which is the published model exactly.
#
# References
# ----------
# Bjornsson M et al., Clin Pharmacokinet 2023;62:1427-1443.
#   https://doi.org/10.1007/s40262-023-01288-6
# Yassen A et al., Anesthesiology 2006;104:1232-1242.
#   https://doi.org/10.1097/00000542-200606000-00019
# Eriksen J et al., J Pharm Pharmacol 1989;41:803-805.
#   https://doi.org/10.1111/j.2042-7158.1989.tb06374.x
# Nasser AF et al., Clin Pharmacokinet 2014;53:813-824.
#   https://doi.org/10.1007/s40262-014-0155-0
# Kuhlman JJ et al., J Anal Toxicol 1996;20:369-378.
#   https://doi.org/10.1093/jat/20.6.369
# Escher M et al., Clin Ther 2007;29:1620-1631.
#   https://doi.org/10.1016/j.clinthera.2007.08.007
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# -----------------------------------------------------------------------------

BUPRENORPHINE_KE0       <- 0.00447        # /min; Yassen 2006, t1/2 155 min
# Sublingual fraction absorbed, fitted to Bjornsson 2023's power law over
# 2-32 mg; see the header.
BUPRENORPHINE_F_SL      <- 0.422692       # small-dose limit
BUPRENORPHINE_SL_SATURATION <- list(Imax = 0.816527, ID50 = 3.42670)  # ID50 mg
BUPRENORPHINE_KA_SL     <- 0.0171905401   # /min; fitted with the lag, see the header
BUPRENORPHINE_TLAG_SL   <- 17.587178      # min; fitted with ka, see the header
BUPRENORPHINE_F_IN      <- 0.482          # Eriksen 1989
BUPRENORPHINE_KA_IN     <- 0.0227053660   # /min; plasma peak at 30.6 min
BUPRENORPHINE_WITHDRAWAL <- 1.25          # ng/mL; band floor and endCe
BUPRENORPHINE_MIN_AGE   <- 18             # years; youngest age the CL term sees

#' Buprenorphine sublingual bioavailability at a given dose (Bjornsson 2023)
#'
#' The dose-dependent fraction of the source model, 0.14 x (dose/16)^-0.371,
#' for the tests.  The engine applies the Imax form fitted to it
#' (`BUPRENORPHINE_SL_SATURATION`).
#'
#' @param doseMg sublingual dose, mg
#' @returns the fraction reaching the circulation
#' @keywords internal
buprenorphineSublingualF <- function(doseMg) 0.14 * (doseMg / 16)^-0.371

#' Buprenorphine pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM evaluate clearance at the pharmacokinetic weight and scale
#'   the volumes and intercompartmental clearances to fat-free mass; when
#'   \code{FALSE}, evaluate clearance at total body weight and use the
#'   published fixed volumes. Clearance always takes the model's own age
#'   covariate.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
buprenorphine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size  <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  fixV  <- if (isTRUE(adjustToFFM)) size$volume    else 1
  fixCL <- if (isTRUE(adjustToFFM)) size$clearance else 1

  ageCL <- max(age, BUPRENORPHINE_MIN_AGE)   # see AGE in the header
  cl1 <- 52.1 * (ageCL / 35)^-0.233 * (pkW / 72.4)^0.413 / 60  # L/min
  v1  <- 64.3 * fixV
  v2  <- 130  * fixV
  v3  <- 1580 * fixV
  cl2 <- 186  / 60 * fixCL
  cl3 <- 60.3 / 60 * fixCL

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_SL = BUPRENORPHINE_KA_SL,
    bioavailability_SL = BUPRENORPHINE_F_SL,
    tlag_SL = BUPRENORPHINE_TLAG_SL,
    ka_IN = BUPRENORPHINE_KA_IN,
    bioavailability_IN = BUPRENORPHINE_F_IN,
    tlag_IN = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bjornsson M et al., Clin Pharmacokinet 2023;62:1427-1443 (intravenous ",
    "three-compartment disposition, healthy-volunteer Vc; sublingual reduced ",
    "to one first-order input with dose-dependent F, 0.14 at 16 mg); ",
    "intranasal from Eriksen J et al., J Pharm Pharmacol 1989;41:803-805; ",
    "ke0 from Yassen A et al., Anesthesiology 2006;104:1232-1242. ",
    "https://doi.org/10.1007/s40262-023-01288-6"
  )

  return(
    list(
      PK = PK,
      # No tPeak: ke0 is supplied directly.  See the header.
      tPeak = 0,
      ke0 = BUPRENORPHINE_KE0,
      MEAC = 0,
      typical = 2.2,
      upperTypical = 3,
      lowerTypical = BUPRENORPHINE_WITHDRAWAL,
      # Each sublingual dose is scaled by its own fraction absorbed; see the header.
      sublingualSaturation = BUPRENORPHINE_SL_SATURATION,
      reference = reference
    )
  )
}
