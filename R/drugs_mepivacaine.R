# -----------------------------------------------------------------------------
# Mepivacaine: intravenous disposition, regional anesthesia (RA) absorption
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total (bound + unbound) racemic
# mepivacaine in plasma, R(-) plus S(+).
#
# DISPOSITION: THE ENANTIOMERS, REDUCED TO TWO COMPARTMENTS
# =========================================================
# Burm et al. (Anesth Analg 1997;84:85-89) gave 60 mg of the racemate
# intravenously over 10 min to 10 volunteers and measured each enantiomer:
#
#     R(-): CL 0.79 L/min, Vss 103 L, terminal half-life 113 min
#     S(+): CL 0.35 L/min, Vss  57 L, terminal half-life 123 min
#
# The central volumes and distribution clearances were not reported, so each
# enantiomer is APPROXIMATED as one compartment with V = Vss.  That keeps each
# enantiomer's clearance and Vss, but gives terminal half-lives of 90 and 113
# min rather than 113 and 123, and no early distribution phase.
#
# A racemic dose puts half into each pool, so the plasma response to a unit
# dose is
#
#     C(t) = A exp(-a t) + B exp(-b t),   A = 0.5 / VR, a = CLR / VR,
#                                         B = 0.5 / VS, b = CLS / VS
#
# That is an ordinary biexponential with positive coefficients, and it is
# EXACTLY the response of a two-compartment mammillary model with
#
#     V1 = 1 / (A + B),  k21 = (A b + B a) / (A + B),
#     k10 = a b / k21,   k12 = a + b - k21 - k10
#
# (k21 lies between a and b, so k10 and k12 are both positive).  The reduction
# is exact for total racemate: any dose, by any route, gives the same total
# concentration as the two parallel pools would.  What it cannot report is
# either enantiomer alone, and its "peripheral compartment" is a mathematical
# device, not a tissue.  The total-racemate area after a dose D is
# D (0.5 / CLR + 0.5 / CLS), which the reduction preserves.
#
# REGIONAL ANESTHESIA (RA): FIRST-ORDER ABSORPTION FROM THE INJECTED TISSUE
# =========================================================================
# An "mg RA" dose is placed in a tissue depot and absorbed into the central
# compartment by first-order kinetics, ka_RA, with bioavailability 1 and no
# lag.  Simon and colleagues (2002) published arterial plasma concentrations
# after 600 mg of 1.5% mepivacaine with epinephrine 5 mcg/mL for axillary
# block (mean peak 3.89 mcg/mL at 24.6 min), with a one-rate absorption
# half-time of 3.84 min; joined to the disposition above that rate predicts a
# peak of 7.2 mcg/mL at 19 min, and does not transfer.  A two-rate input
# (47% at 0.167/min, 53% at 0.0050/min) reproduces the published means with
# this disposition and F = 1.  ka_RA is the single first-order rate that best
# reproduces that two-rate curve from 5 to 180 min (weighted least squares,
# each residual divided by the concentration + 0.3 mcg/mL): 0.0272/min, an
# absorption half-time of 25 min.  It predicts a peak of 5.2 mcg/mL at 68
# min, a third above the observed mean and later than it: a single depot,
# with a disposition that has no distribution phase, cannot give both the
# fast rise and the flat plateau.  The overprediction is on the side of
# caution for systemic exposure.
#
# The profile is that of axillary block WITH EPINEPHRINE; Simon's mepivacaine
# exposure (AUC 30.4 mg.h/L) also exceeds what this intravenous clearance
# allows at F = 1 (about 20.6), which neither F nor ka can reconcile.  F = 1
# is an assumption.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The volunteers' parameters are taken as the 70 kg reference adult's:
# volumes x FFM / FFM_ref, clearances x that ^ 0.75 with the switch on, the
# published values for everyone with it off (legacyVolume = 1).  Scaling the
# derived two-compartment parameters is the same as scaling each enantiomer's
# V and CL, because every volume and every clearance carries the same factor.
#
# NO EFFECT SITE, NO BAND
# =======================
# As for bupivacaine (R/drugs_bupivacaine.R): systemic total plasma
# concentration only, tPeak 0, no band.
#
# References
# ----------
# Burm AG et al., Anesth Analg 1997;84:85-89.
#   https://doi.org/10.1097/00000539-199701000-00016
# Simon MJ et al., 2002, axillary brachial plexus block with lidocaine and
#   mepivacaine (arterial plasma).
#   https://pdfs.semanticscholar.org/4623/591309e36ca59b5673f2370e6376704da390.pdf
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer, from a
# research handoff on local anesthetic tissue-injection pharmacokinetics.)
# -----------------------------------------------------------------------------

#' Mepivacaine pharmacokinetics (intravenous and regional anesthesia)
#'
#' Burm et al. (1997) enantiomer disposition, reduced exactly to a
#' two-compartment model for the racemate, with a first-order absorption rate
#' for tissue injection (RA).  See the header.
#'
#' @inheritParams bupivacaine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mepivacaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Each enantiomer as one compartment, V = Vss, half the racemic dose each.
  A <- 0.5 / 103
  a <- 0.79 / 103
  B <- 0.5 / 57
  b <- 0.35 / 57

  # The equivalent two-compartment mammillary model (see the header).
  v1  <- 1 / (A + B)
  k21 <- (A * b + B * a) / (A + B)
  k10 <- a * b / k21
  k12 <- a + b - k21 - k10

  default <- list(
    v1 = v1 * size$volume,
    v2 = v1 * k12 / k21 * size$volume,
    v3 = 1,
    cl1 = v1 * k10 * size$clearance,
    cl2 = v1 * k12 * size$clearance,
    cl3 = 0,
    ka_RA = 0.0272,            # 1/min, fitted to the axillary curve (header)
    bioavailability_RA = 1,
    tlag_RA = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Burm AG et al., Anesth Analg 1997;84:85-89 (intravenous enantiomer ",
    "clearances and volumes, each enantiomer approximated as one compartment, ",
    "reduced exactly to two compartments for the racemate). Regional ",
    "anesthesia (RA) absorption is first-order, fitted to axillary block with ",
    "epinephrine (Simon 2002), F = 1 assumed. ",
    "https://doi.org/10.1097/00000539-199701000-00016"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,
      MEAC = 0,
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference
    )
  )
}
