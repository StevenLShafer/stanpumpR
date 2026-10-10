# -----------------------------------------------------------------------------
# Ropivacaine: intravenous disposition, regional anesthesia (RA) absorption
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total (bound + unbound) parent drug in
# plasma.
#
# DISPOSITION: OLDER ADULTS
# =========================
# Three compartments from the intravenous deuterium-labelled tracer of Simon
# et al. (Anesth Analg 2006;102:276-282), given 25 min after an epidural dose,
# arterial sampling to 24 h, for the oldest group (61 years and over), as
# transcribed and completed for a 70 kg patient in the Hartog thesis
# (chapter 8), which reused it for hip infiltration:
#
#     Vc = 7 L, Vp1 = 12.6 L, Vp2 = 28.7 L
#     CL = 0.300 L/min, Q1 = 0.967 L/min, Q2 = 0.400 L/min
#
# Hartog transcribed CL, Q1, Q2 and Vc and derived Vp1 and Vp2.  This is an
# OLDER-ADULT parameter set: Simon found clearance about 0.19 L/min lower in
# the oldest group than in the youngest.  Healthy volunteers' intravenous
# ropivacaine has CL about 0.40 L/min, Vss 40 L and a terminal half-life of
# 1.7 h (Emanuelsson 1997), but those three summary values do not identify a
# compartmental model, so they are not used.  In a younger adult this model
# overpredicts exposure.
#
# REGIONAL ANESTHESIA (RA): FIRST-ORDER ABSORPTION FROM THE INJECTED TISSUE
# =========================================================================
# An "mg RA" dose is placed in a tissue depot and absorbed into the central
# compartment by first-order kinetics, ka_RA, with bioavailability 1 and no
# lag.  ka_RA is PROVISIONAL.  No study identifies ropivacaine's absorption
# after a peripheral nerve block against an intravenous reference with the
# whole input law published, so ka_RA is the single first-order rate that,
# with the disposition above and F = 1, reproduces the mean peak of 1.28
# mcg/mL after axillary block with 5 mg/mL plain ropivacaine, 30-40 mL by
# weight, taken as 175 mg (Vainionpaa et al., Anesth Analg 1995;81:534-538).
# The predicted time of peak, about 110 min, is later than the observed
# median of 52 min, while Vainionpaa's terminal half-life after the block,
# 7.1 h against 1.7 h intravenously, says that absorption is slow and
# rate-limiting late on.  A single first-order depot cannot match both the
# early rise and the long tail: matching the 52 min peak time instead needs
# ka 0.0147/min and gives a peak of 2.1 mcg/mL.  After epidural injection
# Simon found two parallel processes (27% with a 10.7 min half-time, 77% with
# 248 min), which a single first-order depot does not represent.
#
# The one fully stated ropivacaine tissue input, Hartog's for periarticular
# hip infiltration with epinephrine 10 mcg/mL, is a Weibull with shape 1.03,
# i.e. almost exactly first-order, with rate 0.0394/h (an absorption
# half-time of 17.6 h).  It describes that site and formulation, not a nerve
# block, and is not used here.
#
# F = 1 is an assumption: no site-specific absolute bioavailability has been
# measured after a nerve block or an infiltration.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are those of a 70 kg patient, so they are the 70 kg
# reference adult's: volumes x FFM / FFM_ref and clearances x that ^ 0.75 with
# the switch on, the published values for everyone with it off
# (legacyVolume = 1).  The age effect on clearance is not modelled: the set is
# the older group's for every age.  The tissue absorption rate is not scaled.
#
# NO EFFECT SITE, NO BAND
# =======================
# As for bupivacaine (R/drugs_bupivacaine.R): systemic total plasma
# concentration only, tPeak 0, no band.
#
# NOT MODELLED
# ============
# Site-specific and epinephrine-specific absorption, parallel fast and slow
# depots, the Weibull input, perineural catheter infusions, the postoperative
# rise in alpha-1-acid glycoprotein (which raises total but not unbound
# concentration during long infusions), and the age effect on clearance.
#
# References
# ----------
# Simon MJ et al., Anesth Analg 2006;102:276-282.
#   https://doi.org/10.1213/01.ane.0000185038.86939.74
# Hartog Y, thesis, Erasmus University Rotterdam, chapter 8.
#   https://repub.eur.nl/pub/93139/y-hartog-2.pdf
# Vainionpaa VA et al., Anesth Analg 1995;81:534-538.
#   https://doi.org/10.1097/00000539-199509000-00019
# Emanuelsson BM et al., Ther Drug Monit 1997;19:126-131.
#   https://doi.org/10.1097/00007691-199704000-00002
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer, from a
# research handoff on local anesthetic tissue-injection pharmacokinetics.)
# -----------------------------------------------------------------------------

#' Ropivacaine pharmacokinetics (intravenous and regional anesthesia)
#'
#' Simon et al. (2006) older-adult intravenous tracer disposition, with a
#' provisional first-order absorption rate for tissue injection (RA).  See the
#' header.
#'
#' @inheritParams bupivacaine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ropivacaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = 7.0 * size$volume,
    v2 = 12.6 * size$volume,
    v3 = 28.7 * size$volume,
    cl1 = 0.300 * size$clearance,
    cl2 = 0.9666666667 * size$clearance,
    cl3 = 0.400 * size$clearance,
    ka_RA = 0.00612,           # 1/min, provisional, peak-matched (header)
    bioavailability_RA = 1,
    tlag_RA = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Simon MJ et al., Anesth Analg 2006;102:276-282 (intravenous tracer ",
    "disposition, patients 61 years and over, as completed by Hartog). ",
    "Regional anesthesia (RA) absorption is first-order with a provisional ",
    "rate matched to the mean peak after axillary block (Vainionpaa 1995), ",
    "F = 1 assumed. https://doi.org/10.1213/01.ane.0000185038.86939.74"
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
