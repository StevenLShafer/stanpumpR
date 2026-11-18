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
# weight, taken as 175 mg (Vainionpaa et al., Anesth Analg 1995;81:534-538),
# in THAT study's typical patient: its pharmacokinetic subgroup, Table 1,
# 75 kg, 168 cm, 37 years, 6 men and 4 women, whose disposition is this
# model's anchored at Simon's cohort (below), the volumes x 0.839 and the
# clearances x 0.877 of the published values.  ka_RA = 0.004749/min (an
# absorption half-time of 146 min; Steven L. Shafer, 2026-11-18, re-anchored
# and recalibrated together; it was 0.00612/min, fitted on the published
# disposition, before the anchoring).  The predicted time of peak, about 133
# min, is later than the observed median of 52 min, while Vainionpaa's
# terminal half-life after the block, 7.1 h against 1.7 h intravenously,
# says that absorption is slow and rate-limiting late on.  A single
# first-order depot cannot match both the early rise and the long tail:
# matching the 52 min peak time instead needs ka 0.0145/min and gives a
# peak of 2.4 mcg/mL in the same patient.  After epidural injection
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
# Simon reports absolute volumes and clearances (L, mL/min, not per kg) for
# a group whose median was 85 kg, 178 cm, 74 years, 8 men and 1 woman (Table
# 1); Hartog's reading of them as a 70 kg patient's was an assumption, not
# the paper's.  So the published values are that group's typical patient's,
# and with the switch on they are anchored at that patient (ROPIVACAINE_COHORT;
# Steven L. Shafer, 2026-11-18; docs/weight-adjustment.md, Anchoring at the
# source's cohort): volumes x FFM / FFM_ref divided by 1.134, clearances x
# (FFM / FFM_ref)^0.75 divided by 1.099.  With the switch off, the published
# values for everyone (legacyVolume = 1).  The age effect on clearance is not
# modelled: the set is the older group's for every age.  The tissue
# absorption rate is not scaled.
#
# NO EFFECT SITE, NO BAND
# =======================
# As for bupivacaine (R/drugs_bupivacaine.R): systemic total plasma
# concentration only, tPeak 0, no band.
#
# NOT MODELLED
# ============
# Site-specific and epinephrine-specific absorption, parallel fast and slow
# depots, the Weibull input, the postoperative rise in alpha-1-acid
# glycoprotein (which raises total but not unbound concentration during long
# infusions), and the age effect on clearance.  A perineural catheter
# infusion is offered as "mg/hr RA", a constant rate into the same depot at
# the same ka (2026-10-10).
#
# References
# ----------
# Simon MJG et al., Anesth Analg 2006;102:276-282.
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

# Simon 2006 (PMID 16368843), Table 1, read on the PDF 2026-11-18: the oldest
# group (61 years and over), whose disposition this is: median 85 kg, 178 cm,
# 74 years, 8 men and 1 woman.  Factors 1.134 on volume, 1.099 on clearance.
ROPIVACAINE_COHORT <- list(weight = 85, height = 178, age = 74, men = 8, women = 1)

# Vainionpaa 1995 (PMID 7653818), Table 1: the pharmacokinetic subgroup whose
# mean peak ka_RA reproduces, 75 kg, 168 cm, 37 years, 6 men and 4 women.
ROPIVACAINE_KA_COHORT <- list(weight = 75, height = 168, age = 37, men = 6, women = 4)

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
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1,
                        cohort = do.call(cohortSizeFactors, ROPIVACAINE_COHORT))

  default <- list(
    v1 = 7.0 * size$volume,
    v2 = 12.6 * size$volume,
    v3 = 28.7 * size$volume,
    cl1 = 0.300 * size$clearance,
    cl2 = 0.9666666667 * size$clearance,
    cl3 = 0.400 * size$clearance,
    ka_RA = 0.004749,          # 1/min, provisional, peak-matched (header)
    bioavailability_RA = 1,
    tlag_RA = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Simon MJG et al., Anesth Analg 2006;102:276-282 (intravenous tracer ",
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
      reference = reference,
      # PPX, the toxic metabolite (R/drugs_PPX.R; Sunder 2026): formed from
      # ALL of this model's clearance, mass for mass as fitted, through one
      # transit compartment with the 4.9 min mean of Sunder's metabolic
      # delay.  (Claude Code, 2026-11-17, at the request of Steven L. Shafer.)
      metabolite = list(
        name              = "PPX",
        kFormation        = default$cl1 / default$v1,
        firstPassFraction = 0,
        mwRatio           = 1,
        kTransit          = 1 / PPX_DELAY
      )
    )
  )
}
