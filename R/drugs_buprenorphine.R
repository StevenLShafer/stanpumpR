# -----------------------------------------------------------------------------
# Buprenorphine: Bjornsson 2023 three-compartment disposition (absolute scale),
# a first-order sublingual route, ke0 from Yassen 2006
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
# INTRANASAL: WITHDRAWN 2026-10-27
# =================================
# Until 2026-10-27 an intranasal research route followed Eriksen 1989: nine
# volunteers, 0.3 mg by nasal spray against 0.3 mg intravenously,
# bioavailability 48.2% (s.e.m. 8.4%), mean time to peak 30.6 min, carried as
# F 0.482 and a ka (0.0227 /min) that put the plasma peak at 30.6 min on this
# disposition.  The predicted peak after 0.3 mg was 0.447 ng/mL against the
# 1.77 ng/mL Eriksen reported (by radioimmunoassay; a mean of individual
# maxima).  No bioavailability closes that: it would need F = 0.482 x 1.77 /
# 0.447 = 1.91.  The gap lies in the disposition or the assay, not the
# absorption, intranasal data were never part of Bjornsson's fit, and no
# intranasal product is approved, so the route was withdrawn by decision of
# Steven L. Shafer on the 2026-10-27 opioid potency audit
# (docs/opioid-review-2026-10-27.md).  Restoring it needs a fit of a full
# intranasal concentration-time profile, not a ka chosen to hit a tmax.
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
# compartment stand in for it.
#
# Yassen's ke0 belongs to Yassen's own disposition (V1 8.39, V2 23.3, V3 127 L;
# CL 1.45, Q2 1.55, Q3 0.953 L/min; Table 1 prints Q2 as 155), not to
# Bjornsson's.  Put directly on Bjornsson's, it moved the effect-site peak
# after an intravenous bolus to 134 min.  So it is carried as a time to peak
# (Minto 2003): on Yassen's own disposition ke0 = 0.00447 /min peaks the
# effect site at 61.2 min after a bolus, and getDrugPK() solves ke0 against
# Bjornsson's disposition from that.  Yassen observed peak effect at 70-100
# min, a little later because the receptor-binding step adds to the delay.
# Escher 2007's 120 min maximum (after 0.002 mg/kg) is for the RIII reflex
# and electrical thresholds only; cold-pressor tolerance peaked at 30 min.
# Changed from ke0 = 0.00447 /min by decision of Steven L. Shafer,
# 2026-10-12, in the opioid MEAC and tPeak audit.
#
# MEAC AND THE BAND
# =================
# As a high-affinity partial agonist buprenorphine does not add to full
# agonists the way the MEAC total assumes, so MEAC is 0 and it is not on the
# MEAC panel.  The band (0.2-1 ng/mL, typical 0.4) and the time-until-
# threshold level (0.2 ng/mL) are the library's own, from simulating the
# analgesic regimens of the Oxford postoperative studies with this model at
# the effect site, by decision of Steven L. Shafer, 2026-10-27:
#   McQuay 1980 (Br J Anaesth 52:1013, full text read): 20 patients after hip
#   replacement (45-75 y, under 80 kg; mean about 65 y and 66 kg) given 0.3 mg
#   IV at induction and 0.3 mg IV or IM 3 h later, then diamorphine on demand
#   from 6 h.  Analgesia lasted a median 480 min after the second dose (time
#   to the fifth demand; the second to fourth gave similar medians).  This
#   model at 65 y, 66 kg, 170 cm, male: effect site 0.20 ng/mL at that time
#   (plasma 0.15), peak 1.0 ng/mL 41 min after the second dose.  The lower
#   edge and the threshold are that estimate: a minimum effective
#   concentration by the demand-analgesia method, but simulated for one
#   reference patient at the group's median time, not measured at any
#   patient's demand, and each earlier demand had already delivered 0.25 mg
#   of diamorphine.  It is illustrative, not a validated threshold.
#   The upper edge is that peak; it is also about the top of 0.3 mg IV every
#   6 h at steady state (0.46-1.08), and the group's sublingual patients had
#   excellent analgesia at plasma levels that never exceeded 1 ng/mL (McQuay
#   1980, unpublished observations).  The typical value is the steady-state
#   peak of 0.4 mg sublingual every 8 h (0.17-0.42; Bullingham 1981's dose,
#   about 9 h of analgesia, abstract) and of a 20 mcg/h patch delivered as an
#   infusion (0.39; Reidenberg 2001's measured patch peaks, quoted by Johnson
#   2005, were 0.18, 0.19 and 0.47 ng/mL at 5, 10 and 20 mcg/h).
# The band is read on the effect site: the Oxford group found the effect far
# outlasts the plasma level, with no correlation between peak plasma
# concentration and duration of analgesia (Bullingham 1980, Clin Pharmacol
# Ther 28:667; McQuay 1980; both full text read), and Adriaensen 1985 found
# no correlation between level and analgesia in chronic pain (quoted by
# Johnson 2005, J Pain Symptom Manage 29:297, full text read).  Yassen's slow
# equilibration (time to peak 61 min) carries much of that lag.  The band
# replaced Hart 2026's range for opioid use disorder (AGNP consensus
# guidelines, update 2026, Pharmacopsychiatry 59:166-295, Table 5, doi
# 10.1055/a-2860-7861: 0.5-2.5 ng/mL, 24 h after a dose, alert level 4
# ng/mL), which is for substitution treatment, not analgesia, the use that
# keeps methadone's and morphine's ranges off their panels; it is recorded in
# therapeuticRanges.csv (band_differs).  That range had replaced 1.25-3 ng/mL
# (withdrawal suppression to blockade of opioid reinforcement), and the
# threshold was 1.25 ng/mL, withdrawal suppression.
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
# PREGNANCY
# =========
# Dose-normalised exposure to sublingual buprenorphine is consistently lower
# in pregnancy.  Bastian 2017 (Caritis group; 13-14 women on maintenance,
# each studied in pregnancy and again after delivery) found the AUC about 50%
# lower in pregnancy, with the time to peak and the salivary pH unchanged.
# Zhu 2025 (22 women, 33 studies, the same group) found the dose-normalised
# AUC 155 in pregnancy against 218 after delivery (a ratio of 1.41 over the
# whole of pregnancy) and 119 +/- 34 against 218 +/- 113 in the second half of
# pregnancy (p = 0.014).  Liu 2020 attributes the change to faster CYP3A
# N-dealkylation and UGT glucuronidation, not to absorption, and Zhang
# 2018's physiologically based model reproduces the lower exposure.  The
# pregnancy data are apparent sublingual clearances; because the mechanism
# is metabolic and the absorption was unchanged, the ratio is carried to the
# ELIMINATION clearance of Bjornsson's absolute disposition.  With the
# Patient Profile's Pregnant field on, CL1 is multiplied by
# BUPRENORPHINE_PREGNANCY_CL_FACTOR = 218 / 119 = 1.83 (the second half of
# pregnancy, the library's reference state of late pregnancy); the volumes,
# the distribution clearances and the sublingual absorption
# (ka, F and its dose dependence, lag) are unchanged, so an intravenous dose
# is changed exactly as a sublingual one.  The plotted concentration is total
# plasma buprenorphine; norbuprenorphine, whose formation rises with the
# clearance, is not modelled.  The 61.2 min time to peak effect is held, as
# it is for every covariate, so getDrugPK() solves a ke0 about 24% smaller
# on the faster-clearing disposition.  Ansari 2016 gives the direction only.
# (Claude Code, 2026-10-19, at the request of Steven L. Shafer; see
# R/pregnancy.R and inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Bjornsson M et al., Clin Pharmacokinet 2023;62:1427-1443.
#   https://doi.org/10.1007/s40262-023-01288-6
# Yassen A et al., Anesthesiology 2006;104:1232-1242.
#   https://doi.org/10.1097/00000542-200606000-00019
# Eriksen J et al., J Pharm Pharmacol 1989;41:803-805.
#   https://doi.org/10.1111/j.2042-7158.1989.tb06374.x (the withdrawn
#   intranasal route)
# Nasser AF et al., Clin Pharmacokinet 2014;53:813-824.
#   https://doi.org/10.1007/s40262-014-0155-0
# Kuhlman JJ et al., J Anal Toxicol 1996;20:369-378.
#   https://doi.org/10.1093/jat/20.6.369
# Escher M et al., Clin Ther 2007;29:1620-1631.
#   https://doi.org/10.1016/j.clinthera.2007.08.007
# McQuay HJ, Bullingham RES, Paterson GMC, Moore RA.  Br J Anaesth
#   1980;52:1013-1019.  https://doi.org/10.1093/bja/52.10.1013 (the band)
# Bullingham RES, McQuay HJ, Moore A, Bennett MR.  Clin Pharmacol Ther
#   1980;28:667-672.  https://doi.org/10.1038/clpt.1980.219 (the band)
# Johnson RE, Fudala PJ, Payne R.  J Pain Symptom Manage 2005;29:297-326.
#   https://doi.org/10.1016/j.jpainsymman.2004.07.005 (the band)
# Bastian JR, Chen H, Zhang H et al.  Am J Obstet Gynecol 2017;216:64.e1-
#   64.e7.  https://doi.org/10.1016/j.ajog.2016.09.095 (pregnancy)
# Zhu Y, Bastian JR, Caritis SN et al.  J Addict Med 2025;19:129-134.
#   https://doi.org/10.1097/ADM.0000000000001380 (pregnancy: the factor)
# Liu Y et al.  Ther Drug Monit 2020;42:264-270.
#   https://doi.org/10.1097/FTD.0000000000000724 (pregnancy: mechanism)
# Zhang H, Kalluri HV, Bastian JR et al.  Br J Clin Pharmacol
#   2018;84:2075-2087.  https://doi.org/10.1111/bcp.13642 (pregnancy, PBPK)
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# -----------------------------------------------------------------------------

# Yassen 2006's ke0 (0.00447 /min, t1/2 155 min) carried as the bolus peak
# time it gives on Yassen's own disposition; see the header.
BUPRENORPHINE_TPEAK     <- 61.2           # min after an intravenous bolus
# Sublingual fraction absorbed, fitted to Bjornsson 2023's power law over
# 2-32 mg; see the header.
BUPRENORPHINE_F_SL      <- 0.422692       # small-dose limit
BUPRENORPHINE_SL_SATURATION <- list(Imax = 0.816527, ID50 = 3.42670)  # ID50 mg
BUPRENORPHINE_KA_SL     <- 0.0171905401   # /min; fitted with the lag, see the header
BUPRENORPHINE_TLAG_SL   <- 17.587178      # min; fitted with ka, see the header
# Effect site at the median end of analgesia in McQuay 1980; see the header.
BUPRENORPHINE_ANALGESIA_END <- 0.2       # ng/mL; endCe and lower band edge
BUPRENORPHINE_MIN_AGE   <- 18             # years; youngest age the CL term sees
# Elimination clearance in pregnancy relative to Bjornsson's value: Zhu 2025,
# dose-normalised AUC 218 after delivery against 119 in the second half of
# pregnancy, 218 / 119 = 1.83 (see the header).
BUPRENORPHINE_PREGNANCY_CL_FACTOR <- 1.83

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
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   elimination clearance by 1.83 (Zhu 2025; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
buprenorphine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          pregnant = FALSE)
{
  size  <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW   <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  fixV  <- if (isTRUE(adjustToFFM)) size$volume    else 1
  fixCL <- if (isTRUE(adjustToFFM)) size$clearance else 1

  ageCL <- max(age, BUPRENORPHINE_MIN_AGE)   # see AGE in the header
  cl1 <- 52.1 * (ageCL / 35)^-0.233 * (pkW / 72.4)^0.413 / 60  # L/min
  # Pregnancy (see the header): elimination clearance only; absorption as is.
  if (isTRUE(pregnant)) cl1 <- cl1 * BUPRENORPHINE_PREGNANCY_CL_FACTOR
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
    tlag_SL = BUPRENORPHINE_TLAG_SL
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bjornsson M et al., Clin Pharmacokinet 2023;62:1427-1443 (intravenous ",
    "three-compartment disposition, healthy-volunteer Vc; sublingual reduced ",
    "to one first-order input with dose-dependent F, 0.14 at 16 mg), ",
    "https://doi.org/10.1007/s40262-023-01288-6; ",
    "effect site from Yassen A et al., Anesthesiology 2006;104:1232-1242 ",
    "(ke0 carried as a 61.2 min time to peak); band simulated from the ",
    "postoperative demand analgesia of McQuay HJ et al., Br J Anaesth ",
    "1980;52:1013-1019."
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", BUPRENORPHINE_PREGNANCY_CL_FACTOR),
      paste0("Zhu Y et al., J Addict Med 2025;19:129-134. ",
             "https://doi.org/10.1097/ADM.0000000000001380; Bastian JR et al., ",
             "Am J Obstet Gynecol 2017;216:64.e1-64.e7. ",
             "https://doi.org/10.1016/j.ajog.2016.09.095")))
  }

  return(
    list(
      PK = PK,
      # Yassen's ke0 as a time to peak; getDrugPK() solves ke0.  See the header.
      tPeak = BUPRENORPHINE_TPEAK,
      MEAC = 0,
      # Band: simulated analgesic exposure (McQuay 1980); see the header.
      typical = 0.4,
      upperTypical = 1,
      lowerTypical = BUPRENORPHINE_ANALGESIA_END,
      # Each sublingual dose is scaled by its own fraction absorbed; see the header.
      sublingualSaturation = BUPRENORPHINE_SL_SATURATION,
      reference = reference
    )
  )
}
