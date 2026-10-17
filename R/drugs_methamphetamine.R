# -----------------------------------------------------------------------------
# Methamphetamine (d-, S-(+)-methamphetamine): intranasal, plasma only
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, venous plasma.
#
# RESEARCH MODEL, OPT-IN
# ======================
# Methamphetamine is in the library as an evidence-labelled research model,
# behind the "Illicit drugs" opt-in (off by default; see R/illicit-drugs.R).
# It is a plasma-exposure estimate, not dosing advice, a safety threshold, an
# impairment, psychosis or toxicity threshold, or an individual clinical
# prediction.  It describes the d-enantiomer given intranasally to experienced
# adult users under study conditions; street material differs in purity,
# enantiomer content and adulterants.
#
# THE ENANTIOMER MATTERS
# ======================
# This is the d- (S-(+)-) enantiomer, the one Harris and Cook studied.
# Mendelson's crossover found the enantiomers' kinetics broadly similar but
# the elimination half-lives significantly different (l 13.3-15.0 h versus
# d 10.2-10.7 h), and the d-enantiomer's area under the curve 30% smaller
# after the racemate than after d alone.  l-methamphetamine (the nasal
# decongestant) and the racemate are therefore NOT this model and are not in
# the library.  Methamphetamine is also distinct from the library's
# mixedAmphetamineSalts, an oral prescription amphetamine product.
#
# WHAT IS MODELLED, AND WHAT IS NOT
# =================================
# Parent d-methamphetamine in plasma after an intranasal dose, and nothing
# else.  There is no effect site (tPeak = 0), no MEAC and no concentration
# band: heart rate, blood pressure and subjective effects peak within minutes
# of an intranasal dose while plasma peaks near 2.7 h, so the effect is in
# clear counter-clockwise hysteresis with plasma and no published model
# resolves it into a ke0 the engine could carry.  The amphetamine metabolite
# is not modelled: its formation fraction is not identified by an AUC ratio
# alone, and the engine resolves one metabolite level only.
#
# DISPOSITION: ONE COMPARTMENT, AND THE SOURCES AGREE IT FITS
# ===========================================================
# Harris and colleagues gave deuterium-labelled intravenous methamphetamine
# concurrently with an intranasal or smoked dose in eight experienced users
# and reported, by non-compartmental analysis: clearance 272 mL/h/kg,
# steady-state volume 4.2 L/kg, mean residence time 16 h and an intravenous
# elimination half-life of 11.4 h (10.7 h intranasal).
#
# Those three summaries are mutually consistent with ONE compartment, which is
# why the reduction here is a mild one: CL x MRT = 0.272 x 16 = 4.35 L/kg,
# against the reported Vss of 4.2 L/kg, and ln(2) x 4.2 / 0.272 = 10.7 h,
# against the reported 10.7-11.4 h.  Cook's oral study reached the same
# structure independently: "A one-compartment pharmacokinetic model
# incorporating a lag time fits the data best", with a mean half-life of
# 10.1 h.  So:
#
#     v1  = 4.2 L/kg x 70 = 294 L     (Harris Vss as the one-compartment volume)
#     cl1 = 0.272 L/h/kg x 70 / 60    (= 0.31733 L/min)
#     v2 = v3 = 1, cl2 = cl3 = 0
#
# giving a half-life of 10.70 h.  Harris's parameters are per kilogram, so the
# 70 kg values above are a restatement, not a new fit.
#
# ABSORPTION: INTRANASAL ONLY, WITH ka DERIVED FROM THE OBSERVED PEAK
# ===================================================================
# Harris measured intranasal bioavailability as 79% of the dose, with the
# maximum plasma concentration at 2.7 h.  The source reports no absorption
# rate constant, so ka is DERIVED: in this one-compartment model, with the
# elimination rate constant fixed by Harris's own clearance and volume, the
# time of the peak determines ka uniquely through
#
#     tmax = ln(ka / k) / (ka - k)
#
# Solving tmax = 2.7 h with k = 0.0647619/h gives ka = 1.120677/h
# (0.0186780/min, an absorption half-time of 37.1 min).  That is a derived
# approximation, not a published estimate; it reproduces Harris's observed
# peak time exactly and nothing more.  (The same approach sets hydrocodone's
# oral ka in this library.)
#
# WHY NO OTHER ROUTE IS OFFERED (see inst/extdata/illicitRouteRegistry.csv)
# =========================================================================
#   * Intravenous.  Harris's intravenous parameters are a non-compartmental
#     summary, and Vss used as a one-compartment volume misstates the first
#     minutes after a bolus, where distribution dominates.  Not offered.
#   * Oral.  The absolute oral bioavailability, 67.2 +/- 3.1%, is reported in
#     Cook's 1993 smoking paper as a calculation across that study and the
#     1992 oral study.  No absorption rate constant or lag for an IMMEDIATE
#     release oral dose was found in the accessible sources; the published
#     oral time course (Schepers) is a SUSTAINED release tablet, whose peak
#     spanned 2-12 h and which must not be used as an immediate-release input.
#     Anchored, not offered.
#   * Smoked.  Harris reports 67% of the estimated DELIVERED dose or 37.4% of
#     the nominal PIPE load, and Cook 90.3 +/- 10.4% of the INHALED dose:
#     three numbers with three denominators, which must never be multiplied
#     together.  stanpumpR has no pulmonary route, and smoke must not be
#     aliased to the intranasal route.  Not offered.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Harris reports clearance and volume per kilogram, so the published scaling
# is linear in weight: with the fat-free-mass switch off, pkSizeFactors()'s
# legacy factors (weight / 70 for both) reproduce 272 mL/h/kg and 4.2 L/kg at
# any weight exactly.  With the switch on, volumes scale with the Al-Sallami
# fat-free mass ratio and clearance with its 0.75 power.  The absorption rate
# constant is not scaled.
#
# References
# ----------
# Harris DS, Boxenbaum H, Everhart ET, et al.  The bioavailability of
#   intranasal and smoked methamphetamine.  Clin Pharmacol Ther
#   2003;74(5):475-486.  https://doi.org/10.1016/j.clpt.2003.08.002
#   (eight experienced users; clearance 272 mL/h/kg, steady-state volume
#   4.2 L/kg, mean residence time 16 h, intravenous half-life 11.4 h;
#   intranasal bioavailability 79% with the peak at 2.7 h).
# Cook CE, Jeffcoat AR, Sadler BM, et al.  Pharmacokinetics of oral
#   methamphetamine and effects of repeated daily dosing in humans.  Drug
#   Metab Dispos 1992;20(6):856-862.
#   https://pubmed.ncbi.nlm.nih.gov/1362938/  (one compartment with a lag
#   fits oral data best; mean half-life 10.1 h).
# Cook CE, Jeffcoat AR, Hill JM, et al.  Pharmacokinetics of methamphetamine
#   self-administered to human subjects by smoking S-(+)-methamphetamine
#   hydrochloride.  Drug Metab Dispos 1993;21(4):717-723.
#   https://pubmed.ncbi.nlm.nih.gov/8104133/  (smoked bioavailability
#   90.3 +/- 10.4% of the inhaled dose; the oral 67.2 +/- 3.1% calculated
#   across this study and the 1992 one).
# Mendelson J, Uemura N, Harris D, et al.  Human pharmacology of the
#   methamphetamine stereoisomers.  Clin Pharmacol Ther 2006;80(4):403-420.
#   https://doi.org/10.1016/j.clpt.2006.06.013  (the enantiomers' half-lives
#   differ; the racemate is not the d-enantiomer).
# Schepers RJF, Oyler JM, Joseph RE, et al.  Methamphetamine and amphetamine
#   pharmacokinetics in oral fluid and plasma after controlled oral
#   methamphetamine administration.  Clin Chem 2003;49(1):121-132.
#   https://doi.org/10.1373/49.1.121  (the sustained-release oral time course,
#   which is not an immediate-release input).
# -----------------------------------------------------------------------------

# Harris 2003, restated at a 70 kg reference adult (his values are per kg)
METHAMPHETAMINE_CL_PER_KG <- 0.272   # L/h/kg
METHAMPHETAMINE_V_PER_KG  <- 4.2     # L/kg, Vss used as a one-compartment volume
# Derived from Harris's observed intranasal peak at 2.7 h (see the header)
METHAMPHETAMINE_KA_IN     <- 1.120677 / 60   # 1/min
METHAMPHETAMINE_F_IN      <- 0.79            # intranasal bioavailability

#' Methamphetamine (d-) pharmacokinetics: plasma, intranasal
#'
#' @param weight patient weight (kg)
#' @param height patient height (cm)
#' @param age patient age (years)
#' @param sex \code{"male"} or \code{"female"}
#' @param adjustToFFM scale to the Al-Sallami fat-free mass; when \code{FALSE},
#'   scale linearly with weight, reproducing Harris's per-kilogram values
#' @returns a list in the shape \code{getDrugPK()} expects; plasma only, no
#'   effect site and no modelled metabolite (see the header)
#' @export
methamphetamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  v1  <- METHAMPHETAMINE_V_PER_KG * FFM_REFERENCE_WEIGHT * size$volume
  cl1 <- METHAMPHETAMINE_CL_PER_KG * FFM_REFERENCE_WEIGHT / 60 * size$clearance
  v2  <- 1                       # one compartment
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    # Intranasal: Harris's bioavailability, ka derived from his observed peak
    ka_IN = METHAMPHETAMINE_KA_IN,
    bioavailability_IN = METHAMPHETAMINE_F_IN,
    tlag_IN = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # plasma only: the effects lead plasma by hours (hysteresis)
  MEAC  <- 0

  # No established plasma range applies to this model, so no band is drawn.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Harris DS et al., Clin Pharmacol Ther 2003;74(5):475-486 ",
    "(https://doi.org/10.1016/j.clpt.2003.08.002). d-methamphetamine, one ",
    "compartment from his clearance and steady-state volume, intranasal ",
    "bioavailability 79%, absorption rate derived from his observed 2.7 h ",
    "peak. Plasma only, intranasal only; no effect site, no amphetamine ",
    "metabolite; research use, not dosing advice."
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
