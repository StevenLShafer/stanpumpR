# -----------------------------------------------------------------------------
# Sertraline: oral, two compartments, bioavailability that rises with the dose
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# The four reductions below are the coordinator's decisions, recorded as such.
# Verified by tests/testthat/test-drugs-sertraline.R, which re-derives the
# absorption fit and works its pins out from the published numbers.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of plasma sertraline (Concentration.Units ng).  The
# source reports hours and L/h; they are divided by 60 here.
#
# SOURCE
# ======
# Alhadab AA, Brundage RC, AAPS J 2020;22:73: a model-based meta-analysis of
# 60 mean concentration-time profiles (748 concentrations) from 27 studies in
# healthy adults, including intravenous sertraline and single and multiple
# oral doses of 5 to 400 mg (NONMEM).
# Because the intravenous profiles anchor the scale, the parameters are
# ABSOLUTE, not divided by an unknown F:
#
#     CL  = 59.7 L/h
#     V1  = 1200 L
#     Q   = 161 L/h
#     V2  = 928 L after a single dose, 1350 L with multiple doses
#     ka(t) = 0.855 x t^1.37 / (3.7^1.37 + t^1.37)  /h, t in hours since dose
#     F(D)  = 0.639 x D / (15.5 + D)   single dose, D in mg
#
# The between-study and between-arm variability of a meta-analysis is not the
# variability between patients, and none is used.
#
# REDUCTION 1: THE MULTIPLE-DOSE PERIPHERAL VOLUME, 1350 L
# =========================================================
# The source switches V2 between single and multiple dosing; a drug model here
# has one set of parameters, and which regimen a dose table is cannot be read
# from one row.  Sertraline is given chronically, so V2 is the multiple-dose
# 1350 L.  Single-dose profiles are then a little off.  For a single 100 mg
# dose in the reference man (first-order fit below), against V2 = 928 L:
#
#                       V2 1350    V2 928
#     peak, ng/mL         25.2      25.7     (-2%)
#     time of peak, h      5.5       5.7
#     at 24 h, ng/mL      11.5      13.3     (-13%)
#     at 72 h, ng/mL       4.1       3.8     (+10%)
#     terminal t1/2, h    33.0      26.6
#
# The AUC is F x D / CL either way.  The 33 h terminal half-life is a little
# longer than the label's ~26 h, which is a single-dose value; the 26.6 h of
# the single-dose volume matches it.  (Coordinator's decision, 2026-10-10.)
#
# REDUCTION 2: ka(t) AS FIRST-ORDER ABSORPTION AFTER A LAG
# ========================================================
# The engine's oral input is first-order after a lag; a rate that rises with
# time since the dose is not representable.  The cumulative fraction absorbed
# under ka(t), 1 - exp(-integral_0^t ka), was fitted by least squares over
# 0-48 h on a 0.1 h grid by 1 - exp(-ka x (t - tlag)) (zero before tlag):
#
#     ka   = 0.4087317 /h
#     tlag = 1.4324891 h
#
# RMS error 0.016 in fraction absorbed, largest 0.108 at 1.4 h, where ka(t)
# has begun absorbing and the lag has not ended.  For 100 mg in the reference
# man, the source's ka(t) peaks at 25.9 ng/mL at 5.9 h; the fit at 25.2 ng/mL
# at 5.5 h.  The literature's single-dose peak is 4.5-8.4 h.  The test
# re-derives the fit with optim().  The lag is kept, so time until threshold
# is blank for the 86 min after each oral dose (test-recovery-lag.R).
# (Coordinator's decision, 2026-10-10.)
#
# REDUCTION 3: THE SINGLE-DOSE F(D), APPLIED TO EVERY DOSE
# ========================================================
# F(D) is carried as an oralSaturation block of the rising form:
# bioavailability_PO = 0.639 is its maximum, and simCpCe() scales each oral
# dose by D / (15.5 + D) (oralSaturationFraction(), R/routes.R).  F is 0.39
# at 25 mg, 0.49 at 50, 0.55 at 100 and 0.59 at 200.  The source's convention
# for F with multiple doses could not be recovered, so applying the
# single-dose F(D) to each dose of a repeated regimen, at its own size, is an
# ASSUMPTION.  The abstract says that after multiple doses bioavailability
# was constant with dose (exposure proportional from 5 to 200 mg), and that
# the single-dose F rose up to 50 mg and then plateaued.  So for chronic
# doses below 50 mg this model likely underpredicts: at 25 mg its F is 0.39,
# against 0.55-0.59 at the plateau.  At 50-200 mg F varies by about 20%.  Doses entered as separate rows at the same time are each
# scaled by their own size, so enter a dose as one row.
# (Coordinator's decision, 2026-10-10.)
#
# REDUCTION 4: ORAL ONLY
# ======================
# Absolute parameters would predict intravenous doses correctly, but there is
# no clinical intravenous sertraline product, and the CSV offers oral units
# only.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Healthy adults with no size covariate: the published values are taken as
# the 70 kg reference man's and scaled with pkSizeFactors() -- to fat-free
# mass with the switch on, not at all with it off (legacyVolume = 1).
#
# NO EFFECT SITE, NO METABOLITE
# =============================
# Plasma only: tPeak = 0, MEAC = 0.  The antidepressant effect lags the
# concentration by weeks, which no ke0 effect site describes.  Serotonin
# transporter occupancy by PET (Parsey 2006, 17 healthy volunteers after 4-6
# days of 25-100 mg: KD 1.9 ng/mL, maximal occupancy 106.8%, fitted above
# 100%) is a biomarker, not plotted.  N-desmethylsertraline, the
# weakly active metabolite, is not modelled.
#
# BAND
# ====
# 10-50 ng/mL, typical 30 (its midpoint): the therapeutic reference range of
# the AGNP update 2026 (Hart et al., Pharmacopsychiatry 2026;59:166-295,
# Table 5; trough 24 h after a dose), which narrowed the 2017 update's 10-150:
# 80% of serotonin transporters are occupied at 10 ng/mL, and above 50 ng/mL
# efficacy may fall and adverse reactions rise.  Orientation only; it
# describes troughs at steady state on chronic treatment.
#
# ALTERNATIVES NOT IMPLEMENTED
# ============================
# Zhang 2024 (140 hospitalised Chinese patients, TDM concentrations, ages
# 11-79): CL/F = 76.1 x [1 - 0.0068 x (age - 22)] L/h, V/F 803 L, ka
# 0.098 /h.  Its apparent half-life, ln2 x 803 / 76.1 = 7.3 h at 22 y, is
# inconsistent with sertraline's ~26 h: with sparse trough data V/F and ka
# are poorly identified, and the short half-life is an artefact of the
# design, not a property of Chinese patients.  Castillo 2024 (CYP2C19 and
# CYP2D6 genotype), Monfort 2024 (pregnancy) and Gonzalez de la Cruz 2026 are
# other published models; none is mixed in.
#
# PREGNANCY
# =========
# Sertraline is the one antidepressant whose dose-adjusted serum
# concentration RISES in pregnancy.  Westin 2017 followed 37 sertraline
# pregnancies (34 women) through routine therapeutic drug monitoring, each
# against the same woman's own baseline outside pregnancy: sertraline was 68%
# higher in the middle of the third trimester (95% CI +37% to +106%,
# p < 0.001), and N-desmethylsertraline 120% higher.  The Schoretsanitis 2020
# meta-analysis (18 patients) gives a combined ratio of 1.38.  Heinonen 2021
# (PMID 33751155) instead found dose-adjusted sertraline LOWER during
# pregnancy than at its own 1-month-postpartum reference -- third-trimester
# median concentration/dose 0.19 against 0.25 postpartum, a nonsignificant 24%
# fall (p = 0.345; its alteration-ratio medians, pregnant over postpartum, are
# all below 1: 0.88, 0.69, 0.85), the opposite direction from Westin's rise,
# which Heinonen's own authors read as increased metabolism in pregnancy.
# Freeman 2008 (six to nine women) found large differences between women and
# no significant mean change.  The mechanism
# is probably inhibition of CYP2C19 in pregnancy.  With the Patient
# Profile's Pregnant field on, the elimination clearance CL1 is multiplied by
# SERTRALINE_PREGNANCY_CL_FACTOR = 0.60 (1 / 1.68 = 0.595, rounded as the
# registry gives it): the clearance FALLS, unlike every other drug the flag
# changes.  The volumes, the distribution clearance and the absorption (ka,
# lag and the dose-dependent F(D)) are unchanged.  Alhadab's parameters are
# absolute and the plotted concentration is total plasma sertraline, which
# is what Westin measured.  Late pregnancy (the third trimester) is the state
# described.  Clinically the change matters mainly as a caution against
# reducing the dose reflexively.  Monfort 2024's perinatal model (above) is
# not mixed in.  (Claude Code, 2026-10-19, at the request of Steven L.
# Shafer; see R/pregnancy.R and inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Alhadab AA, Brundage RC, AAPS J 2020;22:73.
#   https://doi.org/10.1208/s12248-020-00455-y
# Hiemke C et al., Pharmacopsychiatry 2018;51:9-62.
#   https://doi.org/10.1055/s-0043-116492
# Parsey RV et al., Biol Psychiatry 2006;59:821-828 (SERT occupancy).
#   https://doi.org/10.1016/j.biopsych.2005.08.010
# Zhang Z et al., Heliyon 2024;10:e25231 (not used).
#   https://doi.org/10.1016/j.heliyon.2024.e25231
# Westin AA, Brekke M, Molden E, Skogvoll E, Spigset O.  PLoS One
#   2017;12:e0181082.  https://doi.org/10.1371/journal.pone.0181082
#   (pregnancy)
# Schoretsanitis G et al.  Expert Opin Drug Metab Toxicol 2020;16:431-440.
#   https://doi.org/10.1080/17425255.2020.1750598 (pregnancy)
# Heinonen E et al.  Eur J Clin Pharmacol 2021;77:1323-1331.
#   https://doi.org/10.1007/s00228-021-03122-z (pregnancy)
# Freeman MP et al.  J Clin Psychopharmacol 2008;28:646-653.
#   https://doi.org/10.1097/JCP.0b013e31818d2048 (pregnancy)
# -----------------------------------------------------------------------------

SERTRALINE_KA   <- 0.4087317 / 60   # 1/min, first-order fit to ka(t) (reduction 2)
SERTRALINE_TLAG <- 1.4324891 * 60   # min

# Elimination clearance in pregnancy relative to Alhadab's value: Westin 2017,
# sertraline 68% HIGHER in the third trimester, 1 / 1.68 = 0.60 (see the
# header).  Less than 1: the clearance falls.
SERTRALINE_PREGNANCY_CL_FACTOR <- 0.60

#' Sertraline pharmacokinetics (oral)
#'
#' Alhadab and Brundage (2020), a model-based meta-analysis in healthy adults:
#' two compartments with absolute parameters, the multiple-dose peripheral
#' volume, the time-varying absorption rate reduced to first order after a
#' lag, and a bioavailability that rises with the dose, applied to each oral
#' dose through the returned \code{oralSaturation} block (see the header).
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales the volumes and
#'   clearances to the patient's fat-free mass; \code{FALSE} uses the
#'   published values for everyone.
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   elimination clearance by 0.60, a fall (Westin 2017; see the file's
#'   header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sertraline <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       pregnant = FALSE)
{
  # Size scaling (see the header): fixed published values for the reference man
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- 1200 * size$volume
  v2  <- 1350 * size$volume            # multiple-dose value (reduction 1)
  v3  <- 1                             # two compartments
  # Pregnancy (see the header): elimination clearance only, which falls.
  clPregnancy <- if (isTRUE(pregnant)) SERTRALINE_PREGNANCY_CL_FACTOR else 1
  cl1 <- 59.7 / 60 * size$clearance * clPregnancy   # L/min
  cl2 <- 161  / 60 * size$clearance
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = SERTRALINE_KA,
    bioavailability_PO = 0.639,        # maximum of F(D) (reduction 3)
    tlag_PO = SERTRALINE_TLAG
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL: AGNP 2026 (Hart 2026, Table 5) therapeutic reference range.
  # Orientation only.
  typical      <- 30
  upperTypical <- 50
  lowerTypical <- 10

  reference <- paste0(
    "Alhadab AA, Brundage RC, AAPS J 2020;22:73. Model-based meta-analysis ",
    "in healthy adults; two compartments, absolute parameters, multiple-dose ",
    "peripheral volume, time-varying absorption reduced to first order after ",
    "a lag, single-dose bioavailability D/(15.5 + D) applied to each dose; ",
    "oral only. https://doi.org/10.1208/s12248-020-00455-y"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", SERTRALINE_PREGNANCY_CL_FACTOR, " (a fall)"),
      paste0("Westin AA et al., PLoS One 2017;12:e0181082. ",
             "https://doi.org/10.1371/journal.pone.0181082")))
  }

  return(
    list(
      PK = PK,
      tPeak = 0,                       # no effect site: see the header
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      # Alhadab 2020 single-dose F(D) = 0.639 x D / (15.5 + D), D in mg per
      # administration.  Applied to each oral dose by simCpCe().
      oralSaturation = list(form = ORAL_SATURATION_RISING, D50 = 15.5,
                            exampleDoses = c(25, 50, 100, 200))
    )
  )
}
