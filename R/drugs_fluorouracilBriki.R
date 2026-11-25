# -----------------------------------------------------------------------------
# Fluorouracil, intravenous (Briki 2026): plasma 5-FU in adults with cancer,
# two compartments with parallel saturable and linear elimination
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L), so Vmax is in mg/min and Km in
# mg/L.
#
# Drafted by Claude Code, 2026-11-05, at the request of Steven L. Shafer, as a
# first-wave chemotherapy drug (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source P10).  Every number below was
# first read from the full text in PubMed Central (PMC13661680), whose Table 3
# had lost its parameter subscripts (Vmax, V1 and V2 all read "V"), so each
# row was identified from its units and checked against a percentage the
# text states (the test repeats each check).  Shafer supplied the PDF
# (2026-11-06): its Table 3 (page 7), with subscripts, and its three printed
# equations confirm every value and every identification, and its Methods
# confirm BSA by Mosteller's formula.
#
# THE SOURCE
# ==========
# Briki M, Thoueille P, Joerger M, Chatelut E, Sterle M, Schmitt A, Wagner AD,
# Buclin T, Carrara S, Guidi M.  Model-based meta-analysis of individual
# patient data for the characterization of intravenous 5-fluorouracil
# population pharmacokinetics.  Cancer Chemother Pharmacol 2026;96:63
# (doi 10.1007/s00280-026-04885-x).  Individual data from three sources were
# pooled: Terret 2000 (21 patients, rich sampling), Mueller 2013 (30, rich)
# and routine monitoring at the Dijon cancer centre (675), 2332
# concentrations in 726 adults: 422 men and 304 women, medians by source 63-69
# years (range 24-90), 67-76 kg (32-126), 168-171 cm, BSA (Mosteller)
# 1.7-1.9 m2 (1.2-2.5), CKD-EPI 2021 eGFR medians 91-99 (2-133).  Regimens
# LV5FU2, FOLFOX, FOLFIRI and FOLFIRINOX: a bolus with or without a 46-h
# continuous infusion.  HPLC-UV, LC-MS/MS and LC-UV (LLOQ 0.013-0.05 mg/L).
# NONMEM 7.6, ODEs (ADVAN13), FOCE-I.
#
# THE MODEL: THE AUTHORS' ALTERNATIVE, WITH LEAN BODY MASS (Table S3)
# ===================================================================
# Steven L. Shafer chose (2026-11-25, PR #897) the authors' alternative final
# model, Table S3 of the supplement (MOESM1, a Word file, supplied by Shafer
# 2026-11-25; its equations read from the file's own equation markup), over
# the main model of Table 3.  Two compartments; elimination from the central
# compartment by a saturable pathway and a parallel linear clearance:
#
#     Vmax 411 mg/h       Km 1.89 mg/L       CL 59.2 L/h
#     V1 9.58 L           Q 112 L/h          V2 13.6 L
#
#     TVVmax = Vmax (1 + thetaAGE (AGE - 60)/60) (LBM/55)^thetaLBM,
#              thetaAGE -0.292, thetaLBM 0.484 (bootstrap 95% 0.311-0.690),
#              LBM by Janmahasatian (kg)
#     TVCL   = CL (BSA/1.7)^thetaBSA,  thetaBSA 1.62, BSA by Mosteller
#     TVV2   = V2 (BW/70)^1 (fixed)
#
# WHY THIS MODEL (Shafer's reasoning, 2026-11-25): a sex term on a clearance
# without a weight term is a surrogate for body size, and gives no rational
# basis for scaling a patient unlike the cohort.  The test is whether
# lean-mass scaling reproduces the sex effect.  It does, at the exponent the
# data estimate: in Fig S3 the cohort's men carry about 56 kg of lean mass
# and its women about 39.5 kg (read from the density peaks), and 1.42^0.484 =
# 1.185, against the main model's sex factor 1.183; at the library's default
# FFM^0.75 it would be 1.30, which the bootstrap interval excludes.  Fig S2B:
# with LBM on Vmax, sex adds nothing (removing it, dOFV 0.718); LBM was the
# strongest univariate covariate on Vmax (Table S2, dOFV -166 against -95.3
# for sex).  The authors kept sex in their main model as "a simpler metric,
# immune to misestimation", a practical choice, not a better fit.
#
# THE MAIN MODEL (Table 3), recorded, not used: Vmax 429 mg/h x (1 - 0.289
# (AGE - 60)/60) x 1.183 in men, Km 2.34 mg/L, CL 52.6 L/h x (BSA/1.7)^2.61,
# V1 9.21 L, Q 125 L/h, V2 14.6 L x BW/70.  Its rows were identified, the PMC
# text having lost the subscripts, from their units and the percentages the
# text states (men's Vmax 18% higher; an 80-year-old's 17% lower than a
# 40-year-old's; BSA 1.5 against 2.0 giving a linear clearance 53% lower).
# The two models' parameter sets differ throughout (Km, CL and the BSA
# exponent all moved when LBM replaced sex), so none of Table 3's values is
# mixed with Table S3's.
#
# WHAT IT DOES NOT COVER
# ======================
# * DPD deficiency.  The DPD phenotype was tested and not retained, and too
#   few genotypes were reported to test.  A patient with a DPYD variant or low
#   DPD activity clears 5-FU far more slowly than this curve; the CPIC and
#   DPWG recommendations (registry G08, G09) are dose reductions, not a
#   clearance, and none is applied.
# * The slow rise during a 46-h infusion.  5-FU inhibits its own metabolism,
#   and the plateau takes 18-26 h to settle (the paper's introduction); the
#   model has no time-dependent term, so its plateau comes within hours.
# * Circadian variation, likewise not modelled.
# * Renal function: eGFR was tested and not retained.
# * Children: fitted in adults 24-90 years.
#
# DOSING AND THE TARGET
# =====================
# Doses in mg (or mg/hr for the infusion), or per m2 (mg/m2, mg/m2/hr; Mosteller
# BSA, R/bsa-dosing.R).  The target for colorectal cancer is an AUC of 20-30
# mg h/L over the 46-h infusion (the IATDMCT recommendation, Beumer 2019,
# PMID 29923599, read in full on PubMed Central; Briki's own dose-adjustment
# grid uses 20-29); that is an exposure, not a concentration band, and is not
# drawn (inst/extdata/therapeuticRanges.csv).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own size terms are kept as fitted and take nothing more
# (rule 2 of "Every parameter is scaled"): lean body mass on Vmax, BSA on
# CL, weight on V2.  With the switch off they are evaluated as published:
# Janmahasatian's lean body mass, BSA and weight from total body weight.
# With it on, Vmax reads the patient's fat-free mass (Al-Sallami's, the
# library's, which is Janmahasatian's times a maturation term that is 1 in
# adult men and 1.01-1.03 in adult women), and BSA and V2 read the
# pharmacokinetic weight (size$pkWeight), as for cefazolin.
#
# V1 and Q have no size term, so with the switch on they belong to Briki's
# typical patient, not to the 70 kg reference man, and are anchored at that
# patient (FLUOROURACIL_BRIKI_COHORT, below; docs/weight-adjustment.md,
# Anchoring at the source's cohort; Gemini's chemotherapy audit of
# 2026-11-23, F22).  No parameter now carries a sex term, so the pooled
# cohort's factors apply.  With the switch off they are the published values.
# Age is held within the source's 24-90 years (its linear term would
# otherwise extrapolate without limit).
#
# The cohort (Table 2, page 6, read on the PDF and its page image,
# 2026-11-25): three studies, medians printed per study, entered as three
# groups --
#     SID1 Terret 2000   21 patients, 14 men  7 women, 65 y, 68 kg, 168 cm
#     SID2 Mueller 2013  30 patients, 20 men 10 women, 63 y, 76 kg, 171 cm
#     SID3 Dijon TDM    675 patients, 388 men 287 women, 69 y, 67 kg, 170 cm
# (726 patients, 422 men and 304 women, as the text says).  Heights are
# printed, so none is derived.  The factors (cohortSizeFactors()): volume
# 0.8989, clearance 0.9231.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human pharmacokinetics of 5-FU in pregnancy were found (PubMed,
# 2026-11-05); the only pregnancy studies are in rats and baboons.  The
# registry records it as unknown, and nothing is applied.
#
# NO EFFECT SITE
# ==============
# Plasma only: tPeak and MEAC are zero.
# -----------------------------------------------------------------------------

# Briki 2026, Table S3 (the alternative final model) and its equations
FLUOROURACIL_BRIKI <- list(
  vmax = 411,          # mg/h at LBM 55 kg and age 60
  km = 1.89,           # mg/L
  cl = 59.2,           # L/h, linear, at BSA 1.7 m2
  v1 = 9.58,           # L
  q = 112,             # L/h
  v2 = 13.6,           # L at 70 kg
  thetaAge = -0.292,   # on Vmax, per (age - 60)/60
  thetaLBM = 0.484,    # on Vmax, (LBM/55)
  lbmReference = 55,   # kg
  thetaBSA = 1.62,     # on CL, (BSA/1.7)
  thetaBW = 1          # on V2, (BW/70), fixed
)
FLUOROURACIL_AGE_RANGE <- c(24, 90)   # years, the source's range

# Briki 2026, Table 2 (page 6), read on the PDF 2026-11-25: medians per
# study.  V1 and Q, with no size term, are anchored at this cohort (see BODY
# SIZE above).
FLUOROURACIL_BRIKI_COHORT <- list(groups = list(
  list(weight = 68, height = 168, age = 65, men = 14,  women = 7),    # SID1
  list(weight = 76, height = 171, age = 63, men = 20,  women = 10),   # SID2
  list(weight = 67, height = 170, age = 69, men = 388, women = 287)   # SID3
))

#' Body surface area by Mosteller's formula, m2
#' @param weight kg
#' @param height cm
#' @noRd
bsaMosteller <- function(weight, height) sqrt(weight * height / 3600)

#' Fluorouracil, intravenous (Briki 2026)
#'
#' Two compartments with parallel Michaelis-Menten and linear elimination,
#' run by the numerical engine (\code{advanceMichaelisMenten()}); see the
#' header of \code{R/drugs_fluorouracilBriki.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
fluorouracilBriki <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- FLUOROURACIL_BRIKI
  # V1 and Q, with no covariate, are anchored at Briki's cohort; Vmax (lean
  # mass), CL (BSA) and V2 (weight) keep their own terms
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, FLUOROURACIL_BRIKI_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  lbm <- if (isTRUE(adjustToFFM)) size$ffm else lbwJanmahasatian(weight, height, sex)
  a <- min(max(age, FLUOROURACIL_AGE_RANGE[1]), FLUOROURACIL_AGE_RANGE[2])

  vmax <- p$vmax * (1 + p$thetaAge * (a - 60) / 60) *
    (lbm / p$lbmReference)^p$thetaLBM / 60                     # mg/min
  km <- p$km                                                   # mg/L
  clLinear <- p$cl * (bsa / 1.7)^p$thetaBSA / 60               # L/min

  default <- list(
    v1 = p$v1 * size$volume,
    v2 = p$v2 * (w / 70)^p$thetaBW,
    v3 = 1,                                  # placeholder: two compartments
    # The low-concentration limit, linear plus Vmax / Km, for the help tables
    # only: the simulation uses the michaelisMenten block.
    cl1 = clLinear + vmax / km,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Briki M, Thoueille P, Joerger M, Chatelut E, et al. Model-based meta-analysis ",
    "of individual patient data for the characterization of intravenous ",
    "5-fluorouracil population pharmacokinetics. Cancer Chemother Pharmacol ",
    "2026;96:63. 726 adults, bolus and 46-h infusions. The authors' alternative ",
    "model (supplement Table S3): two compartments, Vmax 411 mg/h x (1 - 0.292 ",
    "(age-60)/60) x (lean body mass/55 kg)^0.484, Km 1.89 mg/L, linear CL 59.2 L/h ",
    "x (BSA/1.7)^1.62 (Mosteller), V1 9.58 L, Q 112 L/h, V2 13.6 L x weight/70; ",
    "V1 and Q anchored at the study's typical patient. ",
    "DPD deficiency not represented. Plasma 5-FU only. ",
    "https://doi.org/10.1007/s00280-026-04885-x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(
      vmax = vmax,
      km = km,
      clLinear = clLinear
    )
  )
}
