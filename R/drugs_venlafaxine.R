# -----------------------------------------------------------------------------
# Venlafaxine: apparent oral one-compartment model, with O-desmethylvenlafaxine
# (desvenlafaxine) as its active metabolite (Wang 2022)
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer, from
# the full text and figures of the source (PMC9772442, and the publisher's
# PDF supplied by Dr Shafer).  Verified by tests/testthat/test-drugs-venlafaxine.R,
# which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  The source reports L, L/h and 1/h.
#
# THE SOURCE
# ==========
# Wang and colleagues pooled a bioequivalence study (24 healthy Chinese men,
# 50 mg immediate-release venlafaxine, 15 samples each over 36 hours,
# venlafaxine only) with therapeutic drug monitoring in 127 psychiatric
# inpatients (Guangzhou; 14 to 86 years; 25 to 300 mg/day, 84% sustained
# release; mostly morning troughs of venlafaxine and ODV).  NONMEM ADVAN6,
# concentrations in umol/L (their Figures 2, 4 and 5).  Table 2:
#
#     CL/F (venlafaxine)   80.9 L/h      V/F   628 L
#     CLM/F (ODV)          22.1 L/h      VM/F  238 L
#     Ka                   0.63 /h, fixed from a two-compartment pre-fit
#     FP                   0.048, fraction of the absorbed dose converted to
#                          ODV before reaching the circulation
#     morbid state on CL/F        x (1 - 0.617)
#     amisulpride on CL/F         x (1 - 0.392);  on CLM/F  x (1 + 0.593)
#
# K23, THE CONVERSION RATE CONSTANT
# =================================
# Table 2 does not list K23, the rate constant from venlafaxine to ODV, and
# the supplementary material the text cites is not published.  It is not
# missing, though.  Figure 1, the model diagram, gives the venlafaxine
# compartment ONE exit, K23 into ODV; ODV leaves by K30.  Venlafaxine has no
# elimination of its own, so its CL/F is the conversion:
#
#     K23 = (CL/F) / (V/F)        and        K30 = (CLM/F) / (VM/F)
#
# The text's own check agrees: it reports a venlafaxine half-life of 5.4 h in
# healthy subjects, and ln 2 x 628 / 80.9 = 5.38 h.  Any further route out of
# the venlafaxine compartment would make it shorter.  (Steven L. Shafer,
# 2026-10-10: "Table 2 has the numbers for the model.")
#
# In this package's terms: kFormation = cl1 / v1 (the parent's own k10, as for
# amiodarone and fluoxetine), computed from the scaled cl1 and v1.
#
# FIRST PASS
# ==========
# Figure 1 splits the depot: Ka x (1 - FP) into venlafaxine, Ka x FP straight
# into ODV.  The engine adds a first-pass branch to the metabolite without
# taking it from the parent, so the parent carries bioavailability_PO = 1 - FP
# = 0.952 and the metabolite firstPassFraction = FP.  Together these are
# exactly Figure 1.  Both are on the apparent scale: the unknown absolute
# bioavailability is inside CL/F and V/F.
#
# MOLAR BASIS
# ===========
# Concentrations were modelled in umol/L, so conversion is mole for mole.
# Plotted in ng/mL, ODV formed from a milligram of venlafaxine is
# 263.38 / 277.40 = 0.9495 mg (mwRatio).  Doses are mg of venlafaxine as
# labelled (products are labelled as the base); no salt conversion.
#
# WHICH POPULATION
# ================
# Psychiatric patients, Steven L. Shafer's choice (2026-10-10): venlafaxine
# CL/F = 80.9 x (1 - 0.617) = 30.98 L/h, half-life 14.0 h.  The app has no
# health-status field, and patients are who take the drug and whom the AGNP
# range describes.  The healthy value (80.9 L/h, 5.4 h) is on the help page.
# The patients' data were troughs; the shape within a dosing interval comes
# from the healthy volunteers' rich profiles.  ODV's clearance did not differ
# by health status: 22.1 L/h, half-life 7.5 h.
#
# NOT MODELLED
# ============
# Amisulpride (no co-medication field).  Formulation: Ka is the
# immediate-release value; sustained release was not a significant covariate
# on clearance, and its slower absorption is not represented, so the curve is
# the immediate-release one.  CYP2D6 and CYP2C19 genotype were not available
# to the authors.  Age and sex were tested and not retained.  N-desmethyl and
# N,O-didesmethyl venlafaxine.  IIV (the Table 2 "%CV" column holds values
# such as 0.219 and 1.38, not percentages) and residual error.
#
# NO EFFECT SITE; A BAND ON ODV ONLY
# ===================================
# Both are active reuptake inhibitors; no concentration-response relation for
# antidepressant effect exists, so tPeak is zero and both rows plot plasma.
# prodrug = FALSE.  The therapeutic reference range of the active moiety,
# venlafaxine PLUS ODV, is 140 to 600 ng/mL (Lense et al., Psychopharmacology
# 2024;241:275-289, replacing AGNP 2017's 100 to 400), which no single row can
# carry, so venlafaxine's row has no band.  Lense also gives ODV's own range
# during venlafaxine treatment, 85 to 380 ng/mL, which is the desvenlafaxine
# row's band (inst/extdata/therapeuticRanges.csv).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Weight was tested and not retained.  The fixed published values are the
# pooled cohort's typical patient's (24 healthy men and 127 patients, about 62
# kg), and with the switch on they are anchored at that patient
# (VENLAFAXINE_COHORT; Steven L. Shafer, 2026-11-18): the reference man's
# volumes are the published values divided by 0.830 and his clearances by
# 0.869.  Legacy factors 1 reproduce the published values with the switch
# off.  ODV is anchored identically.
#
# References
# ----------
# Wang Z, et al. Joint population pharmacokinetic modeling of venlafaxine and
#   O-desmethyl venlafaxine in healthy volunteers and patients to evaluate the
#   impact of morbidity and concomitant medication. Front Pharmacol
#   2022;13:978202. https://doi.org/10.3389/fphar.2022.978202
# Hiemke C, et al. Pharmacopsychiatry 2018;51:9-62 (AGNP 2017 range).
# Lense XM, et al. Venlafaxine's therapeutic reference range in the treatment
#   of depression revised. Psychopharmacology 2024;241:275-289.
#   https://doi.org/10.1007/s00213-023-06484-7
# -----------------------------------------------------------------------------

VENLAFAXINE_CL_HEALTHY <- 80.9     # L/h, CL/F in healthy volunteers
VENLAFAXINE_MORBID     <- 0.617    # fractional fall in CL/F in patients
VENLAFAXINE_V1         <- 628      # L, V/F
VENLAFAXINE_KA         <- 0.63     # 1/h, fixed in the source
VENLAFAXINE_FP         <- 0.048    # first-pass fraction to ODV
VENLAFAXINE_MW_RATIO   <- 263.38 / 277.40   # ODV / venlafaxine, g/mol

VENLAFAXINE_REFERENCE <- paste0(
  "Wang Z, et al. Joint population pharmacokinetic modeling of venlafaxine ",
  "and O-desmethyl venlafaxine in healthy volunteers and patients to evaluate ",
  "the impact of morbidity and concomitant medication. Front Pharmacol ",
  "2022;13:978202 (psychiatric-patient clearance; K23 = CL/V from Figure 1). ",
  "https://doi.org/10.3389/fphar.2022.978202"
)

# Wang 2022 (PMID 36569310), Table 1 (p. 4), read on the PDF 2026-11-18: study
# 1, 24 healthy men, 62.5 kg, 23.1 years; study 2, 127 patients, 61.95 kg,
# 37.9 years, 57 men and 70 women (44.88% men); China, published 2022.
# No height is printed: each sex's is the country's mean adult height for the
# birth year (NCD Risk Factor Collaboration, eLife 2016;5:e13410, height at 18
# by birth year), less the 0.2 cm (men) and 0.3 cm (women) a cohort loses per
# decade of age after 18 that the same paper reports. China: born 1999 (held
# at 1996) for study 1, 171.7 cm; born 1984 for study 2, 170.0 cm for men and
# 158.2 for women.
# The model is anchored at this cohort (Steven L. Shafer, 2026-11-18;
# docs/weight-adjustment.md, Anchoring at the source's cohort);
# desvenlafaxine, fitted jointly, with it.
# Factors 0.830 on volume, 0.869 on clearance.
VENLAFAXINE_COHORT <- list(groups = list(
  list(weight = 62.5, height = 171.7, age = 23.1, men = 24, women = 0),
  list(weight = 61.95, height = 170.0, age = 37.9, men = 57, women = 0),
  list(weight = 61.95, height = 158.2, age = 37.9, men = 0, women = 70)))

#' Venlafaxine pharmacokinetics, forming desvenlafaxine (ODV)
#'
#' Apparent oral one-compartment model of Wang et al. (2022), with the
#' psychiatric patients' clearance.  See the file header.
#'
#' @param weight weight in kg
#' @param height height in cm
#' @param age age in years (not used by the source)
#' @param sex sex as a string (not used by the source)
#' @param adjustToFFM scale volumes to the patient's fat-free mass and
#'   clearances to that ratio to the 0.75 power; when \code{FALSE}, use the
#'   published values unscaled.
#' @returns a list in the shape \code{getDrugPK()} expects, naming
#'   desvenlafaxine as the active metabolite
#' @export
venlafaxine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Fixed published parameters: legacy factors 1.  Desvenlafaxine must make
  # this call identically.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1, cohort = do.call(cohortSizeFactors, VENLAFAXINE_COHORT))

  clPatient <- VENLAFAXINE_CL_HEALTHY * (1 - VENLAFAXINE_MORBID)   # L/h
  v1  <- VENLAFAXINE_V1 * size$volume
  cl1 <- clPatient / MINS_PER_HOUR * size$clearance                 # L/min

  default <- list(
    v1 = v1,
    v2 = 1,                     # one compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = VENLAFAXINE_KA / MINS_PER_HOUR,
    # The (1 - FP) branch of Figure 1; FP goes to ODV directly
    bioavailability_PO = 1 - VENLAFAXINE_FP,
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = 0,        # no effect-site model; see the header
      MEAC = 0,         # not an opioid
      typical = 0,      # the active moiety's range is for the sum; no band
      upperTypical = 0,
      lowerTypical = 0,
      reference = VENLAFAXINE_REFERENCE,
      prodrug = FALSE,
      metabolite = list(
        name              = "desvenlafaxine",
        # K23: venlafaxine's only exit is conversion to ODV (Figure 1)
        kFormation        = cl1 / v1,
        firstPassFraction = VENLAFAXINE_FP,
        mwRatio           = VENLAFAXINE_MW_RATIO
      )
    )
  )
}
