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
# THE MODEL (Table 3)
# ===================
# Two compartments; elimination from the central compartment by a saturable
# pathway and a parallel linear clearance:
#
#     Vmax 429 mg/h       Km 2.34 mg/L       CL 52.6 L/h
#     V1 9.21 L           Q 125 L/h          V2 14.6 L
#
#     TVVmax = Vmax (1 + thetaAGE (AGE - 60)/60) (1 + thetaMale SEX),
#              thetaAGE -0.289, thetaMale 0.183, SEX 1 for men
#     TVCL   = CL (BSA/1.7)^thetaBSA,  thetaBSA 2.61, BSA by Mosteller
#     TVV2   = V2 (BW/70)^1 (fixed)
#
# Identification of the stripped rows: Vmax and Km are the only rows in mg/h
# and mg/L; of the two volumes, the one carrying the fixed weight exponent is
# V2 (the text: "the IIV of V2 was entirely explained by an allometric BW
# effect"), so the other, with 82% IIV, is V1.  The covariate thetas follow
# the order of the equations, and reproduce the text: men's Vmax 18% higher
# (1.183); an 80-year-old's 17% lower than a 40-year-old's (0.904/1.096 =
# 0.824); an 80-year-old woman's 30% lower than a 40-year-old man's (0.697);
# and a BSA of 1.5 giving a linear clearance 53% lower than 2.0 (0.75^2.61 =
# 0.472).
#
# The authors also fitted an alternative model with lean body mass on Vmax
# in place of sex (in the supplement, not read).  Sex here carries that
# effect, they say, since the Janmahasatian lean mass is built from sex,
# weight and height.
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
# The model's own size covariates (BSA on CL, weight on V2) are kept as
# fitted and take nothing more: as for cefazolin, they are evaluated at the
# pharmacokinetic weight (size$pkWeight) with the switch on and at total body
# weight with it off, BSA from that weight and the height.
#
# V1, Q and Vmax have no size term, so with the switch on they belong to
# Briki's typical patient, not to the 70 kg reference man, and are anchored
# at that patient (FLUOROURACIL_BRIKI_COHORT, below; Steven L. Shafer's
# rules of 2026-11-18 and 2026-11-19, docs/weight-adjustment.md, Anchoring
# at the source's cohort; Gemini's chemotherapy audit of 2026-11-23, F22).
# V1 and Q take the pooled cohort's factors.  Vmax carries the source's own
# sex term (x 1.183 in men), so it takes the clearance factor of the cohort's
# patient of the patient's OWN sex (cohortSexSizeFactors()): the difference
# in fat-free mass between the sexes is then left to the source's sex term,
# which the authors read as the lean-mass effect, and only the difference
# from the cohort's man or woman of the same sex is added, so sex is not
# counted twice.  (Until 2026-11-25 Vmax was not scaled at all, for that
# reason; the rule of 2026-11-19, that a clearance whose covariates are not
# size keeps them and takes the anchored clearance factor as well, at the
# patient's own sex where it carries a sex term, supersedes it.)  Age is
# held within the source's 24-90 years (its linear term would otherwise
# extrapolate without limit).  With the switch off V1, Q and Vmax are the
# published values.
#
# The cohort (Table 2, page 6, read on the PDF and its page image,
# 2026-11-25): three studies, medians printed per study, entered as three
# groups --
#     SID1 Terret 2002   21 patients, 14 men  7 women, 65 y, 68 kg, 168 cm
#     SID2 Mueller 2013  30 patients, 20 men 10 women, 63 y, 76 kg, 171 cm
#     SID3 Dijon TDM    675 patients, 388 men 287 women, 69 y, 67 kg, 170 cm
# (726 patients, 422 men and 304 women, as the text says).  Heights are
# printed, so none is derived.  The factors (cohortSizeFactors()): pooled
# volume 0.8989 and clearance 0.9231, so V1 and Q rise by 11.3% and 8.3% at
# the reference man; Vmax's clearance factor at the cohort's man 0.9842 and
# at its woman 0.8447, so the reference man's Vmax rises by 1.6%.
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

# Briki 2026, Table 3 and its equations
FLUOROURACIL_BRIKI <- list(
  vmax = 429,          # mg/h
  km = 2.34,           # mg/L
  cl = 52.6,           # L/h, linear
  v1 = 9.21,           # L
  q = 125,             # L/h
  v2 = 14.6,           # L at 70 kg
  thetaAge = -0.289,   # on Vmax, per (age - 60)/60
  thetaMale = 0.183,   # on Vmax, men
  thetaBSA = 2.61,     # on CL, (BSA/1.7)
  thetaBW = 1          # on V2, (BW/70), fixed
)
FLUOROURACIL_AGE_RANGE <- c(24, 90)   # years, the source's range

# Briki 2026, Table 2 (page 6), read on the PDF 2026-11-25: medians per
# study.  V1, Q and Vmax, with no size term, are anchored at this cohort;
# Vmax, which carries the source's sex term, at its patient of the patient's
# own sex (see BODY SIZE above).
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
  # V1 and Q, with no covariate, are anchored at Briki's pooled cohort; Vmax,
  # which carries the source's sex term, at the cohort's patient of the
  # patient's own sex; CL (BSA) and V2 (weight) keep their own terms
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, FLUOROURACIL_BRIKI_COHORT))
  sexAnchor <- cohortSexSizeFactors(FLUOROURACIL_BRIKI_COHORT, sex)
  vmaxSize <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                            legacyVolume = 1, legacyClearance = 1,
                            cohort = sexAnchor)$clearance
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  a <- min(max(age, FLUOROURACIL_AGE_RANGE[1]), FLUOROURACIL_AGE_RANGE[2])
  male <- as.numeric(identical(sex, "male"))

  vmax <- p$vmax * (1 + p$thetaAge * (a - 60) / 60) * (1 + p$thetaMale * male) / 60 *
    vmaxSize                                                                          # mg/min
  km <- p$km                                                                          # mg/L
  clLinear <- p$cl * (bsa / 1.7)^p$thetaBSA / 60                                      # L/min

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
    "2026;96:63. 726 adults, bolus and 46-h infusions: two compartments, Vmax 429 ",
    "mg/h x (1 - 0.289 (age-60)/60) x 1.183 in men, Km 2.34 mg/L, linear CL 52.6 ",
    "L/h x (BSA/1.7)^2.61 (Mosteller), V1 9.21 L, Q 125 L/h, V2 14.6 L x weight/70. ",
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
