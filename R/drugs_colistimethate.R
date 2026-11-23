# -----------------------------------------------------------------------------
# Colistimethate (Coly-Mycin M; colistimethate sodium, CMS), the inactive
# prodrug of colistin: two compartments forming colistin in one, from Garonzik
# 2011's population model in critically ill adults
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in mg
# of colistin base activity (CBA), so the plotted concentrations are mg/L
# (= mcg/mL), total plasma, of CMS and of the colistin it forms (its own drug,
# colistin, R/drugs_colistin.R).
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# chose this source that day ("Use Garonzik for colistin now") over waiting
# for the final model's supplement (Nation 2017, Table S2).  Source, read in
# full: Garonzik SM, Li J, Thamlikitkul V, Paterson DL, Shoham S, Jacob J,
# Silveira FP, Forrest A, Nation RL.  Population pharmacokinetics of
# colistin methanesulfonate and formed colistin in critically ill patients
# from a multicenter study provide dosing suggestions for various categories
# of patients.  Antimicrob Agents Chemother 2011;55:3284-3294.
# doi 10.1128/AAC.01733-10 (PMID 21555763, PMC3122440).
#
# THE SOURCE
# ==========
# 105 critically ill adults (Thailand and the US, 2009-2010; median 71 years,
# 19-92; 59.1 kg, 30-106; Jelliffe creatinine clearance 28.7 mL/min/1.73 m2,
# 0-169), 12 on intermittent haemodialysis and 4 on CRRT, each on a
# physician-chosen CMS regimen (75-410 mg CBA a day), 8 samples across one
# interval on day 3 or 4; CMS and formed colistin by HPLC; S-ADAPT.  "A linear
# PK model with two compartments for CMS and one compartment for formed
# colistin" (equations 1-3): all of CMS's nonrenal clearance, CLNRCMS, forms
# colistin, so colistin's volume and clearance are apparent, "conditioned on
# the unknown fraction (fm)".  Table 2 and equations 6-8:
#
#     V1   = 11.9 L x (WT/60)            V2 18.7 L       CLD1 7.98 L/h
#     CLT_CMS = 0.0613 x CrCL + 1.90 L/h   (renal + nonrenal; the nonrenal
#                                            1.90 L/h forms colistin)
#     V3/fm 45.1 L                         (colistin, R/drugs_colistin.R)
#     CLT_C/fm = 0.0147 x CrCL + 2.19 L/h
#
# with CrCL in mL/min/1.73 m2.  Dialysis and CRRT clearances (Table 2) are
# held at 0: the Patient Profile has no renal replacement.  Sex, age and
# APACHE II were tested on clearance, and body size (actual and ideal
# weight, BSA, BMI) on volume; only weight on V1 was retained, and the
# authors "did not ... find any significant trends in clearance of either CMS
# or colistin against body size".
#
# CREATININE CLEARANCE
# ====================
# The source used the Jelliffe equation, and its Table 3 footnote d says that
# "other means (e.g., Cockcroft and Gault equation) may be used to estimate
# CrCL which would then be normalized to a body surface area of 1.73 m2".  So
# CrCL is Cockcroft-Gault (creatinineClearanceCG()) from the Patient
# Profile's creatinine (adultEquivalentCreatinine(); an assumed normal value
# when blank), times 1.73 / the Mosteller body surface area, both at the
# pharmacokinetic weight with the switch on and total weight with it off,
# held within the studied 0-169 (COLISTIN_CRCL_MAX).
#
# THE DOSE BASIS (derived; recorded in the PR, 2026-11-16)
# ========================================================
# Doses are entered as the US product is labelled and as the source prescribed
# them, in mg of colistin base activity (CBA).  Neither the source's text nor
# its tables say in what unit the model's dose entered, but its own dosing
# algorithm settles it: equation 10 asks 30 mg CBA a day for each 1 mg/L of
# colistin at a CrCL of 0, where this model needs 24 x 2.19 = 52.6 of its own
# units, a ratio of 1.75 (1.64-1.87 over the CrCL range the authors recommend
# it for, 0-70).  The same ratio follows from the source's own conversion,
# "150 mg CBA is equivalent to approximately ... 400 mg CMS sodium", times the
# colistin a CMS molecule releases: CMS sodium C58H105N16Na5O28S5 (the
# label's formula; 1749.8 g/mol from standard atomic weights, the label
# printing "approximate molecular weight of 1750") less five CH2SO3Na groups
# is colistin A, C53H100N16O13, 1169.5 g/mol; 400/150 x 1169.5/1749.8 =
# 1.782, which is COLISTIMETHATE_BASIS.  So the model's amounts are colistin
# released, and its HPLC assay measured CMS as colistin.  Every volume and
# clearance here, and colistin's, is divided by it, as bendamustine's are by
# its base fraction, so that a dose in mg CBA gives the concentrations the
# model gives.  The final model's papers use 1 million IU ~ 33 mg CBA ~ 80 mg
# CMS (Nation 2017; Tsuji 2019), which gives 1.60 by the same arithmetic,
# and which the earlier review in this PR derived from Nation's parameters
# against Nation's own regression: each paper's convention reproduces its own
# dosing algorithm.
#
# CHECKS
# ======
# Garonzik's derived terminal half-lives (medians of the individual fits, so
# not independent of the model): CMS 11, 5.6 and 4.6 h and colistin 13, 13
# and 9.1 h at a CrCL below 10, 11-69 and above 70.  This model at 5, 40 and
# 100 mL/min/1.73 m2: CMS 10.7, 6.0 and 3.8 h; colistin 13.8, 11.3 and 8.5 h.
# Against Nation 2017's look-up table (Clin Infect Dis 64:565, the final fit
# on 214 patients, 110 of them not in this one; doses meant to give most
# patients a Css,avg of 2 mg/L), the intercept-adjusted regression it rests
# on makes the median about 2.9 mg/L; this model gives 4.4, 2.5, 2.0 and 1.7
# mg/L at a CrCL of 0, 35, 75 and 100: the source's renal effect is steeper
# than the final model's, which used Cockcroft-Gault in mL/min.  The label's
# "half-life of 2-3 hours" is from a microbiological assay that cannot tell
# CMS from colistin, and is not a check.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on V1, at the pharmacokinetic weight with the
# switch on and total weight with it off, and not otherwise scaled.  The
# renal terms (0.0613 and 0.0147 x CrCL) carry Cockcroft-Gault, which has
# weight inside, normalised to 1.73 m2; they are never scaled or anchored.
# Everything else has no size term in the source: CMS's V2 and CLD1, the
# non-renal intercepts (1.90 L/h, CMS's, which forms colistin; 2.19 L/h,
# colistin's), and colistin's V3/fm.  With the switch on these are scaled,
# the volumes by the library's factor for a volume, FFM / FFM_ref, and the
# clearances by its factor for a clearance, (FFM / FFM_ref)^0.75, under
# Steven L. Shafer's first-principles rule of 2026-11-19: a covariate not
# detected in a dataset is not evidence that the pharmacokinetics are
# insensitive to it, and Garonzik's finding of no trend in clearance against
# body size, in 105 patients, is a fact about that cohort.  They are
# anchored at Garonzik's cohort (COLISTIMETHATE_COHORT, below), whose
# typical patient sits at factors of 0.8107 (volume) and 0.8543 (clearance)
# from the reference man, so that patient receives the published values.
# The formation of colistin is the scaled CMS non-renal clearance, so
# colistin's model (R/drugs_colistin.R, scaled the same way and fitted
# jointly, METABOLITE_FORMATION_BASIS "joint") still sees all of it.  With
# the switch off everything is as published.
#
# DOSE, ROUTE AND POPULATION
# ==========================
# Coly-Mycin M Parenteral, "colistimethate sodium equivalent to 150 mg colistin
# base activity per vial"; the label's dose is "2.5 to 5 mg/kg per day of
# colistin base in 2 to 4 divided doses".  Intravenous only: the label also
# gives it intramuscularly, which the source did not study.  Fitted in adults
# and offered at every age, the help page saying so.
#
# PREGNANCY, THE MIC LINE AND THE BAND
# ====================================
# Pregnancy was an exclusion criterion and no pregnancy pharmacokinetics were
# found (registry row).  CMS is inactive, so it has no line or band.
# Colistin's line, free colistin at the 2 mg/L breakpoint (4.1 mg/L total),
# and why it sits above the consensus target: R/drugs_colistin.R.
# -----------------------------------------------------------------------------

# Garonzik 2011, Table 2 and equations 6-8
COLISTIMETHATE_V1     <- 11.9    # L per 60 kg (V1POP)
COLISTIMETHATE_V1_WT  <- 60      # kg
COLISTIMETHATE_V2     <- 18.7    # L
COLISTIMETHATE_Q      <- 7.98    # L/h (CLD1)
COLISTIMETHATE_CLR    <- 0.0613  # L/h per mL/min/1.73 m2 (CLRSLOPE)
COLISTIMETHATE_CLNR   <- 1.90    # L/h (CLNRCMS), all forming colistin
COLISTIN_CRCL_MAX     <- 169     # mL/min/1.73 m2, the most studied
# The model's amounts per mg CBA (see THE DOSE BASIS): 400 mg CMS sodium per
# 150 mg CBA x colistin A (1169.5) / CMS sodium (1749.8)
COLISTIMETHATE_BASIS  <- 400 / 150 * 1169.5 / 1749.8

# Garonzik 2011, Table 1 (read in the PMC full text, PMC3122440), all 105
# patients: medians 71 years, 59.1 kg, 162.0 cm; 68 men and 37 women; two US
# sites and one in Thailand.  Height is printed, so nothing is derived.  The
# size-free parameters of CMS and colistin are anchored at this cohort
# (Steven L. Shafer's first-principles rule, 2026-11-19; see the header's
# BODY SIZE).
COLISTIMETHATE_COHORT <- list(weight = 59.1, height = 162.0, age = 71,
                              men = 68, women = 37)

#' Colistimethate, the prodrug of colistin (Garonzik 2011)
#'
#' Two compartments forming colistin; see the header of
#' \code{R/drugs_colistimethate.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
colistimethate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                           creatinine = NULL)
{
  w <- colistimethateWeight(weight, height, age, sex, adjustToFFM)
  crcl <- colistimethateCrCL(w, height, age, sex, creatinine)
  size <- colistimethateSizeFree(weight, height, age, sex, adjustToFFM)
  f <- COLISTIMETHATE_BASIS
  v1 <- COLISTIMETHATE_V1 * w / COLISTIMETHATE_V1_WT / f
  clnr <- COLISTIMETHATE_CLNR * size$clearance   # L/h, all forming colistin
  cl1 <- (COLISTIMETHATE_CLR * crcl + clnr) / 60 / f

  default <- list(
    v1  = v1,
    v2  = COLISTIMETHATE_V2 * size$volume / f,
    v3  = 1,
    cl1 = cl1,
    cl2 = COLISTIMETHATE_Q * size$clearance / 60 / f,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = COLISTIMETHATE_REFERENCE,
    metabolite = list(
      name              = "colistin",
      # all of CMS's nonrenal clearance forms colistin (fm-scaled)
      kFormation        = clnr / 60 / f / v1,
      firstPassFraction = 0,
      mwRatio           = 1          # the source's amounts are colistin
    )
  )
}

#' The weight colistimethate's and colistin's terms read: the pharmacokinetic
#' weight with the switch on, total weight with it off
#' @keywords internal
colistimethateWeight <- function(weight, height, age, sex, adjustToFFM)
{
  if (isTRUE(adjustToFFM)) pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight
  else weight
}

#' The size factors of colistimethate's and colistin's size-free parameters:
#' the fat-free-mass factors anchored at Garonzik's cohort with the switch on,
#' 1 with it off (see BODY SIZE in R/drugs_colistimethate.R)
#' @keywords internal
colistimethateSizeFree <- function(weight, height, age, sex, adjustToFFM)
{
  pkSizeFactors(weight, height, age, sex, adjustToFFM,
                legacyVolume = 1, legacyClearance = 1,
                cohort = do.call(cohortSizeFactors, COLISTIMETHATE_COHORT))
}

#' Garonzik's creatinine clearance, mL/min/1.73 m2: Cockcroft-Gault
#' normalised to 1.73 m2 of Mosteller body surface area, as the source's Table
#' 3 footnote d allows, held within the studied 0-169
#' @param w the weight the model reads, kg (\code{colistimethateWeight()})
#' @keywords internal
colistimethateCrCL <- function(w, height, age, sex, creatinine)
{
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(w, age, sex, scr) * 1.73 / bsaMosteller(w, height)
  min(max(crcl, 0), COLISTIN_CRCL_MAX)
}

COLISTIMETHATE_REFERENCE <- paste0(
  "Garonzik SM, Li J, Thamlikitkul V, Paterson DL, Shoham S, Jacob J, Silveira FP, ",
  "Forrest A, Nation RL, Antimicrob Agents Chemother 2011;55:3284-3294, Table 2: ",
  "colistimethate (CMS) in two compartments (V1 11.9 L x WT/60, V2 18.7 L, CLD1 ",
  "7.98 L/h, clearance 0.0613 x CrCL + 1.90 L/h, the nonrenal 1.90 L/h forming ",
  "colistin), colistin in one (V3/fm 45.1 L, CL/fm 0.0147 x CrCL + 2.19 L/h); with the ",
  "fat-free-mass switch on, V2, CLD1, V3/fm and the nonrenal clearances scaled to ",
  "fat-free mass from Garonzik's typical patient; CrCL ",
  "Cockcroft-Gault per 1.73 m2 from the serum creatinine (assumed normal when ",
  "blank); no dialysis; doses in mg colistin base activity, the model's amounts ",
  "taken as 1.782 x the dose (derived). https://doi.org/10.1128/AAC.01733-10")
