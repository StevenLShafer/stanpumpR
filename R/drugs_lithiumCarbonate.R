# -----------------------------------------------------------------------------
# Lithium carbonate (Findling 2010): serum lithium, mmol/L
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# millimoles of lithium, so the plotted concentration is mmol/L (= mEq/L).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source
# (PMC2967308, the author manuscript: Table 2, its footnotes and the Methods).
#
# THE SOURCE
# ==========
# Findling RL, Landersdorfer CB, Kafantaris V, et al.  First-dose
# pharmacokinetics of lithium carbonate in children and adolescents.  J Clin
# Psychopharmacol 2010;30:404-410 (doi 10.1097/JCP.0b013e3181e66a62).  Thirty-
# nine patients aged 7 to 17 with bipolar I disorder (weight 51.6 +/- 16.9 kg,
# creatinine clearance 132.6 +/- 44.5 mL/min), a single fasting dose of 600 or
# 900 mg of lithium carbonate in immediate-release capsules, serum at 0, 0.5,
# 1, 1.5, 2, 4, 8, 12 and 24 h and at 48 or 72 h.  NONMEM, two compartments,
# first-order absorption after a lag, linear elimination.  Allometric scaling
# by fat-free mass (Janmahasatian), standardised to 53 kg:
#
#     CL_i = CL_pop (FFM_i / 53)^0.75          V_i = V_pop (FFM_i / 53)
#
# Table 2, the fat-free-mass column (population estimate):
#
#     CL       1.79 L/h
#     V        56.4 L  ("total volume of distribution (Vc + Vp), average ...
#                       calculated from individual parameter estimates")
#     T1/2abs  29.9 min        Tlag  17.4 min
#     T1/2a    2.43 h          T1/2b 27.4 h  (averages of individual estimates)
#
# The paper prints no central volume or intercompartmental clearance.  CL,
# the total volume and both half-lives close a two-compartment model, and
# solving them gives V1 21.52 L, V2 34.88 L and Q 3.026 L/h at 53 kg of
# fat-free mass (k12 0.1406, k21 0.0868 /h).  That is approximate, because
# the volume and half-lives are averages of individual estimates rather than
# population values, and the test checks the derivation reproduces them.
# Landersdorfer 2017's central volume in children, 23.6 L per 53 kg of lean
# body weight (abstract), agrees.  Q is scaled as a clearance, to the 0.75
# power, as the paper scales CL.
#
# Allometric scaling made the children's clearance match adults': "Allometrically
# scaled clearance and volume of distribution from the population analysis
# were within the range reported in adults."  It is applied here to adults
# through the library's fat-free mass (ffmAlSallami(), which for an adult IS
# Janmahasatian's).  Checked against adults the fit never saw:
#   - Westin 2017 (BMJ Open 7:e015738, PMC5353288, read in full): nonpregnant
#     women's serum lithium 10-16 h after the dose, normalised to 24 mmol/day,
#     0.53 mmol/L (95% CI 0.48-0.59).  This model, given 24 mmol once a day
#     to a 65 kg, 165 cm woman, is 0.56 mmol/L 12 h after the dose; its
#     average over the day is 0.67 in her and 0.55 in the reference man.
#   - Clark 2022 (J Clin Pharmacol 62:1385, PMC9796861, read in full):
#     nonpregnant elimination clearance 21.5 +/- 3.0 mL/min (1.29 L/h) and
#     volume 26.8 L in three women by one-compartment fits.
#   - The label: half-life "approximately 18 to 36 hours".
#
# RENAL FUNCTION IS NOT MODELLED
# ==============================
# Lithium is cleared by the kidney alone, yet Findling found no relation of
# clearance to creatinine clearance in these children ("the lack of
# correlation of lithium clearance with creatinine clearance was not
# expected"), and Wing 1997 found none in sixteen adults; the adult models
# that did find one (Jermain 1991, Yukawa 1993, Alqahtani 2020) could not be
# read in full.  So this model has NO renal term: it describes normal renal
# function, and in renal impairment, dehydration, sodium depletion, or with an
# NSAID, an ACE inhibitor, an angiotensin receptor blocker or a thiazide, the
# real concentration is higher than the curve.  [decision, Shafer] Whether to
# add a clearance proportional to creatinine clearance until an adult model
# with a renal term is read.
#
# DOSE BASIS
# ==========
# Doses are milligrams of LITHIUM CARBONATE, Li2CO3 (73.89 g/mol, two moles
# of lithium per mole), which is how the US capsules and tablets are labelled;
# the drug is named for the salt so that a milligram can never be read as
# elemental lithium, a fivefold difference.  The molar block converts each
# dose at 36.94 g of carbonate per mole of lithium, so 300 mg is 8.12 mmol;
# the label's own figure is 8 mEq ("Each 5 mL ... contains 8 mEq of lithium
# ion (Li+) which is equivalent to the amount of lithium in 300 mg of lithium
# carbonate"), so a dose of the oral solution is entered as the carbonate it
# is equivalent to.  Absorption is complete ("reported to be completely
# absorbed", label), so F = 1.  Sustained-release products are not offered:
# no model of them could be read in full.
#
# BODY SIZE
# =========
# The model carries its own size covariate, fat-free mass, so the switch is
# accepted and ignored (docs/weight-adjustment.md, option 1).
#
# PREGNANCY
# =========
# Westin 2017 (13 women, 14 pregnancies, each against her own nonpregnant
# values; linear mixed model): dose-adjusted serum lithium at week 34 "34%
# lower than at baseline (CI -44% to -23%, p<0.001)", "necessitating a 50%
# increase in the lithium dose".  For a drug whose steady state is dose over
# clearance, clearance is 1 / (1 - 0.34) = 1.52 times the nonpregnant value.
# Clark 2022 (three women, rich profiles each trimester) measured it
# directly: elimination clearance "increased an average of 63.5% by the third
# trimester", with the volume "relatively stable".  Applied: clearance
# x 1.52, volumes unchanged.  Lithium clearance falls back within days of
# delivery (Westin; Clark, by 4-9 weeks), when the toxicity risk is greatest.
# -----------------------------------------------------------------------------

# Findling 2010, Table 2 (fat-free-mass model) and the derivation above
LITHIUM_CL     <- 1.79       # L/h at 53 kg fat-free mass
LITHIUM_VSS    <- 56.4       # L at 53 kg
LITHIUM_V1     <- 21.52      # L, derived
LITHIUM_V2     <- LITHIUM_VSS - LITHIUM_V1
LITHIUM_Q      <- 3.026      # L/h, derived
LITHIUM_FFM_REF <- 53        # kg
LITHIUM_T_ABS  <- 29.9       # min, absorption half-life
LITHIUM_TLAG   <- 17.4       # min
# Grams of lithium carbonate per mole of lithium (Li 6.94, C 12.011, O 15.999)
LITHIUM_CARBONATE_MW_PER_LI <- (2 * 6.94 + 12.011 + 3 * 15.999) / 2
# Westin 2017: third-trimester concentration 34% below the nonpregnant value
LITHIUM_PREGNANCY_CL_FACTOR <- 1 / (1 - 0.34)

#' Lithium carbonate (Findling 2010)
#'
#' Two compartments, scaled by fat-free mass, for serum lithium in mmol/L
#' after oral lithium carbonate; see the header of
#' \code{R/drugs_lithiumCarbonate.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies clearance
#'   by \code{LITHIUM_PREGNANCY_CL_FACTOR} (Westin 2017, week 34).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lithiumCarbonate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             pregnant = FALSE)
{
  # Own size covariate: fat-free mass; the switch is ignored
  ffm <- ffmAlSallami(weight, height, age, sex)
  r <- ffm / LITHIUM_FFM_REF
  clPregnancy <- if (isTRUE(pregnant)) LITHIUM_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = LITHIUM_V1 * r,
    v2  = LITHIUM_V2 * r,
    v3  = 1,                          # placeholder: two compartments
    cl1 = LITHIUM_CL / 60 * r^0.75 * clPregnancy,
    cl2 = LITHIUM_Q / 60 * r^0.75,
    cl3 = 0,
    ka_PO = log(2) / LITHIUM_T_ABS,
    bioavailability_PO = 1,
    tlag_PO = LITHIUM_TLAG
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Findling RL et al., J Clin Psychopharmacol 2010;30:404-410: two ",
    "compartments after oral lithium carbonate, CL 1.79 L/h and total volume ",
    "56.4 L at 53 kg fat-free mass (CL ^0.75, V ^1), V1 21.5 L and Q 3.03 L/h ",
    "derived from the printed half-lives, absorption half-life 29.9 min, lag ",
    "17.4 min, F 1.  No renal term.  Doses are lithium carbonate, 36.94 g ",
    "per mol of lithium; plotted as serum lithium, mmol/L. ",
    "https://doi.org/10.1097/JCP.0b013e3181e66a62"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", round(LITHIUM_PREGNANCY_CL_FACTOR, 2),
             " (week 34, volumes unchanged)"),
      paste0("Westin AA et al., BMJ Open 2017;7:e015738. ",
             "https://doi.org/10.1136/bmjopen-2016-015738")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0.6,
    upperTypical = 0.8,
    lowerTypical = 0.4,
    reference = reference,
    molar = list(molecularWeight = LITHIUM_CARBONATE_MW_PER_LI)
  )
}
