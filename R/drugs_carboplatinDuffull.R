# -----------------------------------------------------------------------------
# Carboplatin (Duffull 1997): unbound plasma carboplatin, two compartments,
# lean body weight and an adjusted Cockcroft-Gault creatinine clearance
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, who
# chose this source for the library's carboplatin over Ekhart 2006 (fitted,
# but with no renal term) and Chatelut 1995 (a renal formula whose disposition
# would have to be derived from published means) (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source C02).  Every number below was
# read from the PDF, which he supplied: Tables 1 and 2, equations 5.1-5.4, the
# Methods and the Discussion.
#
# THE SOURCE
# ==========
# Duffull SB, Begg EJ, Robinson BA, Deely JJ.  A sequential Bayesian
# algorithm for dose individualisation of carboplatin.  Cancer Chemother
# Pharmacol 1997;39:317-326 (doi 10.1007/s002800050578).  Twelve women with
# ovarian cancer, 49-68 years (mean 59.5), 46-85 kg, 152-177 cm, adjusted
# Cockcroft-Gault GFR 35-98 mL/min, each with a 30-minute infusion and
# samples to 24 h (48 h when the GFR was below 60).  Carboplatin was measured
# INTACT, by HPLC, in plasma ULTRAFILTRATE (limit of detection 0.25 mg/L): the
# unbound drug, the species of the Calvert AUC.  NONMEM, two compartments.
#
# THE MODEL (equations 5.1-5.4)
# =============================
#     Cl   (L/h) = 0.15 LBW^0.75 + 0.78 ClCr
#     Vss  (L)   = 0.38 LBW
#     V1   (L)   = 0.26 LBW              (so V2 = 0.12 LBW)
#     Clic (L/h) = 4.4 (LBW/56)^0.75     (see below)
#
# LBW is Pesola's lean body weight from height (Table 1's footnote):
# 0.9 (height - 150) + 45 kg, plus 5 kg for men.  ClCr is the Cockcroft-Gault
# creatinine clearance in L/h, ADJUSTED as the paper says: serum creatinine
# below 60 uM set to 60 uM, and the weight in the equation the lower of LBW
# and total body weight.  At the cohort's mean LBW (56 kg) and adjusted ClCr
# (74.2 mL/min = 4.45 L/h) the equations give Cl 6.5 L/h, V1 14.6 L and Vss
# 21.3 L against Table 2's geometric means 6.8, 14.8 and 21.6 (the test
# checks them).  The units of ClCr in 5.1 are L/h: in mL/min the renal term
# alone would be 58 L/h.
#
# CLIC: TABLE 2, NOT EQUATION 5.4 (Shafer's decision, 2026-11-06)
# ===============================================================
# The paper contradicts itself.  Equation 5.4 prints Clic = 0.15 LBW^0.75,
# 3.1 L/h at LBW 56 and 2.7-3.4 L/h over the study's LBW of 48-64 kg; Table 2
# gives a geometric mean of 4.4 L/h with a range of 3.9-5.0, which that
# equation cannot produce.  Its 0.15 is also equation 5.1's non-renal
# coefficient, which reads as a copying error.  So the equation's form is
# kept and Table 2 sets its level: 4.4 (LBW/56)^0.75, a coefficient of 0.215.
# Clic shapes only the distribution phase; the AUC, and so Calvert dosing,
# depends on Cl alone.  (A published erratum was reported for this paper by
# the handoff behind docs/chemotherapy.md; none could be found, in PubMed or
# in the paper.)
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate, LBW, computed from HEIGHT and sex
# and not from weight, so the scaling switch reaches it only through the
# weight the adjusted Cockcroft-Gault compares with LBW: the pharmacokinetic
# weight (size$pkWeight) with the switch on, total body weight with it off.
# Pesola's formula is an adult one and falls to zero at 100 cm, so height is
# held at the study's lowest, 152 cm, and below 152 cm the size descriptor is
# the lower of that LBW and the weight: a child, outside the source, gets a
# size from its weight rather than from an adult's lean mass.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine, or an assumed normal one for the
# age and sex when it is blank (adultEquivalentCreatinine()), floored at
# 60 uM as the paper does.  Fitted in GFR 35-98 mL/min; below 35 the renal
# term is extrapolated.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No usable human data: Van Calsteren 2010 (PMID 20846067) compared platinum
# in 2 pregnant with 2 nonpregnant women and saw a lower AUC and higher
# clearance in pregnancy, too few to quantify.  Carboplatin is cleared mainly
# by filtration, so the renal term takes the rise in filtration of pregnancy
# (x 1.5 on ClCr, pregnancyGfrFactor(), only when the creatinine is blank).
#
# WHAT IT DOES NOT COVER
# ======================
# * Men and the young: twelve women, 49-68 years.  The sex enters only
#   through LBW (+5 kg) and Cockcroft-Gault (x 0.85 for women).
# * Children: see BODY SIZE.  Duong 2019 (registry C04) is the paediatric
#   source still to be read.
# * High doses and the obese: conventional doses only; LBW from height
#   ignores fat, which suits the drug but was never tested in obesity.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plasma only.  The Calvert target AUC (typically 4-7 mg/mL min) is an
# exposure, not a concentration band, and is not drawn.
# -----------------------------------------------------------------------------

# Duffull 1997, equations 5.1-5.4 and Table 2
CARBOPLATIN_DUFFULL <- list(
  clNonRenal = 0.15,   # L/h per LBW^0.75
  clRenal = 0.78,      # per L/h of adjusted ClCr
  vss = 0.38,          # L/kg LBW
  v1 = 0.26,           # L/kg LBW
  clic = 4.4,          # L/h at LBW 56 kg (Table 2's geometric mean)
  clicLBW = 56         # kg, the cohort's mean LBW (Table 1)
)
CARBOPLATIN_SCR_FLOOR_UM <- 60     # uM: lower creatinines set to 60 (the paper)
CARBOPLATIN_HEIGHT_MIN <- 152      # cm: the study's lowest; Pesola is adult

#' Pesola's lean body weight, as Duffull 1997 used it
#' @param height cm
#' @param sex "male" or "female"
#' @noRd
leanBodyWeightPesola <- function(height, sex)
  0.9 * (height - 150) + 45 + if (sex == SEX_MALE) 5 else 0

#' Carboplatin (Duffull 1997)
#'
#' Two compartments for unbound plasma carboplatin, with lean body weight and
#' an adjusted Cockcroft-Gault creatinine clearance; see the header of
#' \code{R/drugs_carboplatinDuffull.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
carboplatinDuffull <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        creatinine = NULL, pregnant = FALSE)
{
  p <- CARBOPLATIN_DUFFULL
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  lbwAdult <- leanBodyWeightPesola(max(height, CARBOPLATIN_HEIGHT_MIN), sex)
  lbw <- if (height < CARBOPLATIN_HEIGHT_MIN) min(lbwAdult, w) else lbwAdult

  scr <- max(adultEquivalentCreatinine(creatinine, age, sex),
             CARBOPLATIN_SCR_FLOOR_UM / SCR_UMOL_PER_MG_DL)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  clcr <- creatinineClearanceCG(min(lbw, w), age, sex, scr) * 60 / 1000 *
    gfrPregnancy                                                    # L/h

  cl   <- p$clNonRenal * lbw^0.75 + p$clRenal * clcr                # L/h
  clic <- p$clic * (lbw / p$clicLBW)^0.75                           # L/h

  default <- list(
    v1  = p$v1 * lbw,
    v2  = (p$vss - p$v1) * lbw,
    v3  = 1,                          # placeholder: two compartments
    cl1 = cl / 60,
    cl2 = clic / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Duffull SB, Begg EJ, Robinson BA, Deely JJ. A sequential Bayesian algorithm ",
    "for dose individualisation of carboplatin. Cancer Chemother Pharmacol ",
    "1997;39:317-326. Twelve women with ovarian cancer: Cl 0.15 LBW^0.75 + 0.78 ",
    "ClCr L/h (Cockcroft-Gault with LBW, creatinine floored at 60 uM, from the ",
    "entered or an assumed normal creatinine), V1 0.26 LBW, Vss 0.38 LBW, Clic ",
    "4.4 (LBW/56)^0.75 L/h (Table 2's mean; the printed equation 5.4 disagrees ",
    "with it), LBW by Pesola from height. Unbound plasma carboplatin.",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    " https://doi.org/10.1007/s002800050578"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
