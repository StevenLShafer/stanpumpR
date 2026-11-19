# -----------------------------------------------------------------------------
# Fluorouracil, intravenous (Tan 2026): plasma 5-FU, two compartments with
# Michaelis-Menten elimination alone
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L), so Vmax is in mg/min and Km in
# mg/L.
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer, as
# the library's second 5-FU model beside fluorouracil (Briki 2026), as
# levodopaSimon sits beside levodopa (docs/chemotherapy.md,
# inst/extdata/chemotherapyRegistry.csv, source P09).  Every number below was
# read from the PDF, which Shafer supplied (2026-11-06): Tables 1-3 and the
# Results.
#
# THE SOURCE
# ==========
# Tan Z, Sancho-Araiz A, Voller S, Peeters SLJ, Manten T, Deenen MJ, Maring
# JG, Bet PM, Mathot RAA, Knibbe CAJ, Moes DJAR.  Advancing precision dosing
# of 5-FU: population PK model development, limited sampling strategies, and
# fit-for-use application.  Clin Pharmacokinet 2026;65:1027-1042
# (doi 10.1007/s40262-026-01645-1).  140 Dutch adults from four prospective
# studies (333 dosing records), median age 63, mean BSA 1.94 m2 (SD 0.25),
# 55% men, mostly colorectal and pancreatic cancer; 46-h infusions with or
# without a bolus (median bolus 385, infusion 2340 mg/m2).  DPYD genotyped in
# all (71% wild type).  NONMEM.
#
# THE MODEL (Table 2)
# ===================
# Two compartments, elimination by a saturable pathway only:
#
#     Vmax = 1900 mg/h x (BSA/1.94)^1.14     Km = 8.04 mg/L
#     V1 = 19.7 L    Q = 52.4 L/h    V2 = 15 L
#
# BSA was the only covariate retained; DPYD activity score, sex and the
# cycle were tested and not retained.  The paper does not name its BSA
# formula (BSA was collected as each centre recorded it); Mosteller's is
# used, as for fluorouracil.  The test reproduces Table 3's median AUC for
# 2400 mg/m2 over 46 h (21 mg h/L) and with a 400 mg/m2 bolus (29).
#
# HOW IT DIFFERS FROM BRIKI 2026 (fluorouracil)
# =============================================
# Briki's model, fitted to 726 patients, has a parallel LINEAR clearance
# (52.6 L/h) beside a smaller saturable pathway (Vmax 429 mg/h, Km 2.34 mg/L);
# Tan's, in 140, has saturable elimination alone, with a larger capacity
# and a higher Km.  For a 60-year-old man of BSA 1.94 m2 (Mosteller), 2400
# mg/m2 over 46 h gives an AUC of 20.8 mg h/L here and 17.9 with Briki's
# model, and with a 400 mg/m2 bolus first 28.7 and 24.7: Briki's runs about
# 14% lower throughout, a difference between the two cohorts that neither
# model can settle.  Both are candidates of the `fluorouracil` group
# (R/modelSelection.R), which chooses for the patient by the studied ranges
# and then the larger study, so an adult the two both include gets Briki's.
#
# WHAT IT DOES NOT COVER
# ======================
# DPD deficiency (not retained, though 29% of the patients carried a DPYD
# variant), the slow rise during a 46-h infusion, circadian variation,
# renal function (not tested on the final model), children.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# As for fluorouracil: BSA from the pharmacokinetic weight with the switch on
# and total weight with it off.  V1, V2 and Q, with no covariate, belong to
# the cohort's typical patient, and with the switch on they are anchored at
# that patient (FLUOROURACIL_TAN_COHORT, below; Steven L. Shafer, 2026-11-19,
# after Carbon's second repository audit, A2-F4).  Tan's typical patient is
# so close to the 70 kg reference man in fat-free mass that the factor is
# 1.000 (to three places): the anchor changes nothing measurable, and is
# applied for consistency.  A model with a size covariate on one parameter is
# not thereby exempt on the others.  With the switch off they are fixed.
#
# PREGNANCY: see fluorouracil (unknown; nothing applied).
# NO EFFECT SITE, NO BAND: the AUC target (20-30 mg h/L) is not drawn.
# -----------------------------------------------------------------------------

# Tan 2026, Table 2
FLUOROURACIL_TAN <- list(
  vmax = 1900,        # mg/h at BSA 1.94 m2
  bsaReference = 1.94,
  bsaExponent = 1.14,
  km = 8.04,          # mg/L
  v1 = 19.7,          # L
  q = 52.4,           # L/h
  v2 = 15             # L
)

# Tan 2026, Table 1 (p. 1032), the Overall column: 140 patients, 77 men and
# 63 women; median age 63, mean weight 78.64 kg, mean BSA 1.94 m2.  No height
# is printed; it is back-solved from the weight and BSA by Du Bois's formula
# (174.8 cm; the paper's BSA formula is not named).
FLUOROURACIL_TAN_COHORT <- list(weight = 78.64, height = 174.8, age = 63, men = 77, women = 63)

#' Fluorouracil, intravenous (Tan 2026)
#'
#' Two compartments with Michaelis-Menten elimination alone, Vmax scaled by
#' body surface area; run by \code{advanceMichaelisMenten()}.  See the header
#' of \code{R/drugs_fluorouracilTan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with a
#'   \code{michaelisMenten} block
#' @export
fluorouracilTan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- FLUOROURACIL_TAN
  # V1, V2 and Q, with no covariate, are anchored at Tan's 140 patients
  # (FLUOROURACIL_TAN_COHORT); Vmax carries its own BSA term
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, FLUOROURACIL_TAN_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  vmax <- p$vmax * (bsa / p$bsaReference)^p$bsaExponent / 60      # mg/min
  km <- p$km                                                       # mg/L

  default <- list(
    v1 = p$v1 * size$volume,
    v2 = p$v2 * size$volume,
    v3 = 1,                                  # placeholder: two compartments
    # The low-concentration limit, Vmax / Km, for the help tables only: the
    # simulation uses the michaelisMenten block.
    cl1 = vmax / km,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Tan Z, Sancho-Araiz A, Voller S, et al. Advancing precision dosing of 5-FU: ",
    "population PK model development, limited sampling strategies, and fit-for-use ",
    "application. Clin Pharmacokinet 2026;65:1027-1042. 140 adults: two ",
    "compartments with Michaelis-Menten elimination alone, Vmax 1900 mg/h x ",
    "(BSA/1.94)^1.14 (Mosteller), Km 8.04 mg/L, V1 19.7 L, Q 52.4 L/h, V2 15 L. ",
    "DPD deficiency not represented. Plasma 5-FU only. ",
    "https://doi.org/10.1007/s40262-026-01645-1"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    michaelisMenten = list(vmax = vmax, km = km)
  )
}
