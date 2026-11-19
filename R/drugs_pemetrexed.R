# -----------------------------------------------------------------------------
# Pemetrexed, intravenous (Latz 2006): plasma pemetrexed
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C20).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables 2 and 3 and the covariate equations, on the page
# images.
#
# THE SOURCE
# ==========
# Latz JE, Chaudhary A, Ghosh A, Johnson RD.  Population pharmacokinetic
# analysis of ten phase II clinical trials of pemetrexed in cancer patients.
# Cancer Chemother Pharmacol 2006;57:401-411 (doi 10.1007/s00280-005-0036-1).
# 287 adults (128 women) from ten single-agent phase II trials, 26-79 years,
# BSA 1.26-2.50 m2, Cockcroft-Gault creatinine clearance 44-225 mL/min
# (median 92.6); 500 or 600 mg/m2 over 10 min every 21 days, WITHOUT the
# folic acid and vitamin B12 now given with every dose; sparse plasma samples
# to 36 h; HPLC-UV and LC-MS/MS; NONMEM, FOCE with interaction.
#
# THE MODEL (Table 3)
# ===================
# Two compartments, linear:
#
#     CL (mL/min) = 43.0 + 47.2 x CrCL / 92.6
#     V1 (L) = 6.13 x BSA^1.32       (raw BSA in m2, not centred)
#     Q = 14.5 mL/min    V2 = 3.38 L
#
# CrCL is "standard" Cockcroft-Gault in mL/min, not normalised to 1.73 m2,
# with no cap stated.  The paper names neither the weight in Cockcroft-
# Gault (taken as total weight, "standard" being set against the lean-weight
# variant the authors rejected) nor the BSA formula (Mosteller's is used, as
# for the library's other chemotherapy models).  Q is 14.5 mL/min in Table 3
# and 14.4 in the Abstract and Discussion; the table's is used (the half-life
# moves by less than 0.5%).
#
# Checks: at the median CrCL 92.6 mL/min and BSA 1.74 m2 the model gives CL
# 90.2 mL/min, a terminal half-life of 3.50 h (the paper: "about 3.5 h") and
# Vss 16.1 L (16.3 at the paper's own V1 of 12.9 L, which is V1 at the mean
# BSA).  The Alimta label gives CL 91.8 mL/min, Vss 16.1 L, half-life 3.5 h.
#
# The intercept means clearance never falls below 43 mL/min (for the
# cohort's typical patient; see BODY SIZE) however poor the kidneys, which
# the data cannot support below a CrCL of about 40 mL/min (the lowest
# studied was 36): the authors themselves suggest a sigmoid or a
# split line would be better there.  Pemetrexed is contraindicated below 45
# mL/min.  Folate deficiency (CL 33% lower, n = 5) was found and left out of
# the final model.
#
# DOSE BASIS
# ==========
# Pemetrexed acid: the paper doses "mg pemetrexed per m2", and Alimta, the
# disodium heptahydrate, is labelled and dosed as the acid.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine through adultEquivalentCreatinine()
# (an adult equation), or the library's assumed normal creatinine when blank,
# in Cockcroft-Gault with the weight below.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# BSA and the Cockcroft-Gault weight are evaluated at the pharmacokinetic
# weight with the switch on and total weight with it off.  Q, V2 and the
# non-renal intercept of the clearance (43.0 mL/min, a fixed value, which
# takes the clearance factor as amikacin's does), with no covariate, belong
# to the cohort's typical patient, and with the switch on they are anchored at
# that patient (PEMETREXED_LATZ_COHORT, below; Steven L. Shafer, 2026-11-19,
# after Carbon's second repository audit, A2-F4): scaled from the 70 kg
# reference man, as Q and V2 were, Latz's own typical patient received a V2
# 14% and a Q 10% below the published values, and the intercept was not
# scaled at all.  The Cockcroft-Gault slope is renal and is not anchored.  A
# model with a size covariate on one parameter is not thereby exempt on the
# others.  With the switch off they are fixed.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pregnancy pharmacokinetics found.  Pemetrexed is cleared mostly
# unchanged by the kidney, so the creatinine clearance takes the rise in
# filtration of pregnancy (x 1.5, pregnancyGfrFactor(), only when the
# creatinine is blank): physiological, an assumption.
#
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Latz 2006, Table 3
PEMETREXED_LATZ <- list(
  clIntercept = 43.0,     # mL/min
  clCrCL = 47.2,          # mL/min at the median CrCL
  crclMedian = 92.6,      # mL/min
  v1 = 6.13,              # L per m2^1.32
  v1BSAExponent = 1.32,
  q = 14.5,               # mL/min
  v2 = 3.38               # L
)

# Latz 2006, the index data set on which the model was built (8 trials, 209
# patients), as confirmed on the PDF 2026-11-19: Table 1, 108 men and 101
# women; Table 2, medians 57.3 years, 66.0 kg, BSA 1.74 m2.  No height is
# printed; it is back-solved from the weight and BSA by Du Bois's formula
# (166.7 cm; the paper's BSA formula is not named).  The validation set is
# not the fit's cohort and is left out.
PEMETREXED_LATZ_COHORT <- list(weight = 66.0, height = 166.7, age = 57.3, men = 108, women = 101)

#' Pemetrexed, intravenous (Latz 2006)
#'
#' Two compartments; Cockcroft-Gault creatinine clearance on clearance and
#' body surface area on V1.  See the header of \code{R/drugs_pemetrexed.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
pemetrexed <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  p <- PEMETREXED_LATZ
  # Q, V2 and the non-renal intercept, with no covariate, are anchored at
  # Latz's index set (PEMETREXED_LATZ_COHORT); V1 carries its own BSA term
  # and the renal slope its Cockcroft-Gault clearance
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, PEMETREXED_LATZ_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  crcl <- creatinineClearanceCG(w, age, sex, scr) * gfrPregnancy      # mL/min

  # the non-renal intercept, a fixed value, takes the clearance factor
  cl <- p$clIntercept * size$clearance +
    p$clCrCL * crcl / p$crclMedian                                     # mL/min

  default <- list(
    v1  = p$v1 * bsa^p$v1BSAExponent,
    v2  = p$v2 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl / 1000,
    cl2 = p$q / 1000 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Latz JE, Chaudhary A, Ghosh A, Johnson RD. Population pharmacokinetic analysis ",
    "of ten phase II clinical trials of pemetrexed in cancer patients. Cancer ",
    "Chemother Pharmacol 2006;57:401-411. 287 adults: two compartments, CL 43.0 + ",
    "47.2 x CrCL/92.6 mL/min (Cockcroft-Gault, from the entered or an assumed ",
    "normal creatinine), V1 6.13 x BSA^1.32 L (Mosteller), Q 14.5 mL/min, V2 3.38 L. ",
    "Fitted without folate and vitamin B12 supplementation. Plasma pemetrexed",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1007/s00280-005-0036-1"
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
