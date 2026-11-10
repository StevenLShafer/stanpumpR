# -----------------------------------------------------------------------------
# Vincristine, intravenous, in adults (Igarashi 2021): plasma vincristine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C31).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Table 1, and Table 2 with its footnote on the page image.
#
# THE SOURCE
# ==========
# Igarashi T, Kishi S, Hosono N, et al.  Population pharmacokinetic model
# development and exposure-response analysis of vincristine in patients with
# malignant lymphoma.  Cancer Chemother Pharmacol 2021;87:501-511 (doi
# 10.1007/s00280-020-04220-y).  30 Japanese adults (17 women), 25-82 years,
# BSA 1.29-2.10 m2, on R-CHOP-like regimens; 0.4-2.0 mg (capped at 2 mg) as a
# 10-min infusion; plasma at 10 and 45 min and 3, 7 and 19 h; LC-MS, 0.3-50
# ng/mL; NONMEM, FOCE.
#
# THE MODEL (Table 2 and its footnote)
# ====================================
# Three compartments, linear:
#
#     CL = 31.1 L/h x (BSA/1.60)^1.74 x (age/60)^-0.485
#     V1 = 13.2 L
#     Q2 = 60.2 L/h, V2 = 33.9 L   (fixed "at the average value")
#     Q3 = 81.1 L/h, V3 = 544 L
#
# The paper's own check reproduces: BSA 1.4 to 1.9 m2 and age 53 to 82 give
# 1.701- and 0.809-fold changes in CL (printed 1.7 and 0.8).  Derived here,
# not printed: for the typical patient the half-lives are 3.0 min, 36 min and
# 17.6 h and Vss 591 L, and 2 mg over 10 min gives a peak of 64 ng/mL and an
# AUC of 64 ng h/mL, within Table 3's medians (49-65 and 45-65).
#
# Weaknesses the paper leaves: the 3-min phase is shorter than the first
# sample (10 min), so V1 is poorly determined (bootstrap 95% CI 1.2-18.5 L);
# Q2 and V2 were fixed at an unstated average; only 659 of 1000 bootstraps
# converged; the median peaks (55-65 ng/mL) exceed the assay's calibrated 50
# ng/mL with no dilution described; samples end at 19 h, about one terminal
# half-life.
#
# The BSA formula is not named (Mosteller's is used, as for the library's
# other chemotherapy models; DuBois would change CL by 2-3%).  The age term
# is a negative power, floored at 18 (ADULT_AGE_TERM_FLOOR; the source's
# youngest was 25).
#
# DOSE BASIS
# ==========
# Doses are entered as labelled, mg of vincristine sulfate; the paper does
# not say whether it recorded the sulfate or the base (0.894 of it), and the
# assay measures the base, so the parameters map the dose as given to the
# base measured.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# BSA is computed at the pharmacokinetic weight with the switch on and total
# weight with it off; the volumes and intercompartmental clearances, which
# carry no covariate, scale by the library's factors with the switch on and
# are fixed with it off.
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND: the neuropathy and exposure-response relations are
# not a therapeutic range.
# -----------------------------------------------------------------------------

# Igarashi 2021, Table 2
VINCRISTINE_IGARASHI <- list(
  cl = 31.1,            # L/h at BSA 1.60 m2, age 60
  bsaReference = 1.60,
  bsaExponent = 1.74,
  ageReference = 60,
  ageExponent = -0.485,
  v1 = 13.2,            # L
  q2 = 60.2, v2 = 33.9, # L/h, L (fixed in the source)
  q3 = 81.1, v3 = 544   # L/h, L
)

#' Vincristine, intravenous, in adults (Igarashi 2021)
#'
#' Three compartments; body surface area and age on clearance.  See the
#' header of \code{R/drugs_vincristineIgarashi.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vincristineIgarashi <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- VINCRISTINE_IGARASHI
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  bsa <- bsaMosteller(w, height)
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  cl <- p$cl * (bsa / p$bsaReference)^p$bsaExponent *
    (ageTerm / p$ageReference)^p$ageExponent                         # L/h

  default <- list(
    v1  = p$v1 * size$volume,
    v2  = p$v2 * size$volume,
    v3  = p$v3 * size$volume,
    cl1 = cl / 60,
    cl2 = p$q2 / 60 * size$clearance,
    cl3 = p$q3 / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Igarashi T, Kishi S, Hosono N, et al. Population pharmacokinetic model ",
    "development and exposure-response analysis of vincristine in patients with ",
    "malignant lymphoma. Cancer Chemother Pharmacol 2021;87:501-511. 30 adults: ",
    "three compartments, CL 31.1 L/h x (BSA/1.60)^1.74 x (age/60)^-0.485 (Mosteller; ",
    "age floored at 18), V1 13.2 L, Q2 60.2 L/h, V2 33.9 L, Q3 81.1 L/h, V3 544 L. ",
    "Doses as labelled (vincristine sulfate). Plasma vincristine in ng/mL. ",
    "https://doi.org/10.1007/s00280-020-04220-y"
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
