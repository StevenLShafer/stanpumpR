# -----------------------------------------------------------------------------
# Caspofungin in adults: Wurthwein 2013, two compartments, weight on CL and V1
# (the adults' candidate of the caspofungin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg
# of caspofungin, concentrations in mcg/mL (= mg/L) of total caspofungin in
# plasma.  Concentration only.  Drafted by Claude Code, 2026-11-28, on Steven L.
# Shafer's decision of that day: Wurthwein 2013 for adults, Xu 2026 (R/drugs_
# caspofunginXu.R) for children.
#
# SOURCE
# ======
# Wurthwein G, Cornely OA, Trame MN, Vehreschild JJ, Vehreschild MJGT,
# Farowski F, Muller C, Boos J, Hempel G, Hallek M, Groll AH.  Population
# pharmacokinetics of escalating doses of caspofungin in a phase II study of
# patients with invasive aspergillosis.  Antimicrob Agents Chemother
# 2013;57:1664-1671 (PMID 23335740, PMC3623320, doi 10.1128/AAC.01912-12;
# card papers/digests/PMID_23335740.md).  Read on the PDF Shafer supplied on
# 2026-11-28, with its supplement (two figures, no numbers).  46 adults with
# invasive aspergillosis at three German university hospitals, 70, 100, 150 or
# 200 mg once daily as a 2-hour infusion for up to 28 days; 462 plasma samples
# (day 1 profiles, then peaks and troughs on days 4, 7, 14 and 28), total
# caspofungin by LC-MS/MS; NONMEM 7.
#
# Table 1: 21 men and 25 women, median age 61 years (18-74), median weight 76
# kg (43-104); no height or BMI printed.
#
# Table 2, final model, at the median body weight of 76 kg:
#
#     CL = 0.411 L/h x [1 + 0.0102 (BW - 76)]
#     V1 = 5.85 L    x [1 + 0.0102 (BW - 76)]   (one factor for both)
#     Q  = 0.843 L/h
#     V2 = 6.53 L
#
# The Methods give the covariate's form, CL_i = theta1 [1 + theta2 (BW -
# BW_median)], and the Results say one fractional change serves CL and V1.
# The abstract's V1 of 5.785 L is not the table's 5.85 L (the bootstrap mean
# is 5.78); the table's final estimate is used.  Gender, age and creatinine
# clearance were significant in the forward step but kept RSEs of 34-46% and
# were not retained.  A Michaelis-Menten model gave Km 289 mg/L, far above the
# concentrations, so elimination is linear.
#
# CHECKS.  Table 3's steady-state geometric-mean AUCs, 170, 243, 365 and 487
# mg h/L at 70-200 mg, are dose / 0.411 L/h to the printed digit (the tests
# pin them), and its Cmax and Cmin are compared in the tests.  The CANCIDAS
# label's adults (Table 8, harmonic-mean clearance 10.6 mL/min, AUC 87.3 mg
# h/L at 50 mg) clear caspofungin faster than these patients: a 70 kg adult
# here clears 6.4 mL/min (AUC 130 mg h/L at 50 mg).  Wurthwein's own
# comparison is with the CASLAMB patients (CL 0.462 L/h), close to its own.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL and V1 keep the source's weight term, evaluated at the pharmacokinetic
# weight with the switch on and at the total body weight with it off.  The
# term is linear and is not held to the studied 43-104 kg: it stays positive
# down to -22 kg, so it is finite for every legal patient, and an adult of
# 150 kg receives 1.75 times the 76 kg clearance.  Q and V2, with no size
# term, take the FFM factors.  BODY SIZE ANCHOR: Q and V2 are anchored at the
# reference man, not at Wurthwein's cohort.  Table 1 prints the cohort's
# median weight (76 kg), age (61 years) and sexes (21 men, 25 women) but no
# height or BMI, so its typical height would come from NCD-RisC's German
# heights for the birth cohort about 1947, which this session could not reach
# (papers/pending-work.csv).  With the switch off, Q and V2 are the published
# values (legacy factors 1).
#
# PREGNANCY: none applied; see the caspofungin registry row (no study found;
# about 1.4% of a dose is excreted unchanged, so the rise in filtration does
# not reach it).
# BAND AND THRESHOLD: none; the targets are AUC/MIC ratios.
# DOSE BASIS: caspofungin, as CANCIDAS is labelled and as the paper dosed it.
# -----------------------------------------------------------------------------
CASPOFUNGIN_WURTHWEIN <- list(
  cl = 0.411,          # L/h at 76 kg
  v1 = 5.85,           # L at 76 kg (Table 2's final model)
  q = 0.843,           # L/h
  v2 = 6.53,           # L
  weightMedian = 76,   # kg
  weightSlope = 0.0102 # fractional change per kg, on CL and V1
)

#' Caspofungin in adults (Wurthwein 2013)
#'
#' Two compartments in adults with invasive aspergillosis, a linear weight
#' term on clearance and the central volume; the adults' candidate of the
#' caspofungin group.  See the header of
#' \code{R/drugs_caspofunginWurthwein.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
caspofunginWurthwein <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- CASPOFUNGIN_WURTHWEIN
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  f <- 1 + p$weightSlope * (w - p$weightMedian)

  default <- list(
    v1  = p$v1 * f,
    v2  = p$v2 * size$volume,
    v3  = 1,                     # placeholder: a two-compartment model
    cl1 = p$cl * f / 60,
    cl2 = p$q * size$clearance / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = paste0(
      "Wurthwein G et al., Antimicrob Agents Chemother 2013;57:1664-1671, Table 2 (46 adults ",
      "with invasive aspergillosis, 18-74 years, 43-104 kg): two compartments, CL 0.411 L/h ",
      "and V1 5.85 L x [1 + 0.0102 (weight - 76 kg)], Q 0.843 L/h and V2 6.53 L scaled by ",
      "fat-free mass; total plasma caspofungin. https://doi.org/10.1128/AAC.01912-12")
  )
}
