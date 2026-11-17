# -----------------------------------------------------------------------------
# Fentanyl: three compartments, allometric in weight (Bae 2020), plasma
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in mcg
# of fentanyl, concentrations in ng/mL.
#
# SOURCE
# ======
# Bae J, Kwon M, Lee YH, Lee EK, Choi BM, Noh GJ.  An allometric
# pharmacokinetic model and minimum effective analgesic concentration of
# fentanyl in patients undergoing major abdominal surgery.  Br J Anaesth
# 2020;125(6):976-985.  PMID 32861508.  https://doi.org/10.1016/j.bja.2020.06.066
# Read in full from the archive's PDF (PMID_32861508.pdf), 2026-11-17; Table 2
# was also checked on the page image by the digest card
# (papers/digests/PMID_32861508.md).  The Supplementary material (the
# competing models of Table S1 and the NONMEM control stream) was not read.
#
# 95 adults, 2092 plasma concentrations (Table 1): the five data sets the
# library's earlier pooled set came from, supplied by Steven L. Shafer
# (McClain and Hug 1980, n = 7, volunteers 22-29 y; Scott and Stanski 1987,
# n = 19, 20-88 y; Hudson 1986, n = 10, 55-80 y; Varvel 1989, n = 8, 33-57 y;
# Shafer 1990, n = 21, mean 58 y), and 30 Korean patients (32-73 y,
# 46.4-84.5 kg) given 100 mcg before open abdominal surgery under propofol
# and remifentanil, 14 arterial samples to 12 h, LC-MS/MS (LLOQ 0.01 ng/mL).
# Weights 40-105 kg across the data sets.  NONMEM VII, FOCE-I.
#
#     V1  = 10.1  x (WT/70)^1.23   L
#     V2  = 26.5  x (WT/70)^1.23   L
#     V3  = 206   x (WT/70)^1.23   L
#     CL  = 0.704 x (WT/70)^0.313  L/min
#     Q2  = 2.38  x (WT/70)^0.313  L/min
#     Q3  = 1.49  x (WT/70)^0.313  L/min
#
# The two exponents were estimated (volumes 1.23, 95% CI 0.87-1.61;
# clearances 0.313, 0.037-0.583), chosen over fixed 1 and 0.75 by 12.6
# points of the objective function (P = 0.002); MDAPE 22.7% (23.7% with the
# fixed exponents).  No other covariate entered.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own weight terms are evaluated at the pharmacokinetic weight
# with the switch on (size$pkWeight: 70 kg x FFM / 54.5 kg) and at total
# weight with it off, as for alprazolam and oxytocin.  The reference man
# (70 kg, 170 cm, 35 y) therefore receives the published values in both
# positions.  The studied weights were 40-105 kg; outside them the exponents
# are an extrapolation, and the clearance exponent is the less certain of the
# two (its interval runs from almost 0 to 0.58).
#
# EFFECT SITE
# ===========
# tPeak 3.6 min, Shafer and Varvel 1991 (Fig 2, EEG), as before; ke0 is
# solved from it on this model.  Bae estimated no effect site (their
# effect-site concentrations used Scott and Stanski's ke0 of 0.147 /min).
#
# MEAC
# ====
# 0.6 ng/mL, the library's value, near Gourlay 1988's 0.63 +/- 0.25 ng/mL
# (Anesth Analg 67:329-337), which is a MINIMUM EFFECTIVE CONCENTRATION in
# blood, sampled immediately before each patient-controlled demand, not a
# post-titration MEAC.  Bae 2020 measured both after open abdominal surgery:
# median MEC 0.72 and MEAC 0.99 ng/mL (venous plasma).  Not changed with the
# PK: see docs/opioid-review-2026-10-27.md.
#
# HISTORY
# =======
# Until 2026-11-17 the library used a pooled set of the same five legacy
# studies whose values are printed in none of them (V1 12.1, V2 35.7,
# V3 224 L; CL 0.632, Q2 2.8, Q3 1.55 L/min; volumes on fat-free mass,
# clearances on its 0.75 power), adopted in November 2019 (a20fd1c) with its
# derivation unrecorded.  Bae refitted those five with 30 new patients:
# clearance 11% higher, V1 17% and the slow volume 8% smaller at 70 kg.
# Replaced at the request of Steven L. Shafer, 2026-11-17.
# -----------------------------------------------------------------------------

# Bae 2020, Table 2 (70 kg)
FENTANYL_V1  <- 10.1     # L
FENTANYL_V2  <- 26.5     # L
FENTANYL_V3  <- 206      # L
FENTANYL_CL1 <- 0.704    # L/min
FENTANYL_CL2 <- 2.38     # L/min (Q1 in the paper)
FENTANYL_CL3 <- 1.49     # L/min (Q2 in the paper)
FENTANYL_VOLUME_EXPONENT    <- 1.23
FENTANYL_CLEARANCE_EXPONENT <- 0.313
FENTANYL_REFERENCE_WEIGHT   <- 70   # kg

#' Fentanyl pharmacokinetics
#'
#' Bae et al. (2020): three compartments, volumes x (WT/70)^1.23 and
#' clearances x (WT/70)^0.313.  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) evaluates the weight terms at
#'   the pharmacokinetic weight; \code{FALSE} at total body weight.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
fentanyl <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  wt <- if (isTRUE(adjustToFFM)) {
    pkSizeFactors(weight, height, age, sex)$pkWeight
  } else weight
  v <- (wt / FENTANYL_REFERENCE_WEIGHT)^FENTANYL_VOLUME_EXPONENT
  k <- (wt / FENTANYL_REFERENCE_WEIGHT)^FENTANYL_CLEARANCE_EXPONENT

  default <- list(
    v1  = FENTANYL_V1  * v,
    v2  = FENTANYL_V2  * v,
    v3  = FENTANYL_V3  * v,
    cl1 = FENTANYL_CL1 * k,
    cl2 = FENTANYL_CL2 * k,
    cl3 = FENTANYL_CL3 * k
  )

  MEAC <- 0.6
  list(
    PK = list(default = default),
    tPeak = 3.6,      # Shafer & Varvel 1991, Fig 2 (EEG)
    MEAC = MEAC,
    typical = MEAC * 1.2,
    upperTypical = MEAC * 2.0,
    lowerTypical = MEAC * 0.8,
    reference = paste0(
      "Bae J et al., Br J Anaesth 2020;125(6):976-985. ",
      "https://doi.org/10.1016/j.bja.2020.06.066 (three compartments, ",
      "volumes x (WT/70)^1.23, clearances x (WT/70)^0.313; tPeak 3.6 min ",
      "from Shafer SL, Varvel JR, Anesthesiology 1991;74:53-63)"
    )
  )
}
