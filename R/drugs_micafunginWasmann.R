# -----------------------------------------------------------------------------
# Micafungin: Wasmann 2019, normal-weight and morbidly obese adults, two
# compartments (a candidate of the micafungin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-27, on Steven L. Shafer's instruction of
# 2026-11-26 ("go on to micafungin").
#
# THE SOURCE
# ==========
# Wasmann RE, Smit C, ter Heine R, et al. Pharmacokinetics and probability of
# target attainment for micafungin in normal-weight and morbidly obese adults.
# J Antimicrob Chemother 2019;74:978-985 (PMID 30649375, doi
# 10.1093/jac/dky554).  24 adults (12 women), single 100 or 200 mg doses over
# 1 h, nine samples to 24 h; 8 of normal weight and 16 morbidly obese, the
# obese on the day of bariatric surgery; NONMEM 7.4, FOCE-I.  Table 1 (page
# image), median: normal weight 4 men and 4 women, 31 years, 70.8 kg, BMI
# 22.5; obese at 100 mg 3 men and 5 women, 51 years, 156 kg, BMI 44.4; obese
# at 200 mg 5 men and 3 women, 46 years, 141 kg, BMI 43.5.  Weights 61.5-184
# kg, ages 22-61.  Table 2, final model (page image):
#
#     CL = 0.690 L/h x (TBW/70)^0.74
#     Vc = 5.84 L    x (TBW/70)^1.17
#     Vp = 6.96 L    x (TBW/70)^0.71
#     Q  = 7.15 L/h
#
# Age, sex, lean body weight and BMI were tested and not retained.
#
# THE LABEL
# =========
# The micafungin label (Table 7) gives patients' clearances of 0.20-0.36
# mL/min/kg, 0.8-1.5 L/h at 70 kg, against this model's 0.69 L/h in healthy
# volunteers; Hebert 2005's healthy volunteers (cited by Wasmann) 0.72 L/h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL, Vc and Vp keep the source's weight terms, at the pharmacokinetic weight
# with the switch on.  Q, with no size term, takes the FFM clearance factor
# anchored at the cohort (cohortSizeFactors(), Table 1's three groups with
# their BMIs).  With the switch off Q is the published 7.15 L/h.
#
# PREGNANCY, BAND, DOSE BASIS: as R/drugs_micafunginJullien.R.
# -----------------------------------------------------------------------------

MICAFUNGIN_WASMANN_CL     <- 0.690   # L/h at 70 kg
MICAFUNGIN_WASMANN_CL_WT  <- 0.74
MICAFUNGIN_WASMANN_VC     <- 5.84    # L
MICAFUNGIN_WASMANN_VC_WT  <- 1.17
MICAFUNGIN_WASMANN_VP     <- 6.96    # L
MICAFUNGIN_WASMANN_VP_WT  <- 0.71
MICAFUNGIN_WASMANN_Q      <- 7.15    # L/h
MICAFUNGIN_WASMANN_WT_REF <- 70      # kg

#' Micafungin (Wasmann 2019)
#'
#' Two compartments in normal-weight and morbidly obese adults, total weight
#' on the clearance and both volumes; see the header of
#' \code{R/drugs_micafunginWasmann.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
micafunginWasmann <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = cohortSizeFactors(groups = list(
                          list(weight = 70.8, age = 31, men = 4, women = 4, bmi = 22.5),
                          list(weight = 156, age = 51, men = 3, women = 5, bmi = 44.4),
                          list(weight = 141, age = 46, men = 5, women = 3, bmi = 43.5))))
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / MICAFUNGIN_WASMANN_WT_REF

  default <- list(
    v1  = MICAFUNGIN_WASMANN_VC * w^MICAFUNGIN_WASMANN_VC_WT,
    v2  = MICAFUNGIN_WASMANN_VP * w^MICAFUNGIN_WASMANN_VP_WT,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = MICAFUNGIN_WASMANN_CL * w^MICAFUNGIN_WASMANN_CL_WT / 60,
    cl2 = MICAFUNGIN_WASMANN_Q * size$clearance / 60,
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
    reference = paste0(
      "Wasmann RE, Smit C, ter Heine R, et al. Pharmacokinetics and probability ",
      "of target attainment for micafungin in normal-weight and morbidly obese ",
      "adults. J Antimicrob Chemother 2019;74:978-985, doi 10.1093/jac/dky554. ",
      "24 adults (61.5-184 kg), two compartments: CL 0.690 L/h x (weight/70)^0.74, ",
      "Vc 5.84 L x (weight/70)^1.17, Vp 6.96 L x (weight/70)^0.71, Q 7.15 L/h.")
  )
}
