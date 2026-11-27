# -----------------------------------------------------------------------------
# Micafungin: Jullien 2017, ventilated ICU patients with sepsis, two
# compartments (a candidate of the micafungin group, R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-27, on Steven L. Shafer's instruction of
# 2026-11-26 ("go on to micafungin").
#
# THE SOURCE
# ==========
# Jullien V, Azoulay E, Schwebel C, et al. Population pharmacokinetics of
# micafungin in ICU patients with sepsis and mechanical ventilation.
# J Antimicrob Chemother 2017;72:181-189 (PMID 27609051, doi
# 10.1093/jac/dkw352).  The EMPIRICUS trial's micafungin arm: 99 adults in 19
# French ICUs, 100 mg over 1 h, five samples after the first dose, 436
# concentrations, total plasma micafungin by HPLC; NONMEM 7.3, FOCE-I.
# Table 1: 66 men and 33 women, age median 61.4 years (29.9-92.7), weight
# median 84.5 kg (48-141), serum albumin median 21 g/L; no height or BMI.
# Final model (read on the page images, 2026-11-26):
#
#     CL = 1.34 L/h x (BW/84)^0.59 x 1.14 (albumin <= 25 g/L) x 0.75 (SOFA >= 10)
#     Vc = 11.8 L   x (BW/84)^0.61 x 1.14 (albumin <= 25 g/L)
#     Vp = 7.68 L   x (BW/84)^0.67 x 1.14 (albumin <= 25 g/L)
#     Q  = 4.67 L/h
#
# Its Table 3 gives the mean AUC after 100 mg at 84 kg as 74.6 mg h/L with
# SOFA < 10 and albumin > 25 g/L, which is 100 / 1.34.
#
# COVARIATES
# ==========
# Albumin: the Patient Profile's Serum albumin (g/dL).  At or below 2.5 g/dL
# the factor 1.14 is applied to CL, Vc and Vp; above it, or when the field is
# blank, it is not: a blank field is the model's reference category,
# albumin > 25 g/L, although the cohort's own median was 21 g/L.
# SOFA: there is no field; it is held below 10 (the reference category; the
# cohort's median was 7).  A patient with a SOFA score of 10 or more clears
# about a quarter less, and is drawn too low.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL, Vc and Vp keep the source's weight terms, evaluated at the
# pharmacokinetic weight with the switch on (total weight with it off).  Q has
# no size term and takes the FFM clearance factor.  BODY SIZE ANCHOR: at the
# reference man, not the cohort: Table 1 prints no height or BMI, and the
# French cohort's typical height was not assembled from NCD-RisC in this
# session.  With the switch off Q is the published 4.67 L/h.
#
# PREGNANCY
# =========
# No pharmacokinetic study of micafungin in pregnancy was found (PubMed,
# 2026-11-27); the label has animal data only.  Not applied.
#
# EFFECT SITE AND BAND
# ====================
# None: the targets are AUC/MIC ratios (Andes 2011), not a concentration.
#
# DOSE BASIS
# ==========
# Doses are micafungin as labelled (the vials are labelled as micafungin,
# supplied as the sodium salt).
# -----------------------------------------------------------------------------

MICAFUNGIN_JULLIEN_CL       <- 1.34   # L/h at 84 kg
MICAFUNGIN_JULLIEN_CL_WT    <- 0.59
MICAFUNGIN_JULLIEN_VC       <- 11.8   # L
MICAFUNGIN_JULLIEN_VC_WT    <- 0.61
MICAFUNGIN_JULLIEN_VP       <- 7.68   # L
MICAFUNGIN_JULLIEN_VP_WT    <- 0.67
MICAFUNGIN_JULLIEN_Q        <- 4.67   # L/h
MICAFUNGIN_JULLIEN_WT_REF   <- 84     # kg
MICAFUNGIN_JULLIEN_ALBUMIN_FACTOR <- 1.14
MICAFUNGIN_JULLIEN_ALBUMIN_CUT    <- 2.5   # g/dL (25 g/L)

#' Micafungin (Jullien 2017)
#'
#' Two compartments in ventilated ICU patients with sepsis, weight on the
#' clearance and both volumes and a step in serum albumin; see the header of
#' \code{R/drugs_micafunginJullien.R}.
#'
#' @inheritParams cefazolin
#' @param albumin serum albumin in g/dL, or NULL for the model's reference
#'   (above 2.5 g/dL)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
micafunginJullien <- function(weight, height, age, sex, adjustToFFM = TRUE,
                              albumin = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  pkW <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  w <- pkW / MICAFUNGIN_JULLIEN_WT_REF
  alb <- if (!is.null(albumin) && is.finite(albumin) &&
             albumin <= MICAFUNGIN_JULLIEN_ALBUMIN_CUT)
    MICAFUNGIN_JULLIEN_ALBUMIN_FACTOR else 1

  default <- list(
    v1  = MICAFUNGIN_JULLIEN_VC * w^MICAFUNGIN_JULLIEN_VC_WT * alb,
    v2  = MICAFUNGIN_JULLIEN_VP * w^MICAFUNGIN_JULLIEN_VP_WT * alb,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = MICAFUNGIN_JULLIEN_CL * w^MICAFUNGIN_JULLIEN_CL_WT * alb / 60,
    cl2 = MICAFUNGIN_JULLIEN_Q * size$clearance / 60,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Jullien V, Azoulay E, Schwebel C, et al. Population pharmacokinetics of ",
    "micafungin in ICU patients with sepsis and mechanical ventilation. ",
    "J Antimicrob Chemother 2017;72:181-189, doi 10.1093/jac/dkw352. 99 ICU ",
    "patients, two compartments: CL 1.34 L/h x (weight/84)^0.59, Vc 11.8 L x ",
    "(weight/84)^0.61, Vp 7.68 L x (weight/84)^0.67, Q 4.67 L/h; x 1.14 on CL, ",
    "Vc and Vp at albumin 2.5 g/dL or less",
    if (is.null(albumin)) " (albumin blank: above 2.5 g/dL assumed)" else "",
    "; SOFA held below 10."
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
