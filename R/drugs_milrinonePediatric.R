# -----------------------------------------------------------------------------
# Milrinone, intravenous, in children (Hornik 2019)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, from
# the full text and its supplement, which he supplied.  Named for its
# population, as methylphenidatePediatric and fenoldopamPediatric are, so it
# is never an adult's default; the adult model is milrinone (Bailey 1994).
#
# THE SOURCE
# ==========
# Hornik CP, Yogev R, Mourani PM, et al.  Population pharmacokinetics of
# milrinone in infants, children, and adolescents.  J Clin Pharmacol
# 2019;59:1606-1619 (doi 10.1002/jcph.1499).  Seventy-four patients from
# birth to 18 years (median 13.1 kg, 2.6-157.7; postmenstrual age 37-977
# weeks), 111 opportunistic plasma samples, one compartment.  The final model
# (abstract and Table 2):
#
#     CL (L/h) = 15.9 x (WT/70)^0.75 x PMA^1.12 / (67.7^1.12 + PMA^1.12)
#                     x (CrCl/117)^0.522
#     V  (L)   = 32.2 x (WT/70)
#
# with PMA the postmenstrual age in weeks and CrCl the updated bedside
# Schwartz estimate, 0.413 x height (cm) / serum creatinine (mg/dL), in
# mL/min/1.73 m2, from the child's own creatinine.
#
# COVARIATES HERE
# ===============
# PMA is 40 + 52 x age in years (a term birth, the source's imputation when
# gestational age was missing).  Renal function is the bedside Schwartz
# estimate from the child's own creatinine (patientCreatinine()) below 18,
# as the source computed it; from 18, where that equation was never meant
# to apply, it is CKD-EPI 2009 (also per 1.73 m2) from an adult-equivalent
# creatinine, as dapagliflozin does.  A blank creatinine field means an
# assumed normal creatinine for age and sex.  CrCl was studied from 13 to
# 261 mL/min/1.73 m2 and is held to that range.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own allometric weight terms: the pharmacokinetic
# weight with the switch on, total weight with it off.
#
# NO EFFECT SITE
# ==============
# No time of peak effect was measured in children; plasma only.  The band is
# the 100-300 ng/mL target the source simulated against.
# -----------------------------------------------------------------------------

# Hornik 2019, Table 2
MILRINONE_PED_CL     <- 15.9    # L/h at 70 kg, mature, CrCl 117
MILRINONE_PED_V      <- 32.2    # L at 70 kg
MILRINONE_PED_TM50   <- 67.7    # weeks PMA
MILRINONE_PED_HILL   <- 1.12
MILRINONE_PED_CRCL   <- 117     # mL/min/1.73 m2, the population median
MILRINONE_PED_CRCL_EXPONENT <- 0.522
MILRINONE_PED_CRCL_RANGE <- c(13.1, 261.3)   # studied

#' Milrinone, intravenous, in children (Hornik 2019)
#'
#' One compartment with allometric weight, a postmenstrual-age maturation
#' and creatinine clearance on clearance; see the header of
#' \code{R/drugs_milrinonePediatric.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
milrinonePediatric <- function(weight, height, age, sex, adjustToFFM = TRUE,
                               creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl <- if (age < 18)
    egfrBedsideSchwartz(height, patientCreatinine(creatinine, age, sex)) else
    egfrCKDEPI2009(age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  crcl <- min(max(crcl, MILRINONE_PED_CRCL_RANGE[1]), MILRINONE_PED_CRCL_RANGE[2])

  pma <- 40 + 52 * age
  maturation <- pma^MILRINONE_PED_HILL /
    (MILRINONE_PED_TM50^MILRINONE_PED_HILL + pma^MILRINONE_PED_HILL)
  cl <- MILRINONE_PED_CL * (pkW / 70)^0.75 * maturation *
    (crcl / MILRINONE_PED_CRCL)^MILRINONE_PED_CRCL_EXPONENT

  default <- list(
    v1  = MILRINONE_PED_V * pkW / 70,
    v2  = 1,                     # one compartment
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    "an assumed normal creatinine for age and sex" else
    sprintf("creatinine %.2f mg/dL", creatinine)
  reference <- paste0(
    "Hornik CP et al., J Clin Pharmacol 2019;59:1606-1619.  One compartment in ",
    "74 patients aged 0-18 years: CL 15.9 L/h x (WT/70)^0.75 x maturation ",
    "(TM50 67.7 weeks PMA, Hill 1.12) x (CrCl/117)^0.522, V 32.2 L x WT/70; ",
    "CrCl by bedside Schwartz (CKD-EPI from 18) from ", creatinineNote, ". ",
    "https://doi.org/10.1002/jcph.1499"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 167,
    upperTypical = 300,
    lowerTypical = 100,
    reference = reference
  )
}
