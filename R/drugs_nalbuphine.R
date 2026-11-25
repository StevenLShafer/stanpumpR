# -----------------------------------------------------------------------------
# Nalbuphine: two compartments, intravenous bolus, adults under general anesthesia
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mcg,
# concentrations in ng/mL (mcg/L).  Concentration only: no effect site, MEAC or
# band (no source was read for any of them).  Drafted by Claude Code,
# 2026-11-25, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Anesthesia: "single models of ... nalbuphine").
#
# SOURCE
# ======
# Nie X, Gao X, Gao J, Heng T, Zhang Y, Sun Y, Feng Z, Jia L, Wang M.
# Population pharmacokinetics of nalbuphine in patients undergoing general
# anesthesia surgery.  Front Pharmacol 2023;14:1130287.
# https://doi.org/10.3389/fphar.2023.1130287   (PMID 37025491, read on the PDF)
# 27 adults (12 men, 15 women) at elective surgery, ages 21-76 (mean 53.85),
# weight 48-82 kg (mean 63.32), height 153-175 cm (mean 163.96); nalbuphine
# 0.24 mg/kg (0.18-0.31) intravenously, 353 concentrations by HPLC-MS.  Twenty
# further patients were the external validation.
#
# Table 4 / Equations, final model (two compartments, HNF on Q the only covariate):
#
#     CL = 32.9 L/h     V1 = 32.5 L     V2 = 83.5 L
#     Q  = 245 L/h x (HNF / 617.96)^-0.58
#
# HNF is the hourly net fluid volume infused (mL/h): fluid and blood infused
# less urine out, over the operation.  It is not a Patient Profile field, so it
# is held at the study mean, 617.96 mL/h, where Q = 245 L/h (the study's median
# was 563.6, range 234-1202 mL/h; Q would then be 255, and 460 to 150 L/h across
# the range).  The paper's simulations found no need to adjust dosing for it.
#
# DOSE BASIS: the paper does not state one; the doses are the injection as
# given, so the dose is the product (nalbuphine hydrochloride) as labelled,
# with no conversion (AGENTS.md, dose basis: the approved product).
#
# BODY SIZE: the source has no size covariate (weight, age, sex and renal
# function were not retained), so by the library's rule every parameter takes
# the fat-free-mass factors, anchored at the study's cohort, so that a patient
# like the cohort receives the published values.  With the switch off the
# published values are used as printed.
#
# PREGNANCY: not applied.  Wilson 1986 (PMID 3740151, abstract only) gave six
# women in labor a two-compartment fit, terminal half-life 2.4 h, no comparison
# with non-pregnant women (inst/extdata/pregnancyRegistry.csv).
#
# CHECKS
# ======
# Terminal half-life of the published model at HNF 617.96: 2.6 h; the label's
# plasma half-life is 5 hours, Wilson 1986's 2.4 h in labor.  The sampled
# interval and assay differ; clearance is the parameter to compare (AGENTS.md).
# Clearance 32.9 L/h is 0.52 L/h/kg at the cohort's 63.3 kg, against 1.78 L/h/kg
# in volunteers (Jaillon 1989, as quoted by the paper): the paper itself notes
# the lower clearance under anesthesia.  Not corrected.
#
# References
# ----------
# Nie et al., Front Pharmacol 2023;14:1130287.
# Nalbuphine hydrochloride injection prescribing information (Hospira, ANDA
#   070914), revised 12/2025.
# -----------------------------------------------------------------------------

NALBUPHINE_CL   <- 32.9    # L/h
NALBUPHINE_V1   <- 32.5    # L
NALBUPHINE_V2   <- 83.5    # L
NALBUPHINE_Q    <- 245     # L/h, at HNF = NALBUPHINE_HNF_REF
NALBUPHINE_HNF_REF <- 617.96   # mL/h, the study mean, at which Q is 245

# the model-building cohort (Table 1): 27 patients, 12 men and 15 women
NALBUPHINE_COHORT <- list(weight = 63.32, age = 53.85, height = 163.96,
                          men = 12, women = 15)

#' Nalbuphine pharmacokinetics (intravenous bolus)
#'
#' Two compartments, adults under general anesthesia (Nie 2023); see the header
#' of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
nalbuphine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, NALBUPHINE_COHORT))

  default <- list(
    v1  = NALBUPHINE_V1 * size$volume,
    v2  = NALBUPHINE_V2 * size$volume,
    v3  = 1,
    cl1 = NALBUPHINE_CL * size$clearance / 60,
    cl2 = NALBUPHINE_Q  * size$clearance / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Nie 2023 (27 adults under general anesthesia, 0.24 mg/kg IV): two compartments, CL 32.9 L/h, ",
    "V1 32.5 L, V2 83.5 L, Q 245 L/h at the study's mean hourly net fluid volume (617.96 mL/h); ",
    "no source covariate, so the fat-free-mass scaling anchored at the cohort applies. ",
    "Concentration only. https://doi.org/10.3389/fphar.2023.1130287")

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
