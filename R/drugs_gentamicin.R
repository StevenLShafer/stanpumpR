# -----------------------------------------------------------------------------
# Gentamicin: two-compartment model with a de-indexed eGFR covariate
# (Smit 2020)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total serum.
#
# DISPOSITION
# ===========
# Smit et al. fitted total serum gentamicin in 542 patients, mostly
# overweight or obese, with 208 more for validation:
#
#     CL = 3.53 x (G/74) x 0.751^ICU   L/h
#     Vc = 16.6 x (W/70)               L
#     Q  = 1.48                        L/h
#     Vp = 13.4                        L
#
# where G is the DE-INDEXED CKD-EPI eGFR in mL/min (indexed eGFR x BSA/1.73),
# W total body weight, and ICU an indicator for intensive-care admission.
# The ICU factor is not an input here and is left at 1 (not in ICU).  The
# source warned against adding weight allometry to CL, which already carries
# size through G, and against replacing W in Vc by an adjusted weight.
#
# RENAL FUNCTION
# ==============
# stanpumpR has no creatinine input.  G is the CKD-EPI 2009 equation (the
# version in use when the model was fitted; the exact variant was not
# recoverable from the supplement) at an ASSUMED NORMAL creatinine, de-indexed
# by the Du Bois body surface area (R/renalFunction.R).  Age and sex are
# therefore represented; renal impairment is not, and gentamicin is the drug
# for which that omission matters most.  The training range of G was about
# 6-216 mL/min; renal replacement was excluded.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# With the switch on, the weight that enters Vc and the body surface area is
# the pharmacokinetic weight (70 kg x FFM / FFM_ref), and the size-free Q and
# Vp follow the library convention (Vp x FFM ratio, Q x ratio ^ 0.75).  With
# it off, total body weight enters as published and Q and Vp are fixed.
#
# TARGET
# ======
# Peak/MIC of 8-10 is the historical benchmark, measured in Kashuba 1999 as
# the concentration extrapolated to 30 min after a 30-min infusion, not the
# end-infusion maximum of a two-compartment model.  Troughs are kept below
# 1-2 mg/L to limit toxicity.  No toxicity probability is modelled.
#
# TIME UNTIL THRESHOLD: FREE DRUG AT THE MIC
# ==========================================
# The default threshold (endCe in drugDefaults_global.csv) is the TOTAL
# concentration at which FREE gentamicin equals the MIC of 2 mg/L for the
# Enterobacterales: the susceptible breakpoint of CLSI M100 (from 2023,
# lowered from 4; now FDA-recognised) and EUCAST (S <= 2), and the MIC90 of
# 9,809 US Enterobacterales (Sader 2023).  Gentamicin is essentially unbound
# in serum under physiological conditions (Gordon 1972), so the threshold is
# the MIC itself.  The lowest credible measurements put the free fraction
# near 0.8 (van der Mast 2019; Myers 1978, bracketed at physiological calcium
# and magnesium), which would raise the threshold to 2.5 mg/L.  Around surgery it is given for Gram-negative cover.  See
# R/antibioticThresholds.R.
#
# References
# ----------
# Smit C et al., J Antimicrob Chemother 2020;75:3286-3292.
#   https://doi.org/10.1093/jac/dkaa312
# Kashuba ADM et al., Antimicrob Agents Chemother 1999;43:623-629.
#   https://doi.org/10.1128/AAC.43.3.623
# Sader HS et al., Open Forum Infect Dis 2023;10:ofad058.
#   https://doi.org/10.1093/ofid/ofad058
# Gordon RC et al., Antimicrob Agents Chemother 1972;2:214-216.
#   https://doi.org/10.1128/AAC.2.3.214
# -----------------------------------------------------------------------------

#' Gentamicin pharmacokinetics
#'
#' @inheritParams cefazolin
#' @param adjustToFFM evaluate the central volume and the de-indexed CKD-EPI
#'   eGFR (at an assumed normal creatinine) at the fat-free-mass weight, and
#'   scale the peripheral volume and intercompartmental clearance to fat-free
#'   mass; when \code{FALSE}, use total body weight and the published fixed
#'   peripheral parameters.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
gentamicin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): size$volume is the weight ratio the
  # published Vc and the body surface area see; size$clearance scales the
  # size-free Q and is 1 with the switch off.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = weight / 70, legacyClearance = 1)
  pkW  <- 70 * size$volume

  G <- egfrDeindexed(pkW, height, age, sex)        # mL/min, assumed creatinine
  ICU <- 0                                         # not an input; not in ICU

  cl1 <- 3.53 * (G / 74) * 0.751^ICU / 60          # L/min
  v1  <- 16.6 * size$volume
  cl2 <- 1.48 / 60 * size$clearance
  v2  <- 13.4 * (if (isTRUE(adjustToFFM)) size$volume else 1)
  v3  <- 1                                         # no third compartment
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0     # no effect site: exposure against the MIC is the effect
  MEAC  <- 0

  # Band, total mg/L: trough ceiling of about 1 mg/L up to the 8-10 mg/L
  # historical peak benchmark.  Orientation only.
  typical      <- 8
  upperTypical <- 10
  lowerTypical <- 1

  reference <- paste0(
    "Smit C et al., J Antimicrob Chemother 2020;75:3286-3292. ",
    "Two-compartment total-serum model with de-indexed CKD-EPI eGFR, ",
    "estimated here at an assumed normal creatinine; not in ICU. ",
    "https://doi.org/10.1093/jac/dkaa312"
  )

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
