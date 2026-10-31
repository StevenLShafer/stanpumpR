# -----------------------------------------------------------------------------
# Opicapone (Ongentys): one compartment, apparent oral, absorbed through one
# transit compartment; the marketed micronised capsules taken at bedtime
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer
# (inst/help/models/parkinsons.md, inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA Clinical Pharmacology Review of NDA 212489 (Ongentys, 2020), section
# 4.2.1, the sponsor's population model (report 2018-CP-198), read in full
# with its equations from the page images (pp. 55-56): 21 studies (18 in
# healthy volunteers, 3 in Parkinson's disease), 724 subjects, 9686 fasting
# concentrations; "a one-compartment model with 1 transit absorption
# compartment and first-order elimination".  Final model, Table 9:
#
#     CL/F = 37.6 L/h x (BILI/12)^-0.116 x (1 - 0.138 female) x (1 - 0.307 Asian)
#     V2/F = 49.7 L x (1 - 0.389 Asian)
#     KA   = 1.24 /h x (1 + 0.245 Micro) x (1 - 0.595 Evening) x (1 - 0.408 LDSim)
#     F1   = (Dose/50)^-0.288 x (1 + 0.681 Micro FLD50) x (1 + 0.609 Micro FLD25)
#            x (1 + 0.106 Micro FLD05) x (1 + 0.353 LDSim)
#
# Micro is the micronised (marketed) formulation, FLDxx the 50, 25 and 5 mg
# doses, Evening an evening dose, LDSim levodopa taken at the same time, and
# BILI total bilirubin in umol/L.  Weight "was not found to have a significant
# impact".  F1 is relative, so above 1.
#
# THE PRODUCT AS LABELLED
# =======================
# Ongentys is the micronised capsule, 50 mg (25 mg in moderate hepatic
# impairment), taken once daily at BEDTIME and at least an hour from
# levodopa.  So Micro = 1, Evening = 1 and LDSim = 0:
#
#     KA = 1.24 x 1.245 x 0.405 = 0.625 /h
#     F1 = 1.681 at 50 mg, 0.5^-0.288 x 1.609 = 1.965 at 25 mg
#
# The source's F1 is a lookup at the doses studied, not a curve.  It is
# carried as the power through its two marketed strengths: bioavailability_PO
# is F1 at 50 mg and an oralSaturation block of the power form scales each
# dose by (dose/50)^gamma, with gamma = ln(1.965/1.681) / ln(0.5) = -0.225, so
# both marketed strengths have exactly the published F1; other doses are an
# interpolation the source does not make.  Bilirubin has no Patient Profile
# field and is held at the median, 12 umol/L; race at White.
#
# WHAT IS INFERRED: THE TRANSIT COMPARTMENT
# =========================================
# "1 transit absorption compartment" with "KA ... the transit absorption rate
# constant" is read as levodopa's (R/drugs_levodopa.R): the dose passes from
# its depot through one transit compartment into the central compartment,
# both at KA (transitChain, n = 2, R/advanceTransitChain.R).  The review does
# not draw the structure.  With it the peak after a bedtime 50 mg capsule
# falls at 2.9 h, 400 ng/mL (label tmax 2 h, 1-4, in the morning; LeWitt
# 2023's patients dosed at night, Cmax 459 +/- 252 ng/mL).
#
# BODY SIZE (docs/weight-adjustment.md): no size covariate was retained, so
# the 71 kg (median) parameters are scaled by the library's fat-free-mass
# factors, which are 1 with the switch off.
#
# CHECK (man, switch off): CL/F 22.4 L/h, V/F 29.6 L, half-life 0.9 h.  In 16
# patients on 50 mg at night for 14 days (LeWitt 2023, Clin Neuropharmacol
# 46:51, read in full) the AUC was 2022 ng.h/mL, a CL/F of 24.7 L/h; the
# label gives a half-life of 1-2 h.
#
# THE PLASMA IS NOT THE EFFECT
# ============================
# Opicapone binds COMT so tightly that erythrocyte COMT inhibition outlasts
# the drug by days: its half-life is 62 h after one dose (Almeida 2013) and
# over 100 h after repeated doses (Rocha 2013), against a plasma half-life of
# about an hour, and in patients more than 65% inhibition persists over the
# whole day (LeWitt 2023).  The curve drawn is plasma opicapone only; it falls
# to nothing within hours of each bedtime dose while the inhibition, and the
# rise in levodopa's AUC (+60-80%, LeWitt 2023), last.  No effect site, no
# band: no concentration-effect model with published parameters exists.
#
# References
# ----------
# FDA CDER, Clinical Pharmacology Review, NDA 212489 (Ongentys).
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2020/212489Orig1s000ClinPharmR.pdf
# LeWitt PA et al. Clin Neuropharmacol 2023;46:51-58.
#   https://doi.org/10.1097/WNF.0000000000000538
# -----------------------------------------------------------------------------

# FDA review of NDA 212489, Table 9 (sponsor's final model)
OPICAPONE_CL        <- 37.6    # L/h
OPICAPONE_V         <- 49.7    # L
OPICAPONE_KA        <- 1.24    # /h
OPICAPONE_KA_MICRO  <- 0.245
OPICAPONE_KA_EVENING <- -0.595
OPICAPONE_F_GAMMA   <- -0.288
OPICAPONE_F_MICRO50 <- 0.681
OPICAPONE_F_MICRO25 <- 0.609
OPICAPONE_CL_FEMALE <- -0.138

#' Opicapone's relative bioavailability at a marketed strength
#'
#' F1 of Table 9 for the micronised capsule, without levodopa.
#' @param dose 25 or 50 (mg)
#' @keywords internal
opicaponeF1 <- function(dose)
  (dose / 50)^OPICAPONE_F_GAMMA *
    (1 + OPICAPONE_F_MICRO50 * (dose == 50)) * (1 + OPICAPONE_F_MICRO25 * (dose == 25))

#' Opicapone pharmacokinetics (oral, bedtime)
#'
#' One compartment, apparent (/F), absorbed through one transit compartment,
#' from the sponsor's model in the FDA review of NDA 212489, for the micronised
#' capsules taken at bedtime.  See the header of \code{R/drugs_opicapone.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
opicapone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  cl <- OPICAPONE_CL * (if (sex == SEX_FEMALE) 1 + OPICAPONE_CL_FEMALE else 1)
  ka <- OPICAPONE_KA * (1 + OPICAPONE_KA_MICRO) * (1 + OPICAPONE_KA_EVENING)

  default <- list(
    v1 = OPICAPONE_V * size$volume, v2 = 1, v3 = 1,
    cl1 = cl * size$clearance / 60, cl2 = 0, cl3 = 0,
    ka_PO = ka / 60,
    bioavailability_PO = opicaponeF1(50),   # relative; scaled per dose below
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # the dosing compartment and one transit compartment, both emptied at KA
    transitChain = list(n = 2L, ktr = ka / 60, kMetabolic = 0),
    # F1 through the two marketed strengths: exactly Table 9's at 25 and 50 mg
    oralSaturation = list(form = ORAL_SATURATION_POWER, Dref = 50,
                          exponent = log(opicaponeF1(25) / opicaponeF1(50)) / log(0.5)),
    reference = paste0(
      "FDA Clinical Pharmacology Review of NDA 212489 (Ongentys), Table 9: one ",
      "compartment, CL/F 37.6 L/h (x 0.862 in women; bilirubin at 12 umol/L), ",
      "V2/F 49.7 L, absorbed through one transit compartment at 1.24/h x 1.245 ",
      "(micronised) x 0.405 (bedtime); relative F 1.681 at 50 mg and 1.965 at ",
      "25 mg; plasma only (COMT inhibition outlasts it by days). ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2020/212489Orig1s000ClinPharmR.pdf"
    )
  )
}
