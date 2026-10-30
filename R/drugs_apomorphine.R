# -----------------------------------------------------------------------------
# Apomorphine hydrochloride injection (Apokyn), subcutaneous: one
# compartment, apparent, with the UPDRS motor effect site
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of apomorphine.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, who
# supplied Nasser 2024 (inst/help/models/parkinsons.md,
# inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# FDA CDER, Clinical Pharmacology and Biopharmaceutics Review of NDA 21-264
# (Apokyn), Appendix II, Pharmacometrics Review (J. Duan; team leader J.
# Gobburu), read in full from the scanned review (part 1 pp. 49-51, part 2
# pp. 1-15).  Study APOM0073: six patients with idiopathic Parkinson's
# disease, each given a single subcutaneous dose on three days (their
# titrated maintenance dose and that dose +/- 2 mg, 2 to 10 mg), sampled to
# 2 h (the sponsor) or 4 h (the reviewer), UPDRS Part III (items 18-31)
# before the dose and at 20 and 45 min and 1.5, 2, 3 and 4 h.  NONMEM V,
# ADVAN2; two compartments fitted worse.  "Estimates of Pharmacokinetic
# Parameters of Apomorphine" (review p. 52), apparent:
#
#     ka   14.9 /h    (BSV 141%)
#     CL/F 191 L/h    (BSV 30%)
#     V/F  153 L      (BSV 25%)
#
# and "Motor Score Pharmacodynamic Parameter Estimates" (p. 53), an inhibitory
# sigmoid Emax model on an effect compartment,
#
#     UPDRS III = B0 (1 - Emax Ce^N / (Ce^N + EC50^N))
#     ke0 5.36 /h (equilibration half-time 7.8 min), EC50 10.7 ng/mL,
#     B0 24.3, Emax 1 (fixed), N 3 (fixed)
#
# Nasser A et al., J Pharmacokinet Pharmacodyn 2024;51:385-393 (PMID
# 38578533), read in full with its correction and supplement, used exactly
# these numbers (its Tables 1 and 2) to compare SC with sublingual
# apomorphine; its simulated typical SC peak, 11 min and 5.28 ng/mL per mg,
# is the check (tests): the model's is 10.9 min and 5.20 ng/mL.  Nasser's
# means over simulated patients with 30% variability are less exact checks:
# the maximal response 27 min after 4 mg (the model's typical patient: 24
# min), and AUC0-90 224 per mg (printed ng.h/mL, evidently ng.min/mL; the
# model: 255).
#
# DOSE BASIS: the doses are mg of apomorphine hydrochloride (hemihydrate), the
# label strength (10 mg/mL), and are entered as labelled; the review does not
# say whether its concentrations are of the salt or the base (the base is
# 85.5% of the salt), and the model is used as fitted.
#
# WHERE TO BE CAREFUL
# ===================
# Six patients, no covariate.  The noncompartmental clearance in the same
# study was 225 L/h, and the review quotes 230 and 280 L/h elsewhere; in
# healthy volunteers it was 282-370 L/h.  The variability of ka is large.
# Moderate renal impairment raised Cmax 50% and AUC 16-28% (the label starts
# at 1 mg), hepatic impairment Cmax 25%; neither is in the model.  COMT
# metabolites were not detected in humans; CYP metabolism is minor; no
# genotype data.  Not the continuous SC infusion (Onapgo, APO-go), whose
# long-term kinetics this single-dose model does not describe, and not the
# sublingual film (Agbo 2021 needs two transit compartments; registry).
#
# BODY SIZE (docs/weight-adjustment.md): no size covariate was estimated, so
# the volume and clearance are scaled by the library's fat-free-mass
# factors, which are 1 with the switch off (the published model).
#
# THRESHOLD AND BAND: endCe is the motor EC50, 10.7 ng/mL, the "therapeutic
# concentration (about 10 ng/mL)" of the review, so "time until threshold"
# measures when the effect falls below half its maximum; the band is van
# Laar's minimum effective and minimum toxic (dyskinesia) concentrations
# during infusion, 3.9 and 15.2 ng/mL (van Laar 1998, as the review quotes
# it; van Laar's abstract gives the motor response at a mean of 4.7 ng/mL and
# dyskinesia at 8.5, and the paper itself was not read).
#
# References
# ----------
# FDA CDER, Clinical Pharmacology and Biopharmaceutics Review, NDA 21-264.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2004/21-264_Apokyn_BioPharmr_P1.pdf
# Nasser A et al. J Pharmacokinet Pharmacodyn 2024;51:385-393.
#   https://doi.org/10.1007/s10928-024-09914-x
# -----------------------------------------------------------------------------

# FDA review of NDA 21-264, Pharmacometrics Review, pp. 52-53
APOMORPHINE_KA   <- 14.9    # /h
APOMORPHINE_CL   <- 191     # L/h, apparent
APOMORPHINE_V    <- 153     # L, apparent
APOMORPHINE_KE0  <- 5.36    # /h, UPDRS Part III
APOMORPHINE_EC50 <- 10.7    # ng/mL
APOMORPHINE_B0   <- 24.3    # UPDRS Part III at baseline
APOMORPHINE_HILL <- 3       # fixed
# van Laar 1998, as the review quotes it: minimum effective and minimum
# toxic concentrations, ng/mL
APOMORPHINE_MEC  <- 3.9
APOMORPHINE_MTC  <- 15.2

#' Apomorphine, subcutaneous injection
#'
#' One compartment, apparent (/F), with the UPDRS Part III effect site, from
#' the pharmacometrics review of NDA 21-264 (Apokyn); doses in mg of
#' apomorphine hydrochloride.  See the header of \code{R/drugs_apomorphine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
apomorphine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = APOMORPHINE_V * size$volume, v2 = 1, v3 = 1,
    cl1 = APOMORPHINE_CL * size$clearance / 60, cl2 = 0, cl3 = 0,
    ka_SC = APOMORPHINE_KA / 60,
    bioavailability_SC = 1,          # apparent parameters
    tlag_SC = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, ke0 = APOMORPHINE_KE0 / 60, MEAC = 0,
    typical = APOMORPHINE_EC50, upperTypical = APOMORPHINE_MTC,
    lowerTypical = APOMORPHINE_MEC,
    reference = paste0(
      "FDA Pharmacometrics Review of NDA 21-264 (Apokyn), study APOM0073 (6 ",
      "patients): one compartment, ka 14.9/h, CL/F 191 L/h, V/F 153 L; UPDRS ",
      "Part III effect site ke0 5.36/h, EC50 10.7 ng/mL, Hill 3; doses as the ",
      "hydrochloride; as used by Nasser 2024. Band 3.9-15.2 ng/mL (van Laar 1998). ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2004/21-264_Apokyn_BioPharmr_P1.pdf"
    )
  )
}
