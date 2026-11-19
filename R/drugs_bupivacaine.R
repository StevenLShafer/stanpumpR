# -----------------------------------------------------------------------------
# Bupivacaine: intravenous disposition, regional anesthesia (RA) absorption
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total (bound + unbound) parent drug in
# plasma.
#
# DISPOSITION
# ===========
# Two compartments from the simultaneous intravenous stable-isotope tracer in
# adult surgical patients of Burm et al. (Clin Pharmacokinet 1987;13:191-203):
#
#     Vc = 33 L, Vss = 68 L, CL = 0.52 L/min, terminal half-life 143 min
#
# Vp = Vss - Vc = 35 L.  The intercompartmental clearance was not published;
# it is reconstructed from the rounded published means (Vc, Vss, CL and the
# terminal half-life) with k10 = CL/Vc, r = Vp/Vc, beta = ln 2 / t1/2,
#
#     k21 = beta (k10 - beta) / (k10 - beta (1 + r)),   Q = k21 Vp
#
# which gives Q = 0.3208 L/min.  That keeps the IV clearance, Vss and terminal
# slope exactly; the distribution half-life is 23.3 min rather than the
# reported 22 because the published means were rounded.
#
# REGIONAL ANESTHESIA (RA): FIRST-ORDER ABSORPTION FROM THE INJECTED TISSUE
# =========================================================================
# An "mg RA" dose is placed in a tissue depot and absorbed into the central
# compartment by first-order kinetics, ka_RA, with bioavailability 1 and no
# lag.  No published study of bupivacaine nerve block or infiltration
# identifies the absorption rate against an intravenous reference, so ka_RA is
# PROVISIONAL: it is the single first-order rate that, with the disposition
# above and F = 1, reproduces the mean peak of 1.66 mcg/mL after 168.75 mg of
# plain bupivacaine for combined femoral and sciatic block (Robison et al.,
# Br J Anaesth 1991;66:228-231) in THAT study's typical patient: the plain
# group, 62 kg, median 65 years, 5 men and 6 women, Edinburgh, whose height
# is not printed and is taken as the United Kingdom's mean for the birth year
# (BUPIVACAINE_KA_COHORT, below).  Its disposition is this model's anchored
# at Burm's cohort, the volumes x 0.749 and the clearances x 0.805 of the
# published values.  ka_RA = 0.011958/min, an absorption half-time of 58 min
# (Steven L. Shafer, 2026-11-18, re-anchored and recalibrated together; it
# was 0.0187/min, fitted on the published disposition, before the
# anchoring, and remains so with the switch off, which gives the published
# disposition; Carbon's second repository audit, 2026-11-19, A2-F2).  It is
# calibrated to that peak by construction; the predicted
# time of peak, 61 min, is a model output.  The same study's epinephrine
# group peaked at 0.98 mcg/mL, and plain infiltration of the lower abdominal
# wall at 2.23 mcg/mL after 200 mg (Martin and Neill 1987); on the published
# disposition these gave ka 0.0076 and 0.024/min by the same method:
# absorption depends on the site and the formulation, and one rate cannot
# describe them
# all.  Epinephrine is not a covariate.
#
# F = 1 is an assumption: bupivacaine is absorbed essentially completely after
# epidural injection (Burm 1987), but absolute bioavailability after a
# peripheral nerve block or an infiltration has not been measured.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The published parameters are fixed values for Burm's six bupivacaine
# subjects, mean 79 kg, 178 cm, 31 years, 5 men and 1 woman (Table I), so
# they are that cohort's typical patient's, and with the switch on they are
# anchored at that patient (BUPIVACAINE_COHORT; Steven L. Shafer, 2026-11-18;
# docs/weight-adjustment.md, Anchoring at the source's cohort): volumes x
# FFM / FFM_ref divided by 1.079, clearances x (FFM / FFM_ref)^0.75 divided
# by 1.059.  With the switch off, the published values for everyone
# (legacyVolume = 1).  The tissue absorption rate is not scaled.
#
# NO EFFECT SITE, NO BAND
# =======================
# Plotted as plasma only, and the tPeak is 0: the model describes systemic
# total plasma concentration, not the local nerve effect, and no
# plasma-effect-site equilibration has been estimated.  No band is drawn:
# total plasma concentration alone does not predict local anesthetic systemic
# toxicity, which depends on the unbound fraction (alpha-1-acid glycoprotein
# rises after surgery), the rate of rise and the patient.
#
# NOT MODELLED
# ============
# Site-specific and epinephrine-specific absorption, parallel fast and slow
# tissue depots, liposomal bupivacaine (a different input altogether),
# protein binding and the unbound fraction (and so the postoperative rise in
# total concentration during a long infusion), and the R/S enantiomers
# (levobupivacaine is not this model).  A perineural catheter infusion is
# offered as "mg/hr RA", a constant rate into the same depot at the same ka
# (2026-10-10).
#
# References
# ----------
# Burm AG et al., Clin Pharmacokinet 1987;13:191-203 (IV and epidural tracer).
#   https://doi.org/10.2165/00003088-198713030-00004
# Robison C et al., Br J Anaesth 1991;66:228-231 (femoral and sciatic block).
#   https://doi.org/10.1093/bja/66.2.228
# Martin J, Neill RS, Br J Anaesth 1987;59:1425-1430 (abdominal infiltration).
#   https://doi.org/10.1093/bja/59.11.1425
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer, from a
# research handoff on local anesthetic tissue-injection pharmacokinetics.)
# -----------------------------------------------------------------------------

# Burm 1987 (PMID 3665324), Table I (p. 196), read on the PDF 2026-11-18: the
# bupivacaine subjects' means, 79 kg, 178 cm, 31 years, 5 men and 1 woman.
# Factors 1.079 on volume, 1.059 on clearance.
BUPIVACAINE_COHORT <- list(weight = 79, height = 178, age = 31, men = 5, women = 1)

# Robison 1991 (PMID 1817626), Table I: the plain group whose mean peak ka_RA
# reproduces, 62 kg, median 65 years, 5 men and 6 women, Edinburgh (accepted
# 1990).  No height is printed: each sex's is the United Kingdom's mean adult
# height for the birth year 1991 - 65 = 1926 (NCD Risk Factor Collaboration,
# eLife 2016;5:e13410, height at 18 by birth year, 171.6 cm for men and 158.8
# for women), less the 0.2 cm (men) and 0.3 cm (women) a cohort loses per
# decade of age after 18 that the same paper reports.
BUPIVACAINE_KA_COHORT <- list(groups = list(
  list(weight = 62, height = 170.7, age = 65, men = 5, women = 0),
  list(weight = 62, height = 157.4, age = 65, men = 0, women = 6)))

#' Bupivacaine pharmacokinetics (intravenous and regional anesthesia)
#'
#' Burm et al. (1987) intravenous tracer disposition, with a provisional
#' first-order absorption rate for tissue injection (RA).  See the header.
#'
#' @param weight,height,age,sex patient covariates (kg, cm, years, sex)
#' @param adjustToFFM \code{TRUE} (the default) scales the published 70 kg
#'   parameters to the patient's fat-free mass; \code{FALSE} uses them as
#'   published for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
bupivacaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1,
                        cohort = do.call(cohortSizeFactors, BUPIVACAINE_COHORT))

  # Burm 1987; Q reconstructed from the published means (see the header).
  vc  <- 33
  vss <- 68
  cl  <- 0.52
  beta <- log(2) / 143
  k10 <- cl / vc
  r   <- (vss - vc) / vc
  k21 <- beta * (k10 - beta) / (k10 - beta * (1 + r))
  q   <- k21 * (vss - vc)

  default <- list(
    v1 = vc * size$volume,
    v2 = (vss - vc) * size$volume,
    v3 = 1,
    cl1 = cl * size$clearance,
    cl2 = q * size$clearance,
    cl3 = 0,
    # The rate was fitted with the disposition each switch position gives the
    # calibration patient: anchored with the switch on, the published values
    # with it off (Carbon's second repository audit, 2026-11-19, A2-F2: the
    # anchored rate with the switch off put the calibration peak 22% low).
    ka_RA = if (isTRUE(adjustToFFM)) 0.011958 else 0.0187,  # 1/min, provisional, peak-matched (header)
    bioavailability_RA = 1,
    tlag_RA = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Burm AG et al., Clin Pharmacokinet 1987;13:191-203 (intravenous tracer ",
    "disposition). Regional anesthesia (RA) absorption is first-order with a ",
    "provisional rate matched to the mean peak after femoral and sciatic ",
    "block (Robison 1991) in that study's typical patient, F = 1 assumed. https://doi.org/10.2165/00003088-198713030-00004"
  )

  return(
    list(
      PK = PK,
      tPeak = 0,
      MEAC = 0,
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = reference
    )
  )
}
