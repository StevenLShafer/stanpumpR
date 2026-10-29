# -----------------------------------------------------------------------------
# Topiramate: three compartments, absolute; oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Bamgboye et al. (J Clin Pharmacol 2026, PMC13084296): 20 adults with
# epilepsy or migraine on oral topiramate given a single 25 mg stable-isotope-
# labelled intravenous dose, 246 samples to 96 h; three compartments, linear,
# allometric weight with exponents fixed at 0.75 (clearances) and 1
# (volumes).  Table 2, at 70 kg:
#
#     CL 1.31 L/h   V1 9.84 L   Q2 197 L/h   V2 39.1 L   Q3 0.6 L/h   V3 9.01 L
#
# Enzyme-inducing co-medication multiplied CL by 1.63 (not applied: the app
# has no co-medication field).  Age, sex and creatinine clearance were not
# significant.  This replaces the model the handoff proposed (Lee 2024,
# PMC10863906), whose clearance carries a power of the daily dose a linear
# engine cannot hold (p. 428: x (CLcr/90)^0.277 x (dose/100)^0.193), and
# whose main text gives neither V/F nor ka.
#
# ORAL ROUTE
# ==========
# Absolute oral bioavailability is complete: 109 +/- 11% against intravenous
# (Clark 2013), about 100% (Ahmed 2015).  bioavailability_PO = 1.  The handoff's
# F of 80% is contradicted by both.  No source estimated an absorption rate;
# the median time to peak after an oral dose is 1 h (Ahmed 2015, 1-2 h in the
# label).  ka 2.0 /h puts the plasma peak at 1.2 h in the reference patient.
# That value is a LITERATURE-ANCHORED calibration, not an estimate, and is
# recorded so in the registry's parameter audit.
#
# CHECKS (70 kg)
# ==============
# Intravenous CL 1.33 L/h after 100 mg in Clark 2013 (model 1.31); terminal
# half-life about 30 h (20-30 h cited by Bamgboye; Clark's single-dose 41-42 h
# is longer); 100 mg orally peaks at about 2.0 mg/L (Lim 2016: about 2 mg/L);
# 150 mg twice daily averages 9.5 mg/L at steady state (Meador, quoted by Lim:
# 9.3 mg/L on 300 mg/day).
#
# BODY SIZE: the model's own allometric weight, evaluated at the
# pharmacokinetic weight with the switch on.  Extended-release capsules
# (Trokendi XR, Qudexy XR; peak about 20 h) have no published input model and
# are not offered.
#
# BAND: 5-20 mcg/mL (ILAE, Patsalos 2008), typical 10.
#
# PREGNANCY
# =========
# Two paired therapeutic-drug-monitoring studies agree on a moderate rise in
# clearance.  Ohman and colleagues (Epilepsy Res 2009;87:124-129; 17
# pregnancies in 15 women) found a third-trimester dose/concentration ratio
# of 67.4 L/day against a baseline outside pregnancy of 38.8 L/day in the same
# women, "an average increase by 71.8%".  Westin and colleagues (Epilepsia
# 2009;50:480-485; 15 pregnancies in 12 women) found the concentration/dose
# ratio 30% lower in the second trimester (n = 11) and 34% lower in the third
# (n = 8).  Voinescu 2018 (n = 10) reports a maximal 1.39-fold clearance in
# the second trimester; MONEAD (Pennell 2022) saw a fall that was not
# significant.
#
# With the Patient Profile's Pregnant field on, the elimination clearance CL
# (cl1) is multiplied by TOPIRAMATE_PREGNANCY_CL_FACTOR = 1.6: Ohman 67.4 /
# 38.8 = 1.74, Westin 1 / (1 - 0.34) = 1.52, geometric mean sqrt(1.74 x
# 1.52) = 1.62, so 1.6.  The volumes, the distribution clearances Q2 and Q3,
# the absorption and the bioavailability are unchanged.  Bamgboye's model is
# absolute, so the factor is on the true clearance.  The model has no renal
# term to carry it, and the 50% rise in filtration alone (about 70% renal:
# 0.7 x 1.5 + 0.3 = 1.35) would undershoot what was measured.  Topiramate is
# under 20% bound, so total and unbound change alike.  Some of the source
# women were on inducers; the ratios are within each woman, so that largely
# cancels.  See R/pregnancy.R.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# Bamgboye et al., J Clin Pharmacol 2026.  https://doi.org/10.1002/jcph.70191
# Clark AM et al., Epilepsia 2013;54:1099-1105.  https://doi.org/10.1111/epi.12134
# Ahmed GF et al., Br J Clin Pharmacol 2015;79:820-830.
#   https://doi.org/10.1111/bcp.12556
# Lim CN et al., J Clin Pharmacol 2016;56:714-722.  https://doi.org/10.1002/jcph.646
# Ohman I, Sabers A, de Flon P, Luef G, Tomson T.  Epilepsy Res
#   2009;87:124-129.  https://doi.org/10.1016/j.eplepsyres.2009.08.004
#   (pregnancy)
# Westin AA, Nakken KO, Johannessen SI, et al.  Epilepsia 2009;50:480-485.
#   https://doi.org/10.1111/j.1528-1167.2008.01776.x (pregnancy)
# Voinescu PE, Park S, Chen LQ, et al.  Neurology 2018;91:e1228-e1236.
#   https://doi.org/10.1212/WNL.0000000000006240 (pregnancy)
# Pennell PB, Karanam A, Meador KJ, et al.  JAMA Neurol 2022;79:370-379.
#   https://doi.org/10.1001/jamaneurol.2021.5487 (pregnancy, MONEAD)
# -----------------------------------------------------------------------------

# Elimination clearance in the third trimester relative to the published
# value: the geometric mean of Ohman 2009 (67.4 / 38.8 = 1.74) and Westin 2009
# (1 / 0.66 = 1.52), 1.62 (see the header).
TOPIRAMATE_PREGNANCY_CL_FACTOR <- 1.6

#' Topiramate pharmacokinetics (oral)
#'
#' Bamgboye et al. (2026) absolute three-compartment disposition with oral
#' bioavailability 1; see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   elimination clearance by \code{TOPIRAMATE_PREGNANCY_CL_FACTOR} (Ohman
#'   2009, Westin 2009; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
topiramate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  a <- (w / 70)^0.75
  b <- w / 70
  # Pregnancy (see the header): elimination clearance only.
  clPregnancy <- if (isTRUE(pregnant)) TOPIRAMATE_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = 9.84 * b, v2 = 39.1 * b, v3 = 9.01 * b,
    cl1 = 1.31 * a / 60 * clPregnancy, cl2 = 197 * a / 60, cl3 = 0.6 * a / 60,
    ka_PO = 2.0 / 60,            # literature-anchored, see the header
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bamgboye et al., J Clin Pharmacol 2026: three compartments from a ",
    "stable-isotope intravenous dose, allometric on weight; oral ",
    "bioavailability 1 (Clark 2013); ka anchored to the 1 h median peak. ",
    "https://doi.org/10.1002/jcph.70191"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", TOPIRAMATE_PREGNANCY_CL_FACTOR),
      paste0("Ohman I et al., Epilepsy Res 2009;87:124-129. ",
             "https://doi.org/10.1016/j.eplepsyres.2009.08.004")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 10, upperTypical = 20, lowerTypical = 5,
    reference = reference
  )
}
