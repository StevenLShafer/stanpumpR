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
# engine cannot hold, whose exponents were not retrievable, and which gives
# neither V/F nor ka.
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
# References
# ----------
# Bamgboye et al., J Clin Pharmacol 2026.  https://doi.org/10.1002/jcph.70191
# Clark AM et al., Epilepsia 2013;54:1099-1105.  https://doi.org/10.1111/epi.12134
# Ahmed GF et al., Br J Clin Pharmacol 2015;79:820-830.
#   https://doi.org/10.1111/bcp.12556
# Lim JS et al., J Clin Pharmacol 2016.  https://doi.org/10.1002/jcph.646
# -----------------------------------------------------------------------------

#' Topiramate pharmacokinetics (oral)
#'
#' Bamgboye et al. (2026) absolute three-compartment disposition with oral
#' bioavailability 1; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
topiramate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  a <- (w / 70)^0.75
  b <- w / 70

  default <- list(
    v1  = 9.84 * b, v2 = 39.1 * b, v3 = 9.01 * b,
    cl1 = 1.31 * a / 60, cl2 = 197 * a / 60, cl3 = 0.6 * a / 60,
    ka_PO = 2.0 / 60,            # literature-anchored, see the header
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 10, upperTypical = 20, lowerTypical = 5,
    reference = paste0(
      "Bamgboye et al., J Clin Pharmacol 2026: three compartments from a ",
      "stable-isotope intravenous dose, allometric on weight; oral ",
      "bioavailability 1 (Clark 2013); ka anchored to the 1 h median peak. ",
      "https://doi.org/10.1002/jcph.70191"
    )
  )
}
