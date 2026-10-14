# -----------------------------------------------------------------------------
# Valproate: one compartment, apparent oral, three oral products and IV
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), TOTAL valproic acid.  Drafted by Claude
# Code, 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Teixeira-da-Silva P et al. (Pharmaceutics 2022;14:811, PMC9031051): 836
# patients with epilepsy, 0.1 to 93 years, 1751 routine levels (Salamanca),
# with 368 more for external evaluation; one compartment, first-order
# absorption, NONMEM.  Read from the full text:
#
#     CL/F = 0.646 L/h x (WT/70)^0.75 x 1.640 (phenytoin) x 1.386
#            (phenobarbital) x 1.521 (carbamazepine), with an age term centred
#            on 15 years
#     V/F  = 14 L x (WT/70), FIXED
#     ka   = 2.64 /h syrup, 0.78 /h gastro-resistant (delayed-release)
#            tablet, 0.38 /h prolonged-release tablet, all FIXED from earlier
#            studies
#
# Daily dose was deliberately NOT a covariate (the authors treat it as a
# confounder of saturable protein binding).
#
# WHAT IS LEFT OUT, AND WHY
# =========================
# The AGE term.  The handoff gave (AGE/15)^-0.0154, but the equation is in a
# table the session could not retrieve, and the paper's own worked examples
# do not reproduce that exponent (1.484 L/h at 35 y and 73.5 kg in
# monotherapy, where -0.0154 gives 0.66).  Until the published equation is
# read, the age term is omitted: the model is CL/F = 0.646 x (WT/70)^0.75.
# If the exponent is -0.0154 the omission changes clearance by under 5%
# between 5 and 80 years.
#
# CO-MEDICATION.  The app has no co-medication field, so the inducer
# multipliers (confirmed by back-calculation from the worked examples) are
# not applied: the curve is valproate WITHOUT phenytoin, phenobarbital or
# carbamazepine.  With any of them, clearance is about 40-65% higher.
#
# PROTEIN BINDING.  Valproate's binding to albumin saturates in the
# therapeutic range, so the unbound fraction rises with the total level and
# total clearance rises with dose.  The curve is TOTAL valproate on a linear
# model; the unbound concentration is not plotted.
#
# FORMULATIONS (one disposition, a product-specific input)
# ========================================================
#   "mg PO DR"      divalproex delayed-release (enteric) tablet, ka 0.78 /h,
#                   the default oral product
#   "mg PO liquid"  valproic acid syrup or capsule, ka 2.64 /h
#   "mg PO ER"      divalproex extended-release tablet, ka 0.38 /h, and a
#                   bioavailability of 0.89 relative to delayed release
#                   (Dutta and Zhang 2004 meta-analysis: AUC ratio 0.89,
#                   0.85-0.94; the Depakote ER label's basis for giving 8-20%
#                   more drug on conversion)
#   "mg", "mg/hr"   valproate sodium injection, intravenous
# Every dose is in mg of valproic acid equivalents, as the US labels state
# strengths.  The intravenous route rests on the label: intravenous and oral
# doses are interchangeable mg for mg, absolute oral bioavailability being
# about 1 (the paper's introduction: above 90%), so the apparent parameters
# are read as absolute.  That is an assumption anchored to the label, not an
# estimate, and is recorded so in the registry.
#
# CHECKS (70 kg adult)
# ====================
# Half-life 15.0 h (the paper's introduction: 12-16 h in adults); 1000 mg/day
# gives an average steady state of 64 mg/L, inside the 50-100 mg/L reference
# range; 500 mg of delayed release peaks at 3.6 h.
#
# BODY SIZE: the model carries its own weight covariate, evaluated at the
# pharmacokinetic weight with the switch on and total weight with it off.
#
# BAND: 50-100 mcg/mL total (ILAE, Patsalos 2008), typical 75.
#
# References
# ----------
# Teixeira-da-Silva P et al., Pharmaceutics 2022;14:811.
#   https://doi.org/10.3390/pharmaceutics14040811
# Dutta S, Zhang Y, Biopharm Drug Dispos 2004;25:345-352.
#   https://doi.org/10.1002/bdd.420
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# -----------------------------------------------------------------------------

#' Valproate pharmacokinetics (oral delayed release, syrup, extended release, IV)
#'
#' Teixeira-da-Silva et al. (2022), weight only; see the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
valproate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  default <- list(
    v1  = 14 * (pkW / 70),
    v2  = 1, v3 = 1,
    cl1 = 0.646 * (pkW / 70)^0.75 / 60,
    cl2 = 0, cl3 = 0,
    ka_PO = 0.78 / 60,                 # delayed-release (enteric) tablet
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 75, upperTypical = 100, lowerTypical = 50,
    reference = paste0(
      "Teixeira-da-Silva P et al., Pharmaceutics 2022;14:811. One compartment, ",
      "apparent oral, weight only (age term and co-medication not applied); ",
      "delayed-release, syrup and extended-release inputs; intravenous on the ",
      "label's 1:1 conversion. https://doi.org/10.3390/pharmaceutics14040811"
    ),
    oralFormulations = list(
      liquid = list(ka_PO = 2.64 / 60, bioavailability_PO = 1, tlag_PO = 0),
      ER     = list(ka_PO = 0.38 / 60, bioavailability_PO = 0.89, tlag_PO = 0)
    )
  )
}
