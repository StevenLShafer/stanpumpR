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
# The AGE term.  Equation 1 and Table 2 print it as (AGE/15)^-0.0154 (RSE
# 64%), as the handoff gave, but the paper's own worked examples do not
# reproduce it (1.484 L/h at 35 y and 73.5 kg in monotherapy, where -0.0154
# gives 0.66).  The age term is omitted: the model is
# CL/F = 0.646 x (WT/70)^0.75.  The omission changes clearance by under 5%
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
# PREGNANCY
# =========
# Total valproate falls in pregnancy mainly because its binding falls.  Nau
# and Krauer (J Clin Pharmacol 1986;26:215-221; 65 mother-fetus pairs, weeks
# 13 to 41) found the maternal free fraction rose from 10% in early pregnancy
# to 20% at term; Yerby and colleagues (Neurology 1992;42(4 Suppl 5):12-16)
# found the FREE concentration rose by 25% by delivery; Tomson (Ther Drug
# Monit 2005;27:718-721) states that unbound levels remain unchanged despite
# a fairly marked fall in the total.  No paired study giving the total CL/F
# in numbers was retrievable (Voinescu 2018, n = 5, gives them only in its
# tables).
#
# With the Patient Profile's Pregnant field on, CL/F (cl1) is multiplied by
# VALPROATE_PREGNANCY_CL_FACTOR = 1.6, on every route (the oral products and
# the injection share the one clearance).  Arithmetic: total = free / fu; the
# free concentration x 1.25 (Yerby) and fu x 2.0 (10% to 20%, Nau and
# Krauer) give a total of 1.25 / 2.0 = 0.625 of baseline at the same dose,
# so total CL/F x 1 / 0.625 = 1.6 (the clearance of unbound drug falls to
# 0.8).  If Yerby's rise in the free concentration is a dosing artefact
# (free unchanged, Tomson 2005), the factor would be the fu ratio, 2.0.  The
# volume and the absorption are unchanged.  Low confidence: the factor is
# derived from two binding studies, not a measured clearance.
#
# WHAT THIS MEANS FOR THE CURVE: the library plots TOTAL valproate, and the
# 50-100 mg/L band is a total-concentration band.  In late pregnancy the
# plotted total falls, often below the band, while the unbound, active
# concentration does not fall (it may rise); a normal unbound level then
# goes with a low total, and a free valproate level, not the total, is the
# guide to the dose.  The free fraction is higher still in labour (oxytocin
# and free fatty acids; Nau and Krauer).  The saturable binding that raises
# fu with dose is not modelled in or out of pregnancy, nor are the
# trimesters before the third.  See R/pregnancy.R.  (Claude Code,
# 2026-10-19.)
#
# References
# ----------
# Teixeira-da-Silva P et al., Pharmaceutics 2022;14:811.
#   https://doi.org/10.3390/pharmaceutics14040811
# Dutta S, Zhang Y, Biopharm Drug Dispos 2004;25:345-352.
#   https://doi.org/10.1002/bdd.420
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Nau H, Krauer B.  J Clin Pharmacol 1986;26:215-221.
#   https://doi.org/10.1002/j.1552-4604.1986.tb02937.x (pregnancy)
# Yerby MS, Friel PN, McCormick K.  Neurology 1992;42(4 Suppl 5):12-16.
#   PMID 1574166 (pregnancy)
# Tomson T.  Ther Drug Monit 2005;27:718-721.
#   https://doi.org/10.1097/01.ftd.0000179843.63515.8f (pregnancy)
# -----------------------------------------------------------------------------

# Total CL/F in late pregnancy relative to the published value: the free
# concentration x 1.25 (Yerby 1992) over the free fraction x 2.0 (Nau and
# Krauer 1986) leaves the total at 0.625 of baseline; 1 / 0.625 = 1.6.
VALPROATE_PREGNANCY_CL_FACTOR <- 1.6

#' Valproate pharmacokinetics (oral delayed release, syrup, extended release, IV)
#'
#' Teixeira-da-Silva et al. (2022), weight only; see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the total
#'   CL/F by \code{VALPROATE_PREGNANCY_CL_FACTOR} (Nau and Krauer 1986, Yerby
#'   1992; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
valproate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  # Pregnancy (see the header): total CL/F only.
  clPregnancy <- if (isTRUE(pregnant)) VALPROATE_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = 14 * (pkW / 70),
    v2  = 1, v3 = 1,
    cl1 = 0.646 * (pkW / 70)^0.75 / 60 * clPregnancy,
    cl2 = 0, cl3 = 0,
    ka_PO = 0.78 / 60,                 # delayed-release (enteric) tablet
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Teixeira-da-Silva P et al., Pharmaceutics 2022;14:811. One compartment, ",
    "apparent oral, weight only (age term and co-medication not applied); ",
    "delayed-release, syrup and extended-release inputs; intravenous on the ",
    "label's 1:1 conversion. https://doi.org/10.3390/pharmaceutics14040811"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("total clearance x ", VALPROATE_PREGNANCY_CL_FACTOR,
             " (the free fraction doubles; the unbound level does not fall)"),
      paste0("Nau H, Krauer B, J Clin Pharmacol 1986;26:215-221. ",
             "https://doi.org/10.1002/j.1552-4604.1986.tb02937.x")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 75, upperTypical = 100, lowerTypical = 50,
    reference = reference,
    oralFormulations = list(
      liquid = list(ka_PO = 2.64 / 60, bioavailability_PO = 1, tlag_PO = 0),
      ER     = list(ka_PO = 0.38 / 60, bioavailability_PO = 0.89, tlag_PO = 0)
    )
  )
}
