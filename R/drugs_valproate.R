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
#            (phenobarbital) x 1.521 (carbamazepine) x (AGE/15)^-0.0154
#            (Equation 1, which the worked examples confirm for carbamazepine;
#            Table 2 gives the carbamazepine coefficient as 0.512, i.e. 1.512,
#            a disagreement within the paper.  Not applied here: the app has
#            no co-medication field.)
#     V/F  = 14 L x (WT/70), FIXED
#     ka   = 2.64 /h syrup, 0.78 /h gastro-resistant (delayed-release)
#            tablet, 0.38 /h prolonged-release tablet, all FIXED from earlier
#            studies
#
# Daily dose was deliberately NOT a covariate (the authors treat it as a
# confounder of saturable protein binding).
#
# THE AGE TERM (applied from 2026-10-29, Steven L. Shafer)
# =============
# Equation 1 and Table 2 print it as (AGE/15)^-0.0154 (RSE 64%), and it is
# applied as printed: CL/F = 0.646 x (WT/70)^0.75 x (AGE/15)^-0.0154.  It
# moves clearance by under 5% between 5 and 80 years.  The paper's own worked
# examples do not reproduce it (1.484 L/h at 35 y and 73.5 kg in monotherapy,
# where the equation gives 0.66), which the paper does not explain; the
# printed equation is the authority.  Age is floored at 0.1 years, the
# youngest patient studied, because the power goes to infinity at birth.
#
# WHAT IS LEFT OUT, AND WHY
# =========================
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
# a fairly marked fall in the total (citing only Yerby 1992, so it is the
# same finding, not a second one).
#
# MEASURED TOTAL CLEARANCE (read 2026-11-01).  Two paired studies give the
# total ratio in numbers, and they disagree:
#   - Voinescu 2018 (Neurology 91:e1228, full text, the table under Figure
#     3): 5 women, 5 pregnancies, 27 clinical-laboratory levels; apparent
#     oral clearance = dose in mg/kg / level, so PER KILOGRAM; mixed model
#     adjusted for age and hours after the dose.  Third trimester over
#     baseline: total 1.142 (95% CI 0.926-1.408, p 0.214), free 0.820
#     (0.46-1.47, p 0.504); none significant.  Expressed per kilogram, so an
#     absolute ratio is larger by the weight gained (about 1.3 at a 15%
#     gain, inference: the paper prints no weights).
#   - Johannessen Landmark 2018 (Epilepsy Res 141:31-37, ABSTRACT only):
#     Norwegian TDM database, 51 pregnancies in 33 women; mean total
#     concentration/dose 0.48 before pregnancy and 0.29 umol/L/mg in the
#     third trimester, a total CL/F ratio of 1.66 (absolute, not per kg).
#     The abstract calls it a 46% fall; 0.29/0.48 is a 40% fall, which the
#     abstract does not explain.
# Koerner 1989 (Ther Drug Monit 11:228-230, PMID 2499082, abstract, 9 women
# followed monthly) agrees in direction: total fell despite dose increases
# in four, free did not, free fraction and clearance rose, and clearance
# adjusted for body weight did not change.
#
# The two parts of the derivation below are checked separately by
# Voinescu: its free-clearance ratio 0.820 agrees with Yerby's 25% rise in
# the free level (unbound CL/F 0.8), but its implied rise in free fraction,
# 1.142 / 0.820 = 1.39, is well below Nau and Krauer's 2.0.  Their term
# pairs were sampled AT DELIVERY, when free fatty acids and oxytocin
# displace valproate (their own abstract; Bardy 1990, Ther Drug Monit
# 12:40-46, PMID 2305420), so 2.0 overstates the antepartum free fraction.
# The factor is retained because the largest data set, Johannessen
# Landmark's 1.66, lands on it, not because the binding derivation is
# exact; Voinescu's 1.142 is not adopted (n 5, not significant, per kg, and
# its absolute equivalent's interval reaches 1.6).
#
# With the Patient Profile's Pregnant field on, CL/F (cl1) is multiplied by
# VALPROATE_PREGNANCY_CL_FACTOR = 1.6, on every route (the oral products and
# the injection share the one clearance).  Arithmetic: total = free / fu; the
# free concentration x 1.25 (Yerby) and fu x 2.0 (10% to 20%, Nau and
# Krauer) give a total of 1.25 / 2.0 = 0.625 of baseline at the same dose,
# so total CL/F x 1 / 0.625 = 1.6 (the clearance of unbound drug falls to
# 0.8).  If Yerby's rise in the free concentration is a dosing artefact
# (free unchanged, Tomson 2005), the factor would be the fu ratio, 2.0.  The
# volume and the absorption are unchanged.  Low confidence: the measured
# total ratios run from 1.14 per kg (n 5) to 1.66 (51 pregnancies, abstract
# only), and the factor sits at the top of that range.  Johannessen
# Landmark's ratio is not per kilogram while the model's weight term also
# rises with the weight entered, so at a 15% weight gain the curve's total
# clearance rises about 7% more than 1.6 (fat-free-mass scaling on).
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
# Voinescu PE et al.  Neurology 2018;91:e1228-e1236.
#   https://doi.org/10.1212/WNL.0000000000006240 (pregnancy)
# Johannessen Landmark C et al.  Epilepsy Res 2018;141:31-37.
#   https://doi.org/10.1016/j.eplepsyres.2018.02.004 (pregnancy)
# Koerner M et al.  Ther Drug Monit 1989;11:228-230.
#   https://doi.org/10.1097/00007691-198905000-00002 (pregnancy)
# Bardy AH et al.  Ther Drug Monit 1990;12:40-46.
#   https://doi.org/10.1097/00007691-199001000-00008 (labour)
# -----------------------------------------------------------------------------

# Total CL/F in late pregnancy relative to the published value: the free
# concentration x 1.25 (Yerby 1992) over the free fraction x 2.0 (Nau and
# Krauer 1986) leaves the total at 0.625 of baseline; 1 / 0.625 = 1.6.  The
# measured third-trimester total ratios are 1.142 per kg (Voinescu 2018,
# n 5, not significant) and 1.66 (Johannessen Landmark 2018, 51 pregnancies,
# abstract); see the header.
VALPROATE_PREGNANCY_CL_FACTOR <- 1.6

# The age term on CL/F, (AGE/15)^-0.0154 (Equation 1), and the youngest age it
# sees: 0.1 years, the youngest patient studied (see the header).
VALPROATE_AGE_EXPONENT <- -0.0154
VALPROATE_MIN_AGE      <- 0.1   # years

#' Valproate pharmacokinetics (oral delayed release, syrup, extended release, IV)
#'
#' Teixeira-da-Silva et al. (2022): clearance on weight and age (Equation 1's
#' \eqn{(AGE/15)^{-0.0154}}, age floored at 0.1 years), volume on weight; see
#' the header of the file.
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
    cl1 = 0.646 * (pkW / 70)^0.75 * (max(age, VALPROATE_MIN_AGE) / 15)^VALPROATE_AGE_EXPONENT /
    60 * clPregnancy,
    cl2 = 0, cl3 = 0,
    ka_PO = 0.78 / 60,                 # delayed-release (enteric) tablet
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Teixeira-da-Silva P et al., Pharmaceutics 2022;14:811. One compartment, ",
    "apparent oral, weight and age (co-medication not applied); ",
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
