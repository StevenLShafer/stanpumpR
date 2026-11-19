# -----------------------------------------------------------------------------
# Glyburide (glibenclamide): two compartments, oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  No glucose or insulin effect is plotted, and the
# active metabolites M1 and M2 are not modelled; the effect site (below) is
# glyburide's own.  Drafted
# by Claude Code, 2026-10-17, at the request of Steven L. Shafer; see the
# antidiabetic registry, inst/extdata/antidiabeticRegistry.csv.
#
# SOURCE
# ======
# Rambiritch V, Naidoo P, Maharaj B, Pillai G.  Population pharmacokinetic
# modeling of glibenclamide in poorly controlled South African type 2
# diabetic subjects.  Clin Pharmacol Adv Appl 2016;8:83-92 (PMC4948726).
# 24 patients, 2.5, 5, 10 and 20 mg/day at steady state.  A two-compartment
# disposition model with first-order absorption, no lag, apparent
# parameters; "no formal covariate analysis was conducted":
#
#     ka   = 0.53 /h     CL/F = 2.16 L/h
#     Vc/F = 11.70 L     Q/F  = 3.84 L/h     Vp/F = 68.1 L
#
# The authors note their peak concentrations were about twice those of other
# studies.  The PD companion paper's EC50 (printed 85.20 mg/L, likely
# mcg/L) is an endpoint of its own and is not used.
#
# EFFECT SITE (Steven L. Shafer's decision, 2026-11-19)
# ===========
# Rydberg T, Jonsson A, Karlsson MO, Melander A.  Concentration-effect
# relations of glibenclamide and its active metabolites in man: modelling of
# pharmacokinetics and pharmacodynamics.  Br J Clin Pharmacol 1997;43:
# 373-381 (PMID 9146849, read in full).  Eight healthy volunteers, 3.5 mg
# intravenously and orally in a crossover, with intravenous doses of the
# metabolites M1 and M2; effect, the percent fall in blood glucose against
# placebo.  Glibenclamide's own effect compartment: ke0 1.59 /h (CV 36%),
# Emax 56%, CEss50 108 ng/mL (Table 4), on its absolute disposition (Table
# 3: V1 3.63 L, k10 1.30 /h, k12 0.447 /h, k21 0.916 /h).  On that
# disposition an intravenous bolus puts the effect-site peak at 38 minutes.
# It is the only measured glyburide effect site: Rambiritch's own PK/PD
# companion (Clin Pharmacol 2016;8:141-153, PMID 27713650) found no direct
# concentration-effect relation and drove its glucose model by dose.
#
# ke0 is supplied directly, GLYBURIDE_KE0 = 1.59 /h, not as the 38-minute
# tPeak (Claude Code, 2026-11-19, departing from the tPeak Shafer approved,
# for the reason the insulins' effect site gives, R/insulinEffectSite.R):
# Rambiritch's model is an apparent fit to oral data whose plasma falls far
# more slowly than Rydberg's measured intravenous disposition (k10 0.18
# against 1.30 /h), so a 38-minute tPeak solved on it gives ke0 = 3.7 /h,
# 2.3 times the measured value, and an effect that follows plasma almost
# without delay.  getDrugPK() solves this model's own time to peak effect
# from the ke0.
#
# What the effect site leaves out: M1 and M2 lower glucose too, more potently
# at low concentrations (CEss50 23 and 37 ng/mL) and more slowly (ke0 0.178
# and 0.479 /h), so the glucose-lowering effect outlasts the parent's effect
# site; the library has no model of either metabolite.  MEAC and the band
# stay 0 (concentration only).
#
# BODY SIZE: no covariates, so the parameters are the cohort's typical
# patient's (71.1 kg, 156 cm, 20 of 22 women); with the switch on they are
# anchored at that patient (GLYBURIDE_COHORT; Steven L. Shafer, 2026-11-18),
# the reference man's volumes divided by 0.782 and clearances by 0.832, and
# with it off fixed (legacyVolume = 1).
#
# PREGNANCY
# =========
# Hebert 2009 (Obstetric-Fetal Pharmacology Research Network, steady state)
# compared 40 women with gestational diabetes at 28-38 weeks with 26
# nonpregnant women with type 2 diabetes.  Compartmental CL/F was 18.3 +/-
# 10.3 against 9.6 +/- 6.9 L/h (18.3 / 9.6 = 1.91; non-compartmental 17.6
# against 9.0 L/h, 1.96, in CYP2C9*1/*1 women, and 13.7 against 6.9, 1.99,
# in carriers of a variant), and V/F 83 +/- 65 against 53 +/- 36 L (83 / 53
# = 1.57, p = 0.04).  The unbound fraction (about 1%) and the urinary
# recovery of the metabolite M1 were unchanged, so the authors attribute the
# rise to induced hepatic and intestinal metabolism (CYP2C9, CYP3A).
# Pregnant mice agree (Shuster 2014: clearance doubled).
#
# With the Patient Profile's Pregnant field on, CL/F is multiplied by
# GLYBURIDE_PREGNANCY_CL_FACTOR = 1.9 and both apparent volumes, Vc/F and
# Vp/F, by GLYBURIDE_PREGNANCY_V_FACTOR = 1.57: Hebert fitted ONE
# compartment, so his ratio is of the total apparent volume and is applied
# to both of Rambiritch's.  Q/F and ka are unchanged (Hebert's absorption
# times are in tables not retrieved).  The ratios come from a different,
# American, one-compartment analysis and are applied as ratios to
# Rambiritch's South African fit of men and women; the volume ratio is the
# weaker finding (p = 0.04, wide SDs).  Binding is unchanged, so total and
# unbound move together.  Claude Code, 2026-10-19.  See R/pregnancy.R.
#
# References
# ----------
# Rambiritch V et al., Clin Pharmacol Adv Appl 2016;8:83-92.
#   https://doi.org/10.2147/CPAA.S102676
# Hebert MF, Ma X, Naraharisetti SB, et al.  Are we optimizing gestational
#   diabetes treatment with glyburide?  The pharmacologic basis for better
#   clinical practice.  Clin Pharmacol Ther 2009;85:607-614.
#   https://doi.org/10.1038/clpt.2009.5 (pregnancy)
# Shuster DL, Risler LJ, Liang CK, et al.  Maternal-fetal disposition of
#   glyburide in pregnant mice is dependent on gestational age.  J Pharmacol
#   Exp Ther 2014;350:425-434.  https://doi.org/10.1124/jpet.114.213470
#   (pregnancy, mice)
# -----------------------------------------------------------------------------

# Pregnancy (see the header).  Hebert 2009: CL/F 18.3 / 9.6 L/h = 1.91,
# rounded to 1.9; V/F 83 / 53 L = 1.57, on both apparent volumes.
GLYBURIDE_PREGNANCY_CL_FACTOR <- 1.9
GLYBURIDE_PREGNANCY_V_FACTOR  <- 1.57

# Rambiritch 2016, Table 1 (printed p. 85), read on the PDF 2026-11-18: means
# 71.1 kg, 156 cm, 54 years, 2 men and 20 women (Table 1 accounts for 22 of
# the 24 enrolled).  The model is anchored at this cohort (Steven L. Shafer, 2026-11-18;
# docs/weight-adjustment.md, Anchoring at the source's cohort). Factors 0.782 on
# volume, 0.832 on clearance.
GLYBURIDE_COHORT <- list(weight = 71.1, height = 156, age = 54, men = 2, women = 20)

# Rydberg 1997, Table 4: glibenclamide's own ke0 (see the header).
GLYBURIDE_KE0 <- 1.59   # /h

#' Glyburide (glibenclamide) pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption (Rambiritch 2016); see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the changes of late pregnancy:
#'   CL/F x 1.9 and both apparent volumes x 1.57 (see the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
glyburide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1, cohort = do.call(cohortSizeFactors, GLYBURIDE_COHORT))
  # Pregnancy (see the header): CL/F and both volumes; Q/F unchanged.
  clPregnancy <- if (isTRUE(pregnant)) GLYBURIDE_PREGNANCY_CL_FACTOR else 1
  vPregnancy  <- if (isTRUE(pregnant)) GLYBURIDE_PREGNANCY_V_FACTOR else 1

  default <- list(
    v1  = 11.70 * size$volume * vPregnancy, v2 = 68.1 * size$volume * vPregnancy,
    v3 = 1,
    cl1 = 2.16 * size$clearance / 60 * clPregnancy,
    cl2 = 3.84 * size$clearance / 60, cl3 = 0,
    ka_PO = 0.53 / 60,
    bioavailability_PO = 1,   # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Rambiritch 2016 (24 adults with type 2 diabetes): two compartments, ka ",
    "0.53/h, CL/F 2.16 L/h, Vc/F 11.7 L, Q/F 3.84 L/h, Vp/F 68.1 L; no ",
    "covariates. Effect site: Rydberg 1997's ke0 1.59/h (healthy volunteers, ",
    "IV and oral, Br J Clin Pharmacol 43:373); the active metabolites are not ",
    "modelled. ",
    "https://doi.org/10.2147/CPAA.S102676"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", GLYBURIDE_PREGNANCY_CL_FACTOR, ", Vc/F and Vp/F x ",
             GLYBURIDE_PREGNANCY_V_FACTOR),
      paste0("Hebert MF et al., Clin Pharmacol Ther 2009;85:607-614. ",
             "https://doi.org/10.1038/clpt.2009.5")))
  }

  list(
    PK = PK, ke0 = GLYBURIDE_KE0 / 60, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
