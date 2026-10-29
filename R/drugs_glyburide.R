# -----------------------------------------------------------------------------
# Glyburide (glibenclamide): two compartments, oral
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Concentration only: no glucose or insulin effect
# is plotted, and the active metabolites M1 and M2 are not modelled.  Drafted
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
# BODY SIZE: no covariates; the parameters are scaled by the library's
# fat-free-mass factors, fixed with the switch off (legacyVolume = 1).
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
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
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
    "covariates; concentration only. https://doi.org/10.2147/CPAA.S102676"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", GLYBURIDE_PREGNANCY_CL_FACTOR, ", Vc/F and Vp/F x ",
             GLYBURIDE_PREGNANCY_V_FACTOR),
      paste0("Hebert MF et al., Clin Pharmacol Ther 2009;85:607-614. ",
             "https://doi.org/10.1038/clpt.2009.5")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
