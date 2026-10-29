# -----------------------------------------------------------------------------
# Vasopressin (arginine vasopressin), intravenous (Fabian 1969): exogenous
# plasma vasopressin in healthy volunteers, in microunits per mL
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.  Doses in
# units (U); amounts in mU, concentrations in mU/L = mcU/mL.
#
# Drafted by Claude Code, 2026-10-22, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the published articles, which he supplied.
#
# THE SOURCE
# ==========
# Fabian M, Forsling ML, Jones JJ, Pryor JS.  The clearance and antidiuretic
# potency of neurohypophysial hormones in man, and their plasma binding and
# stability.  J Physiol 1969;204:653-668 (doi 10.1113/jphysiol.1969.sp008937,
# PMC1351579).  Six volunteers aged 20-22 (five men, one woman) were given
# arginine vasopressin (Parke-Davis Pitressin) at 120 mU/min into a forearm
# vein for 20 minutes; plasma was sampled 2 minutes before, and 4 to 10
# minutes after, the infusion stopped, and assayed for antidiuretic activity
# in the rat against the same preparation, so concentrations are in the
# units of the dose.  Table 1 (medians, 95% confidence limits):
#
#     t1/2 5.6 min (3.9-9.5),  V 8.5 L (5.2-11.0),  CL 1.0 L/min (0.4-1.1)
#
# and the median concentration at equilibrium was 120 mcU/mL (Fig. 4), the
# infusion rate over the clearance.  The fall after the infusion was a single
# exponential over the whole 10 minutes sampled (the Discussion), so the
# model is one compartment.  V and CL are taken as printed; together they
# give a half-life of 5.9 minutes, inside the confidence limits of the
# median 5.6 (medians of three quantities need not agree exactly).  V is
# about two thirds of the extracellular volume, as the authors say.
#
# Corroboration.  The label (Vasostrict) gives a clearance of 9 to 25
# mL/min/kg in vasodilatory shock, which brackets this one at 70 kg (14
# mL/min/kg).  Davison 1993 (below) measured 0.67 L/min in women 10-12 weeks
# postpartum, by radioimmunoassay at physiological concentrations.  Landry
# 1997 (Circulation 95:1122) found about 30 pg/mL in septic shock on 0.01
# U/min, against an endogenous 3.1 pg/mL: this model gives 10 mcU/mL above
# baseline, 18.9 pg/mL at the label's 530 U/mg, 22 pg/mL with the baseline,
# 27% under Landry, whose value Davison's 0.67 L/min would reproduce (31
# pg/mL).  At Davison's 400 U/mg standard it was 25 and 28 pg/mL: the
# conversion, the label's since 2026-10-28 on Steven L. Shafer's decision,
# moves this comparison, not the curve.  Baumann 1976 (J Clin Invest
# 57:1109), with an iodinated tracer, found a quarter of this clearance (4.1
# mL/min/kg) and a 24-minute half-life; it is the outlier, and the tracer
# is not the hormone.
#
# WHAT IS PLOTTED
# ===============
# Exogenous vasopressin, above the patient's own, in mcU/mL (microunits per
# mL, = mU/L): the unit the source measured and the unit the drug is dosed
# in, so no conversion to mass is involved.  At the label's 530 U/mg
# (VASOSTRICT, NDA 204485, section 11; Davison used a 400 U/mg standard), 1
# mcU/mL is 1.89 pg/mL.  Vasopressin does not appear to bind plasma protein
# (the label, section 12.3; Fabian's estimate of about 30% bound is not
# carried, by Steven L. Shafer's decision of 2026-10-28), so the total
# plotted is the unbound concentration.  The endogenous concentration is about 1-10 pg/mL (inappropriately
# low, about 3 pg/mL, in septic shock; Landry 1997) and is not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Six young adults whose weights are not reported: fixed values, scaled to
# fat-free mass with the switch on and left as published with it off
# (legacyVolume = 1).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# Davison JM, Sheills EA, Philips PR, Barron WM, Lindheimer MD.  Metabolic
# clearance of vasopressin and an analogue resistant to vasopressinase in
# human pregnancy.  Am J Physiol 1993;264:F348-F353 (doi
# 10.1152/ajprenal.1993.264.2.F348).  The same six women by three-tier
# constant infusion in late pregnancy and again 10-12 weeks postpartum: the
# metabolic clearance of vasopressin was 3.4 against 0.67 L/min (the
# abstract's means; Table 1 by tier, 3.7, 3.4, 3.0 against 0.7, 0.6, 0.7),
# unaltered by tripling the concentration.  The rise is placental
# vasopressinase (cystine aminopeptidase).  So in late pregnancy the
# clearance is multiplied by 3.4 / 0.67 = 5.07; the volume was not measured
# and is unchanged.  This is an extrapolation in two ways: Davison measured
# physiological concentrations (2 to 9 pg/mL) by radioimmunoassay, and the
# ratio is applied here to a bioassay model at pressor doses, ten times
# higher.  Clearance did not change over Davison's threefold range, but no
# study has measured it at pressor concentrations in pregnancy; if
# vasopressinase saturates there, the factor overstates the rise.
#
# EFFECT SITE: tPeak 3 MINUTES (PROVISIONAL CLINICAL OBSERVATION); NO BAND
# =======================================================================
# No published time to peak pressor effect after a bolus was found (the
# label's "peak within 15 minutes" is for an infusion).  3 minutes is Dr.
# Shafer's provisional clinical observation, until published data are
# available: the pressor response to a bolus is not immediate, as the heart
# rate response to epinephrine is.  No pressure is predicted.
# -----------------------------------------------------------------------------

VASOPRESSIN_V  <- 8.5    # L, Fabian 1969 Table 1 (median)
VASOPRESSIN_CL <- 1.0    # L/min, Fabian 1969 Table 1 (median)
VASOPRESSIN_TPEAK <- 3   # min, provisional clinical observation (S. L. Shafer)
# Davison 1993: late pregnancy 3.4 L/min, 10-12 weeks postpartum 0.67 L/min
VASOPRESSIN_PREGNANCY_CL <- 3.4 / 0.67

#' Vasopressin, intravenous (Fabian 1969)
#'
#' One compartment, from the median clearance and volume of six young adults
#' given arginine vasopressin by infusion; concentrations in mcU/mL above the
#' patient's own.  See the header of \code{R/drugs_vasopressin.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   clearance by 5.07, the paired late-pregnancy to postpartum ratio of
#'   Davison 1993 (placental vasopressinase).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
vasopressin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  clPregnancy <- if (isTRUE(pregnant)) VASOPRESSIN_PREGNANCY_CL else 1

  default <- list(
    v1  = VASOPRESSIN_V * size$volume,
    v2  = 1,                          # placeholders: a one-compartment model
    v3  = 1,
    cl1 = VASOPRESSIN_CL * size$clearance * clPregnancy,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Fabian M, Forsling ML, Jones JJ, Pryor JS, J Physiol 1969;204:653-668. ",
    "One compartment, the medians of 6 young adults given arginine ",
    "vasopressin by infusion: V 8.5 L, CL 1.0 L/min. Exogenous vasopressin ",
    "above the patient's own, in mcU/mL (1 mcU/mL is 1.89 pg/mL at the label's 530 U/mg). ",
    "Time to peak effect 3 min: provisional clinical observation (S. L. ",
    "Shafer), pending published data. ",
    "https://doi.org/10.1113/jphysiol.1969.sp008937"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      "clearance x 5.07",
      "Davison JM et al., Am J Physiol 1993;264:F348-F353"))
  }

  list(
    PK = PK,
    tPeak = VASOPRESSIN_TPEAK,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
