# -----------------------------------------------------------------------------
# Doxapram, intravenous (Clements 1979): plasma doxapram in a healthy volunteer
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-20, at the request of Steven L. Shafer, who
# supplied every paper read below; each was read in full unless it says
# otherwise.
#
# THE SOURCE
# ==========
# Clements JA, Robson RH, Prescott LF.  The disposition of intravenous
# doxapram in man.  Eur J Clin Pharmacol 1979;16:411-416 (PMID 527638,
# doi 10.1007/BF00568202).  Six healthy fasting men, 30-42 years, 63-72 kg
# (mean 71.4), were given 1.5 mg/kg doxapram hydrochloride over 2 minutes;
# five were later given 6.5 mg/kg over 2 hours, and three a variable-rate
# infusion.  Venous plasma to 24 hours, by gas chromatography.  Three
# compartments fitted significantly better than two.  The paper prints ONE
# complete triexponential, Subject 2's bolus (Table 1, weighting 1/variance):
#
#     A 2.01, 1.14, 0.25 ug/mL;  exponents 0.0484, 0.0105, 0.0018 /min
#
# and for the rest only means (Table 2, five subjects): half-lives 5.5, 62
# and 355 min, V1 443 mL/kg, clearance 5.93 mL/min/kg.  Those means are five
# numbers for a model with six parameters, and they are not one model's
# numbers: taken together with the reported mean of 0.9 ug/mL 2 h after the
# bolus they need a negative terminal coefficient (they are means of
# individual fits).  So the model is Subject 2's curve, converted exactly to
# three compartments by threeCompartmentFromTriexponential() (R/vasoactive.R):
#
#     V1 0.441, V2 0.248, V3 0.898 L/kg;  CL 5.19, Q2 6.29, Q3 2.75 mL/min/kg
#
# which reproduces his triexponential exactly (the test checks it).  Subject 2
# is not the mean subject: his clearance is a fifth below the group's and
# his fast phase slower (14 min against a mean 5.5).  The checks below are
# why he was chosen over the alternatives anyway.
#
# DOSE BASIS
# ==========
# Dopram is doxapram hydrochloride monohydrate (label: MW 432.98), and a
# dose is entered as labelled.  The coefficients are per 1.5 mg/kg of the
# hydrochloride as given, so the model needs no salt factor.  The paper's
# printed V1 (0.40 L/kg) and clearance (4.78 mL/min/kg) for Subject 2 are
# reproduced only with the dose converted to base by 378.52/414.98 = 0.912
# (the anhydrous hydrochloride); the same group's proceedings abstract
# (Clements, J Pharm Pharmacol 1979;31 Suppl:41P, PMID 42716) gives the
# group's clearance as 6.5 mL/min/kg, which is Table 2's 5.93 / 0.912, the
# hydrochloride basis.  Robson & Prescott 1979 (below) converted with about
# 0.87, the monohydrate's ratio.  None of these conversions enters the model.
#
# WHY THIS SUBJECT, AND WHAT ELSE WAS CONSIDERED
# ==============================================
# Roozekrans M et al. (Clin Pharmacol Ther 2017;102:115-122, PMID 28001306),
# the only modern population model: nine healthy men, 85.4 kg, arterial
# LC-MS/MS, two compartments with V1 9.0 L, V2 32.5 L, CL 0.36 L/min, Q 1.0
# L/min, sampled only to 190 min.  Its central volume is a third of
# Clements' (arterial sampling, and a 9-minute loading infusion rather than
# a bolus), and with no sample after 3 h it has no terminal phase (98 min
# against Clements' 6 h).  Clements' Subject 1 is a third candidate: the
# authors' constant-concentration regimen for him (bolus 43.6 mg, 452
# ug/min and two exponential infusions) fixes his three compartments (V1
# 21.8 L, CL 0.226 L/min, Vss 64 L), but his weight is not given.
#
# Each was run against the studies it was NOT fitted to (doses as the
# hydrochloride; ug/mL):
#
#   Robson & Prescott 1979 (Br J Clin Pharmacol 7:81-87, PMID 32901; probably
#   the same Edinburgh volunteers): bolus AUC 272 +- 101 ug min/mL observed;
#   Subject 2 gives 289, Roozekrans 295.  End of a 2-h infusion (3.25-3.5
#   mg/kg/h) 4.0 +- 1.1 observed; Subject 2 5.3-5.7, Roozekrans 6.9-7.4.
#
#   Komatsu R et al. (Anesth Analg 2005;101:1368-1373, PMID 16243996): nine
#   volunteers, 72 kg, Clements' stepped regimen doubled; 2.4-2.6 ug/mL
#   measured at thresholds reached over hours.  Subject 2 gives 2.75, 2.55,
#   2.48 at 3, 4, 5 h; Roozekrans 3.70, 3.41, 3.22; Subject 1 4.3-4.8.
#
#   Mattila M, Larma I (Br J Clin Pharmacol 1987;23:371-372, PMID 3567056):
#   nine patients after abdominal surgery, 64 kg, 1 mg/kg then 1 mg/kg/h;
#   1.5 ug/mL at 2 h and 1.21 at 4 h.  Subject 2 gives 1.98 and 2.28,
#   Roozekrans 2.37 and 2.71.  Every model is high in these patients, and
#   their mean FELL from 2 to 4 h on a constant infusion, which no linear
#   model fitted in volunteers can do.
#
#   Clements' own Table 4 regimen (3, 2, 1.5, 1, 0.5 mg/min) was built to
#   hold 2 ug/mL from 25 min in a 70-kg man, and three subjects measured
#   1.9-2.8 on it (proceedings abstract).  Here Roozekrans is better: Subject
#   2 rises from 1.45 at 25 min to 2.0 at 4 h.
#
# Subject 2 is closest wherever the dose runs for hours, which is how
# doxapram is usually given, and Roozekrans is closest in the first half hour
# of an infusion.  Measured concentrations in patients run from about half
# to twice this model's (Robson & Prescott's patients in ventilatory failure
# reached 6.6-6.8 ug/mL on 1.5 mg/min, about twice; one reached 1.6, half).
#
# EFFECT SITE: tPeak 5.8 MIN, DERIVED FROM ROOZEKRANS 2017
# ========================================================
# Roozekrans fitted an effect compartment for the rise in cardiac output,
# t1/2ke0 3.2 +- 2.0 min (SE), on its own two-compartment model.  It prints
# no time to peak effect.  A bolus through that model and that ke0 peaks at
# 5.8 min (doxapramRoozekransTPeak() below, which the test runs); the time
# to peak, not the ke0, is what carries across to a different
# pharmacokinetic model, and getDrugPK() solves Subject 2's ke0 from it.
# Three cautions: the endpoint is cardiac output, not ventilation; nobody in
# Roozekrans' study received a bolus (every dose was a 9- or 20-min loading
# infusion); and the standard error puts the time to peak anywhere from
# about 3.5 to 8 min.  The label gives 20-40 s to onset and 1-2 min to the
# peak VENTILATORY effect after an injection; Dr. Shafer chose the measured,
# modelled 5.8 min over the label (a label is frozen at approval; AGENTS.md).
#
# BAND: 1.5 TO 3 ug/mL, TYPICAL 2 (THE LIBRARY'S, NOT A THERAPEUTIC RANGE)
# ========================================================================
# No therapeutic range is established.  Clements designed his regimen for 2
# ug/mL, citing Calverley 1978 for an approximate doubling of minute
# ventilation at that concentration; Komatsu cites Calverley 1983 for 1.5 to
# 3.0 ug/mL as the range that effectively increases minute ventilation.
# Neither Calverley paper was read.  MEAC and the threshold are zero.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The parameters are per kilogram (Subject 2's weight is not printed; the
# group's mean is 71.4 kg): the 70-kg values scaled to fat-free mass with the
# switch on, and linearly in weight with it off (the default legacy factors).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No study of doxapram's pharmacokinetics in pregnancy was found (PubMed,
# 2026-11-20); recorded as unknown, nothing applied.
#
# WHAT THIS MODEL IS NOT
# ======================
# Not a neonatal model.  Doxapram's neonatal pharmacokinetics (Flint 2021,
# Pediatr Res 89:1268, PMID 32698193; Ogawa 2015, Eur J Pediatr 174:509,
# PMID 25248340) depend on gestational and postnatal age and serum AST, and
# the label says "not for use in neonates" (benzyl alcohol).  The active
# metabolite keto-doxapram (AHR 5955), formed by CYP3A4, is not plotted.  No
# ventilation or blood pressure is predicted.
# -----------------------------------------------------------------------------

# Clements 1979, Table 1, Subject 2's bolus, weighting 1/variance
DOXAPRAM_DOSE <- 1.5                       # mg/kg doxapram hydrochloride
DOXAPRAM_COEF <- c(2.01, 1.14, 0.25)       # ug/mL (mg/L)
DOXAPRAM_EXP  <- c(0.0484, 0.0105, 0.0018) # /min

# Roozekrans 2017, Table 1: its two-compartment model and effect site
DOXAPRAM_ROOZEKRANS <- list(v1 = 9.0, v2 = 32.5, cl1 = 0.36, cl2 = 1.0,
                            t12ke0 = 3.2)

DOXAPRAM_TPEAK <- 5.8   # min, doxapramRoozekransTPeak() rounded; see header

#' The time to peak effect of Roozekrans 2017's doxapram model
#'
#' A bolus through Roozekrans' two-compartment model and its effect
#' compartment (t1/2ke0 3.2 min, the rise in cardiac output): the time at
#' which the effect-site concentration peaks.  See the header of
#' \code{R/drugs_doxapram.R}.
#'
#' @param p Roozekrans' parameters, \code{DOXAPRAM_ROOZEKRANS}
#' @returns minutes
#' @keywords internal
doxapramRoozekransTPeak <- function(p = DOXAPRAM_ROOZEKRANS)
{
  k10 <- p$cl1 / p$v1
  k12 <- p$cl2 / p$v1
  k21 <- p$cl2 / p$v2
  s <- k10 + k12 + k21
  lambda <- (s + c(1, -1) * sqrt(s^2 - 4 * k10 * k21)) / 2
  coef <- (k21 - lambda) / (rev(lambda) - lambda) / p$v1
  ke0 <- log(2) / p$t12ke0
  effectSitePeakTime(coef, lambda, ke0)
}

#' Doxapram, intravenous (Clements 1979)
#'
#' Three compartments from one healthy volunteer's triexponential, per kg,
#' with a time to peak effect of 5.8 min derived from Roozekrans 2017; see
#' the header of \code{R/drugs_doxapram.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxapram <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)

  # mg/L per mg/kg of the hydrochloride: the per-kg unit response
  p <- threeCompartmentFromTriexponential(DOXAPRAM_COEF / DOXAPRAM_DOSE,
                                          DOXAPRAM_EXP)
  p <- lapply(p, `*`, FFM_REFERENCE_WEIGHT)

  default <- list(
    v1  = p$v1  * size$volume,
    v2  = p$v2  * size$volume,
    v3  = p$v3  * size$volume,
    cl1 = p$cl1 * size$clearance,
    cl2 = p$cl2 * size$clearance,
    cl3 = p$cl3 * size$clearance
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Clements JA, Robson RH, Prescott LF, Eur J Clin Pharmacol 1979;16:411-416. ",
    "Three compartments from Subject 2's bolus triexponential (Table 1): ",
    "V1 0.44, V2 0.25, V3 0.90 L/kg, CL 5.2 mL/min/kg; doses as the ",
    "hydrochloride. Time to peak effect 5.8 min, derived from Roozekrans M et ",
    "al., Clin Pharmacol Ther 2017;102:115-122 (t1/2ke0 3.2 min, cardiac ",
    "output). Checked against Robson & Prescott 1979, Komatsu 2005 and ",
    "Mattila & Larma 1987. https://doi.org/10.1007/BF00568202"
  )

  list(
    PK = PK,
    tPeak = DOXAPRAM_TPEAK,
    MEAC = 0,
    typical = 2,
    upperTypical = 3,
    lowerTypical = 1.5,
    reference = reference
  )
}
