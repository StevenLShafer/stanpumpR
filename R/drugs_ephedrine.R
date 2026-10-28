# -----------------------------------------------------------------------------
# Ephedrine, intravenous: one compartment derived from the oral population
# model of Csajka 2005, with oral bioavailability taken as complete
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-28, at the request of Steven L. Shafer, who
# supplied the source and chose this construction over keeping the drug
# blocked (inst/extdata/vasoactiveRegistry.csv): no human pharmacokinetics of
# intravenous ephedrine has been published (PubMed, searched 2026-10-28), the
# labels of the intravenous products say only that "limited data after IV
# administration" support urinary excretion, and the FDA review of the first
# of them (NDA 208289) itself extrapolated from oral data.
#
# THE SOURCE
# ==========
# Csajka C, Haller CA, Benowitz NL, Verotta D.  Mechanistic pharmacokinetic
# modelling of ephedrine, norephedrine and caffeine in healthy subjects.  Br
# J Clin Pharmacol 2005;59:335-345 (doi 10.1111/j.1365-2125.2005.02254.x),
# read in full.  24 healthy adults (14 women; 52-91.5 kg, mean not printed),
# single oral doses of 25 mg ephedrine sulphate (16 subjects) or a herbal
# supplement analysed to contain 17.3 mg ephedrine (8 subjects); plasma by
# LC-MS/MS to 18-24 h and urine to 14-24 h.  Ephedrine is one compartment
# (a second did not improve the fit, dOBJ -1) with first-order absorption, a
# lag, renal elimination and a minor saturable conversion to norephedrine.
# Table 3:
#
#     V/F                      181 L
#     CL_R ("CLRE/FE")         0.34 L/min
#     F_E,pharm  0.59;  F_E,herbal  0.78   ("apparent bioavailabilities")
#
# with half-life 6.1 h (181/0.34, the conversion being minor).
#
# THE DERIVATION
# ==============
# The model loads dose x F_E into the body and fits urine with plasma, so
# CL_R is the absolute renal clearance (urinary amount over plasma area,
# whatever the bioavailability), and F_E is the fraction of the dose that
# the model must load to account for the ephedrine recovered in urine: F x
# fe, the bioavailability times the fraction of a systemic dose excreted
# unchanged.  On oral data, any F gives the same curve with
#
#     V  = 181 F / F_E          CL = 0.34 F / F_E          (half-life fixed)
#
# Taking F = 1 (the user's decision, and what the FDA review assumed), with
# the herbal F_E = 0.78, whose dose was analysed ephedrine base:
#
#     V 232.1 L,  CL 0.436 L/min (26.2 L/h),  half-life 6.15 h,  fe 0.78
#
# F < 1 would make the true clearance and volume smaller in proportion, down
# to the limit fe = 1: V 181 L and CL 0.34 L/min, 22% below.
#
# The pharmaceutical F_E agrees once its dose is read as the salt.  25 mg of
# ephedrine sulphate, (C10H15NO)2 H2SO4, 428.54 g/mol for two molecules of
# base at 165.23, is 19.28 mg base, and 0.59 / 0.7711 = 0.765 against the
# herbal 0.78: the paper's "32% greater relative bioavailability" of the
# herbal product (0.78/0.59 = 1.32) is within its standard errors of the
# sulphate-to-base ratio, 1.297.  One number in the paper does not fit that
# reading: the largest urinary amount, 22.2 mg (range 7.35-22.2), exceeds
# both base doses; the paper does not say which subject or formulation.
#
# THE CHECK: GURLEY 1998 (NOT FITTED)
# ===================================
# Gurley BJ, Gardner SF, White LM, Wang PL.  Ther Drug Monit 1998;20:439-445
# (abstract only; doi 10.1097/00007691-199808000-00015): 10 subjects, 25 mg
# ephedrine and three ma huang products, non-compartmental Vss/F 2.98 L/kg
# (men) and 3.49 (women), CL/F 0.37 and 0.48 L/h/kg.  This model at 70 kg is
# 3.32 L/kg and 0.374 L/h/kg: the volume between the sexes and the clearance
# at the men's value.  Berlin 2001 (Eur J Clin Pharmacol 57:447; nasal and
# oral) reports a half-life of about 6 h.
#
# WHAT THIS MODEL IS NOT
# ======================
# It has no distribution phase.  The oral data cannot show one (absorption
# hides it), so after an intravenous bolus the true concentrations in the
# first minutes are higher than drawn; the size of the difference is
# unknown.  Elimination is mostly renal and depends on urine pH (Csajka's CL_R
# = 0.4723 - 0.0172 pH L/min, falling as urine becomes alkaline), and
# renal impairment slows it, but there is no renal or pH covariate.  Plasma
# (-)-ephedrine base is plotted; norephedrine is not.
#
# DOSE BASIS
# ==========
# The US intravenous products (Akovaz, Corphedra, Emerphed) are labelled and
# dosed as ephedrine sulphate, so a dose here is entered as the sulphate and
# every volume and clearance is divided by 0.7711, which is exact for a
# linear model.  Ephedrine hydrochloride (Rezipres; 201.69 g/mol, 0.8192
# base) carries 6% more base per milligram.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source prints litres and no mean weight; 70 kg (FFM_REFERENCE_WEIGHT)
# is assumed.  Scaled to fat-free mass with the switch on; with it off the
# published values serve everyone (legacyVolume = 1).
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pharmacokinetic study of ephedrine in pregnancy was found (PubMed,
# 2026-10-28), although it is a standard vasopressor at caesarean delivery.
# Ngan Kee 2009 (Anesthesiology 111:506) measured maternal arterial
# concentrations at delivery only.  Nothing is applied.
#
# EFFECT SITE AND BAND
# ====================
# No time to peak pressor effect after an intravenous bolus has been
# measured with concentrations, so tPeak is 0 and only the plasma is drawn.
# There is no therapeutic concentration range, so no band.
# -----------------------------------------------------------------------------

# Csajka 2005, Table 3
EPHEDRINE_V_APPARENT  <- 181     # L, V/F
EPHEDRINE_CL_RENAL    <- 0.34    # L/min, absolute renal clearance
EPHEDRINE_FE_HERBAL   <- 0.78    # fraction of the (base) dose loaded
# Ephedrine sulphate, two molecules of base per molecule of salt
EPHEDRINE_BASE_MW     <- 165.23
EPHEDRINE_SULFATE_MW  <- 428.54

#' Ephedrine, intravenous (derived from Csajka 2005)
#'
#' One compartment derived from the oral population model of Csajka 2005
#' with oral bioavailability taken as complete; doses are ephedrine sulphate.
#' See the header of \code{R/drugs_ephedrine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ephedrine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  base <- 2 * EPHEDRINE_BASE_MW / EPHEDRINE_SULFATE_MW

  default <- list(
    v1  = EPHEDRINE_V_APPARENT / EPHEDRINE_FE_HERBAL * size$volume / base,
    v2  = 1,                          # placeholders: a one-compartment model
    v3  = 1,
    cl1 = EPHEDRINE_CL_RENAL / EPHEDRINE_FE_HERBAL * size$clearance / base,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Csajka C, Haller CA, Benowitz NL, Verotta D, Br J Clin Pharmacol ",
    "2005;59:335-345. No intravenous pharmacokinetics has been published; ",
    "one compartment derived from this oral model with bioavailability taken ",
    "as complete: V 181/0.78 = 232 L, CL 0.34/0.78 = 0.436 L/min (half-life ",
    "6.1 h), at an assumed 70 kg. No distribution phase, so the first minutes ",
    "after a bolus are higher than drawn. Doses as ephedrine sulfate (the ",
    "model's base x 428.54/330.46). https://doi.org/10.1111/j.1365-2125.2005.02254.x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
