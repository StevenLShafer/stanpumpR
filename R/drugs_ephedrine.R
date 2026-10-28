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
# with half-life 6.1 h (ln 2 x V/CL, CL in L/h; the conversion being minor).
#
# THE DERIVATION
# ==============
# Csajka's Table 3 reports V/F = 181 L and CL_RE/F_E = 0.34 L/min.  The model
# loads dose x F_E into the gut (F_E = F x fe, the bioavailability times the
# fraction of a systemic dose excreted unchanged in urine) and its CL_RE is
# the absolute renal clearance, because urine is fitted against plasma: the
# urinary rate is CL_RE times the real plasma concentration, so CL_RE equals
# the urinary amount over the plasma area whatever F is.  Writing the total
# clearance CL and the renal clearance CL_RE = fe CL,
#
#     CL_RE / F_E = (fe CL) / (F fe) = CL / F
#
# so the reported 0.34 is the APPARENT TOTAL (oral) clearance CL/F, not the
# renal clearance, and 181 is V/F.  Taking F = 1 (the user's decision;
# CL/F already folds the bioavailability in) gives the absolute values
# directly, with no further division:
#
#     V 181 L,  CL 0.34 L/min (20.4 L/h),  half-life 6.1 h
#
# An earlier draft divided both by the herbal F_E = 0.78 (V 232 L, CL 0.436
# L/min), which double-counts F_E and makes concentrations about 22% too low;
# CodeRabbit caught it on PR #605 and the user chose this reading (2026-10-28).
# The half-life (ln 2 x V/CL) is unchanged either way, since V and CL were
# scaled alike; only the absolute scale moves.
#
# THE CHECK: GURLEY 1998 (NOT FITTED)
# ===================================
# Gurley BJ, Gardner SF, White LM, Wang PL.  Ther Drug Monit 1998;20:439-445
# (abstract only; doi 10.1097/00007691-199808000-00015): 10 subjects, 25 mg
# ephedrine and three ma huang products, non-compartmental Vss/F 2.98 L/kg
# (men) and 3.49 (women), CL/F 0.37 and 0.48 L/h/kg.  This model at 70 kg is
# 2.59 L/kg and 0.29 L/h/kg (Csajka's apparent values at F = 1), the same
# order but running about a fifth below Gurley's, as two single-dose studies
# in different people with different assays may; a bioavailability nearer
# Gurley's apparent F (ephedrine is about 85% absorbed) would close most of
# the gap, but F = 1 is the stated assumption.  Berlin 2001 (Eur J Clin
# Pharmacol 57:447; nasal and oral) reports a half-life of about 6 h.
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

# Csajka 2005, Table 3.  V/F and CL/F, which with F = 1 are the absolute V
# and total clearance (the reported CL_RE/F_E equals CL/F; see the header).
EPHEDRINE_V   <- 181     # L, V/F
EPHEDRINE_CL  <- 0.34    # L/min, apparent total clearance CL/F
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
    v1  = EPHEDRINE_V  * size$volume / base,
    v2  = 1,                          # placeholders: a one-compartment model
    v3  = 1,
    cl1 = EPHEDRINE_CL * size$clearance / base,
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Csajka C, Haller CA, Benowitz NL, Verotta D, Br J Clin Pharmacol ",
    "2005;59:335-345. No intravenous pharmacokinetics has been published; ",
    "one compartment taken from this oral model's V/F and CL/F with F = 1: ",
    "V 181 L, CL 0.34 L/min (20.4 L/h; half-life 6.1 h), at an assumed 70 kg. ",
    "No distribution phase, so the first minutes after a bolus are higher ",
    "than drawn. Doses as ephedrine sulfate (the model's base x 428.54/330.46). ",
    "https://doi.org/10.1111/j.1365-2125.2005.02254.x"
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
