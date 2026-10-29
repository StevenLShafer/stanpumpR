# -----------------------------------------------------------------------------
# Magnesium sulfate (Du 2019): the rise in serum magnesium above baseline
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# millimoles of magnesium, so the plotted concentration is mmol/L.
#
# Drafted by Claude Code, 2026-10-18, at the request of Steven L. Shafer, as
# one of the vasoactive and resuscitation drugs used in intensive care
# (inst/help/models/vasoactive.md, inst/extdata/vasoactiveRegistry.csv).  Every
# number below was read from the full text of the source in PubMed Central
# (PMC6518930), Table 3 and the Methods.
#
# THE SOURCE
# ==========
# Du L, Wenning LA, Carvalho B, Duan J, Xu Y, Migoya E, Dutta S, Flood P,
# Drover DR.  Population pharmacokinetic modeling to evaluate standard
# magnesium sulfate treatments and alternative dosing regimens for women with
# preeclampsia.  J Clin Pharmacol 2019;59:374-385 (doi 10.1002/jcph.1328).
# Ninety-two women with preeclampsia (age 19-44, weight 57-157 kg, median
# 85 kg; serum creatinine 0.4-2.1 mg/dL, median 0.8), 623 serum samples, every
# woman given 4 g of MgSO4.7H2O over 20 minutes and then 2 g/h, continued for
# 24 hours after delivery, with samples to 12 hours after it stopped.  NONMEM
# 7.3, FOCE-I.  The dependent variable was the CHANGE in serum magnesium from
# each woman's own baseline, in mg/L.  Final model, Table 3:
#
#     CL = 3.72 L/h  x (WT/85)^0.75 x (Cr/0.8)^-0.731
#     Q  = 3.66 L/h  x (WT/85)^0.75
#     Vc = 15.4 L    x (WT/85)
#     Vp = 17.0 L    x (WT/85)
#
# with the weight exponents fixed and creatinine in mg/dL.  The text says CL
# was "further adjusted for ... antepartum or postpartum status", but Table 3
# lists every fixed effect and none is an antepartum shift: the antepartum
# indicator defines the two occasions of the inter-occasion variability on CL
# (23.9%), so the typical clearance is 3.72 L/h either side of delivery.
#
# DOSE BASIS AND WHAT IS PLOTTED
# ==============================
# Doses are grams of magnesium sulfate heptahydrate, MgSO4.7H2O, which is how
# US injections are labelled (1 g = 4.06 mmol = 8.12 mEq of magnesium).  The
# drug is named for the salt so that a "g" can never be read as elemental
# magnesium, a tenfold difference.  The molar block converts each dose at
# 246.47 g of the salt per mole of magnesium, so the plot is the rise in serum
# magnesium in mmol/L above the patient's own baseline, which is not
# modelled -- exactly as intravenous calcium is plotted (R/drugs_calcium.R).
# The study's median baseline was 0.74 mmol/L (18 mg/L); add the patient's own
# to read a total.  The paper's own simulations do the same.
#
# The model was fitted in mg/L of magnesium.  Its volumes are only consistent
# with doses converted to elemental magnesium: 4 g of salt is 394 mg of
# magnesium, which in 15.4 L is a rise of about 1 mmol/L, whereas 4000 mg in
# 15.4 L would be 10.7 mmol/L.  The test confirms the reading against the
# paper's own simulated exposure, which the fit does not use: the Zuspan
# regimen gives a 24-hour average total magnesium of 1.6 mmol/L at 85 kg and
# creatinine 0.8 mg/dL (Table 4, note).
#
# RENAL FUNCTION
# ==============
# Clearance falls with serum creatinine as published.  The Patient Profile's
# creatinine is used as entered, through adultEquivalentCreatinine() so that a
# child's creatinine is put on the adult scale the equation was fitted on; a
# blank field means the assumed normal for age and sex (0.9 mg/dL for an adult
# man, 0.8 for an adult woman, which is the model's reference).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Every parameter carries the source's own weight covariate, so it is
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, which reproduces the
# published equations exactly.  Note the reference weight is 85 kg, the
# study's median, not 70.
#
# WHAT THIS MODEL IS NOT
# ======================
# Pregnant women with preeclampsia, in whom glomerular filtration is raised and
# the volumes expanded; use in any other adult is an extrapolation, as it is
# for intravenous calcium.  Only the intravenous route was studied: the
# paper's intramuscular simulations borrowed an absorption rate and
# bioavailability from another source, so no intramuscular unit is offered.
# No band is drawn because the plotted quantity is a change above an
# unmodelled baseline; the paper's putative therapeutic range of 1.5 to
# 2.5 mmol/L and its 3.5 mmol/L toxicity margin are TOTAL concentrations.
# -----------------------------------------------------------------------------

# Du 2019, Table 3
MAGNESIUM_CL      <- 3.72     # L/h at 85 kg and creatinine 0.8 mg/dL
MAGNESIUM_Q       <- 3.66     # L/h at 85 kg
MAGNESIUM_VC      <- 15.4     # L at 85 kg
MAGNESIUM_VP      <- 17.0     # L at 85 kg
MAGNESIUM_CR_EXP  <- -0.731   # exponent of creatinine / 0.8
MAGNESIUM_WT_REF  <- 85       # kg, the study median
MAGNESIUM_CR_REF  <- 0.8      # mg/dL, the study median
# Grams of MgSO4.7H2O per mole of magnesium (Mg 24.305, S 32.06, O 15.999,
# H 1.008)
MAGNESIUM_SULFATE_HEPTAHYDRATE_MW <- 246.47

#' Magnesium sulfate (Du 2019)
#'
#' Two compartments for the rise in serum magnesium in preeclampsia; see the
#' header of \code{R/drugs_magnesiumSulfate.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value for age and sex
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
magnesiumSulfate <- function(weight, height, age, sex, adjustToFFM = TRUE,
                             creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)

  w <- pkW / MAGNESIUM_WT_REF
  default <- list(
    v1  = MAGNESIUM_VC * w,
    v2  = MAGNESIUM_VP * w,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = MAGNESIUM_CL / 60 * w^0.75 * (scr / MAGNESIUM_CR_REF)^MAGNESIUM_CR_EXP,
    cl2 = MAGNESIUM_Q / 60 * w^0.75,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")

  reference <- paste0(
    "Du L et al., J Clin Pharmacol 2019;59:374-385. ",
    "Two-compartment model in 92 women with preeclampsia given MgSO4.7H2O ",
    "4 g over 20 min then 2 g/h; plotted as the rise in serum magnesium ",
    "(mmol/L) above an unmodelled baseline (median 0.74 mmol/L).  ",
    "CL 3.72 L/h x (WT/85)^0.75 x (Cr/0.8)^-0.731, Vc 15.4 L, Q 3.66 L/h, ",
    "Vp 17.0 L at 85 kg; ", creatinineNote, ". ",
    "https://doi.org/10.1002/jcph.1328"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    molar = list(molecularWeight = MAGNESIUM_SULFATE_HEPTAHYDRATE_MW)
  )
}
