# -----------------------------------------------------------------------------
# Topotecan, intravenous (Mould 2002): total plasma topotecan
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv).  Every
# number below was read from the PDF, which Shafer supplied (2026-11-06):
# Tables I, V and VI and Eqs. 1-3, on the page images.
#
# THE SOURCE
# ==========
# Mould DR, Holford NHG, Schellens JHM, et al.  Population pharmacokinetic
# and adverse event analysis of topotecan in patients with solid tumors.
# Clin Pharmacol Ther 2002;71:334-348 (doi 10.1067/mcp.2002.123553).  245
# adults (165 women), 34-82 years, 36.5-121 kg, Cockcroft-Gault 15.5-150
# mL/min, from nine trials; 30-min infusions of 0.2-2.0 mg/m2 daily for 5
# days; total topotecan (lactone and carboxylate) in plasma by HPLC, LLOQ 0.1
# ng/mL; NONMEM, FOCE-I.
#
# THE MODEL (Table V and its footnote; Eqs. 1-3)
# ==============================================
# Two compartments, linear:
#
#     CL = (5.77 + 12.8 x RF) x (WT/70)^0.75 x exp(-0.120 x (ECOG - 1))  L/h
#     RF = CLCR/70 mL/min x 70 kg/WT
#     V1 = 19.3 x (WT/70) L;  Q = 90.4 x (WT/70)^0.75 L/h;  V2 = 45.7 x (WT/70) L
#
# CLCR is Cockcroft-Gault in mL/min, capped at 150 (the paper's own cap);
# with 70/WT it is Cockcroft-Gault at 70 kg, so renal function is free of
# body size, as the Methods say ("This procedure defines renal function
# independently of body size").  The printed equation is used, on Shafer's
# decision (2026-11-06): Table VI's worked clearances and the Discussion's
# median AUC reproduce only without the 70/WT term, as if the raw Cockcroft-
# Gault had been entered in the standardised slot, so they are not the test
# pins.  At the paper's reference (70 kg, CLCR 70, ECOG 1) CL is 18.57 L/h,
# 69% renal, Vss 65.0 L, half-lives 5.6 min and 2.68 h.  The Results'
# V1 19.2 and V2 44.5 L are Table V's 19.3 and 45.7, which are used.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# ECOG performance status 1 (no field; 0 raises CL by 13%, 2 lowers it by
# 11%).  The fitted infusion duration (0.435 h) is replaced by the infusion
# as entered; the early research formulation's factor is not applied.  Oral
# topotecan was not in the analysis.
#
# RENAL FUNCTION
# ==============
# The Patient Profile's serum creatinine through adultEquivalentCreatinine()
# (an adult equation), or the library's assumed normal creatinine when blank.
#
# DOSE BASIS
# ==========
# Doses as labelled (Hycamtin, mg of topotecan base as the hydrochloride); the
# paper does not state its basis.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own weight covariate, at the pharmacokinetic weight with the
# switch on and total weight with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No pregnancy pharmacokinetics found.  About two-thirds of the clearance is
# renal, so the creatinine clearance takes the rise in filtration of
# pregnancy (x 1.5, pregnancyGfrFactor(), only when the creatinine is blank):
# physiological, an assumption.
#
# NO EFFECT SITE, NO BAND: the paper's neutropenia relation is on AUC.
# -----------------------------------------------------------------------------

# Mould 2002, Table V
TOPOTECAN_MOULD <- list(
  clNonRenal = 5.77,  # L/h at 70 kg
  clRenal = 12.8,     # L/h at RF 1
  ecogFactor = -0.120,
  clcrCap = 150,      # mL/min, the paper's cap on Cockcroft-Gault
  v1 = 19.3,          # L at 70 kg
  q = 90.4,           # L/h at 70 kg
  v2 = 45.7           # L at 70 kg
)

#' Topotecan, intravenous (Mould 2002)
#'
#' Two compartments; weight and Cockcroft-Gault renal function on clearance,
#' performance status held at 1.  See the header of
#' \code{R/drugs_topotecan.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
topotecan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  p <- TOPOTECAN_MOULD
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  clcr <- min(creatinineClearanceCG(w, age, sex, scr), p$clcrCap) *
    gfrPregnancy                                                     # mL/min
  rf <- clcr / 70 * 70 / w                                           # as printed

  cl <- (p$clNonRenal + p$clRenal * rf) * (w / 70)^0.75              # ECOG 1
  default <- list(
    v1  = p$v1 * w / 70,
    v2  = p$v2 * w / 70,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl / 60,
    cl2 = p$q * (w / 70)^0.75 / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Mould DR, Holford NHG, Schellens JHM, et al. Population pharmacokinetic and ",
    "adverse event analysis of topotecan in patients with solid tumors. Clin ",
    "Pharmacol Ther 2002;71:334-348. 245 adults: two compartments, CL (5.77 + 12.8 x ",
    "CLCR/70 x 70/WT) x (WT/70)^0.75 L/h (Cockcroft-Gault capped at 150 mL/min, from ",
    "the entered or an assumed normal creatinine; ECOG held at 1), V1 19.3, V2 45.7 L ",
    "x WT/70, Q 90.4 L/h x (WT/70)^0.75. Total plasma topotecan in ng/mL",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1067/mcp.2002.123553"
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
