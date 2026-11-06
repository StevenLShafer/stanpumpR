# -----------------------------------------------------------------------------
# Melphalan, high-dose intravenous (Nath 2010): total plasma melphalan
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C33).
# Every number below was read from the PDF, which Shafer supplied
# (2026-11-06): Tables 2-7, the Cockcroft-Gault and fat-free-mass equations
# and the final equations, on the page images.
#
# THE SOURCE
# ==========
# Nath CE, Shaw PJ, Trotman J, et al.  Population pharmacokinetics of
# melphalan in patients with multiple myeloma undergoing high dose therapy.
# Br J Clin Pharmacol 2010;69:484-497 (doi 10.1111/j.1365-2125.2010.03638.x).
# 100 adults (59 men), 36-73 years, 42-132 kg, before autologous stem-cell
# transplant; 192 (115-216) mg/m2 over a median 35 min; dense or D-optimal
# plasma samples to 8 h; total and unbound melphalan by HPLC-UV (detection
# limit 0.1 mcg/mL total); NONMEM, FOCE with interaction.
#
# THE MODEL (Table 5, total melphalan; the authors' preferred model)
# ==================================================================
# Two compartments, linear, clearance the sum of a non-renal and a renal part:
#
#     CL_NR = 17 L/h x (HCT/34)^0.462 x (FFM/50)^0.75
#     CL_R  = 11.1 L/h x CLcr/88       CLcr in mL/min per 70 kg
#     V1 = 13.2 L x FFM/50     Q = 30.6 L/h     V2 = 15 L
#
# FFM is Janmahasatian 2005 on total weight; CLcr is Cockcroft-Gault on
# total weight, printed with the creatinine in umol/L and 0.814, then
# normalised to 70 kg, so the weight cancels and the renal term depends only
# on age, sex and creatinine.  The FFM reference is 50 kg as printed in both
# final equations and both table footers, although the covariate screen used
# 53 and the Methods say the median (53.3) was the centre: an inconsistency,
# implemented as printed.  The unbound model (Table 5's second column) and
# Table 6's alternative (CLcr in mL/min, unstable on bootstrap) are not used.
#
# Checks: at HCT 34, FFM 50 and CLcr 88 the model gives CL 28.1 L/h (39.5%
# renal, the paper's "about 40%"), half-lives of 0.121 and 0.914 h against
# Table 7's medians 0.13 and 0.97 h, and an AUC of 13.1 mg h/L for the median
# 368 mg against Table 7's median 12.8.
#
# WHAT IS HELD AT THE REFERENCE
# =============================
# Haematocrit, which the Patient Profile does not carry: 34%, the study
# median and the model's reference, so the term is 1.  At a haematocrit of
# 20% the non-renal clearance would be 22% lower.  Albumin, protein, age,
# sex and weight were tested and not retained.
#
# RENAL FUNCTION
# ==============
# Cockcroft-Gault at 70 kg (creatinineClearanceCG(), whose 72 with mg/dL is
# the paper's 0.814 with umol/L to 0.05%), from adultEquivalentCreatinine()
# (an adult equation), or the library's assumed normal creatinine when blank.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# FFM is the model's own size covariate, computed with ffmAlSallami(), which
# is Janmahasatian's equation in adults (the source's) with a maturation term
# for children; the switch does not change it, as for the Eleveld models.  Q
# and V2, with no covariate, scale by the library's factors with the switch
# on and are fixed with it off.
#
# PREGNANCY (R/pregnancy.R)
# =========================
# No human pregnancy pharmacokinetics found.  The renal term takes the rise in
# filtration of pregnancy (x 1.5 on CLcr, pregnancyGfrFactor(), only when the
# creatinine is blank): physiological, an assumption.
#
# NO EFFECT SITE, NO BAND: the exposure-toxicity AUCs are not a range.
# -----------------------------------------------------------------------------

# Nath 2010, Table 5 (total melphalan)
MELPHALAN_NATH <- list(
  clNonRenal = 17,       # L/h at HCT 34, FFM 50 kg
  hctExponent = 0.462,
  hctReference = 34,     # %
  ffmReference = 50,     # kg, as printed
  clRenal = 11.1,        # L/h at CLcr 88 mL/min/70 kg
  clcrReference = 88,    # mL/min/70 kg
  v1 = 13.2,             # L at FFM 50 kg
  q = 30.6,              # L/h
  v2 = 15                # L
)

#' Melphalan, high-dose intravenous (Nath 2010)
#'
#' Two compartments; fat-free mass on the non-renal clearance and V1, a
#' Cockcroft-Gault renal clearance, haematocrit held at its reference.  See
#' the header of \code{R/drugs_melphalan.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE}, with the creatinine
#'   blank, raises the creatinine clearance by the rise in filtration of
#'   pregnancy
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
melphalan <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  p <- MELPHALAN_NATH
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  ffm <- ffmAlSallami(weight, height, age, sex)
  scr <- adultEquivalentCreatinine(creatinine, age, sex)
  gfrPregnancy <- pregnancyGfrFactor(pregnant, creatinine)
  clcr <- creatinineClearanceCG(70, age, sex, scr) * gfrPregnancy   # mL/min/70 kg

  clNR <- p$clNonRenal * (ffm / p$ffmReference)^0.75                # HCT 34
  clR  <- p$clRenal * clcr / p$clcrReference

  default <- list(
    v1  = p$v1 * ffm / p$ffmReference,
    v2  = p$v2 * size$volume,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = (clNR + clR) / 60,
    cl2 = p$q / 60 * size$clearance,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Nath CE, Shaw PJ, Trotman J, et al. Population pharmacokinetics of melphalan ",
    "in patients with multiple myeloma undergoing high dose therapy. Br J Clin ",
    "Pharmacol 2010;69:484-497. 100 adults: two compartments, CL 17 x (FFM/50)^0.75 ",
    "+ 11.1 x CLcr/88 L/h (Cockcroft-Gault per 70 kg, from the entered or an assumed ",
    "normal creatinine; haematocrit held at 34%), V1 13.2 x FFM/50 L, Q 30.6 L/h, ",
    "V2 15 L. Total plasma melphalan",
    if (gfrPregnancy != 1) paste0("; ", pregnancyReference(
      "creatinine clearance x 1.5, the rise in filtration of pregnancy")) else "",
    ". https://doi.org/10.1111/j.1365-2125.2010.03638.x"
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
