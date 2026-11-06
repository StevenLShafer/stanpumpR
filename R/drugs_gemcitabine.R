# -----------------------------------------------------------------------------
# Gemcitabine, intravenous (Tham 2008): plasma gemcitabine, forming dFdU
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-06, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C18).
# Every number below was read from the PDF and its erratum, which Shafer
# supplied (2026-11-06): Tables 1 and 2, Figure 1, Eqs. 1-5 as corrected, on
# the page images.
#
# THE SOURCE
# ==========
# Tham L-S, Wang L-Z, Soo RA, Lee H-S, Lee S-C, Goh B-C, Holford NHG.  Does
# saturable formation of gemcitabine triphosphate occur in patients?  Cancer
# Chemother Pharmacol 2008;63:55-64 (doi 10.1007/s00280-008-0707-9); erratum
# 2015;75:657 (doi 10.1007/s00280-015-2678-y), which corrects Eqs. 1 and 3.
# 56 Asian adults with NSCLC (39 men), 39-74 years, 58 +/- 12 kg; 1000 mg/m2
# over 30 min or 750 mg/m2 over 75 min after carboplatin; plasma gemcitabine
# and dFdU (with tetrahydrouridine) to 2 h after the infusion, PBMC dFdCTP in
# 33; HPLC; NONMEM VI, FOCE-I, bootstrap medians.
#
# THE MODEL (Table 2, Figure 1b; per 70 kg, 1.76 m)
# =================================================
# Gemcitabine in two compartments, V1 2.96 L, V2 47.6 L, Q 156 L/h, total
# clearance CLG 305.2 L/h, apportioned by fractions fixed from a mass balance
# (Abbruzzese 1991): 73% deaminated to dFdU, 23% to intracellular dFdCTP, 4%
# unchanged.  dFdU in one compartment, VU 52.6 L, CLU2O 21.5 L/h.  The size
# covariate is the lean body mass relative to a 70-kg, 1.76-m standard
# (SNFWT, the fat fraction estimated at 0), by the erratum's Eq. 1:
#
#     LBM = LBMMAX x HT^2 x WT / (WT50 x HT^2 + WT)
#
# which is Janmahasatian 2005; the paper prints neither the constants nor the
# exponents.  On Shafer's decision (2026-11-06) the library's ffmAlSallami()
# is used, which is Janmahasatian in adults (42.92 and 30.93 kg/m2 for men,
# 37.99 and 35.98 for women) with a maturation term for children; the
# standard, 70 kg and 1.76 m, takes the male constants (56.13 kg), and the
# exponents are the convention the paper cites (Holford 1996): 3/4 on the
# clearances, 1 on the volumes.
#
# Checks: CLG 70.2/0.23 = 305.2 L/h and its split reproduce Table 2; at the
# reference the half-lives are 0.26 and 19.3 min (V1 is essentially
# unidentified, 90% CI 0.20-7.8 L), dFdU's 1.70 h; 1000 mg/m2 over 30 min
# (1.856 m2) peaks at 40.7 umol/L gemcitabine and 71.6 umol/L dFdU, as
# Figure 2 and the VPC show within their scatter.
#
# In the library's metabolite interface kFormation is CLG2U/V1G (0.73 of the
# parent's own clearance, inside it), and the metabolite is the
# metabolite-only gemcitabineDFDU, fitted jointly (METABOLITE_FORMATION_BASIS
# "joint"); dFdU forms mole for mole (mwRatio 264.19/263.20).  dFdCTP, a
# second metabolite of the same parent, is not carried (the closed-form
# route forms one), nor is the saturable variant the paper tested and did not
# select (Km 263 umol/L, fitted on 10 patients).
#
# DOSE BASIS
# ==========
# Doses as labelled (Gemzar, mg of gemcitabine base as the hydrochloride);
# the paper does not state its basis.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own lean-body-mass covariate, which the switch does not change
# (as for the Eleveld models).
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Tham 2008, Table 2: per 70 kg, 1.76 m (L, L/h)
GEMCITABINE_THAM <- list(
  v1 = 2.96, v2 = 47.6, q = 156,
  clg2t = 70.2, f2t = 0.23, f2u = 0.73,     # CLG = CLG2T / 0.23
  vu = 52.6, clu = 21.5,                    # dFdU
  mwRatio = 264.19 / 263.20                 # dFdU over gemcitabine
)

# Lean body mass over the 70-kg, 1.76-m standard (male constants)
gemcitabineSizeRatio <- function(weight, height, age, sex)
{
  ffmAlSallami(weight, height, age, sex) /
    ffmAlSallami(70, 176, FFM_REFERENCE_AGE, SEX_MALE)
}

#' Gemcitabine, intravenous (Tham 2008)
#'
#' Two compartments, forming dFdU; lean body mass on every parameter.  See
#' the header of \code{R/drugs_gemcitabine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
gemcitabine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- GEMCITABINE_THAM
  s <- gemcitabineSizeRatio(weight, height, age, sex)
  clg <- p$clg2t / p$f2t                                              # L/h
  v1 <- p$v1 * s
  cl1 <- clg * s^0.75 / 60

  default <- list(
    v1  = v1,
    v2  = p$v2 * s,
    v3  = 1,                                 # placeholder: two compartments
    cl1 = cl1,
    cl2 = p$q * s^0.75 / 60,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Tham L-S, Wang L-Z, Soo RA, et al. Does saturable formation of gemcitabine ",
    "triphosphate occur in patients? Cancer Chemother Pharmacol 2008;63:55-64 ",
    "(erratum 2015;75:657). 56 adults: two compartments, V1 2.96 L, V2 47.6 L, Q 156 ",
    "L/h, CL 305.2 L/h per 70 kg/1.76 m, scaled by lean body mass (Janmahasatian, ",
    "exponents 0.75 and 1), 73% of the clearance forming dFdU (one compartment, 52.6 ",
    "L, 21.5 L/h); dFdCTP not carried. Plasma gemcitabine. ",
    "https://doi.org/10.1007/s00280-008-0707-9"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "gemcitabineDFDU",
      kFormation        = p$f2u * cl1 / v1,
      firstPassFraction = 0,
      mwRatio           = p$mwRatio
    )
  )
}

#' dFdU, the deaminated metabolite of gemcitabine (Tham 2008)
#'
#' One compartment scaled by lean body mass; formed by \code{gemcitabine()}.
#' See the header of \code{R/drugs_gemcitabine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
gemcitabineDFDU <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- GEMCITABINE_THAM
  s <- gemcitabineSizeRatio(weight, height, age, sex)
  default <- list(
    v1  = p$vu * s,
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = p$clu * s^0.75 / 60,
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Tham L-S, Wang L-Z, Soo RA, et al. Cancer Chemother Pharmacol 2008;63:55-64 ",
      "(dFdU formed from gemcitabine: one compartment, 52.6 L and 21.5 L/h per 70 ",
      "kg/1.76 m, scaled by lean body mass). https://doi.org/10.1007/s00280-008-0707-9")
  )
}
