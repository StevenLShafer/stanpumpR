# -----------------------------------------------------------------------------
# Bendamustine, intravenous (Owen 2010): plasma bendamustine
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg and concentrations in ng/mL (doses entered in mg of bendamustine
# HYDROCHLORIDE, as labelled; the curve is bendamustine base).
#
# Drafted by Claude Code, 2026-11-07, at the request of Steven L. Shafer
# (docs/chemotherapy.md, inst/extdata/chemotherapyRegistry.csv, source C37a).
# The parameters were read from the PubMed Central author manuscript (Tables
# 1-3 as text); the PDF is still to confirm them.  The dose basis is Shafer's
# decision (2026-11-07) with the TREANDA label he supplied (NDA 022249).
#
# THE SOURCE
# ==========
# Owen JS, Melhem M, Passarell JA, D'Andrea D, Darwish M, Kahl B.
# Bendamustine pharmacokinetic profile and exposure-response relationships in
# patients with indolent non-Hodgkin's lymphoma.  Cancer Chemother Pharmacol
# 2010;66:1039-1049 (doi 10.1007/s00280-010-1254-8).  78 adults with
# rituximab-refractory indolent NHL, 31-84 years, 44-151 kg (median 85 kg,
# BSA 2.0 m2), creatinine clearance 40.7-150 mL/min; 120 mg/m2 over 60 min on
# days 1 and 2 of 21-day cycles; 347 plasma concentrations (sparse, plus 12
# full profiles to 24 h), HPLC-MS/MS (LLOQ 0.10 ng/mL); NONMEM VI, FOCE.
#
# THE MODEL (Table 2)
# ===================
# Three compartments, linear, no covariates (sex, age, race, weight, BSA,
# creatinine clearance, ALT, AST, bilirubin and albumin were tested):
#
#     CL 31.7 L/h   V1 14.1 L   Q1 0.989 L/h   V2 0.92 L   Q2 0.159 L/h   V3 25.2 L
#
# Checks: half-lives 0.290, 0.681 and 110 h (paper 0.29, 0.7 and 110 h) and
# Vss 40.2 L ("approximately 40 L"), which also fixes the pairing (Q1 with
# V2, Q2 with V3).  The terminal phase carries under 1% of the area.  For
# 120 mg/m2 at the cohort's median 2.0 m2 over 1 h, on the base basis below,
# the model peaks at 6038 ng/mL (paper's median Cmax 5839) and the two daily
# doses give an AUC of 13,750 ng.h/mL (median cycle-1 AUC 13,635).
#
# The metabolites M3 and M4 were fitted each alone, with a lagged zero-order
# input of an unknown fraction of the dose (CL/fm, V/fm), not as formation
# from the parent, so neither can be carried as a library metabolite; the
# label puts their concentrations at 1/10 and 1/100 of the parent's.
#
# The TREANDA label (revised 2008, NDA 022249) gives a clearance of about
# 700 mL/min (42 L/h, a third above Owen's) and a Vss of about 25 L: a
# frozen, earlier summary (the AGENTS.md rule on labels), recorded not used.
#
# DOSE BASIS (Shafer, 2026-11-07)
# ===============================
# The dose is entered as bendamustine HYDROCHLORIDE, the labelled unit
# (TREANDA: C16H21Cl2N3O2 . HCl, molecular weight 394.7).  The paper does not
# say whether its amounts were converted to base; the assay measures the
# base, and both checks above favour base amounts (as hydrochloride the model
# gives Cmax 6652 and AUC 15,142, 14% and 11% high).  So the model runs in
# base: a mg of the hydrochloride is 358.26/394.72 = 0.9076 mg of bendamustine
# (C16H21Cl2N3O2 358.26 from standard atomic weights; HCl 36.46; their sum
# 394.72 is the label's 394.7), carried by dividing every volume and
# clearance by that fraction (BENDAMUSTINE_BASE_FRACTION), as for doxorubicin
# and azilsartan.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate (weight and BSA were tested and not retained): the
# parameters are taken as the reference adult's, scaled by the library's
# factors with the switch on and fixed with it off, as for daunorubicin.
# The cohort's median weight was 85 kg, so the reference 70-kg man is
# predicted 15-20% below the study's typical exposure would suggest; that is
# the library's convention for a size-free model, not a finding.
#
# RENAL AND HEPATIC FUNCTION: no term (creatinine clearance 40-150 mL/min and
# the hepatic markers were tested and not retained; the label: no meaningful
# effect of CrCL 40-80 mL/min or mild hepatic impairment).
#
# PREGNANCY (R/pregnancy.R): no comparison found; nothing applied.
# NO EFFECT SITE, NO BAND
# -----------------------------------------------------------------------------

# Owen 2010, Table 2 (L/h, L)
BENDAMUSTINE_OWEN <- list(cl = 31.7, v1 = 14.1, q1 = 0.989, v2 = 0.92,
                          q2 = 0.159, v3 = 25.2)

# Bendamustine base over the hydrochloride (358.26 / 394.72 g/mol)
BENDAMUSTINE_BASE_FRACTION <- 358.26 / 394.72

#' Bendamustine, intravenous (Owen 2010)
#'
#' Three compartments, no covariates; doses entered as the hydrochloride.  See
#' the header of \code{R/drugs_bendamustine.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
bendamustine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- BENDAMUSTINE_OWEN
  f <- BENDAMUSTINE_BASE_FRACTION
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)

  default <- list(
    v1  = p$v1 / f * size$volume,
    v2  = p$v2 / f * size$volume,
    v3  = p$v3 / f * size$volume,
    cl1 = p$cl / f / 60 * size$clearance,
    cl2 = p$q1 / f / 60 * size$clearance,
    cl3 = p$q2 / f / 60 * size$clearance
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Owen JS, Melhem M, Passarell JA, et al. Bendamustine pharmacokinetic profile and ",
    "exposure-response relationships in patients with indolent non-Hodgkin's lymphoma. ",
    "Cancer Chemother Pharmacol 2010;66:1039-1049. 78 adults: three compartments, CL ",
    "31.7 L/h, V1 14.1 L, Q1 0.989 L/h, V2 0.92 L, Q2 0.159 L/h, V3 25.2 L, no ",
    "covariates. Doses as bendamustine hydrochloride (394.7 g/mol), the model run in ",
    "base (358.26 g/mol, x 0.9076). Plasma bendamustine in ng/mL. ",
    "https://doi.org/10.1007/s00280-010-1254-8"
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
