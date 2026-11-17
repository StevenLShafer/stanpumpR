# -----------------------------------------------------------------------------
# Sirolimus (Dansirikul 2006): two compartments, apparent oral, whole blood
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (whole blood).
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  The numbers below were read from the full text of the source (the
# PubMed Central page of PMC1885149: Methods, Tables 1 and 4 and their
# footnotes, and the covariate equation, checked on its image) and from the
# Rapamune label (NDA 021083/021110, 2019), read in full.
#
# THE SOURCE
# ==========
# Dansirikul C, Morris RG, Tett SE, Duffull SB.  A Bayesian approach for
# population pharmacokinetic modelling of sirolimus.  Br J Clin Pharmacol
# 2006;62:420-434 (doi 10.1111/j.1365-2125.2005.02533.x).  25 adult kidney
# transplant recipients (Table 1: 44 +/- 13 years, 22-73; 73 +/- 15 kg,
# 50-115; haematocrit 0.33), once-daily sirolimus, 315 whole-blood
# concentrations from monitoring (HPLC-UV), mostly troughs, with priors for
# the two-compartment parameters from the rich single-dose profiles (to 144 h)
# of Ferron 1997 and Brattstrom 1997.  Co-medication: mycophenolate and a
# steroid in 15, cyclosporin in 3, tacrolimus in 4.  Table 4 (posterior
# medians):
#
#     CL/F 12.94 L/h (at the median age)   Q/F 20.4 L/h
#     Vc/F 117 L                           Vp/F 583 L
#     ka*  2.195 /h, and "ka = ka* + CL/Vc, ka ~ 2.305 /h"
#
# and the covariate model (Equation 3; Table 4, footnote c):
# ln(CL/F) = theta1 + theta2 x age / 44, exp(theta1) = 16.9 and exp(theta2)
# = 0.742, so CL/F = 16.9 x 0.742^(age / 44) L/h, 12.5 L/h at 44 years.
# Weight was not retained.  The paper does not say which oral formulation was
# taken; the label says the tablet's bioavailability is 27% higher than the
# solution's.
#
# AGE
# ===
# The age term is held at 18 years and above (ADULT_AGE_TERM_FLOOR, AGENTS.md):
# the source studied adults from 22.
#
# CHECKED AGAINST THE LABEL (not used in the fit)
# ===============================================
# Terminal half-life: this model 56 h, the label's 62 +/- 16 h in renal
# transplant patients.  Apparent clearance: the label's Table 4 gives 139 +/-
# 63 mL/h/kg (tablets) and 173 +/- 50 (solution) at 2 mg daily WITH
# cyclosporine, which inhibits sirolimus's clearance; this model, 190 mL/h/kg
# at the 70 kg, 35-year-old reference man, in a cohort mostly without
# cyclosporine.  At 2 mg daily its steady-state AUC is 150 ng.h/mL and trough
# 3.6 ng/mL, against the label's 230 and 7.6 (tablets, with cyclosporine): so
# with cyclosporine, which is not modelled, this model underpredicts the
# concentration by about a third.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source found no weight effect, so its values are written for the
# library's reference man (the cohort averaged 73 kg) and scaled by fat-free
# mass with the switch on; with it off they are fixed, as published.
#
# DOSE BASIS
# ==========
# Sirolimus is not a salt: Rapamune is labelled in mg of sirolimus (tablets
# 0.5, 1 and 2 mg; oral solution 1 mg/mL), as the source dosed.
#
# NOT MODELLED
# ============
# Cyclosporine and the CYP3A4 and P-glycoprotein inhibitors and inducers;
# hepatic impairment (the label reduces the maintenance dose by a third to a
# half); sex (the label: clearance 12% lower in men); haematocrit, which
# whole-blood sirolimus follows; CYP3A5 genotype.
#
# THE BAND
# ========
# The Rapamune label: after cyclosporine is withdrawn, trough concentrations
# "should be 16 to 24 ng/mL for the first year following transplantation.
# Thereafter, the target sirolimus concentrations should be 12 to 20 ng/mL"
# (chromatographic assays; immunoassays read higher).  The later range is
# drawn.  With cyclosporine the label gives no number (observed troughs about
# 7 ng/mL on 2 mg daily).
# -----------------------------------------------------------------------------

# Dansirikul 2006, Table 4 and its footnotes
SIROLIMUS_CL_INTERCEPT <- 16.9    # L/h, exp(theta1)
SIROLIMUS_CL_AGE       <- 0.742   # exp(theta2), per (age / 44)
SIROLIMUS_AGE_MEDIAN   <- 44      # years
SIROLIMUS_Q            <- 20.4    # L/h
SIROLIMUS_VC           <- 117     # L
SIROLIMUS_VP           <- 583     # L
SIROLIMUS_KA           <- 2.305   # 1/h, ka* + CL/Vc

#' Sirolimus (Dansirikul 2006)
#'
#' Two compartments, apparent oral, whole blood; see the header of
#' \code{R/drugs_sirolimus.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sirolimus <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  ageTerm <- max(age, ADULT_AGE_TERM_FLOOR)
  cl <- SIROLIMUS_CL_INTERCEPT * SIROLIMUS_CL_AGE^(ageTerm / SIROLIMUS_AGE_MEDIAN)

  default <- list(
    v1  = SIROLIMUS_VC * size$volume,
    v2  = SIROLIMUS_VP * size$volume,
    v3  = 1,
    cl1 = cl / 60 * size$clearance,
    cl2 = SIROLIMUS_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = SIROLIMUS_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Dansirikul C et al., Br J Clin Pharmacol 2006;62:420-434: two ",
    "compartments, apparent oral, whole blood, kidney transplant: CL/F 16.9 x ",
    "0.742^(age/44) L/h (", sprintf("%.1f", cl), " L/h at this age, unscaled), ",
    "Q/F 20.4 L/h, Vc/F 117 L, Vp/F 583 L, ka 2.305 /h; no cyclosporine. ",
    "https://doi.org/10.1111/j.1365-2125.2005.02533.x"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 16,
    upperTypical = 20,
    lowerTypical = 12,
    reference = reference
  )
}
