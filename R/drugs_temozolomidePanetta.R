# -----------------------------------------------------------------------------
# Temozolomide, oral (Panetta 2003): plasma temozolomide in infants, children
# and young adults with primary central nervous system tumours
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L).
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# asked for this paper (stanpumpR-papers-needed-2026-11-16.csv, priority 1)
# and supplied the PDF.  Every number below was read on the pages of the PDF.
# A candidate of the temozolomide group (R/modelSelection.R): the program
# chooses it for children, Ostermann 2004 (R/drugs_temozolomideOstermann.R)
# for adults.
#
# THE SOURCE
# ==========
# Panetta JC, Kirstein MN, Gajjar A, Nair G, Fouladi M, Heideman RL,
# Wilkinson M, Stewart CF.  Population pharmacokinetics of temozolomide and
# metabolites in infants and children with primary central nervous system
# tumors.  Cancer Chemother Pharmacol 2003;52:435-441 (doi
# 10.1007/s00280-003-0670-4).  39 patients (20 boys, 19 girls; Table 1: age
# median 7.1 years, 0.7-21.9; BSA median 1.1 m2, 0.45-2.1) given 145-200
# mg/m2 orally once daily for 5 days every 21 days, 2 h from a meal; plasma
# at 0.25-8 h after the first and fifth doses of courses 1 and 3 (109
# studies, a further 23 kept for validation); HPLC-UV; ADAPT II per study,
# then a linear mixed-effects model of the log parameters (two-stage).
#
# THE MODEL (Fig. 2; Tables 2 and 3)
# ==================================
# First-order absorption into one compartment, bioavailability fixed at 1 (so
# the volume and clearance are apparent, V/F and CL/F):
#
#     ln CL/F (L/h) = 0.42 + 1.08 BSA        ln Vc/F (L) = 1.34 + 1.09 BSA
#     ka 2.4 /h (Table 2's population mean; no covariate)
#
# Table 3 prints these two equations, with BSA in m2, as the covariate models
# of the absolute parameters.  It also prints an age model of the clearance
# per m2 (ln CL/F per m2 = 1.38 + 0.024 age) and says that BSA and age were
# significant "both independently and in combination", but prints no combined
# equation, so the age term is NOT applied: the BSA equations are the only
# complete model of the absolute parameters.
#
# CHECKS (test-drugs-temozolomidePanetta.R): at the median BSA, 1.1 m2, CL/F
# 4.99 L/h and Vc/F 12.7 L give ke 0.393 /h against Table 2's 0.39 (a value
# the equations were not fitted to; Table 2's CL/F 5.4 and Vc/F 14.0 L are
# means, which a log-normal model's typical value sits below); and at an
# adult's 1.8 m2 the equations give CL/F 10.6 L/h and Vc/F 27.2 L, near
# Ostermann's 10.0-11.9 L/h and 30.3 L, a check across the two sources.
#
# HELD TO THE STUDIED RANGE OF BSA [decision], Claude's choice of 2026-11-16,
# for Shafer: the equations are exponential in BSA, so outside the studied
# 0.45-2.1 m2 they were not fitted and do not scale with size (at 0.1 m2 they
# still give 1.7 L/h, at 3 m2 39 L/h).  Below 0.45 m2 and above 2.1 m2 the
# clearance and volume PER m2 are held at their values at the nearer limit,
# i.e. scaled in proportion to BSA, which is how the drug is dosed and what
# the paper's normalisation reduced the variability by.  Continuous at both
# limits.
#
# NOT APPLIED: the age term (above); the dose dependence of clearance the
# paper reports (17% lower at 150 than at 200 mg/m2, a two-stage comparison
# with no model); MTIC and AIC, whose metabolite volumes were fixed to V/F and
# whose rate constants Table 2 does not print.  Renal and hepatic function had
# no effect (temozolomide is cleared by spontaneous hydrolysis).
#
# DOSE BASIS: temozolomide has no salt; doses in mg of temozolomide.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model's own covariate, body-surface area (the paper names no formula;
# Mosteller's is used, Shafer's default since 2026-11-23), computed from TOTAL
# BODY WEIGHT in BOTH switch positions, so this drug's curve does not depend on
# the fat-free-mass switch at all.
#
# Until 2026-11-25 the switch-on case fed `pkSizeFactors()$pkWeight` into the BSA,
# which applied the size adjustment TWICE: Panetta fitted ln CL/F and ln V/F
# against the BSA of the children as they actually were, whatever formula he used,
# so evaluating that covariate at a fat-free-adjusted weight asks the equation a
# question it was never fitted to answer, and the FFM scaling is then also inside
# the covariate it feeds.  Shafer's decision of 2026-11-25 (Group B item 11), on
# the principle that a covariate fitted against real weight is evaluated at real
# weight.  The missing BSA formula is a separate and smaller uncertainty --
# Mosteller and Du Bois differ by a few per cent over the studied 0.45-2.1 m2.
#
# PREGNANCY: see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Panetta 2003, Tables 1-3
TEMOZOLOMIDE_PANETTA <- list(
  cl = 0.42, clBsa = 1.08,       # ln CL/F (L/h) = cl + clBsa x BSA
  v = 1.34, vBsa = 1.09,         # ln Vc/F (L)   = v + vBsa x BSA
  ka = 2.4,                      # /h
  bsaMin = 0.45, bsaMax = 2.1    # m2, the studied range (Table 1)
)

#' Temozolomide's clearance and volume at a body-surface area (Panetta 2003)
#'
#' The exponential equations within the studied BSA, proportional to BSA
#' outside it.
#' @param bsa body-surface area, m2
#' @returns a list: cl (L/h) and v (L)
#' @keywords internal
temozolomidePanettaClV <- function(bsa)
{
  p <- TEMOZOLOMIDE_PANETTA
  b <- min(max(bsa, p$bsaMin), p$bsaMax)
  list(cl = exp(p$cl + p$clBsa * b) * bsa / b,
       v = exp(p$v + p$vBsa * b) * bsa / b)
}

#' Temozolomide, oral (Panetta 2003)
#'
#' First-order absorption into one compartment, body-surface area on
#' clearance and volume, in infants, children and young adults.  See the
#' header of \code{R/drugs_temozolomidePanetta.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
temozolomidePanetta <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- TEMOZOLOMIDE_PANETTA
  # BSA from TOTAL weight whichever way the switch is set: the covariate was fitted
  # against the children's actual size, so `adjustToFFM` must not reach it (see the
  # BODY SIZE note above).  pkSizeFactors() is deliberately not called -- this model
  # carries its own size covariate and needs no generic factors.
  m <- temozolomidePanettaClV(bsaMosteller(weight, height))

  default <- list(
    v1  = m$v,
    v2  = 1,                                 # placeholders: one compartment
    v3  = 1,
    cl1 = m$cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = p$ka / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Panetta JC, Kirstein MN, Gajjar A, et al. Population pharmacokinetics of temozolomide ",
    "and metabolites in infants and children with primary central nervous system tumors. ",
    "Cancer Chemother Pharmacol 2003;52:435-441. 39 patients 0.7-21.9 years: oral, one ",
    "compartment, ka 2.4 /h, ln CL/F = 0.42 + 1.08 BSA (L/h), ln V/F = 1.34 + 1.09 BSA (L), ",
    "proportional to BSA outside the studied 0.45-2.1 m2 (Mosteller BSA). ",
    "https://doi.org/10.1007/s00280-003-0670-4"
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
