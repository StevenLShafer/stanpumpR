# -----------------------------------------------------------------------------
# Levosimendan (Jonsson 2003): plasma levosimendan in heart failure
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-25, on Steven L. Shafer's decision of that
# day (card papers/digests/PMID_41288922.md, [decided]: Jonsson 2003 for the
# parent, Kivikko 2002 for the metabolite OR-1896 on an extended engine).
# Every number below was read on the PDF of the source (stanpumpRPapers
# PMID_12814448.pdf; card papers/digests/PMID_12814448.md, from the digest
# run of 2026-11-25, PR #899).
#
# THE SOURCE
# ==========
# Jonsson EN, Antila S, McFadyen L, Lehtonen L, Karlsson MO.  Population
# pharmacokinetics of levosimendan in patients with congestive heart failure.
# Br J Clin Pharmacol 2003;55(6):544-551 (doi
# 10.1046/j.1365-2125.2003.01778.x; PMC1884248).  Two phase 2-3 efficacy
# studies in NYHA III-IV heart failure (Table 1): a 10-min loading dose of
# 3-36 mcg/kg then 0.05-0.6 mcg/kg/min for 24 h (study 1), or 6 mcg/kg over 5
# min then ascending 0.1-0.4 mcg/kg/min for 6-48 h (study 2); about 10
# samples per patient over 0-60 h; plasma, HPLC, LLOQ 5 ng/mL.  190 patients
# analysed (abstract); 193
# with covariates (Tables 2-3): age 30-84 (median 61), weight 46-130 kg
# (median 76), height 142-193 cm (median 173), 161 men and 32 women.
# NONMEM V, first order.  Two compartments with zero-order input; a third
# compartment did not improve the fit.  Only body weight was retained, on CL
# and V1, LINEARLY about 76 kg (Table 5; text p548):
#
#     CL = 13.3 L/h x (1 + 0.015 (WT - 76))      (1.5% per kg, 95% CI 0.9-2.1)
#     V1 = 16.8 L   x (1 + 0.009 (WT - 76))      (0.9% per kg, 95% CI 0.5-1.3)
#     Q  = 2.33 L/h (text; Table 5 rounds to 2.3)
#     V2 = 30.0 L
#
# Mean terminal half-life of the empirical Bayes estimates 0.84 +/- 0.3 h.
# The typical values give half-lives of 0.74 h (99% of a bolus) and 10.6 h:
# the small slow tail is the typical patient's, which few individuals' fits
# showed (Q's variability is 170% and V2's 260%).
# Sex, age, digoxin, beta blockers, bilirubin and creatinine clearance were
# tested and dropped (Table 4).
#
# WEIGHT OUTSIDE THE STUDIED RANGE (no infinity, no division by zero)
# ===================================================================
# The linear terms reach zero clearance at 9.3 kg and zero central volume at
# a negative weight below 0; extrapolated they would stop the engines for a
# small patient.  They are evaluated within the studied 46-130 kg and
# continued outside it allometrically from the nearest edge (CL by weight^0.75,
# V1 in proportion), which is an assumption: Jonsson studied no one outside
# that range, and no child.  Bertin 2026 (PMID 41288922) studied neonates and
# infants on ECMO and is the record for them, not implemented.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL and V1 carry the source's own weight term, evaluated at the
# pharmacokinetic weight with the fat-free-mass switch on (size$pkWeight)
# and at total weight with it off.  Q and V2 have no size term, so with the
# switch on they take the fat-free-mass factors anchored at the source's
# cohort (median 76 kg, 61 years, 173 cm, 161 men and 32 women), and with it
# off they are the published values.
#
# WHAT IS NOT DRAWN
# =================
# Levosimendan's haemodynamic effect outlasts the parent by days, through
# OR-1896, formed slowly from OR-1855 after intestinal reduction (half-life
# about 70-80 h).  The parent alone is drawn here; the metabolite waits for
# Kivikko 2002 (PMID 12395979, papers/pending-work.csv) and an engine for a
# metabolite formed from a metabolite with back-conversion, to be proposed
# to Shafer before it is built.  So there is no tPeak (the effect follows
# the metabolite, not the parent) and no band.
#
# PRODUCT AND DOSE BASIS.  Not approved in the United States (Simdax,
# Orion, in Europe and elsewhere); dosed as levosimendan, mcg/kg and
# mcg/kg/min, as in the source.
#
# PREGNANCY.  No pharmacokinetic study in pregnancy (PubMed, 2026-11-25);
# recorded unknown.
# -----------------------------------------------------------------------------

LEVOSIMENDAN_CL  <- 13.3 / 60   # L/min at 76 kg
LEVOSIMENDAN_V1  <- 16.8        # L at 76 kg
LEVOSIMENDAN_Q   <- 2.33 / 60   # L/min
LEVOSIMENDAN_V2  <- 30.0        # L
LEVOSIMENDAN_CL_SLOPE <- 0.015  # per kg about 76 kg
LEVOSIMENDAN_V1_SLOPE <- 0.009  # per kg about 76 kg
LEVOSIMENDAN_WT_REF <- 76       # kg
LEVOSIMENDAN_WT_RANGE <- c(46, 130)  # kg, studied
LEVOSIMENDAN_COHORT <- list(weight = 76, age = 61, height = 173,
                            men = 161, women = 32)

#' Levosimendan's weight factors on CL and V1 (Jonsson 2003)
#'
#' Linear about 76 kg within the studied 46-130 kg, continued allometrically
#' from the nearest edge outside it.
#' @param w weight in kg
#' @returns list of `cl` and `v1` multipliers of the 76 kg values
#' @keywords internal
levosimendanWeightFactors <- function(w)
{
  edge <- min(max(w, LEVOSIMENDAN_WT_RANGE[1]), LEVOSIMENDAN_WT_RANGE[2])
  cl <- (1 + LEVOSIMENDAN_CL_SLOPE * (edge - LEVOSIMENDAN_WT_REF)) * (w / edge)^0.75
  v1 <- (1 + LEVOSIMENDAN_V1_SLOPE * (edge - LEVOSIMENDAN_WT_REF)) * (w / edge)
  list(cl = cl, v1 = v1)
}

#' Levosimendan (Jonsson 2003)
#'
#' Two compartments in 190 patients with heart failure, weight linear on CL
#' and V1; see the header of \code{R/drugs_levosimendan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levosimendan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, LEVOSIMENDAN_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  f <- levosimendanWeightFactors(w)

  default <- list(
    v1  = LEVOSIMENDAN_V1 * f$v1,
    v2  = LEVOSIMENDAN_V2 * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = LEVOSIMENDAN_CL * f$cl,
    cl2 = LEVOSIMENDAN_Q * size$clearance,
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
      "Jonsson EN et al., Br J Clin Pharmacol 2003;55(6):544-551. ",
      "Two compartments in 190 patients with NYHA III-IV heart failure: ",
      "CL 13.3 L/h x (1 + 0.015 (WT - 76)), V1 16.8 L x (1 + 0.009 (WT - 76)), ",
      "Q 2.33 L/h, V2 30.0 L; plasma.  The parent only: the long-acting ",
      "metabolite OR-1896 is not drawn.  Not approved in the US. ",
      "https://doi.org/10.1046/j.1365-2125.2003.01778.x")
  )
}
