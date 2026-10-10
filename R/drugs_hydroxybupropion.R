# -----------------------------------------------------------------------------
# Hydroxybupropion: bupropion's active metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Verified by tests/testthat/test-drugs-bupropion.R, which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL of plasma hydroxybupropion.  The source reports
# clearances in L/h; they are divided by MINS_PER_HOUR here.
#
# DISPOSITION
# ===========
# Ghimire et al. 2026, fitted to the same single 150 mg SR dose in 19 adults
# as the parent (R/drugs_bupropion.R has the study).  Two compartments:
#
#     Vc/F   2.2 L
#     CL/F   0.9 L/h
#     Vp/F   17.7 L
#     Q/F    3.1 L/h
#
# These give disposition half-lives of 0.35 h and 18.9 h, close to the ~20 h
# usually quoted for hydroxybupropion.  A FORMED curve, though, falls late
# at the parent's slower 38.5 h rate, because in this model the parent's
# terminal phase is the slower one.
#
# APPARENT AND CONDITIONAL, SO NOT DOSABLE
# ========================================
# The source imposed the fraction of bupropion's clearance forming
# hydroxybupropion, fm = 0.1, rather than estimating it: fm and the
# metabolite's volumes and clearances are not separately identifiable from
# parent and metabolite concentrations alone.  Multiplying the metabolite's
# volumes, clearances and its formation by any common factor leaves the
# FORMED concentrations unchanged, which is why they are identified; but the
# parameters themselves are conditional on fm = 0.1 (and on the parent's
# unknown F, and on the source's mass convention), which is why the volumes
# are implausibly small (2.2 L central).  A direct dose would land on that
# unidentified scale.  Hydroxybupropion is not a marketed drug in any case.
# It is therefore offered with no dosing unit; it appears only when bupropion
# is given.  The same argument as R/drugs_desethylamiodarone.R.
#
# NO EFFECT SITE; THE BAND IS FOR THE SUM
# =======================================
# No validated concentration-response relation exists for the antidepressant
# effect, so tPeak is zero and the row plots plasma concentrations.  The band
# in inst/extdata/drugDefaults_global.csv, 850 to 1500 ng/mL with 1200 as
# typical, is the AGNP 2018 consensus therapeutic reference range (Hiemke et
# al., Pharmacopsychiatry 2018;51:9-62) for bupropion PLUS hydroxybupropion.
# It is drawn on this row because the metabolite dominates the sum (about
# 16.5 times the parent at steady state in this model: fm x CL/F of the parent
# over the metabolite's CL/F, 14.84 / 0.9); it is not a range for the
# metabolite alone, and the parent's few tens of ng/mL should be added to
# compare.  This function mirrors the CSV.  MEAC and endCe are zero.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No covariate was retained.  Fixed published parameters, scaled exactly as
# bupropion is (pkSizeFactors(legacyVolume = 1)): the apparent scale of the
# pair is only consistent if both members scale together.
#
# References
# ----------
# Ghimire et al. J Clin Pharm Ther 2026. https://doi.org/10.1155/jcpt/3265655
# Hiemke C et al. Consensus guidelines for therapeutic drug monitoring in
#   neuropsychopharmacology: update 2017. Pharmacopsychiatry 2018;51:9-62.
# -----------------------------------------------------------------------------

# Ghimire 2026.  Apparent parameters conditional on fm = 0.1, litres and
# litres per hour, converted to per minute in the model.
HYDROXYBUPROPION_V1  <- 2.2    # L, Vc/F
HYDROXYBUPROPION_V2  <- 17.7   # L, Vp/F
HYDROXYBUPROPION_CL1 <- 0.9    # L/h, CL/F
HYDROXYBUPROPION_CL2 <- 3.1    # L/h, Q/F

#' Hydroxybupropion pharmacokinetics
#'
#' Bupropion's active metabolite.  Has no dosing unit of its own: see the
#' file header for why a directly administered dose is not identified by this
#' parameter set.
#'
#' @inheritParams bupropion
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
hydroxybupropion <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Identical to bupropion's call; see the header.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- HYDROXYBUPROPION_V1 * size$volume
  v2  <- HYDROXYBUPROPION_V2 * size$volume
  v3  <- 1                                                         # no third compartment
  cl1 <- HYDROXYBUPROPION_CL1 / MINS_PER_HOUR * size$clearance     # L/min
  cl2 <- HYDROXYBUPROPION_CL2 / MINS_PER_HOUR * size$clearance     # L/min
  cl3 <- 0

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # AGNP 2018 range for bupropion + hydroxybupropion, ng/mL; mirrors the CSV.
  typical      <- 1200
  upperTypical <- 1500
  lowerTypical <- 850

  return(
    list(
      PK = PK,
      tPeak = 0,           # no effect-site model; see the header
      MEAC = 0,            # not an opioid
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = BUPROPION_REFERENCE
    )
  )
}
