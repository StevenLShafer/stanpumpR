# -----------------------------------------------------------------------------
# Desethylamiodarone: amiodarone's active metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-07, at the request of
# Steven L. Shafer, a co-author of the source.  Verified by
# tests/testthat/test-drugs-amiodarone.R, which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of serum desethylamiodarone.  The source
# reports clearances in L/day; they are divided by MINS_PER_DAY here.
#
# DISPOSITION
# ===========
# Pollak, Bouillon and Shafer 2000, fitted to the same 605 trough samples as
# the parent (R/drugs_amiodarone.R has the study).  Two compartments, Table
# II, with interindividual CV:
#
#     V1     2790 L       21.2%
#     V2     7830 L       108.6%
#     CL1    254 L/day    30.4%
#     CL2    151 L/day    44.4%
#
# The half-lives these give are 108.8 h (4.53 days) and 60.39 days.  The
# paper reports a terminal half-life of 62 days (Table II and the Results),
# which Table II's own values do not reproduce; the difference is recorded
# here, not adjusted.  The error model was proportional only: the additive
# term did not contribute and was removed.
#
# APPARENT PARAMETERS, SO NOT DOSABLE
# ===================================
# The metabolite model was fitted sequentially: its input was "the quantity
# of amiodarone permanently cleared from the serum (presumably by metabolism
# to desethylamiodarone)", computed from each patient's post hoc parent
# parameters.  That input is itself apparent (CL1/F x Cp), and it assumes
# that ALL the cleared amiodarone becomes desethylamiodarone, mass for mass.
# These parameters are therefore the true ones divided by the parent's
# bioavailability and by the fraction actually converted (and by the molar
# ratio, which the mass basis absorbs): multiplying the
# metabolite's volumes, clearances and the formation flux by any common
# factor leaves the FORMED concentrations unchanged, which is why they are
# identified, but a direct dose would land on that unidentified scale.  The
# same argument as R/drugs_desmetramadol.R.  Desethylamiodarone is therefore
# offered with no dosing unit; it appears only when amiodarone is given.
#
# NO EFFECT SITE, AND NO THERAPEUTIC RANGE
# ========================================
# Desethylamiodarone appears to be as potent as the parent (Nattel and
# Talajic 1988, Pollak's reference 40), but, as for the parent, no human
# ke0 has been published: tPeak is zero and the row plots serum
# concentrations.  The 1.0 to 2.5 mg/L therapeutic window applies to
# amiodarone only, and no range has been established for the metabolite,
# so the band in inst/extdata/drugDefaults_global.csv is zero (no band is
# drawn) and so is endCe (no threshold).  MEAC is zero: not an opioid.  At
# steady state the metabolite settles at 229 / 254 = 0.90 of the parent's
# concentration, all of the parent's clearance being formation.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No covariate was significant.  Fixed published parameters, scaled exactly
# as amiodarone is (legacy factors 1): the apparent scale of the pair is only
# consistent if both members scale together.
#
# References
# ----------
# Pollak PT, Bouillon T, Shafer SL. Population pharmacokinetics of long-term
#   oral amiodarone therapy. Clin Pharmacol Ther 2000;67:642-652.
#   https://doi.org/10.1067/mcp.2000.107047
# Nattel S, Talajic M. Recent advances in understanding the pharmacology of
#   amiodarone. Drugs 1988;36:121-131.
# -----------------------------------------------------------------------------

# Pollak 2000, Table II.  Apparent parameters, litres and litres per day,
# converted to per minute in the model.
DESETHYLAMIODARONE_V1  <- 2790   # L
DESETHYLAMIODARONE_V2  <- 7830   # L
DESETHYLAMIODARONE_CL1 <- 254    # L/day
DESETHYLAMIODARONE_CL2 <- 151    # L/day

#' Desethylamiodarone pharmacokinetics
#'
#' Amiodarone's active metabolite.  Has no dosing unit of its own: see the
#' file header for why a directly administered dose is not identified by this
#' parameter set.
#'
#' @inheritParams amiodarone
#' @returns a list in the shape \code{getDrugPK()} expects
#' @noRd
desethylamiodarone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Identical to amiodarone's call; see the header.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- DESETHYLAMIODARONE_V1 * size$volume
  v2  <- DESETHYLAMIODARONE_V2 * size$volume
  v3  <- 1                                                        # no third compartment
  cl1 <- DESETHYLAMIODARONE_CL1 / MINS_PER_DAY * size$clearance   # L/min
  cl2 <- DESETHYLAMIODARONE_CL2 / MINS_PER_DAY * size$clearance   # L/min
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

  return(
    list(
      PK = PK,
      tPeak = 0,           # no effect-site model; see the header
      MEAC = 0,            # not an opioid
      # No established range: no band.  The CSV carries the same zeros.
      typical = 0,
      upperTypical = 0,
      lowerTypical = 0,
      reference = AMIODARONE_REFERENCE
    )
  )
}
