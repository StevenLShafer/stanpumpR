# -----------------------------------------------------------------------------
# Norfluoxetine: fluoxetine's active metabolite (Han 2025)
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Verified by tests/testthat/test-drugs-fluoxetine.R, which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# Han 2025, fitted jointly with fluoxetine to steady-state troughs only:
# CL/F 3.24 L/h, V/F 1.52 L, one compartment, with FM fixed at 1.  These give
# a half-life of 20 minutes, where the label gives 4 to 16 days.  The
# metabolite is therefore formation-rate limited in this model and simply
# follows the parent at 2.91 / 3.24 = 0.90 of its concentration in women;
# only its steady-state trough is valid.  R/drugs_fluoxetine.R has the full
# account, and the reproduction of the source's simulated troughs.
#
# Apparent parameters, conditional on FM = 1 and on the parent's unmeasured
# bioavailability, so a direct dose would land on an unidentified scale:
# norfluoxetine has no dosing unit, as desethylamiodarone.  No sex effect was
# retained on its clearance.  Scaled exactly as fluoxetine is.
# -----------------------------------------------------------------------------

NORFLUOXETINE_CL1 <- 3.24   # L/h, CL/F
NORFLUOXETINE_V1  <- 1.52   # L, V/F

#' Norfluoxetine pharmacokinetics
#'
#' Fluoxetine's active metabolite.  Has no dosing unit of its own.  See the
#' file header.
#'
#' @inheritParams fluoxetine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
norfluoxetine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Identical to fluoxetine's call; see the header.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = NORFLUOXETINE_V1 * size$volume,
    v2  = 1,                                                    # one compartment
    v3  = 1,
    cl1 = NORFLUOXETINE_CL1 / MINS_PER_HOUR * size$clearance,   # L/min
    cl2 = 0,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = 0,           # no effect-site model
      MEAC = 0,            # not an opioid
      typical = 0,         # the AGNP range is for the sum; no band
      upperTypical = 0,
      lowerTypical = 0,
      reference = FLUOXETINE_REFERENCE
    )
  )
}
