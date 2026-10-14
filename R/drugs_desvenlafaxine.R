# -----------------------------------------------------------------------------
# Desvenlafaxine (O-desmethylvenlafaxine, ODV): venlafaxine's active
# metabolite (Wang 2022)
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer.
# Verified by tests/testthat/test-drugs-venlafaxine.R, which covers the pair.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.
#
# Wang 2022, fitted jointly with venlafaxine: CLM/F 22.1 L/h, VM/F 238 L, one
# compartment, half-life 7.5 h; no difference between healthy volunteers and
# patients.  R/drugs_venlafaxine.R has the structure, the K23 derivation, the
# first pass and the molar basis.
#
# These are apparent parameters on venlafaxine's scale (divided by the
# parent's unmeasured bioavailability), so they do NOT describe desvenlafaxine
# taken as a drug (Pristiq), whose own oral kinetics differ.  Desvenlafaxine
# therefore has no dosing unit here, as desethylamiodarone.  Scaled exactly
# as venlafaxine is.
# -----------------------------------------------------------------------------

DESVENLAFAXINE_CL1 <- 22.1   # L/h, CLM/F
DESVENLAFAXINE_V1  <- 238    # L, VM/F

#' Desvenlafaxine (O-desmethylvenlafaxine) pharmacokinetics
#'
#' Venlafaxine's active metabolite, on venlafaxine's apparent scale.  Has no
#' dosing unit of its own.  See the file header.
#'
#' @inheritParams venlafaxine
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
desvenlafaxine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Identical to venlafaxine's call; see the header.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = DESVENLAFAXINE_V1 * size$volume,
    v2  = 1,                                                     # one compartment
    v3  = 1,
    cl1 = DESVENLAFAXINE_CL1 / MINS_PER_HOUR * size$clearance,   # L/min
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
      reference = VENLAFAXINE_REFERENCE
    )
  )
}
