# Doses per kilogram and per square metre of body surface area.
#
# A dose unit names its body-size basis after the amount: "mg/kg",
# "mcg/kg/min" and "U/kg" are per kilogram of the patient's weight, "mg/m2",
# "mg/m2/hr" and "mg/m2 PO" per square metre of body surface area, the basis
# on which most chemotherapy is labelled.  simCpCe() and the Visualizer turn
# both into an absolute dose with bodySizeDoseFactor(), so the two cannot
# disagree.
#
# Body surface area is Mosteller's, sqrt(weight x height / 3600) (Mosteller
# RD, N Engl J Med 1987;317:1098), from the patient's own weight and height,
# as an oncology pharmacy computes it.  The FDA labels of the library's
# chemotherapy drugs dose per m2 without naming a formula; Mosteller is the
# one most order sets use, and agrees with Du Bois's to within a few percent
# in adults.  The choice is recorded as a [decision] for Shafer.  (Claude
# Code, 2026-11-23, at the request of Steven L. Shafer.)

#' Is a dose unit per square metre of body surface area?
#'
#' @param units Character vector of dose units.
#' @returns Logical vector the same length as `units`.
#' @noRd
isBsaUnit <- function(units)
{
  grepl("/m2( |/|$)", as.character(units))
}

#' Is a dose unit per kilogram of body weight?
#'
#' @param units Character vector of dose units.
#' @returns Logical vector the same length as `units`.
#' @noRd
isPerKgUnit <- function(units)
{
  grepl("/kg( |/|$)", as.character(units))
}

#' Body surface area for dosing, m2
#'
#' @param weight Patient weight, kg.
#' @param height Patient height, cm.
#' @returns Mosteller body surface area, m2.
#' @noRd
doseBsa <- function(weight, height)
{
  bsaMosteller(weight, height)
}

#' The multiplier that turns a dose in its units into an absolute dose
#'
#' The weight for a per-kilogram unit, the body surface area for a per-m2
#' unit, and 1 for every other unit.
#'
#' @param units Character vector of dose units.
#' @param weight Patient weight, kg.
#' @param height Patient height, cm (needed only for a per-m2 unit).
#' @returns Numeric vector the same length as `units`.
#' @noRd
bodySizeDoseFactor <- function(units, weight, height = NULL)
{
  units <- as.character(units)
  f <- rep(1, length(units))
  kg <- isPerKgUnit(units)
  f[kg] <- weight
  m2 <- isBsaUnit(units)
  if (any(m2))
  {
    if (is.null(height) || length(height) != 1 || !is.finite(height) || height <= 0)
      stop("A dose per m2 needs the patient's height to compute body surface area.")
    f[m2] <- doseBsa(weight, height)
  }
  f
}
