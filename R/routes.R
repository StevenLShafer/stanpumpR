# The route of a dose is not stored separately: it is a word in its Units
# string ("mg PO", "mg/kg IM", "mg IN").  Units with no route word (bolus and
# infusion units, the TCI targets, the gases' L/min and %) are intravenous, or
# for the gases, treated with them.
#
# The route is matched as a whole word anywhere after the first, not only as
# the last word, so that a unit carrying a further qualifier after the route
# (a dosing frequency, "mg PO bid") still reads as its route.

#' Route of administration implied by dose units
#'
#' @param units Character vector of dose units, as in the dose table.
#' @returns Character vector the same length as `units`: one of `DOSE_ROUTES`
#'   (`"IV"`, `"PO"`, `"IM"`, `"IN"`).
#' @noRd
doseRoute <- function(units) {
  units <- as.character(units)
  route <- rep(ROUTE_IV, length(units))
  for (r in setdiff(DOSE_ROUTES, ROUTE_IV)) {
    route[grepl(paste0(" ", r, "( |$)"), units)] <- r
  }
  route
}

#' Is a dose unit an input rate rather than an amount?
#'
#' A rate -- mass per minute, hour or day, with or without per kg -- sets a
#' running input from its time until the next rate row for the drug, whatever
#' its route: the intravenous infusions ("mg/hr", "mcg/kg/min") and the
#' constant-rate oral input ("mg/day PO", `poRateUnits`).  Matched on the
#' "/min", "/hr" or "/day" word, so that a route or other qualifier may follow
#' it.  Every unit offered before the oral rate arrived classifies exactly as
#' the substring tests simCpCe() used ("min" or "hr" anywhere) did, which
#' test-routes.R checks unit by unit.  (Claude Code, 2026-10-07, at the
#' request of Steven L. Shafer.)
#'
#' @param units Character vector of dose units, as in the dose table.
#' @returns Logical vector the same length as `units`.
#' @noRd
isRateUnit <- function(units) {
  grepl("/(min|hr|day)( |$)", as.character(units))
}

#' Order a drug's units by route, keeping the order within each route
#'
#' @param units Character vector of one drug's units.
#' @returns `units`, reordered IV, PO, IM, IN.
#' @noRd
groupUnitsByRoute <- function(units) {
  units[order(match(doseRoute(units), DOSE_ROUTES))]
}

#' Fraction of an oral dose absorbed when absorption saturates
#'
#' Some drugs are absorbed by a carrier that saturates, so the fraction of an
#' oral dose that reaches the circulation falls as the dose rises.  Gabapentin,
#' carried by the L-amino acid transporter, is the case in the library.  Such a
#' drug returns an `oralSaturation` block, and every oral dose is scaled by
#'
#'     1 - Imax * D / (ID50 + D)
#'
#' with D the dose in mg per administration.  That is the inhibitory Emax form
#' of Tran et al. (J Pharmacokinet Pharmacodyn 2017;44:567-579) and contains
#' the hyperbolic Dmax / (D50 + D) as the case Imax = 1.  The drug's
#' `bioavailability_PO` stays the fraction absorbed in the limit of a small
#' dose; the product of the two is the bioavailability of a given dose.
#'
#' Each dose is scaled once, by its own size, and is then an independent input
#' to the linear engines, so superposition still holds.  What this cannot
#' represent is saturation shared between doses: two doses taken together are
#' scaled separately, not as their sum, and overlapping absorption from doses
#' close in time does not compete.
#'
#' @param doseMg oral doses in mg per administration
#' @param saturation `list(Imax, ID50)`, ID50 in mg, or NULL for none
#' @returns the fraction of each dose absorbed, relative to `bioavailability_PO`
#' @keywords internal
oralSaturationFraction <- function(doseMg, saturation)
{
  if (is.null(saturation)) return(rep(1, length(doseMg)))
  1 - saturation$Imax * doseMg / (saturation$ID50 + doseMg)
}

#' Check a drug model's saturable absorption block
#'
#' The same check serves the oral block (`oralSaturation`, gabapentin) and the
#' sublingual one (`sublingualSaturation`, buprenorphine), which have the same
#' form and are applied by the same `oralSaturationFraction()`.
#'
#' @param saturation the block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @param block the block's name, for the error message
#' @returns `saturation`, unchanged, if it is valid; otherwise an error
#' @keywords internal
validateOralSaturation <- function(saturation, drug, block = "oralSaturation")
{
  if (is.null(saturation)) return(NULL)
  ok <- is.list(saturation) &&
    is_valid_number(saturation$Imax, 0, 1) &&
    is_valid_number(saturation$ID50) && saturation$ID50 > 0
  # Imax above 1 would make the fraction absorbed negative at large doses.
  if (!ok)
    stop("Invalid ", block, " for ", drug, ": needs Imax between 0 and 1 ",
         "and a positive ID50 in mg.")
  saturation
}
