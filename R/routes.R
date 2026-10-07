# The route of a dose is not stored separately: it is the suffix of its Units
# string ("mg PO", "mg/kg IM", "mg IN").  Units with no route suffix (bolus and
# infusion units, the TCI targets, the gases' L/min and %) are intravenous, or
# for the gases, treated with them.

#' Route of administration implied by dose units
#'
#' @param units Character vector of dose units, as in the dose table.
#' @returns Character vector the same length as `units`: one of `DOSE_ROUTES`
#'   (`"IV"`, `"PO"`, `"IM"`, `"IN"`).
#' @noRd
doseRoute <- function(units) {
  units <- as.character(units)
  route <- rep(ROUTE_IV, length(units))
  suffix <- sub("^.* ", "", units)
  extravascular <- grepl(" ", units) & suffix %in% DOSE_ROUTES
  route[extravascular] <- suffix[extravascular]
  route
}

#' Order a drug's units by route, keeping the order within each route
#'
#' @param units Character vector of one drug's units.
#' @returns `units`, reordered IV, PO, IM, IN.
#' @noRd
groupUnitsByRoute <- function(units) {
  units[order(match(doseRoute(units), DOSE_ROUTES))]
}
