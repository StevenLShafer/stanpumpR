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

#' Order a drug's units by route, keeping the order within each route
#'
#' @param units Character vector of one drug's units.
#' @returns `units`, reordered IV, PO, IM, IN.
#' @noRd
groupUnitsByRoute <- function(units) {
  units[order(match(doseRoute(units), DOSE_ROUTES))]
}
