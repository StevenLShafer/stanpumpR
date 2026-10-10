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

#' Oral formulation named by dose units
#'
#' The word after "PO" in a formulation unit ("mg PO liquid", "mg/kg PO tablet
#' bid"); see `ORAL_FORMULATIONS`.
#'
#' @param units Character vector of dose units, as in the dose table.
#' @returns Character vector the same length as `units`: the formulation, or
#'   NA for a unit that names none.
#' @noRd
doseFormulation <- function(units) {
  units <- as.character(units)
  formulation <- rep(NA_character_, length(units))
  for (f in ORAL_FORMULATIONS) {
    formulation[grepl(paste0(" ", ROUTE_PO, " ", f, "( |$)"), units)] <- f
  }
  formulation
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

#' Fraction of an oral dose absorbed when it depends on the size of the dose
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
#' Two further forms are declared with a `form` field (the one above is
#' `form = "saturable"`, the default when the field is absent):
#'
#' * `form = "rising"`, `list(form, D50)`: the fraction is D / (D50 + D), so
#'   bioavailability RISES with the dose towards `bioavailability_PO`, which is
#'   then its maximum.  Sertraline (Alhadab and Brundage 2020, whose single-dose
#'   F(D) is 0.639 D / (15.5 + D)).
#' * `form = "power"`, `list(form, exponent, Dref)`: the fraction is
#'   (D / Dref)^exponent, 1 at the reference dose.  This is how an empirical
#'   power of the dose on apparent clearance, CL/F x (D / Dref)^-exponent, is
#'   carried by a linear engine: the steady-state exposure D / CL(D) is the
#'   same as that of the scaled dose on the reference clearance.  Paroxetine
#'   (Kim 2015).  The fraction may exceed 1 above Dref; it is an exposure
#'   scale on apparent parameters, not a physical bioavailability.
#'
#' Each dose is scaled once, by its own size, and is then an independent input
#' to the linear engines, so superposition still holds.  What this cannot
#' represent is dependence shared between doses: two doses taken together are
#' scaled separately, not as their sum, and overlapping absorption from doses
#' close in time does not compete.
#'
#' @param doseMg oral doses in mg per administration
#' @param saturation the drug's `oralSaturation` block, or NULL for none
#' @returns the fraction of each dose absorbed, relative to `bioavailability_PO`
#' @keywords internal
oralSaturationFraction <- function(doseMg, saturation)
{
  if (is.null(saturation)) return(rep(1, length(doseMg)))
  switch(oralSaturationForm(saturation),
    saturable = 1 - saturation$Imax * doseMg / (saturation$ID50 + doseMg),
    rising    = doseMg / (saturation$D50 + doseMg),
    power     = (doseMg / saturation$Dref)^saturation$exponent
  )
}

#' The form of an `oralSaturation` block: "saturable" unless it says otherwise
#' @keywords internal
oralSaturationForm <- function(saturation)
{
  if (is.null(saturation$form)) ORAL_SATURATION_SATURABLE else saturation$form
}

ORAL_SATURATION_SATURABLE <- "saturable"
ORAL_SATURATION_RISING    <- "rising"
ORAL_SATURATION_POWER     <- "power"
ORAL_SATURATION_FORMS <- c(ORAL_SATURATION_SATURABLE, ORAL_SATURATION_RISING,
                           ORAL_SATURATION_POWER)

#' Check a drug model's dose-dependent oral absorption block
#'
#' @param saturation the `oralSaturation` block a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @returns `saturation`, unchanged, if it is valid; otherwise an error
#' @keywords internal
validateOralSaturation <- function(saturation, drug)
{
  if (is.null(saturation)) return(NULL)
  if (!is.list(saturation) ||
      !isTRUE(oralSaturationForm(saturation) %in% ORAL_SATURATION_FORMS))
    stop("Invalid oralSaturation for ", drug, ": form must be one of ",
         paste(ORAL_SATURATION_FORMS, collapse = ", "), ".")
  form <- oralSaturationForm(saturation)
  if (form == ORAL_SATURATION_SATURABLE) {
    ok <- is_valid_number(saturation$Imax, 0, 1) &&
      is_valid_number(saturation$ID50) && saturation$ID50 > 0
    # Imax above 1 would make the fraction absorbed negative at large doses.
    if (!ok)
      stop("Invalid oralSaturation for ", drug, ": needs Imax between 0 and 1 ",
           "and a positive ID50 in mg.")
  } else if (form == ORAL_SATURATION_RISING) {
    if (!(is_valid_number(saturation$D50) && saturation$D50 > 0))
      stop("Invalid oralSaturation for ", drug, ": the rising form needs a ",
           "positive D50 in mg.")
  } else {
    ok <- is_valid_number(saturation$exponent) &&
      is_valid_number(saturation$Dref) && saturation$Dref > 0
    if (!ok)
      stop("Invalid oralSaturation for ", drug, ": the power form needs an ",
           "exponent and a positive reference dose Dref in mg.")
  }
  saturation
}
