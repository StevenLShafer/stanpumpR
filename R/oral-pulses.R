# Pulsed oral release: an extended-release product given as fixed fractions
# released at fixed delays (Adderall XR's two bead populations)
#
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer;
# tests/testthat/test-oral-pulses.R.
#
# A drug model may return an `oralPulses` block naming one or more of
# ORAL_FORMULATIONS, or the reserved ORAL_PULSE_DEFAULT_FORM:
#
#     oralPulses = list(XR = list(fraction = c(0.5, 0.5), delay = c(0, 240)))
#
# Every dose of that formulation ("mg PO XR", and each repeat of
# "mg PO XR qd") is then given as ordinary oral doses: fraction[i] of it at
# its own time plus delay[i] minutes, each absorbed with the drug's default
# oral absorption (ka_PO, bioavailability_PO, tlag_PO).  The fractions sum to
# one, so the amount given is unchanged; only its timing is.
#
# This is a description of release, not of absorption: each pulse is absorbed
# exactly as an immediate-release dose would be.  It suits a product designed
# and shown to be bioequivalent to its immediate-release form given in split
# doses (Adderall XR 20 mg against Adderall 10 mg twice, 4 h apart; Tulloch
# 2002 and the Adderall XR label).  It does not describe a continuous release
# (an osmotic pump), which would need a release-rate input instead.
#
# THE DEFAULT ORAL FORM
# =====================
# `oralPulses = list(default = ...)` (ORAL_PULSE_DEFAULT_FORM) applies to
# every plain oral row -- "mg PO", "mg/kg PO" and their scheduled repeats,
# which carry no formulation word -- and leaves the unit exactly as entered.
#
# That is a different use of the same machinery, and worth being clear about.
# For a named formulation the pulses are a MODEL OF THE PRODUCT: the beads
# really do release in two waves, and the unit says which product was given.
# For the default form they are a DISCRETISATION: the drug is an ordinary
# immediate-release tablet whose published input is a shape the engine cannot
# represent, and the pulses stand in for it.  Hydrochlorothiazide (Van Wart
# 2013) is the case -- its absorption is a Weibull with a shape of 2.84, so
# the depot's rate constant rises with time instead of being constant, and
# eight lagged fractions reproduce the exact system to 0.34% of the peak over
# the whole curve and 0.006% at the peak itself, against the 0.53% and 0.01%
# MDMA's zero-order depot met (docs/antihypertensives.md section 12).
#
# MEASURE THAT ERROR AT THE PULSE TIMES.  It peaks exactly where a lagged
# depot starts and the approximation kinks, and simulationTimeGrid() puts a
# point at every dose time, so that is what a user sees.  A uniform grid steps
# over the kinks and flatters the approximation: on hydrochlorothiazide a
# 0.02 h grid said 0.45% where the truth was 0.64%.
#
# A drug using it this way owes the reader two things, in its own header: the
# published input it is standing in for, and the measured error of standing in
# for it.  Pulses fitted to a shape nobody published would be an invention
# dressed as a product.
#
# A block may not name the default form and a formulation at once.  The two
# would be read off the same dose rows -- an "mg PO XR" row is also an oral
# row -- so which pulses applied would depend on the order the block was
# written in, and a drug with several oral products is not a drug whose
# default input needs discretising.
#
# The expansion runs in simCpCe() after the scheduled doses have been expanded
# and before the doses are converted to base units, so the engines, the
# formulation runs and doseRoute() only ever see ordinary "mg PO" rows.  A
# pulse whose time falls at or after the end of the plot is dropped, the rule
# simCpCe() applies to every dose.  The pulses are not returned for export:
# the exported dose table shows the XR doses as entered, which is what was
# given.

#' Check a drug model's oralPulses block
#'
#' @param pulses the block, or NULL
#' @param drug the drug's name, for the error message
#' @returns the block, unchanged, or NULL
#' @noRd
validateOralPulses <- function(pulses, drug)
{
  if (is.null(pulses)) return(NULL)
  if (!is.list(pulses) || length(pulses) == 0 || is.null(names(pulses)))
    stop("Invalid oralPulses for ", drug, ": a named list, one entry per formulation.")
  if (ORAL_PULSE_DEFAULT_FORM %in% names(pulses) && length(pulses) > 1)
    stop("Invalid oralPulses for ", drug, ": '", ORAL_PULSE_DEFAULT_FORM,
         "' is the plain oral row, which a formulation row also is, so it ",
         "cannot be declared alongside one.")
  for (f in names(pulses)) {
    if (!f %in% c(ORAL_FORMULATIONS, ORAL_PULSE_DEFAULT_FORM))
      stop("Invalid oralPulses for ", drug, ": '", f, "' is not one of ",
           paste(c(ORAL_FORMULATIONS, ORAL_PULSE_DEFAULT_FORM), collapse = ", "))
    p <- pulses[[f]]
    ok <- is.list(p) && is.numeric(p$fraction) && is.numeric(p$delay) &&
      length(p$fraction) >= 1 && length(p$fraction) == length(p$delay) &&
      all(is.finite(p$fraction)) && all(p$fraction > 0) &&
      all(is.finite(p$delay)) && all(p$delay >= 0) &&
      isTRUE(all.equal(sum(p$fraction), 1))
    if (!ok)
      stop("Invalid oralPulses for ", drug, ": the ", f, " needs positive ",
           "fractions summing to 1 and as many delays (minutes, >= 0).")
  }
  pulses
}

#' Give each pulsed-formulation dose as its pulses
#'
#' @param dose one drug's dose rows, in the user's units, numeric Time in
#'   minutes, with scheduled doses already expanded
#' @param pulses the drug's validated oralPulses block, or NULL
#' @param maximum end of the simulation (min); no pulse is given at or after it
#' @returns the dose rows, each pulsed row replaced by one plain oral row per
#'   pulse ("mg PO XR" becomes "mg PO"; a plain oral row keeps its own unit),
#'   ordered by time
#' @noRd
expandOralPulses <- function(dose, pulses, maximum)
{
  if (is.null(pulses) || nrow(dose) == 0) return(dose)
  formulation <- doseFormulation(dose$Units)
  if (ORAL_PULSE_DEFAULT_FORM %in% names(pulses)) {
    # Every oral row that names no formulation, EXCEPT a standing oral rate
    # ("mg/day PO", poRateUnits).  The pulses describe how one dose is
    # released, and a constant rate has no release to discretise: it is
    # already a continuous input on the apparent oral parameters, classified
    # as an infusion by simCpCe().  Splitting it into fractions at delays
    # would turn one rate into several that start at different times, which
    # is not what it means.  It is skipped rather than refused, because the
    # pulses emit amount rows and so coexist with a rate row correctly --
    # unlike the zero-order release, whose terminating zero would replace it
    # (R/oral-zero-order.R).  The validator has already refused a block that
    # names the default form and a formulation at once.
    pulsed <- is.na(formulation) & doseRoute(dose$Units) == ROUTE_PO &
      !isRateUnit(dose$Units)
    formulation[pulsed] <- ORAL_PULSE_DEFAULT_FORM
  } else {
    pulsed <- !is.na(formulation) & formulation %in% names(pulses)
  }
  if (!any(pulsed)) return(dose)

  out <- list(dose[!pulsed, , drop = FALSE])
  for (row in which(pulsed)) {
    p <- pulses[[formulation[row]]]
    extra <- dose[rep(row, length(p$fraction)), , drop = FALSE]
    extra$Time <- as.numeric(dose$Time[row]) + p$delay
    extra$Dose <- dose$Dose[row] * p$fraction
    # A named formulation's unit becomes the plain oral unit the engines read;
    # the default form is already that unit and is left alone.
    extra$Units <- if (formulation[row] == ORAL_PULSE_DEFAULT_FORM)
      as.character(dose$Units[row])
    else
      sub(paste0(" ", ROUTE_PO, " ", formulation[row], "( |$)"),
          paste0(" ", ROUTE_PO, "\\1"), as.character(dose$Units[row]))
    out[[length(out) + 1]] <- extra[extra$Time < maximum, , drop = FALSE]
  }
  out <- do.call(rbind, out)
  out <- out[order(as.numeric(out$Time)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
