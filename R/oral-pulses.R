# Pulsed oral release: an extended-release product given as fixed fractions
# released at fixed delays (Adderall XR's two bead populations)
#
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer;
# tests/testthat/test-oral-pulses.R.
#
# A drug model may return an `oralPulses` block naming one or more of
# ORAL_FORMULATIONS:
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
  for (f in names(pulses)) {
    if (!f %in% ORAL_FORMULATIONS)
      stop("Invalid oralPulses for ", drug, ": '", f, "' is not one of ",
           paste(ORAL_FORMULATIONS, collapse = ", "))
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
#' @returns the dose rows, each pulsed-formulation row replaced by one plain
#'   oral row per pulse ("mg PO XR" becomes "mg PO"), ordered by time
#' @noRd
expandOralPulses <- function(dose, pulses, maximum)
{
  if (is.null(pulses) || nrow(dose) == 0) return(dose)
  formulation <- doseFormulation(dose$Units)
  pulsed <- !is.na(formulation) & formulation %in% names(pulses)
  if (!any(pulsed)) return(dose)

  out <- list(dose[!pulsed, , drop = FALSE])
  for (row in which(pulsed)) {
    p <- pulses[[formulation[row]]]
    extra <- dose[rep(row, length(p$fraction)), , drop = FALSE]
    extra$Time <- as.numeric(dose$Time[row]) + p$delay
    extra$Dose <- dose$Dose[row] * p$fraction
    extra$Units <- sub(paste0(" ", ROUTE_PO, " ", formulation[row], "( |$)"),
                       paste0(" ", ROUTE_PO, "\\1"), as.character(dose$Units[row]))
    out[[length(out) + 1]] <- extra[extra$Time < maximum, , drop = FALSE]
  }
  out <- do.call(rbind, out)
  out <- out[order(as.numeric(out$Time)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
