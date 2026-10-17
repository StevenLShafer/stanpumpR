# -----------------------------------------------------------------------------
# Subcutaneous dosing: the SC route, and zero-order subcutaneous input
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, for
# the antidiabetic library (inst/help/models/antidiabetics.md).  Tests:
# tests/testthat/test-subcutaneous.R.
#
# THE ROUTE
# =========
# A subcutaneous dose ("mg SC", "U SC") enters a tissue depot and is absorbed
# from it first-order, which is what a regional anaesthetic injection ("mg RA")
# already does.  Rather than a seventh set of hand-written absorption states
# in every engine, an SC drug writes its absorption as ka_SC,
# bioavailability_SC and tlag_SC (and optionally ka_SC_slow,
# fraction_SC_slow and tlag_SC_slow for a second, slower depot), and
# getDrugPK() moves them onto the tissue-depot fields the engines already
# carry (ka_RA and the rest).  simCpCe() then flags an SC row as it flags an
# RA row, and an SC rate ("U/hr SC", an insulin pump) as an RA rate into the
# depot, which advanceClosedFormPO_IM_IN() advances exactly.  A drug may offer
# SC or RA, never both, and getDrugPK() refuses a model that declares both.
#
# ZERO-ORDER INPUT
# ================
# Two of the published subcutaneous fits do not absorb every dose first-order:
#
#   * "central": a share of each dose enters the circulation at a constant
#     rate over a fixed time, beside the first-order depot (Tham 2017 for
#     insulin lispro and insulin glargine: "both 0- and first-order
#     absorption").  getDrugPK() gives the depot the rest of the dose; the
#     zero-order share is added here as an intravenous infusion of
#     F x share x D / duration from the dose time for `duration` minutes.
#   * "depot": the whole dose is released into the depot at a constant rate
#     over a fixed time, and absorbed from it first-order (sequential zero- and
#     first-order absorption, Watson 2010 for liraglutide).  Added here as a
#     rate into the depot, D / duration, for `duration` minutes.
#
# Both are exact: every dose's zero-order input is a box, the boxes of all the
# doses add to one piecewise-constant rate, and each engine carries a
# piecewise-constant rate exactly.  The one approximation is a pump rate on a
# drug with a "central" zero-order share (insulin lispro in a pump): its
# zero-order share is the box of each ZERO_ORDER_PUMP_STEP-minute portion of
# the pump's delivery, given at the middle of the portion, which a continuous
# rate would smooth into a ramp.  Its first-order share is exact, a rate into
# the depot.
# -----------------------------------------------------------------------------

# Minutes of pump delivery whose zero-order share is given as one box; see
# the header.  Small beside the shortest zero-order duration in the library
# (insulin lispro, 63.6 minutes).
ZERO_ORDER_PUMP_STEP <- 2

ZERO_ORDER_TARGETS <- c("central", "depot")

# The internal Units label of the rows expandZeroOrderSC() adds.  Never shown
# to the user, never in a dose table they edit.
ZERO_ORDER_UNITS <- "zero-order input"

#' Move a PK set's subcutaneous absorption onto the tissue-depot fields
#'
#' @param pkSet one PK set from a drug model
#' @param drug the drug's name, for the error message
#' @returns `pkSet` with `ka_SC`, `bioavailability_SC`, `tlag_SC`,
#'   `ka_SC_slow`, `fraction_SC_slow` and `tlag_SC_slow` renamed to their
#'   `_RA` equivalents; unchanged for a set with no SC fields
#' @keywords internal
subcutaneousToTissueDepot <- function(pkSet, drug)
{
  sc <- grep("_SC(_slow)?$", names(pkSet), value = TRUE)
  if (length(sc) == 0) return(pkSet)
  if (any(grepl("_RA(_slow)?$", names(pkSet))))
    stop("Invalid model for ", drug, ": a drug may be absorbed subcutaneously ",
         "or by regional anesthesia, not both (they share the tissue depot).")
  names(pkSet)[match(sc, names(pkSet))] <- sub("_SC", "_RA", sc)
  pkSet
}

#' Which tissue route a drug model's PK sets use
#'
#' @param pkSets the `PK` list a drug model returns
#' @returns `ROUTE_SC` when any set declares subcutaneous absorption, else
#'   `ROUTE_RA`
#' @keywords internal
modelTissueRoute <- function(pkSets)
{
  sc <- vapply(pkSets, function(s) any(grepl("_SC(_slow)?$", names(s))), logical(1))
  if (any(sc)) ROUTE_SC else ROUTE_RA
}

#' Check a drug model's zero-order subcutaneous input block
#'
#' @param block the model's `zeroOrderSC`: `list(fraction, duration, into)`,
#'   `fraction` the share of each dose given zero-order (1 for "depot"),
#'   `duration` in minutes, `into` one of `ZERO_ORDER_TARGETS`; or NULL
#' @param pkSets the drug's PK sets, already moved onto the tissue depot
#' @param drug the drug's name, for the error message
#' @returns `block`, unchanged, if it is valid; otherwise an error
#' @keywords internal
validateZeroOrderSC <- function(block, pkSets, drug)
{
  if (is.null(block)) return(NULL)
  bad <- function(why) stop("Invalid zeroOrderSC for ", drug, ": ", why)
  if (!is.list(block) || length(block$into) != 1 || !block$into %in% ZERO_ORDER_TARGETS)
    bad(paste0("into must be one of ", paste(ZERO_ORDER_TARGETS, collapse = ", "), "."))
  if (!is_valid_number(block$duration) || block$duration <= 0)
    bad("needs a positive duration in minutes.")
  if (!is_valid_number(block$fraction, 0, 1) || block$fraction <= 0)
    bad("needs a fraction above 0 and at most 1.")
  if (block$into == "depot" && block$fraction != 1)
    bad("a release into the depot takes the whole dose (fraction 1).")
  if (block$into == "depot" && !is.null(pkSets[[1]]$ka_RA_slow))
    bad("a release into the depot cannot feed a second, slow depot.")
  if (length(pkSets) != 1)
    bad("a drug with zero-order input cannot switch kinetics on a clinical event.")
  if (!isTRUE(pkSets[[1]]$ka_RA > 0))
    bad("the drug must be absorbed subcutaneously (ka_SC).")
  block
}

#' Add the zero-order share of a drug's subcutaneous doses to its dose table
#'
#' Called by `simCpCe()` once the doses are in the engine's base units (rates
#' per minute) and flagged by route.  See the header of this file.
#'
#' @param dose the flagged dose table: SC doses are the `RA` rows, pump rates
#'   the `RArate` rows
#' @param block the drug's `zeroOrderSC` block as `getDrugPK()` returns it,
#'   carrying `share`, the fraction of each dose the boxes deliver; or NULL
#' @param maximum end of the simulation, minutes
#' @returns the dose table with the zero-order rates added (and, for
#'   "depot", the SC doses replaced by them)
#' @keywords internal
expandZeroOrderSC <- function(dose, block, maximum)
{
  if (is.null(block) || !(any(dose$RA) || any(dose$RArate))) return(dose)
  duration <- block$duration
  pump <- dose[dose$RArate, , drop = FALSE]
  pump <- pump[order(pump$Time), , drop = FALSE]
  routeFlags <- intersect(c("Bolus", "PO", "IM", "IN", "SL", "RA", "RAslow",
                            "RArate", "PO2"), names(dose))

  # Each dose's box: its start and the amount it spreads over `duration`
  start  <- dose$Time[dose$RA]
  amount <- dose$Dose[dose$RA] * block$share
  if (nrow(pump) > 0)
  {
    if (block$into == "depot")
      stop("A pump rate cannot be given with a sequential zero-order release ",
           "into the depot: the model was fitted to injections.")
    # The pump's running rate, from each rate row to the next, cut at the end
    # of the window, delivered in steps of ZERO_ORDER_PUMP_STEP minutes.
    ends <- c(pump$Time[-1], maximum)
    for (i in seq_len(nrow(pump)))
    {
      if (pump$Dose[i] <= 0 || ends[i] <= pump$Time[i]) next
      edges <- unique(c(seq(pump$Time[i], ends[i], by = ZERO_ORDER_PUMP_STEP), ends[i]))
      width <- diff(edges)
      start  <- c(start, edges[-length(edges)] + width / 2)
      amount <- c(amount, pump$Dose[i] * width * block$share)
    }
  }
  keep <- amount > 0 & start < maximum
  start <- start[keep]
  amount <- amount[keep]

  template <- dose[1, , drop = FALSE]   # the shape of the rows added below
  if (block$into == "central")
  {
    # The zero-order share enters as an intravenous rate, which must not be
    # confused with a running infusion of the drug's own.
    flagged <- Reduce(`|`, lapply(routeFlags, function(f) dose[[f]]))
    if (any(!flagged))
      stop("This drug cannot be given as an intravenous infusion: its ",
           "subcutaneous model delivers part of each dose as one.")
  } else {
    # The whole dose is released into the depot: the SC rows are replaced.
    dose <- dose[!dose$RA, , drop = FALSE]
  }
  if (length(start) == 0) return(dose)

  # The boxes add to one piecewise-constant rate
  stop_ <- start + duration
  breaks <- sort(unique(c(start, stop_)))
  breaks <- breaks[breaks < maximum]
  rate <- vapply(breaks, function(t) sum(amount[start <= t & t < stop_]) / duration,
                 numeric(1))
  rows <- template[rep(1, length(breaks)), , drop = FALSE]
  rows$Time <- breaks
  rows$Dose <- rate
  rows$Units <- ZERO_ORDER_UNITS
  for (f in routeFlags) rows[[f]] <- FALSE
  if (block$into == "depot") rows$RArate <- TRUE
  out <- rbind(dose, rows)
  out <- out[order(out$Time), , drop = FALSE]
  rownames(out) <- NULL
  out
}
