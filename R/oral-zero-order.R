# -----------------------------------------------------------------------------
# Zero-order oral release: a dose delivered at a constant rate over a fitted
# duration, after a lag
# -----------------------------------------------------------------------------
# Drafted by Claude Code, 2026-10-17, at the request of Steven L. Shafer, for
# Hwang 2023's carvedilol model (R/drugs_carvedilol.R), whose absorption the
# first-order extravascular path cannot represent.  Tests:
# tests/testthat/test-oral-zero-order.R.
#
# WHAT A ZERO-ORDER ORAL FIT IS
# =============================
# Some population models describe oral absorption not with a rate constant but
# with a DURATION: the whole dose enters the circulation at a constant rate
# over D1, beginning after a lag.  The absorption profile is a rectangle, not
# a rising-then-falling exponential, and the two are not interchangeable: a
# zero-order input has a flat top and a sharp corner where release ends, which
# is what distinguishes it in a dataset sampled densely during absorption.
#
#     first order   input(t) = F ka D exp(-ka (t - tlag))     for t > tlag
#     zero order    input(t) = D / D1                         on [tlag, tlag + D1)
#
# In APPARENT coordinates -- a fit reporting CL/F and V/F, as these fits almost
# always are -- the input is the full dose over D1 with NO further
# multiplication by F, because F is already inside the apparent volume and
# clearance.  Multiplying again would be the classic double-counting error.
#
# HOW IT IS IMPLEMENTED, AND WHY THERE IS NO NEW SOLVER
# =====================================================
# A constant rate into the central compartment is an infusion, and this
# package already solves infusions in closed form.  It also already carries a
# constant-rate ORAL input: a rate unit with the PO route word ("mg/day PO",
# poRateUnits), which amiodarone uses, is classified as an infusion on the
# drug's apparent oral parameters, with neither an absorption rate constant
# nor a bioavailability applied (see simCpCe()).
#
# So a zero-order oral DOSE is exactly that infusion, of finite length: rate
# Dose/D1 from tlag to tlag + D1, then zero.  expandOralZeroOrder() rewrites
# each such dose into the rate rows that say so, in the same place and the
# same way that expandScheduledDoses() and expandOralPulses() rewrite theirs.
# Nothing in any engine changes, and no closed-form coefficient is added: the
# dose reaches the already-tested infusion path.
#
# A drug declares it as
#
#     oralZeroOrder = list(duration = 22.98, lag = 12.9)    # minutes
#
# duration must be positive; lag defaults to zero.  The block describes the
# drug's DEFAULT oral form, the one dosed as "mg PO".  It carries its own lag
# rather than reusing tlag_PO because tlag_PO belongs to the first-order depot,
# which a zero-order drug does not have.
#
# OVERLAPPING RELEASES ARE SUMMED
# ===============================
# The infusion rate on the time line is a STEP function: simulationTimeGrid()
# sums the rate rows that fall at one instant and then holds the latest value
# until the next row.  So two releases that overlap -- a dosing interval
# shorter than D1 -- cannot be written as two independent pairs of rows; the
# second pair's start would REPLACE the first release rather than add to it,
# silently losing drug.  The expansion therefore computes the TOTAL rate
# active at every breakpoint and emits that, which is correct whether or not
# the releases overlap.  (Carvedilol's D1 is 23 minutes against a daily dose,
# so they never overlap there, but a four-hourly dose of a two-hour release
# would.)
#
# WHAT IT IS NOT COMBINED WITH
# ============================
# getDrugPK() refuses oralZeroOrder together with an active metabolite,
# several oral formulations, saturable oral or sublingual absorption, parallel
# systems, Michaelis-Menten elimination, or an osmotic or molar dose basis.
# Each of those reads or rewrites the oral dose rows itself -- saturable
# absorption scales rows flagged PO, which a rate row is not, so it would
# silently do nothing -- and each needs its own thought about what a
# rectangular input means for it.  None is needed for carvedilol.
# -----------------------------------------------------------------------------

#' Check a drug's zero-order oral release block
#'
#' @param zeroOrder the \code{oralZeroOrder} a drug model returned, or NULL
#' @param drug the drug's name, for the error message
#' @returns the block, with \code{lag} filled in, or NULL when none was
#'   declared
#' @keywords internal
validateOralZeroOrder <- function(zeroOrder, drug)
{
  if (is.null(zeroOrder)) return(NULL)
  if (!is.list(zeroOrder))
    stop("Invalid oralZeroOrder for ", drug, ": a list with duration ",
         "(minutes) and an optional lag (minutes).")
  duration <- zeroOrder$duration
  lag <- if (is.null(zeroOrder$lag)) 0 else zeroOrder$lag
  if (!is_valid_number(duration) || duration <= 0)
    stop("Invalid oralZeroOrder for ", drug, ": duration must be a positive ",
         "number of minutes.")
  if (!is_valid_number(lag) || lag < 0)
    stop("Invalid oralZeroOrder for ", drug, ": lag must be a number of ",
         "minutes, zero or more.")
  list(duration = duration, lag = lag)
}

#' The amount unit of a dose unit, without its route or formulation words
#'
#' "mg PO" gives "mg", "mg/kg PO" gives "mg/kg".  Used to build the rate unit
#' a zero-order release is carried as, so that a per-kilogram oral dose stays
#' per-kilogram.
#'
#' @param units one dose unit
#' @returns the first word of it
#' @keywords internal
doseAmountUnit <- function(units)
{
  sub(" .*$", "", as.character(units))
}

#' Rewrite each zero-order oral dose as the rate rows that deliver it
#'
#' @param dose one drug's dose rows, after the scheduled and pulsed
#'   expansions, still in the user's units
#' @param zeroOrder the drug's validated \code{oralZeroOrder} block, or NULL
#' @param maximum the end of the plot, minutes
#' @returns \code{dose} with every default-oral-form row replaced by the
#'   constant-rate rows of its release
#' @keywords internal
expandOralZeroOrder <- function(dose, zeroOrder, maximum)
{
  if (is.null(zeroOrder) || nrow(dose) == 0) return(dose)
  # The default oral form only: route PO, not already a rate, no formulation
  # word (a formulation has its own release, and getDrugPK() refuses the
  # combination).
  isZero <- doseRoute(dose$Units) == ROUTE_PO & !isRateUnit(dose$Units) &
    is.na(doseFormulation(dose$Units))
  if (!any(isZero)) return(dose)

  duration <- zeroOrder$duration
  amountUnit <- unique(doseAmountUnit(dose$Units[isZero]))
  if (length(amountUnit) != 1)
    stop("A zero-order oral release needs one amount unit at a time, not ",
         paste(amountUnit, collapse = ", "), ".")

  start <- as.numeric(dose$Time[isZero]) + zeroOrder$lag
  stop_ <- start + duration
  rate  <- as.numeric(dose$Dose[isZero]) / duration

  # The total rate active at each breakpoint: sum over the releases that have
  # begun and have not ended.  Written this way so that overlapping releases
  # add instead of replacing one another (see the header).
  breaks <- sort(unique(c(start, stop_)))
  total <- vapply(breaks, function(t) sum(rate[start <= t & t < stop_]),
                  numeric(1))
  # Drop a breakpoint that repeats the rate already running, and any at or
  # after the end of the plot (the rule simCpCe() applies to every dose).
  keep <- c(TRUE, diff(total) != 0) & breaks < maximum
  breaks <- breaks[keep]
  total <- total[keep]

  out <- dose[!isZero, , drop = FALSE]
  if (length(breaks)) {
    # Carried on the first zero-order row, so the Drug column and any other
    # column the caller added travel with it.
    rows <- dose[rep(which(isZero)[1], length(breaks)), , drop = FALSE]
    rows$Time <- breaks
    rows$Dose <- total
    rows$Units <- paste0(amountUnit, "/min ", ROUTE_PO)
    out <- rbind(out, rows)
  }
  out <- out[order(as.numeric(out$Time)), , drop = FALSE]
  rownames(out) <- NULL
  out
}
