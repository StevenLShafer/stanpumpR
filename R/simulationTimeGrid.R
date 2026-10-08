# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-07, at the request of
# Steven L. Shafer, so that a plot can run for weeks or months ("calibrate
# delta t against duration").  Until then each of the four closed-form engines
# built its own time line and its own dose lines, with the same code repeated
# four times; advanceClosedFormMetabolite()'s header called the refactoring out
# as worth doing on its own.
#
# STATUS: run and verified on R 4.3.3 by tests/testthat/test-simulationTimeGrid.R,
# which pins the legacy grid, checks every knot survives, bounds the step and
# the point count on 52-week plots, compares the values against a dense
# reference, and checks doseLines() against the loop it replaced on random
# dose tables.
# -----------------------------------------------------------------------------
#
# THE TIME LINE EVERY ENGINE SIMULATES ON
# =======================================
# The closed-form engines are exact at whatever times they are asked about, so
# the time line decides only how the curve is DRAWN -- ggplot joins the points
# with straight lines, the equispaced copy the hover readout and the MEAC panel
# read is interpolated from them, the maxima are sampled on them -- and what a
# run costs.  Its knots are set by the doses: every dose time, the instant just
# before a dose (PRE_DOSE_OFFSET), the instant a lagged dose was given, and,
# for advanceClosedForm1(), every event.  advanceStatesOnto() is exact between
# neighbouring points only because the input is constant between them, so a
# knot is never dropped or moved; the grid only adds points between knots.
#
# Up to a day (GRID_LEGACY_MAXIMUM) each gap between knots gets the fill the
# engines always used: GRID_LOG_POINTS offsets in geometric progression from
# `start` (a quarter of the effect-site equilibration half-time, at most a
# minute; gridStart()) towards a day, keeping those that fall inside the gap.
# That puts fine detail after every dose, where the curve bends fastest.  A
# plot of a day or less gets exactly the line it always had, point for point.
#
# Beyond a day that fill fails both ways.  It never reaches past about 20
# hours, so any longer gap was drawn as ONE straight chord: a single oral dose
# on a 52-week plot had 43 points and a straight line across 363 days, and the
# hover readout, the MEAC panel and every trapezoid AUC taken from the series
# read that chord.  And it spends up to 41 points on every gap whatever the
# plot's length, so a dose four times a day for a year cost 52,416 points.
#
# So past a day the fill is scaled to the plot.  After each knot the offsets
# start at
#
#     tau0 = max(start, maximum / GRID_FINE_POINTS)
#
# and grow in the same ratio as before, r = (1440 / start)^(1/41), while each
# step is shorter than
#
#     h = maximum / GRID_UNIFORM_POINTS,
#
# then continue in equal steps of at most h to the end of the gap.  No step
# anywhere on the plot is longer than h, so nothing is drawn as a chord longer
# than 1/GRID_UNIFORM_POINTS of the plot, and the points are bounded: at most
# 2 + log(GRID_FINE_POINTS / (GRID_UNIFORM_POINTS (r - 1))) / log(r) geometric
# offsets per gap (32 at r = 1.194, start = 1), fewer in a short gap, plus at
# most GRID_UNIFORM_POINTS uniform ones over the whole plot.  See
# R/constants.R for how the two counts were chosen.
#
# What the coarser start gives up: a feature that is over within tau0 of a
# knot is not sampled.  On a 52-week plot tau0 is 26 minutes, so every oral
# absorption peak in the library (the earliest, oxycodone's, is at 30 minutes)
# is drawn, but the effect-site peak a few minutes after a bolus of a fast drug
# is not: fentanyl's, 3.6 minutes after the bolus, is drawn at about a quarter
# of its height.  It is far narrower than a pixel there.  The dose instant
# itself is always a point, so a bolus still draws its plasma spike.
# -----------------------------------------------------------------------------


#' The time line a closed-form engine simulates on
#'
#' Sorted unique times from 0 to \code{maximum} (and on to any knot beyond it)
#' that include every knot.  For \code{maximum <= GRID_LEGACY_MAXIMUM} each gap
#' between knots is filled with \code{GRID_LOG_POINTS} geometric offsets from
#' \code{start} towards a day, exactly as the engines always did; beyond that the
#' fill starts at \code{max(start, maximum / GRID_FINE_POINTS)} and no step is
#' longer than \code{maximum / GRID_UNIFORM_POINTS}.  See the header of
#' \code{R/simulationTimeGrid.R}.
#'
#' @param knots times that must be points of the line: the doses, the instant
#'   before each, the instant a lagged dose was given, events.  0 and
#'   \code{maximum} are always added, and anything below 0 is dropped.
#' @param maximum end of the simulation, minutes
#' @param start first offset after each knot, minutes; see \code{gridStart()}
#'
#' @returns sorted unique times, minutes
#' @keywords internal
simulationTimeGrid <- function(knots, maximum, start = 1)
{
  knots <- sort(unique(c(0, knots, maximum)))
  knots <- knots[knots >= 0]
  if (length(knots) < 2) return(knots)

  gapStart <- knots[-length(knots)]
  # diff() is gapEnd - gapStart, the same subtraction the engines' loop did,
  # so the legacy fill below reproduces their line bit for bit.
  distance <- diff(knots)

  if (maximum <= GRID_LEGACY_MAXIMUM)
  {
    # The fill the engines always used, vectorised: every gap's offsets at
    # once rather than one gap at a time, which grew the vector once per knot.
    # The same expression and the same additions give the same doubles, and
    # sort(unique()) does not care in what order they arrive.
    newTimes <- c(exp(log(start) + 0:(GRID_LOG_POINTS - 1) *
                        log(MINS_PER_DAY / start) / GRID_LOG_POINTS))
    gap <- rep(seq_along(distance), each = length(newTimes))
    tau <- rep(newTimes, times = length(distance))
    keep <- tau <= distance[gap]
    return(sort(unique(c(knots, gapStart[gap[keep]] + tau[keep]))))
  }

  h    <- maximum / GRID_UNIFORM_POINTS
  tau0 <- max(start, maximum / GRID_FINE_POINTS)
  r    <- (MINS_PER_DAY / start)^(1 / GRID_LOG_POINTS)

  # The geometric offsets every gap shares: tau0 r^k for as long as the step
  # out of tau0 r^k, tau0 r^k (r - 1), is shorter than h.  The last one is
  # where the uniform steps take over.
  K   <- max(0, ceiling(log(h / ((r - 1) * tau0)) / log(r)))
  geo <- tau0 * r^(0:K)
  tauK <- geo[length(geo)]

  gap  <- rep(seq_along(distance), each = length(geo))
  tau  <- rep(geo, times = length(distance))
  keep <- tau < distance[gap]
  fill <- gapStart[gap[keep]] + tau[keep]

  # Then from tauK to the end of the gap in n equal steps, n the fewest that
  # keeps each at or under h.  Equal steps rather than steps of exactly h, so
  # that the last point never lands a rounding error short of the next knot.
  # Not past the end of the plot: a dose its lag pushes beyond maximum still
  # puts a knot there, but nothing beyond maximum is drawn, and a knot far
  # beyond it must not cost (knot - maximum) / h points.
  long <- which(distance > tauK & gapStart < maximum)
  if (length(long) > 0)
  {
    span <- distance[long] - tauK
    n    <- ceiling(span / h)
    j    <- sequence(n - 1)
    g    <- rep(seq_along(long), n - 1)
    fill <- c(fill, gapStart[long][g] + tauK + j * (span / n)[g])
  }

  sort(unique(c(knots, fill)))
}


#' Where the time line's fill starts after each knot
#'
#' A quarter of the effect-site equilibration half-time, \code{0.693 / ke0 / 4},
#' or a minute if that is longer: the rule each engine used before
#' \code{simulationTimeGrid()} was shared.  A drug with no effect site of its
#' own (\code{ke0} zero) takes \code{fallback}'s instead -- the metabolite
#' engine passes the metabolite's ke0, because a pure prodrug's plotted effect
#' follows its metabolite -- and failing that a minute.
#'
#' @param ke0 effect-site equilibration rate constant, 1/min
#' @param fallback a ke0 to use when \code{ke0} is zero or missing
#'
#' @returns minutes
#' @keywords internal
gridStart <- function(ke0, fallback = NULL)
{
  usable <- function(k) length(k) == 1 && !is.na(k) && k > 0
  k <- if (usable(ke0)) ke0 else fallback
  if (usable(k)) min(0.693 / k / 4, 1) else 1
}


#' The dose inputs at each point of a time line
#'
#' What each engine's per-point loop computed, without the loop: the amount of
#' each kind of point dose landing at each time, and the infusion rate in force
#' over the step INTO each time.  The loop compared every dose with every point,
#' which cost (points x doses) and, with a dose four times a day for a year,
#' most of the run.
#'
#' The rules are the loop's, unchanged:
#' \itemize{
#'   \item a dose lands on the point equal to its time (exact equality, as
#'     \code{dose$Time == timeLine[i]} was); a dose on no point -- before zero,
#'     or past the end of advanceClosedForm1()'s clipped line -- adds nothing;
#'   \item several doses at one point are summed, with \code{sum()}, in table
#'     order, so the doubles are the loop's;
#'   \item an infusion row sets the rate from its time until the next infusion
#'     row, several rows at one time are summed, and a zero-dose row stops it;
#'   \item \code{rate[1]} and \code{dt[1]} are zero, and \code{rate[i]} is the
#'     rate set at or before point \code{i - 1}.
#' }
#' An infusion is any row that is not a bolus and not one of \code{routes}.
#'
#' @param dose the dose table, with \code{Time}, \code{Dose} and \code{Bolus},
#'   and a logical column for each of \code{routes} (a missing one is all
#'   FALSE)
#' @param timeLine the engine's time line
#' @param routes the extravascular routes the engine carries as point inputs,
#'   e.g. \code{c("PO", "IM", "IN")}
#'
#' @returns a list of \code{bolus}, one element per route, \code{infusion} (the
#'   rate set at each point), \code{rate} (the rate over the step into each
#'   point) and \code{dt} (that step's length), each as long as \code{timeLine}
#' @keywords internal
doseLines <- function(dose, timeLine, routes = character(0))
{
  L <- length(timeLine)
  at <- match(dose$Time, timeLine)

  # Sum the doses selected by `use` onto their points.  split() keeps table
  # order within each point, so sum() adds exactly what the loop's
  # sum(dose$Dose[dose$Time == timeLine[i] & ...]) added.  as.numeric() for a
  # hand-built table with whole-number doses, which the loop's assignment into
  # a numeric vector coerced in the same way.
  pointSums <- function(use) {
    use <- use & !is.na(at)
    s <- vapply(split(as.numeric(dose$Dose[use]), at[use]), sum, numeric(1))
    list(at = as.integer(names(s)), sum = unname(s))
  }
  onLine <- function(use) {
    out <- rep(0, L)
    s <- pointSums(use)
    out[s$at] <- s$sum
    out
  }

  isRoute <- lapply(routes, function(r) {
    x <- dose[[r]]
    if (is.null(x)) rep(FALSE, nrow(dose)) else x
  })
  names(isRoute) <- routes

  out <- list(bolus = onLine(dose$Bolus))
  for (r in routes) out[[r]] <- onLine(isRoute[[r]])

  infusion <- !dose$Bolus
  for (r in routes) infusion <- infusion & !isRoute[[r]]
  s <- pointSums(infusion)
  # The rate set at each point is the one set at the last point at or before
  # it that has an infusion row; zero before the first.
  out$infusion <- c(0, s$sum)[findInterval(seq_len(L), s$at) + 1]

  out$rate <- c(0, out$infusion[-L])
  out$dt   <- c(0, diff(timeLine))
  out
}
