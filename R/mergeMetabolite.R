# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, who specified that when morphine is given as well as
# codeine, the morphine plot should show the arithmetic sum.
# Reworked 2026-10-05 to fold into the pipeline's own series shape, and to
# create the row when the metabolite drug was never given directly.
# -----------------------------------------------------------------------------
#
# Morphine is morphine however it arrived.  A patient given codeine and morphine
# together has ONE morphine concentration, and it is the sum of the directly
# administered and the metabolically formed.
#
# That summation is exact, not an approximation.  Both contributions distribute
# through the same morphine disposition, which is linear, so superposition holds
# for the plasma concentration; and the effect site is a linear operator on the
# plasma concentration, so it superposes too.  Ce of the sum equals the sum of
# the Ce's.
#
# Summing also dissolves a double-counting problem.  Were the metabolite plotted
# as a series of its own, a patient given both drugs would show two morphine
# rows and would be counted twice in the total-opioid MEAC.  With one row there
# is one concentration and one MEAC contribution.
#
# The two contributions arrive on different timelines -- each drug builds its
# own around its own dose times -- so they are interpolated onto the union of
# the two before being added, rather than assumed to share a grid.
#
# EXCEPT JUST BEFORE A DOSE
# -------------------------
# A dose lands at a point of its own drug's line, and the engines put a point
# PRE_DOSE_OFFSET (0.01 minute) before it (simulationTimeGrid()), so that the
# interval ending at the dose is too short to see.  Interpolated INTO that
# interval -- linearly for the concentration, or by advanceStatesOnto() for
# the time until threshold -- the dose is spread back across it.  On one
# drug's own line nothing falls inside, but the union puts the other drug's
# points there.  An external audit found the case (finding F19, 2026-10):
# codeine 30 mg by mouth at 0 and 15 mg at 19.995 minutes, morphine 10 mg IV
# at 20, 70 kg, 170 cm, 40-year-old man.  The union put codeine's 19.995 inside
# morphine's (19.99, 20], and the morphine row read 0.286 mcg/mL there instead
# of 0.000535 -- half the bolus, 0.3 seconds early -- with a time until
# threshold of 197 minutes instead of zero.  So the union keeps no point that
# falls strictly inside another contributor's pre-dose interval
# (metaboliteTimeLine()).  Nothing is lost that the plot could show: such an
# interval is a hundredth of a minute, and both of its ends are kept.
# (Claude Code, 2026-10-09, at the request of Steven L. Shafer.)
#
# RECOVERY IS NOT SUMMED -- IT IS RECOMPUTED
# -----------------------------------------
# Recovery is not a concentration and does not superpose; it is the solution of
# a decay-to-threshold problem, and two drugs' times until threshold do not add.
# So the merged series' Recovery column is not built from the two Recovery
# columns at all.  It is solved again, once, from the STATE underneath them:
# every contribution's effect site is a sum of exponentials, the states DO
# superpose, and the combined effect site is a sum of exponentials over the
# union of the eigenvalue sets, which is exactly what recoveryCalc() takes.
# That is exact, and it is why this does not need the jointly simulated washout
# the inhaled gases use (gasCoupledRecovery() in R/gasRecovery.R): there, uptake
# is coupled through a shared alveolus; here, each contribution distributes
# through its own linear disposition and only the sum matters.
#
# Rewritten 2026-10-05 (Claude Code, Claude Opus 5) at the request of
# Steven L. Shafer.  Until then the receiving drug kept the recovery computed
# from its own doses alone, so a patient given only the parent saw no time at
# all for the opioid they actually had, and a patient given both saw the time
# for the injected part only.  With three pairs in the library -- codeine to
# morphine, hydrocodone to hydromorphone, oxycodone to oxymorphone -- that is
# visible at ordinary doses: 40 mg of oxycodone forms oxymorphone past
# oxymorphone's own threshold, and the row showed nothing.  See
# R/recoveryStates.R for the states, and
# tests/testthat/test-recovery-engines.R for the check against stopping
# delivery in the simulation itself.
#
# When the states are not available -- a caller that assembled a drug list by
# hand, or a run with plotRecovery FALSE -- the receiving drug's own Recovery is
# carried through as before, which is the old behaviour rather than a wrong one.
# -----------------------------------------------------------------------------


#' The time line of a metabolite fold
#'
#' The union of the contributors' own time lines, less any point that falls
#' strictly inside one of them's pre-dose interval: an interval no longer than
#' \code{PRE_DOSE_OFFSET} that ends at a point of that line, which is where the
#' engines put the instant before a dose.  A point there would have the dose
#' interpolated back onto it.  See "Except just before a dose" in the header of
#' \code{R/mergeMetabolite.R}.  An interval of that length that does not end at
#' a dose only loses a point the plot cannot show.
#'
#' @param lines the contributors' time lines, a list of numeric vectors (NULL
#'   elements are skipped)
#'
#' @returns sorted unique times
#' @keywords internal
metaboliteTimeLine <- function(lines)
{
  lines <- Filter(function(x) length(x) > 0, lines)
  times <- sort(unique(unlist(lines)))
  keep <- rep(TRUE, length(times))
  for (own in lines)
  {
    own <- sort(unique(own))
    if (length(own) < 2) next
    i <- findInterval(times, own)
    inner <- i >= 1 & i < length(own)
    j <- i[inner]
    short <- own[j + 1] - own[j] <= PRE_DOSE_OFFSET * (1 + 1e-9)
    strictly <- times[inner] > own[j] & times[inner] < own[j + 1]
    keep[which(inner)[short & strictly]] <- FALSE
  }
  times[keep]
}


#' Add a metabolite contribution to a drug's simulated series
#'
#' @param base the existing wide series for the metabolite drug, a data frame
#'   of \code{Time}, \code{Plasma}, \code{Effect Site} and \code{Recovery}, or
#'   NULL when the drug was not given directly and exists only as a metabolite
#' @param addition the metabolite contribution, a data frame of \code{Time},
#'   \code{Cp} and \code{Ce}
#' @param times the time line to put the sum on; by default the union of the
#'   two, from \code{metaboliteTimeLine()}.  \code{foldMetabolites()} passes
#'   the line of every contributor at once, so that several parents feeding one
#'   drug respect each other's doses.
#'
#' @returns a wide series on \code{times}, carrying the arithmetic sum
#' @export
mergeMetaboliteSeries <- function(base, addition, times = NULL)
{
  if (is.null(addition) || nrow(addition) == 0) return(base)

  if (is.null(times))
    times <- metaboliteTimeLine(list(base$Time, addition$Time))

  # rule = 2 holds the end values rather than returning NA.  Both series run to
  # the same simulation end, so this only guards the endpoints against floating
  # point, and never extrapolates a curve into territory it did not cover.
  # A series with one value (a direct call; a simulation always has many) is
  # held, as rule = 2 would hold it: approx() needs two.
  onto <- function(df, column, keepNA = FALSE) {
    y <- df[[column]]
    if (all(is.na(y))) return(rep(NA_real_, length(times)))
    if (sum(!is.na(y)) == 1) return(rep(y[!is.na(y)], length(times)))
    stats::approx(df$Time, y, times, rule = 2, na.rm = !keepNA)$y
  }

  if (is.null(base) || nrow(base) == 0)
    return(data.frame(
      Time           = times,
      Plasma         = onto(addition, "Cp"),
      `Effect Site`  = onto(addition, "Ce"),
      Recovery       = rep(0, length(times)),
      check.names    = FALSE
    ))

  data.frame(
    Time          = times,
    Plasma        = onto(base, "Plasma")        + onto(addition, "Cp"),
    `Effect Site` = onto(base, "Effect Site")   + onto(addition, "Ce"),
    # Recovery is not summed.  The receiving drug's own column is carried here
    # as a placeholder; foldMetabolites() solves for the combined time until
    # threshold from the underlying states and overwrites it.  See the header.
    # keepNA: a stretch where recovery could not be computed stays missing
    # rather than being interpolated across.  foldMetabolites() normally
    # overwrites this from the combined states anyway; this is the fallback
    # path, where the receiving drug's own column is all there is.
    Recovery      = onto(base, "Recovery", keepNA = TRUE),
    check.names   = FALSE
  )
}


#' Fold every metabolite contribution into the drug it is a metabolite of
#'
#' Walks the simulated drug list, and for each drug carrying a metabolite adds
#' that contribution to the metabolite drug's own series, creating the series if
#' the metabolite drug was not given directly.  The receiving drug's plotted
#' series, equispaced grid and maxima are then recomputed from the sum, and so
#' is its time until threshold -- which cannot be taken from either part,
#' because recovery times do not add.  See the header, and
#' \code{foldedRecovery()}.
#'
#' This has to run after every drug has been simulated, because a metabolite
#' contribution crosses from one drug's entry into another's.  It is the same
#' shape of coupling the inhaled gases have: the per-drug independence that
#' processdoseTable() assumes does not hold once drugs feed one another.
#'
#' @param drugs the simulated drug list
#' @param maximum maximum length of simulation in minutes
#' @param plotRecovery should recovery be kept in the plotted series, and the
#'   receiving drug's time until threshold solved again from the combined
#'   effect site?
#'
#' @returns the drug list with metabolite contributions folded in
#' @export
foldMetabolites <- function(drugs, maximum, plotRecovery = FALSE)
{
  if (is.null(drugs) || length(drugs) == 0) return(drugs)

  # Gather first, apply second.  Several parents can feed one metabolite, and
  # every target has to start from its OWN simulation rather than from
  # whatever a previous fold left behind, or re-running the pipeline would add
  # the same contribution again.
  contributions <- list()
  for (parent in names(drugs))
  {
    contribution <- drugs[[parent]]$metaboliteSeries
    if (is.null(contribution)) next

    target <- drugs[[parent]]$metaboliteName
    if (is.null(target) || !nzchar(target)) next

    # The metabolite drug needs its own PK resolved before its row can be
    # finished.  recalculatePK() arranges that for every metabolite a dosed
    # drug names; if it is somehow missing, skip rather than guess.
    if (is.null(drugs[[target]]) || is.null(drugs[[target]]$MEAC)) next

    contributions[[target]] <- c(contributions[[target]], parent)
  }

  for (target in names(contributions))
  {
    parents <- contributions[[target]]
    # One line for every contributor at once: the receiving drug's own doses
    # and each parent's.  The state sets' lines are the series' lines, but are
    # included so that the time until threshold, carried onto this line by
    # advanceStatesOnto(), is kept out of their pre-dose intervals as well.
    times <- metaboliteTimeLine(c(
      list(drugs[[target]]$wideOwn$Time, drugs[[target]]$recoveryStatesOwn$time),
      lapply(parents, function(p) drugs[[p]]$metaboliteSeries$Time),
      lapply(parents, function(p) drugs[[p]]$metaboliteRecoveryStates$time)
    ))
    merged <- drugs[[target]]$wideOwn
    for (parent in parents)
      merged <- mergeMetaboliteSeries(merged, drugs[[parent]]$metaboliteSeries, times)
    if (is.null(merged)) next

    if (plotRecovery)
    {
      recovery <- foldedRecovery(merged$Time, drugs, target, contributions[[target]])
      if (!is.null(recovery)) merged$Recovery <- recovery
    }

    drugs[[target]]$wide <- merged

    X <- finishDrugSeries(merged, drugs[[target]], maximum, plotRecovery)
    drugs[[target]]$results   <- X$results
    drugs[[target]]$equiSpace <- X$equiSpace
    drugs[[target]]$max       <- X$max

    # Record where it came from, so the row can say so and so that a reader of
    # the object can tell a formed contribution from a given one.
    drugs[[target]]$formedFrom <- contributions[[target]]
  }

  drugs
}


#' Time until threshold for a drug that also receives a metabolite contribution
#'
#' The effect site the patient has is the sum of the effect sites the given and
#' the formed drug produce, so the time until it falls to the threshold has to
#' be solved from the combined state rather than taken from either part.  Every
#' contributing state set is carried onto the merged time line, the amplitudes
#' are concatenated, and \code{recoveryCalc()} is asked once per time point.
#'
#' @param times the merged series' time line
#' @param drugs the simulated drug list
#' @param target the receiving drug's name
#' @param parents the names of the drugs contributing metabolite to it
#'
#' @returns minutes, one per element of \code{times}, or NULL when any
#'   contribution did not carry its states and the caller should fall back on
#'   the receiving drug's own Recovery
#' @keywords internal
foldedRecovery <- function(times, drugs, target, parents)
{
  sets <- list()

  # The receiving drug's own doses, when it was given directly.  A drug with no
  # effect site of its own -- a prodrug that were also a metabolite, or one
  # whose potency has not been supplied yet -- carries no states, and this
  # returns NULL rather than reporting a time for part of the picture.
  if (!is.null(drugs[[target]]$wideOwn))
  {
    own <- drugs[[target]]$recoveryStatesOwn
    if (is.null(own)) return(NULL)
    sets <- c(sets, list(own))
  }

  for (parent in parents)
  {
    formed <- drugs[[parent]]$metaboliteRecoveryStates
    if (is.null(formed)) return(NULL)
    sets <- c(sets, list(formed))
  }
  if (length(sets) == 0) return(NULL)

  combinedRecovery(times, sets, drugs[[target]]$endCe)
}
