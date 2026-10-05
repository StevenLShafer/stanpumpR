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
# RECOVERY IS NOT SUMMED
# ----------------------
# Recovery is not a concentration and does not superpose; it is the solution of
# a decay-to-threshold problem for one drug's effect-site state.  The receiving
# drug keeps the recovery computed from its own doses, which understates it
# whenever a metabolite is also contributing.  plotRecovery is documented as
# broken and defaults to FALSE, so nothing reads this today, but it is a real
# limitation to fix alongside the rest of recovery.
# -----------------------------------------------------------------------------


#' Add a metabolite contribution to a drug's simulated series
#'
#' @param base the existing wide series for the metabolite drug, a data frame
#'   of \code{Time}, \code{Plasma}, \code{Effect Site} and \code{Recovery}, or
#'   NULL when the drug was not given directly and exists only as a metabolite
#' @param addition the metabolite contribution, a data frame of \code{Time},
#'   \code{Cp} and \code{Ce}
#'
#' @returns a wide series on the union of the two timelines, carrying the
#'   arithmetic sum
#' @export
mergeMetaboliteSeries <- function(base, addition)
{
  if (is.null(addition) || nrow(addition) == 0) return(base)

  if (is.null(base) || nrow(base) == 0)
    return(data.frame(
      Time           = addition$Time,
      Plasma         = addition$Cp,
      `Effect Site`  = addition$Ce,
      Recovery       = rep(0, nrow(addition)),
      check.names    = FALSE
    ))

  times <- sort(unique(c(base$Time, addition$Time)))

  # rule = 2 holds the end values rather than returning NA.  Both series run to
  # the same simulation end, so this only guards the endpoints against floating
  # point, and never extrapolates a curve into territory it did not cover.
  onto <- function(df, column) {
    y <- df[[column]]
    if (all(is.na(y))) return(rep(NA_real_, length(times)))
    stats::approx(df$Time, y, times, rule = 2)$y
  }

  data.frame(
    Time          = times,
    Plasma        = onto(base, "Plasma")        + onto(addition, "Cp"),
    `Effect Site` = onto(base, "Effect Site")   + onto(addition, "Ce"),
    # Recovery belongs to the receiving drug's own doses; see the header.
    Recovery      = onto(base, "Recovery"),
    check.names   = FALSE
  )
}


#' Fold every metabolite contribution into the drug it is a metabolite of
#'
#' Walks the simulated drug list, and for each drug carrying a metabolite adds
#' that contribution to the metabolite drug's own series, creating the series if
#' the metabolite drug was not given directly.  The receiving drug's plotted
#' series, equispaced grid and maxima are then recomputed from the sum.
#'
#' This has to run after every drug has been simulated, because a metabolite
#' contribution crosses from one drug's entry into another's.  It is the same
#' shape of coupling the inhaled gases have: the per-drug independence that
#' processdoseTable() assumes does not hold once drugs feed one another.
#'
#' @param drugs the simulated drug list
#' @param maximum maximum length of simulation in minutes
#' @param plotRecovery should recovery be kept in the plotted series?
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
    merged <- drugs[[target]]$wideOwn
    for (parent in contributions[[target]])
      merged <- mergeMetaboliteSeries(merged, drugs[[parent]]$metaboliteSeries)
    if (is.null(merged)) next

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
