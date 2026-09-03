# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Opus 5), 2026-09-02, at the request of
# Steven L. Shafer, who specified that when morphine is given as well as
# codeine, the morphine plot should show the arithmetic sum.
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
# -----------------------------------------------------------------------------


#' Add a metabolite contribution to a drug's simulated series
#'
#' @param base the existing series for the metabolite drug, a data frame of
#'   \code{Time}, \code{Cp} and \code{Ce}, or NULL when the drug was not given
#'   directly and exists only as a metabolite
#' @param addition the metabolite contribution, same shape
#'
#' @returns a data frame of \code{Time}, \code{Cp} and \code{Ce} on the union of
#'   the two timelines, carrying the arithmetic sum
#' @export
mergeMetaboliteSeries <- function(base, addition)
{
  if (is.null(addition) || nrow(addition) == 0) return(base)
  if (is.null(base) || nrow(base) == 0) return(addition)

  times <- sort(unique(c(base$Time, addition$Time)))

  # rule = 2 holds the end values rather than returning NA.  Both series run to
  # the same simulation end, so this only guards the endpoints against floating
  # point, and never extrapolates a curve into territory it did not cover.
  onto <- function(df, column)
    stats::approx(df$Time, df[[column]], times, rule = 2)$y

  data.frame(
    Time = times,
    Cp   = onto(base, "Cp") + onto(addition, "Cp"),
    Ce   = onto(base, "Ce") + onto(addition, "Ce")
  )
}


#' Fold every metabolite contribution into the drug it is a metabolite of
#'
#' Walks the simulated drug list, and for each drug carrying a metabolite adds
#' that contribution to the metabolite drug's own series, creating the series if
#' the metabolite drug was not given directly.
#'
#' This has to run after every drug has been simulated, because a metabolite
#' contribution crosses from one drug's entry into another's.  It is the same
#' shape of coupling the inhaled gases have: the per-drug independence that
#' processdoseTable() assumes does not hold once drugs feed one another.
#'
#' @param drugs the simulated drug list
#' @returns the drug list with metabolite contributions folded in
#' @export
foldMetabolites <- function(drugs)
{
  if (is.null(drugs) || length(drugs) == 0) return(drugs)

  for (parent in names(drugs))
  {
    contribution <- drugs[[parent]]$metaboliteSeries
    if (is.null(contribution)) next

    target <- drugs[[parent]]$metaboliteName
    if (is.null(target) || !nzchar(target)) next

    drugs[[target]]$series <- mergeMetaboliteSeries(
      drugs[[target]]$series,
      contribution
    )
    # Record where it came from, so the row can say so and so that a reader of
    # the object can tell a formed contribution from a given one.
    drugs[[target]]$formedFrom <- unique(c(drugs[[target]]$formedFrom, parent))
  }

  drugs
}
