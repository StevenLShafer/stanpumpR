# Time until a concentration falls to a target, once delivery stops.
#
# The concentration after delivery stops is a sum of exponentials,
#
#     C(t) = sum(state * exp(-lambda * t)),
#
# and this returns the time at which it comes down through `target` for the
# LAST time, searched over the next 24 hours (or `horizon`).  Zero means it is
# at or below the target now and stays there; the horizon itself means it is
# still above the target at the end of the search -- which includes a target
# of zero, since a decaying exponential never gets there.
#
# The amplitudes need not all be positive.  The effect site lags the plasma, so
# after a bolus its ke0 term is negative and C(t) RISES before it falls.  That
# is why the last downward crossing is the right one: straight after a bolus the
# effect site may still be below the target, but it is about to go above it, and
# "time until threshold" must be the time until it comes back down.
#
# Rewritten by Claude Code (Claude Fable 5.1), 2026-10-05, at the request of
# Steven L. Shafer; verified on R 4.6.1 by tests/testthat/test-recoveryCalc.R
# and, against stopping delivery in the simulation itself, by
# tests/testthat/test-recovery-engines.R.  The earlier version minimised
# (target - C(t))^2 with optimize(), which had two defects:
#
#   1. It returned 0 whenever C(0) was at or below the target, even when C was
#      still rising towards a peak above it -- so in the first minute or two
#      after every bolus, and for the whole absorption phase of an oral dose, it
#      reported no time at all.
#   2. Its error surface is flat wherever C has decayed to nothing, which is
#      most of a 24-hour search for a short-acting drug, and a golden-section
#      search on a flat surface can settle on the far end of the interval
#      instead of the crossing.
#
# The search here brackets the crossing on a fixed grid, fine at first and
# coarser later, and then refines it with uniroot().  A rise above the target
# that begins and ends between two neighbouring grid points would be missed; the
# grid starts at 3 seconds and grows by about 12% a step, which no
# pharmacokinetic transient is fast enough to slip through.

# Search grid, in minutes: zero, then 0.05 to 24 hours in equal ratios.
RECOVERY_GRID <- c(0, exp(seq(log(0.05), log(MINS_PER_DAY), length.out = 90)))

# How far ahead to look.  A day for a drug timed on its effect site: beyond
# that "more than a day" is the answer that matters for recovery.  A week for
# a drug timed on its plasma -- the antibiotics -- whose time above the MIC
# commonly runs past a day (free vancomycin after 1.5 g, for one).  Chosen by
# Steven L. Shafer, 2026-10-07.  See "Which concentration is timed" in
# R/recoveryStates.R.
RECOVERY_HORIZON_EFFECT <- MINS_PER_DAY
RECOVERY_HORIZON_PLASMA <- MINS_PER_WEEK

# The same grid out to another horizon, with the same ratio between steps.
recoveryGrid <- function(horizon)
{
  if (horizon == MINS_PER_DAY) return(RECOVERY_GRID)
  n <- ceiling(90 * log(horizon / 0.05) / log(MINS_PER_DAY / 0.05))
  c(0, exp(seq(log(0.05), log(horizon), length.out = n)))
}

recoveryCalc <-   function(
  state,
  lambda,
  target,
  horizon = MINS_PER_DAY
  )
{
  f <- function(t) sum(state * exp(-lambda * t)) - target
  grid <- recoveryGrid(horizon)

  # Concentration at every grid time in one go: grid x exponentials.
  excess <- as.vector(exp(-outer(grid, lambda)) %*% state) - target
  above <- which(excess > 0)
  if (length(above) == 0) return(0)

  i <- above[length(above)]
  if (i == length(grid)) return(horizon)

  stats::uniroot(f, c(grid[i], grid[i + 1]),
                 f.lower = excess[i], f.upper = excess[i + 1],
                 tol = 0.01)$root
}
