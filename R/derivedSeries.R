# The time base of the MEAC and interaction panels
#
# Both panels combine several drugs' effect-site curves -- the total opioid is
# a sum, the interaction a function of the total opioid and propofol -- so they
# need one set of times for all of them.  They used to take the 100 equispaced
# points (equiSpace) that simCpCe() keeps for each drug.  Those are 100 minutes
# apart on a one-week plot and step right over a bolus's peak: 100 mcg of
# fentanyl at minute 10 peaks at 2.18 ng/mL at 13.5 minutes, and the nearest
# equispaced points read 0 and 0.22.  Each drug's own simulated times are
# dense where its curve bends (simulationTimeGrid()), so the derived panels use
# the union of those times and the equispaced grid, and read each drug's
# effect site off its full series there.

# Sorted unique times from 0 to `maximum`: the equispaced grid together with
# every simulated time of the drugs in `entries` (each an element of drugs()).
derivedSeriesTimes <- function(entries, maximum)
{
  times <- seq(from = 0, to = maximum, length.out = RESOLUTION)
  for (entry in entries) {
    t <- unique(entry$results$Time)
    times <- c(times, t[t >= 0 & t <= maximum])
  }
  sort(unique(times))
}

# One drug's effect-site concentration at `times`, from its full simulated
# series (long form: Drug, Time, Site, Y).  Zero where it has none, as in the
# equispaced copy: a prodrug's effect is its metabolite's, on that drug's own
# row.
derivedEffectSite <- function(entry, times)
{
  ce <- seriesAt(entry$results, "Effect Site", times)
  if (length(ce) != length(times)) ce <- rep(0, length(times))
  ce[is.na(ce)] <- 0
  pmax(ce, 0)  # rounding can leave -1e-18 where the curve starts
}
