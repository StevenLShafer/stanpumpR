# -----------------------------------------------------------------------------
# The physiology ribbon
# -----------------------------------------------------------------------------
# Steven L. Shafer, 2026-11-24/25 (Gemini's gas-engine audit of 2026-11-23,
# C-01): when the physiology is changed, by the sliders or by events, the
# shaded band is removed from the panel of every drug it changes, and a grey,
# semi-transparent RIBBON is drawn from the drug's curve under nominal
# (normal) physiology to its curve under the physiology given.  There is no
# extra line: the drug keeps its own colours, and the nominal curve is only
# the ribbon's edge.  His reason: a band that stays the same "will appear to
# be a bug because it isn't what the physiology teaches", and a Y axis that
# rescales can hide a change that the ribbon shows within one panel.
#
# The ribbon follows the line the panel draws: the effect-site line where the
# drug has one and it is drawn, the plasma line otherwise.  For an inhaled
# agent that is its brain tension with the default line types, its end-tidal
# tension with the plasma line alone; on the MAC panel, the brain MAC or the
# end-tidal MAC (gasBrainMac()).  The working assumption recorded with the
# decision named the alveolar line for an inhaled agent, before the MAC
# panel's effect-site line became the brain MAC (2026-11-25); following the
# drawn line keeps the ribbon on the curve the user sees.
#
# Drawn only where the two curves differ (a drug the physiology does not
# change keeps its band), never under normalization, never hovered or clicked
# (it is a layer, not a series), and on a log axis from the panel's lowest
# positive value.  The nominal simulation is app_server()'s nominalDrugs().
# -----------------------------------------------------------------------------

#' Relative difference below which a drug's curve is taken as unchanged by
#' its physiology
#' @keywords internal
PHYSIOLOGY_RIBBON_TOLERANCE <- 1e-6

#' Ribbon fill colour and opacity
#' @keywords internal
PHYSIOLOGY_RIBBON_FILL <- "grey40"
#' @rdname PHYSIOLOGY_RIBBON_FILL
#' @keywords internal
PHYSIOLOGY_RIBBON_ALPHA <- 0.3

#' The physiology ribbons of a plot
#'
#' @param data the series simulationPlot() draws (Drug, Time, Y, Site, Wrap),
#'   after its line-type filters and from the window's start
#' @param nominal the drugs at normal physiology: a named list of entries, as
#'   \code{drugs()} returns them, or NULL
#' @param drugs the panel drugs (simulationPlot()'s plotTable$Drug)
#' @param wraps each panel drug's facet value, same length as \code{drugs}
#' @param maximum the end of the plotted window, minutes
#' @returns a data frame of Drug, Wrap, Time, ymin and ymax, one ribbon per
#'   drug whose curve differs from its nominal one; zero rows when none
#' @keywords internal
physiologyRibbons <- function(data, nominal, drugs, wraps, maximum = Inf)
{
  empty <- data.frame(Drug = character(0), Wrap = character(0), Time = numeric(0),
                      ymin = numeric(0), ymax = numeric(0), stringsAsFactors = FALSE)
  if (is.null(nominal) || length(nominal) == 0 || is.null(data) || nrow(data) == 0)
    return(empty)
  out <- list()
  for (d in intersect(names(nominal), drugs)) {
    wrap <- as.character(wraps[match(d, drugs)])
    shown <- data[data$Drug == d & as.character(data$Wrap) == wrap & !is.na(data$Y), ,
                  drop = FALSE]
    site <- if (any(shown$Site == "Effect Site")) "Effect Site" else
      if (any(shown$Site == "Plasma")) "Plasma" else next
    a <- shown[shown$Site == site, c("Time", "Y"), drop = FALSE]
    n <- nominal[[d]]$results
    if (is.null(n)) next
    n <- n[n$Site == site & !is.na(n$Y), c("Time", "Y"), drop = FALSE]
    if (nrow(a) < 2 || nrow(n) < 2) next
    # The drawn curve's own rows, a repeated time (a physiology event's step)
    # included, and the nominal curve's times inside them
    lo <- min(a$Time)
    hi <- min(max(a$Time), maximum)
    a <- a[a$Time <= hi, , drop = FALSE]
    extra <- setdiff(n$Time[n$Time > lo & n$Time < hi], a$Time)
    if (length(extra) > 0) {
      a <- rbind(a, data.frame(Time = extra, Y = seriesStepAt(a$Time, a$Y, extra)))
      a <- a[order(a$Time, seq_len(nrow(a))), , drop = FALSE]
    }
    y0 <- seriesStepAt(n$Time, n$Y, a$Time)
    scale <- max(abs(c(a$Y, y0)), na.rm = TRUE)
    if (!is.finite(scale) || scale == 0 ||
        max(abs(a$Y - y0), na.rm = TRUE) <= PHYSIOLOGY_RIBBON_TOLERANCE * scale) next
    out[[d]] <- data.frame(Drug = d, Wrap = wrap, Time = a$Time,
                           ymin = pmin(a$Y, y0), ymax = pmax(a$Y, y0),
                           stringsAsFactors = FALSE)
  }
  if (length(out) == 0) return(empty)
  r <- do.call(rbind, out)
  rownames(r) <- NULL
  r[!is.na(r$ymin) & !is.na(r$ymax), , drop = FALSE]
}

#' The threshold a panel draws, as steps in time
#'
#' A potent gas's threshold is defined at 37 C and moves with body
#' temperature (gasThresholdForTemperature()), so after a physiology event
#' that changes the temperature the line steps with it (Shafer, 2026-11-25;
#' Gemini's gas-engine audit, C-01), where it stayed at the temperature at
#' time 0 while the time until threshold followed the one in force.  Every
#' other panel's threshold is one value.
#'
#' @param drugs the panel drugs
#' @param endCe each one's threshold at time 0
#' @param steps a named list, by drug, of data frames of Start and endCe
#'   (\code{entry$endCeSteps}), or NULL
#' @param xmin,xmax the window
#' @returns a data frame of Drug, xmin, xmax and endCe
#' @keywords internal
thresholdSteps <- function(drugs, endCe, steps, xmin, xmax)
{
  rows <- lapply(seq_along(drugs), function(i) {
    s <- if (drugs[i] %in% names(steps)) steps[[drugs[i]]]
    if (is.null(s) || nrow(s) == 0)
      return(data.frame(Drug = drugs[i], xmin = xmin, xmax = xmax, endCe = endCe[i],
                        stringsAsFactors = FALSE))
    start <- s$Start
    end <- c(start[-1], Inf)
    keep <- end > xmin & start < xmax
    data.frame(Drug = drugs[i], xmin = pmax(start[keep], xmin),
               xmax = pmin(end[keep], xmax), endCe = s$endCe[keep],
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
