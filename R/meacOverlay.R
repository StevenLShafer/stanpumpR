# Each opioid's % MEAC, drawn on its own panel
#
# Graph Options -> % MEAC draws a thin black line on every opioid panel: that
# opioid's effect-site concentration as a percentage of its MEAC, read against
# labels at the panel's right-hand edge, where Time until threshold puts its
# labels (the two are mutually exclusive).  It answers a question the
# concentration axes cannot: which moiety is carrying the analgesia.  Oral
# hydrocodone is the example that prompted it: the hydromorphone formed from
# it is the larger share of the analgesia once it appears, but at a fifth of
# hydrocodone's MEAC it is a small number on its own axis, and the two panels'
# concentration labels have to be read side by side to see it.
#
# So the scale is SHARED: the highest % MEAC reached by any opioid on the plot
# sets the top of the scale, rounded up to a pretty number, and every opioid
# panel uses that same scale, mapped onto the panel's own data range (its
# lowest to its highest value, before ggplot's 5% expansion).  The panels all
# have the same height, so a given % MEAC is the same height on every panel,
# and the line that rises highest is the opioid doing the most.  Scaled per
# panel instead, every line would reach the top and the labels would carry
# the comparison, which is the squinting this exists to avoid.
#
# The labels therefore do not sit on the left axis's gridlines, as the time
# until threshold's do; each carries a short tick at the right-hand edge.
#
# The series are the ones the % MEAC panel plots (Step B2 of
# simulationPlot()), read on the same dense time base (R/derivedSeries.R), so
# a peak between two equispaced points is not stepped over.  (Claude Code,
# 2026-10-18, at the request of Steven L. Shafer.)

#' Breaks for the shared % MEAC scale
#'
#' @param peak the highest % MEAC reached by any opioid on the plot
#' @return increasing breaks from 0, the last of them at or above \code{peak};
#'   NULL when there is nothing to scale (no opioid reaches its effect site)
#' @noRd
meacOverlayBreaks <- function(peak)
{
  if (length(peak) != 1 || !is.finite(peak) || peak <= 0) return(NULL)
  breaks <- pretty(c(0, peak), n = 4)
  breaks[breaks >= 0]
}

#' Add the % MEAC overlay to a faceted simulation plot
#'
#' @param plotObject the plot, faceted by Wrap, with every panel built
#' @param resultsMEAC each opioid's % MEAC (Drug, Time, Y), from Step B2
#' @param plotTable the panels (Drug, Wrap), in their final order
#' @param wrapFactors the panel factor levels
#' @param minimum,maximum the time axis
#' @return the plot with the lines, ticks and labels added; unchanged when no
#'   opioid reaches its effect site
#' @noRd
addMEACOverlay <- function(plotObject, resultsMEAC, plotTable, wrapFactors,
                           minimum, maximum)
{
  if (is.null(resultsMEAC) || nrow(resultsMEAC) == 0) return(plotObject)
  breaks <- meacOverlayBreaks(max(resultsMEAC$Y, na.rm = TRUE))
  if (is.null(breaks)) return(plotObject)
  top <- max(breaks)

  built <- ggplot2::ggplot_build(plotObject)
  layout <- built$layout$layout
  tick <- 0.015 * (maximum - minimum)

  lines <- NULL
  labels <- NULL
  panelDrug <- as.character(plotTable$Drug)
  panelWrap <- as.character(plotTable$Wrap)
  for (drug in unique(as.character(resultsMEAC$Drug)))
  {
    wrap <- panelWrap[panelDrug == drug][1]
    k <- which(as.character(layout$Wrap) == wrap)
    if (is.na(wrap) || length(k) != 1) next
    limits <- built$layout$panel_scales_y[[layout$SCALE_Y[k]]]$get_limits()
    lo <- limits[1]
    hi <- limits[2]
    if (!all(is.finite(limits)) || hi <= lo) next

    series <- resultsMEAC[resultsMEAC$Drug == drug, ]
    lines <- rbind(lines, data.frame(
      Drug = drug,
      Time = series$Time,
      Y = lo + (hi - lo) * series$Y / top,
      Wrap = wrap
    ))
    text <- paste0(breaks, "%")
    text[length(text)] <- paste(text[length(text)], "MEAC")
    labels <- rbind(labels, data.frame(
      y = lo + (hi - lo) * breaks / top,
      label = text,
      Wrap = wrap
    ))
  }
  if (is.null(lines)) return(plotObject)

  lines$Wrap  <- factor(lines$Wrap,  levels = wrapFactors, ordered = TRUE)
  labels$Wrap <- factor(labels$Wrap, levels = wrapFactors, ordered = TRUE)

  plotObject +
    ggplot2::geom_segment(
      data = labels,
      mapping = ggplot2::aes(x = maximum - tick, xend = maximum, y = y, yend = y),
      color = "black",
      linewidth = 0.5,
      inherit.aes = FALSE,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = labels,
      mapping = ggplot2::aes(x = maximum - tick, y = y, label = label),
      color = "black",
      inherit.aes = FALSE,
      show.legend = FALSE,
      hjust = 1.1,
      vjust = 0.5,
      size = 3 # font size to mm
    ) +
    ggplot2::geom_line(
      data = lines,
      mapping = ggplot2::aes(x = Time, y = Y, group = Drug),
      inherit.aes = FALSE,
      show.legend = FALSE,
      color = "black",
      linetype = "solid",
      linewidth = 0.5
    )
}

#' One opioid's % MEAC at a time, for the hover
#'
#' @param entry the drug's element of drugs()
#' @param time minutes
#' @return the percentage, or NULL for a drug with no MEAC
#' @noRd
meacPercentAt <- function(entry, time)
{
  MEAC <- suppressWarnings(as.numeric(entry$MEAC))
  if (length(MEAC) != 1 || is.na(MEAC) || MEAC <= 0) return(NULL)
  derivedEffectSite(entry, time) / MEAC * 100
}
