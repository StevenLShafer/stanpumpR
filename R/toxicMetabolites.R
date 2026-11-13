# -----------------------------------------------------------------------------
# Toxic metabolites: their panel labels in red
# -----------------------------------------------------------------------------
# Steven L. Shafer asked (2026-11-13) that the metabolites that add only to
# toxicity -- lidocaine's MEGX, pethidine's norpethidine (TOXIC_METABOLITES in
# R/constants.R) -- be drawn with their labels in red, so that a clinician
# reading the plot sees at once that the curve is a hazard and not part of
# the therapeutic effect.
#
# ggplot2 (4.0) draws every strip of a facet with one element_text: a vector
# of colours reaches each strip whole ("not officially supported"), so one
# panel's label cannot be coloured from the theme.  Recolouring the gtable
# after it is built would work for a picture, but shiny's renderPlot() needs
# the ggplot object itself to map a click on the plot to a time and a drug,
# which the app uses to add and edit doses.  So the facet is subclassed: a
# child of the plot's own FacetGrid whose draw_panels() calls its parent's and
# then recolours the strips of the rows whose panel belongs to a toxic
# metabolite.  The plot stays an ordinary ggplot, the layout (and so every
# click coordinate) is untouched, and every route that draws it -- the
# screen, the PowerPoint slide, a panel's own slide (panelPlots(), which
# keeps the facet) -- gets the colour.  (Claude Code, 2026-11-13.)
# -----------------------------------------------------------------------------

#' Is a panel a toxic metabolite's?
#'
#' A panel's facet value is "<drug>\\n(<units>)", or for a peripheral
#' compartment "<drug> <compartment>\\n(<units>)" (R/compartments.R), so the
#' drug is the first word of the first line.
#'
#' @param wrap facet values
#' @returns logical, one per value
#' @keywords internal
isToxicPanel <- function(wrap)
{
  first <- sub("\n.*$", "", as.character(wrap))
  sub(" .*$", "", first) %in% TOXIC_METABOLITES
}

#' Set the colour of every text grob in a grob tree
#' @keywords internal
recolourText <- function(g, colour)
{
  if (inherits(g, "text")) {
    gp <- if (is.null(g$gp)) grid::gpar() else g$gp
    gp$col <- colour
    g$gp <- gp
  }
  if (inherits(g, "gtable")) {
    g$grobs <- lapply(g$grobs, recolourText, colour = colour)
  } else if (!is.null(g$children) && length(g$children) > 0) {
    g$children <- do.call(grid::gList, lapply(g$children, recolourText, colour = colour))
  }
  g
}

#' Recolour the strips of the toxic metabolites' rows in a facet's table
#'
#' @param table the gtable a FacetGrid's draw_panels() returns
#' @param layout the facet layout (\code{ROW} and the facet variable \code{Wrap})
#' @param colour the label colour
#' @returns the table
#' @keywords internal
recolourToxicStrips <- function(table, layout, colour = TOXIC_LABEL_COLOUR)
{
  if (is.null(layout$Wrap)) return(table)
  rows <- unique(layout$ROW[isToxicPanel(layout$Wrap)])
  if (length(rows) == 0) return(table)
  # One strip per row, named by side and row ("strip-l-3", "strip-r-3").
  names <- table$layout$name
  for (r in rows) {
    hit <- grep(paste0("^strip-[lr]-", r, "$"), names)
    for (i in hit) table$grobs[[i]] <- recolourText(table$grobs[[i]], colour)
  }
  table
}

#' A facet whose toxic-metabolite strips are drawn in red
#'
#' @param facet a facet, from \code{ggplot2::facet_grid()}
#' @param colour the colour of those strips' text
#' @returns a facet, a child of \code{facet}
#' @keywords internal
facetToxicLabels <- function(facet, colour = TOXIC_LABEL_COLOUR)
{
  ggplot2::ggproto(NULL, facet,
    draw_panels = function(self, panels, layout, ...) {
      table <- ggplot2::ggproto_parent(facet, self)$draw_panels(panels, layout, ...)
      recolourToxicStrips(table, layout, colour)
    }
  )
}

# -----------------------------------------------------------------------------
# The summed threshold: lidocaine + MEGX against 5 mcg/mL
# -----------------------------------------------------------------------------
# Shafer asked (2026-11-13) for the clinical convention to be drawn: Crouch
# 2023 adds MEGX to lidocaine (and a quarter of GX, not modelled) and reads
# the sum against lidocaine's 5 mcg/mL toxic threshold.  The sum is drawn in
# the metabolite's panel as a companion series (the mechanism that draws the
# Schnider model beside Eleveld's, R/alternativeModels.R), named "lidocaine +
# MEGX" and coloured TOXIC_LABEL_COLOUR, with a dashed line at the
# threshold.  It is drawn only where both drugs' plasma series are, and only
# on the concentration axis (not under normalization, where the threshold
# has no meaning).  The panel's own series stays first in plotResults, so a
# click or hover on the panel still reads MEGX.  (Claude Code, 2026-11-13.)
# -----------------------------------------------------------------------------

#' The name of a summed toxicity series ("lidocaine + MEGX")
#' @keywords internal
toxicSumName <- function(parent, metabolite) paste(parent, "+", metabolite)

#' The summed parent + metabolite plasma series for each TOXIC_SUM_THRESHOLDS row
#'
#' @param allResults the plot's series (Drug, Time, Y, Site, ...)
#' @returns a list: \code{series}, rows in allResults' shape (Site "Plasma"),
#'   and \code{companions}, one row per sum (Drug, drugColor, Parent)
#' @keywords internal
toxicSumSeries <- function(allResults)
{
  series <- allResults[0, , drop = FALSE]
  companions <- data.frame(Drug = character(0), drugColor = character(0),
                           Parent = character(0), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(TOXIC_SUM_THRESHOLDS))) {
    t <- TOXIC_SUM_THRESHOLDS[i, ]
    met <- allResults[allResults$Drug == t$metabolite & allResults$Site == "Plasma", , drop = FALSE]
    par <- allResults[allResults$Drug == t$parent & allResults$Site == "Plasma", , drop = FALSE]
    met <- met[!is.na(met$Y), , drop = FALSE]
    par <- par[!is.na(par$Y), , drop = FALSE]
    if (nrow(met) < 2 || nrow(par) < 2) next
    # On the union of the two lines' times, each interpolated: both are
    # linear interpolants of their own exact points.
    times <- sort(unique(c(met$Time, par$Time)))
    interp <- function(d) stats::approx(d$Time, d$Y, times, rule = 2, ties = "ordered")$y
    rows <- met[rep(1, length(times)), , drop = FALSE]
    rows$Drug <- toxicSumName(t$parent, t$metabolite)
    rows$Time <- times
    rows$Y <- interp(met) + interp(par)
    rownames(rows) <- NULL
    series <- rbind(series, rows)
    companions <- rbind(companions, data.frame(
      Drug = toxicSumName(t$parent, t$metabolite), drugColor = TOXIC_LABEL_COLOUR,
      Parent = t$metabolite, stringsAsFactors = FALSE))
  }
  list(series = series, companions = companions)
}

#' The dashed threshold lines and their labels, one per drawn panel
#'
#' @param plotTable the plot's panels (Drug, Wrap), Wrap a factor
#' @param minimum the start of the time axis, where the label is placed
#' @returns a data frame of Wrap, threshold and label, or NULL
#' @keywords internal
toxicSumThresholds <- function(plotTable, minimum)
{
  t <- TOXIC_SUM_THRESHOLDS[TOXIC_SUM_THRESHOLDS$metabolite %in% as.character(plotTable$Drug), , drop = FALSE]
  if (nrow(t) == 0) return(NULL)
  wrap <- plotTable$Wrap[match(t$metabolite, as.character(plotTable$Drug))]
  data.frame(
    Wrap = wrap,
    threshold = t$threshold,
    Time = minimum,
    label = paste0(toxicSumName(t$parent, t$metabolite), " toxic threshold, ",
                   t$threshold, " ", t$units),
    stringsAsFactors = FALSE
  )
}
