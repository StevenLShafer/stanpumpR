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
