#' Launch Shiny App
#'
#' @export
run_app <- function(config_file = "config.yml") {
  suppressWarnings(suppressPackageStartupMessages({
    library(stanpumpR)
    library(shiny)
    library(shinyjs)
    library(shinydashboard)
    library(tryCatchLog)
    library(httr)
    library(ggplot2)
    library(grid)
    library(openxlsx)
    library(dplyr)
    library(officer)
    library(rhandsontable)
    library(purrr)
    library(png)
    library(tidyr)
    library(lubridate)
  }))

  options(warn = 1)

  config <- config::get(file = config_file)
  if (is.null(config$title)) config$title <- "stanpumpR"
  if (is.null(config$maintainer_name)) config$maintainer_name <- "Steve"
  if (is.null(config$maintainer_email)) config$maintainer_email <- "stanpumpR@gmail.com"
  if (is.null(config$help_link)) config$help_link <- "https://steveshafer.shinyapps.io/stanpumpR_HelpPage"
  if (is.null(config$debug)) config$debug <- FALSE
  .sprglobals$config <- config

  .sprglobals$DEBUG <- config$debug

  ggplot2::theme_update(
    panel.background = ggplot2::element_rect(fill = "white", color = "white"),
    legend.box.background = ggplot2::element_rect(fill = "white", color = "white"),
    panel.grid.major.y = ggplot2::element_line(color = "lightgrey"),
    panel.grid.major.x = ggplot2::element_line(color = "lightgrey"),
    axis.ticks = ggplot2::element_line(color = "lightgrey"),
    axis.ticks.length = grid::unit(.25, "cm"),
    axis.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
    axis.text = ggplot2::element_text(size = ggplot2::rel(1.2)),
    axis.line = ggplot2::element_line(linewidth = 1, color = "black"),
    aspect.ratio = 0.6,
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
    legend.position = "right",
    legend.key = ggplot2::element_blank()
  )

  shiny::addResourcePath("stanpumpr-assets", system.file("www", package = "stanpumpR"))

  shiny::shinyApp(app_ui(), app_server, enableBookmarking = "url")
}
