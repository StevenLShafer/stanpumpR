#' Launch Shiny App
#'
#' @param config_file Path to a YAML configuration file, read by
#'   [config::get()]. Any setting that isn't specified in the config file
#'   falls back to the package defaults. Copy `config.yml.sample` to
#'   `config.yml` for local use.
#' @return A Shiny app object, as returned by [shiny::shinyApp()].
#' @export
run_app <- function(config_file = "config.yml") {
  options(warn = 1)

  config <- tryCatch({
    config::get(file = config_file)
  }, error = function(e) {
    if (!grepl("not found", e$message)) {
      stop(e)
    }
    list()
  })

  config <- c(config, DEFAULT_CONFIG[!names(DEFAULT_CONFIG) %in% names(config)])
  .sprglobals$config <- config

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
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.9)),
    legend.position = "right",
    legend.key = ggplot2::element_blank()
  )

  shiny::addResourcePath("stanpumpr-assets", system.file("www", package = "stanpumpR"))

  shiny::shinyApp(app_ui(), app_server, enableBookmarking = "url")
}
