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
  config <- c(config, DEFAULT_CONFIG[!names(DEFAULT_CONFIG) %in% names(config)])
  scalarFlag <- function(value, name) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(name, " must be true or false")
    }
    value
  }
  config$allow_url_debug <- scalarFlag(config$allow_url_debug, "allow_url_debug")
  config$email_enabled <- scalarFlag(config$email_enabled, "email_enabled")
  config$email_smtp_ssl <- scalarFlag(config$email_smtp_ssl, "email_smtp_ssl")
  if (!is.numeric(config$debug) || length(config$debug) != 1L || !is.finite(config$debug) ||
      !config$debug %in% c(DEBUG_LEVEL_OFF, DEBUG_LEVEL_NORMAL, DEBUG_LEVEL_VERBOSE)) {
    stop("debug must be 0, 1, or 2")
  }
  if (!is.character(config$bookmark_mode) || length(config$bookmark_mode) != 1L || is.na(config$bookmark_mode)) {
    stop("bookmark_mode must be a single string")
  }
  if (!config$bookmark_mode %in% c("disable", "server", "url")) {
    stop("bookmark_mode must be one of: disable, server, url")
  }
  if (!is.character(config$handsontable_license_key) || length(config$handsontable_license_key) != 1L ||
      is.na(config$handsontable_license_key) || !nzchar(config$handsontable_license_key)) {
    stop("handsontable_license_key must be a non-empty string")
  }
  if (isTRUE(config$email_enabled)) {
    requiredEmailConfig <- c("email_username", "email_password", "email_smtp_host")
    missingEmailConfig <- requiredEmailConfig[vapply(requiredEmailConfig, function(x) {
      is.null(config[[x]]) || length(config[[x]]) != 1L || !nzchar(config[[x]])
    }, logical(1))]
    invalidPort <- !is.numeric(config$email_smtp_port) || length(config$email_smtp_port) != 1L ||
      !is.finite(config$email_smtp_port) || config$email_smtp_port < 1 || config$email_smtp_port > 65535
    invalidDomains <- !is.character(config$email_allowed_domains) || length(config$email_allowed_domains) == 0L ||
      anyNA(config$email_allowed_domains) || any(!grepl("^[A-Za-z0-9.-]+$", config$email_allowed_domains))
    if (length(missingEmailConfig) > 0L || invalidPort || invalidDomains) {
      stop("Email is enabled but its credentials, SMTP host, or recipient-domain allowlist is incomplete.")
    }
  }
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

  shiny::shinyApp(app_ui(), app_server, enableBookmarking = config$bookmark_mode)
}
