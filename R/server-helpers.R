# The welcome dialog on its own, for a session restored from a bookmark.  A
# fresh session shows the same text inside the startup drug menu instead
# (R/startup-drugs.R).
showIntroModal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      title = "Welcome to stanpumpR",
      welcomeText(),
      shiny::tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-bs-dismiss` = "modal",
        "OK"
      ),
      # Opens the Help tab at the tour (the click is handled in app.js)
      shiny::tags$a(
        href = "#",
        class = "btn btn-outline-primary ms-2",
        `data-bs-dismiss` = "modal",
        `data-help-page` = "quick-start",
        shiny::icon("circle-question"),
        "Take the tour"
      ),
      footer = NULL,
      easyClose = TRUE,
      size = "m"
    )
  )
}

checkNumericCovariates <- function(age, weight, height, errorFx = NULL,
                                   osmolality = OSMOLALITY_DEFAULT,
                                   creatinine = NULL) {
  msg <- ""
  success <- TRUE
  if (!is_valid_number(age, MIN_AGE, MAX_AGE)) {
    msg <- glue::glue("Age must be between {MIN_AGE} and {MAX_AGE}")
    success <- FALSE
  }
  if (!is_valid_number(weight, MIN_WEIGHT, MAX_WEIGHT)) {
    msg <- glue::glue("Weight must be between {MIN_WEIGHT} and {MAX_WEIGHT}")
    success <- FALSE
  }
  if (!is_valid_number(height, MIN_HEIGHT, MAX_HEIGHT)) {
    msg <- glue::glue("Height must be between {MIN_HEIGHT} and {MAX_HEIGHT}")
    success <- FALSE
  }
  if (!is_valid_number(osmolality, MIN_OSMOLALITY, MAX_OSMOLALITY)) {
    msg <- glue::glue("Serum osmolality must be between {MIN_OSMOLALITY} and {MAX_OSMOLALITY} mOsm/kg")
    success <- FALSE
  }
  # Optional: blank (NA or NULL) means the assumed normal value.  NaN is not
  # blank and is rejected.
  if (!is.null(creatinine) &&
      !(length(creatinine) == 1 && is.na(creatinine) &&
        !(is.numeric(creatinine) && is.nan(creatinine))) &&
      !is_valid_number(creatinine, MIN_CREATININE, MAX_CREATININE)) {
    msg <- glue::glue("Serum creatinine must be between {MIN_CREATININE} and {MAX_CREATININE} mg/dL, or left blank")
    success <- FALSE
  }

  if (nzchar(msg) && is.function(errorFx)) {
    errorFx(msg)
  }
  success
}

# The bookmark URL for the address bar, with the session's debug level kept
#
# The URL is rewritten as a bookmark on every change, and the bookmark has no
# debug parameter, so a reload used to lose ?debug=1 and the debug panel with
# it.  The level is put back when it differs from the configured one, in front
# of `_inputs_`, where Shiny's restore ignores it (after `_values_` it would be
# read as a bookmarked value).  Only the address bar gets it: the URL emailed
# with a slide, or copied from elsewhere, does not switch debugging on for
# whoever opens it.
#
# @param url the bookmark URL from onBookmarked()
# @param level the session's debug level (session$userData$debug(): a number
#   from the URL or config, a string from the debug menu)
# @param default the configured level (config$debug)
withDebugQuery <- function(url, level, default = DEBUG_LEVEL_OFF) {
  if (is.null(default)) default <- DEBUG_LEVEL_OFF
  # The debug menu sends its level as a string
  level <- suppressWarnings(as.numeric(level))
  if (!is_valid_number(level) || isTRUE(level == default)) return(url)
  sep <- regexpr("?", url, fixed = TRUE)
  if (sep < 0) return(paste0(url, "?debug=", level))
  paste0(substr(url, 1, sep), "debug=", level, "&", substring(url, sep + 1))
}
