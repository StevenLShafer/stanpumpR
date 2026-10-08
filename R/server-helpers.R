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
