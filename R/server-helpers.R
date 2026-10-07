showIntroModal <- function() {
  shiny::showModal(
    shiny::modalDialog(
      title = "Welcome to stanpumpR",
      shiny::p(
        "stanpumpR, derived from the original STANPUMP program developed at
        Stanford University,  performs pharmacokinetic simulations
        based on mathematical models published in the peer-reviewed
        literature. stanpumpR is intended to help clinicians and investigators
        better understand the mathematical implications of published models.
        stanpumpR is only an advisory program. How these models are applied to
        individual patients is a matter of clinical judgment by the health care
        provider."
      ),
      shiny::p("stanpumpR does not collect any protected healthcare information."),
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
  # Optional: blank (NA or NULL) means the assumed normal value.
  if (!is.null(creatinine) && !(length(creatinine) == 1 && is.na(creatinine)) &&
      !is_valid_number(creatinine, MIN_CREATININE, MAX_CREATININE)) {
    msg <- glue::glue("Serum creatinine must be between {MIN_CREATININE} and {MAX_CREATININE} mg/dL, or left blank")
    success <- FALSE
  }

  if (nzchar(msg) && is.function(errorFx)) {
    errorFx(msg)
  }
  success
}
