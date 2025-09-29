makeReactiveTrigger <- function() {
  rv <- shiny::reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- shiny::isolate(rv$a + 1)
    }
  )
}

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
      shiny::tags$button(
        type = "button",
        class = "btn btn-warning",
        `data-dismiss` = "modal",
        "OK"
      ),
      footer = NULL,
      easyClose = TRUE,
      fade = TRUE,
      size = "m"
    )
  )
}

checkNumericCovariates <- function(age, weight, height, errorFx = NULL) {
  msg <- ""
  success <- TRUE
  if (!is.numeric(age) || !is.numeric(weight) || !is.numeric(height)) {
    success <- FALSE
  }
  if (!age %btwn% c(MIN_AGE, MAX_AGE)) {
    msg <- glue::glue("Age must be between {MIN_AGE} and {MAX_AGE}")
    success <- FALSE
  }
  if (!weight %btwn% c(MIN_WEIGHT, MAX_WEIGHT)) {
    msg <- glue::glue("Weight must be between {MIN_WEIGHT} and {MAX_WEIGHT}")
    success <- FALSE
  }
  if (!height %btwn% c(MIN_HEIGHT, MAX_HEIGHT)) {
    msg <-glue::glue("Height must be between {MIN_HEIGHT} and {MAX_HEIGHT}")
    success <- FALSE
  }

  if (nzchar(msg) && is.function(errorFx)) {
    errorFx(msg)
  }
  success
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex) {
#  for (idx in seq(nrow(drugDefaults))) {
#    drug <- drugDefaults$Drug[idx]
  for (drug in unique(doseTable$Drug)) {
    idx <- which(drugDefaults$Drug==drug)
    drugs[[drug]]$Color <- drugDefaults$Color[idx]
    drugs[[drug]]$endCe <- drugDefaults$endCe[idx]
    drugs[[drug]]$endCeText <- drugDefaults$endCeText[idx]
    outputComments("Getting PK for", drug)
    drugs[[drug]] <- utils::modifyList(
      drugs[[drug]],
      getDrugPK(
        drug = drug,
        weight = weight,
        height = height,
        age = age,
        sex = sex,
        drugDefaults = drugDefaults[idx, ]
      )
    )
    drugs[[drug]]$DT <- NULL # Remove old dose table, if any
    drugs[[drug]]$equiSpace <- NULL # Ditto
  }

  drugs
}

cleanDT <- function(DT) {
  DT$Drug    <- as.character(DT$Drug)
  DT$Units   <- as.character(DT$Units)
  DT$Dose    <- as.numeric(DT$Dose)
  DT$Time    <- as.character(DT$Time)  # Stored as factors... Arrgh.....
  DT <- DT[DT$Drug != "" & !is.na(DT$Dose) & DT$Time != "" & DT$Units != "", ]
  DT
}
