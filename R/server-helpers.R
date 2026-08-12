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
        `data-bs-dismiss` = "modal",
        "OK"
      ),
      footer = NULL,
      easyClose = TRUE,
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

validateDoseTableInput <- function(DT, drugDefaults = getDrugDefaultsGlobal()) {
  if (!is.data.frame(DT) || !all(c("Drug", "Time", "Dose", "Units") %in% names(DT))) {
    stop("Invalid dose table structure.")
  }
  if (nrow(DT) > MAX_DOSE_ROWS) stop("Dose table exceeds the permitted row limit.")

  drug <- as.character(DT$Drug)
  time <- as.character(DT$Time)
  units <- as.character(DT$Units)
  dose <- suppressWarnings(as.numeric(DT$Dose))
  present <- nzchar(drug) | nzchar(time) | nzchar(units) | !is.na(dose)

  if (any(nchar(drug) > 64L | nchar(time) > 32L | nchar(units) > 32L, na.rm = TRUE)) {
    stop("Dose table contains an overlong value.")
  }
  if (any(nzchar(drug) & !drug %in% drugDefaults$Drug, na.rm = TRUE)) stop("Dose table contains an unknown drug.")
  if (any(nzchar(units) & !units %in% allUnits, na.rm = TRUE)) stop("Dose table contains unknown dose units.")
  if (any(present & (!is.finite(dose) | dose < 0 | dose > MAX_DOSE_VALUE), na.rm = TRUE)) {
    stop("Dose must be finite, non-negative, and within the permitted limit.")
  }
  if (any(nzchar(time) & vapply(time, function(x) !identical(validateTime(x), x), logical(1)), na.rm = TRUE)) {
    stop("Dose table contains an invalid time.")
  }
  invisible(TRUE)
}

validateEventTableInput <- function(ET, eventDefaults = getEventDefaults()) {
  if (!is.data.frame(ET) || !all(c("Time", "Event") %in% names(ET))) stop("Invalid event table structure.")
  if (nrow(ET) > MAX_EVENT_ROWS) stop("Event table exceeds the permitted row limit.")
  time <- as.character(ET$Time)
  event <- as.character(ET$Event)
  if (any(nchar(time) > 32L | nchar(event) > 128L, na.rm = TRUE)) stop("Event table contains an overlong value.")
  if (any(nzchar(event) & !event %in% eventDefaults$Event, na.rm = TRUE)) stop("Event table contains an unknown event.")
  if (any(nzchar(time) & vapply(time, function(x) !identical(validateTime(x), x), logical(1)), na.rm = TRUE)) {
    stop("Event table contains an invalid time.")
  }
  invisible(TRUE)
}

validateTargetTableInput <- function(targetTable) {
  if (!is.data.frame(targetTable) || !all(c("Time", "Target") %in% names(targetTable))) {
    stop("Invalid target table structure.")
  }
  if (nrow(targetTable) > MAX_TARGET_ROWS) stop("Target table exceeds the permitted row limit.")
  time <- as.character(targetTable$Time)
  targetText <- as.character(targetTable$Target)
  target <- suppressWarnings(as.numeric(targetText))
  present <- nzchar(time) | nzchar(targetText)
  if (any(nchar(time) > 32L, na.rm = TRUE)) stop("Target table contains an overlong time.")
  if (any(nzchar(time) & vapply(time, function(x) !identical(validateTime(x), x), logical(1)), na.rm = TRUE)) {
    stop("Target table contains an invalid time.")
  }
  if (any(present & (!is.finite(target) | target < 0 | target > MAX_DOSE_VALUE), na.rm = TRUE)) {
    stop("Target concentrations must be finite and within the permitted limit.")
  }
  invisible(TRUE)
}

isEmailAllowed <- function(email, allowedDomains) {
  if (!isTRUE(isEmailValid(email)) || length(allowedDomains) == 0L) return(FALSE)
  domain <- tolower(sub("^[^@]+@", "", email))
  domain %in% tolower(trimws(allowedDomains))
}

recalculatePK <- function(drugs, drugDefaults, doseTable,
                          age, weight, height, sex) {
  #  for (idx in seq(nrow(drugDefaults))) {
  #    drug <- drugDefaults$Drug[idx]
  for (drug in unique(doseTable$Drug)) {
    idx <- which(drugDefaults$Drug==drug)
    drugs[[drug]]$Color <- drugDefaults$Color[idx]
    drugs[[drug]]$endCe <- drugDefaults$endCe[idx]
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

# When the given element is inside a modal, make sure its first input gets focus
# when the modal opens
modalFocus <- function(tag) {
  if (tag$name == "input") {
    htmltools::tagAppendAttributes(tag, class = "modal-focusme")
  } else {
    htmltools::tagQuery(tag)$find("input")$addClass("modal-focusme")$allTags()
  }
}
