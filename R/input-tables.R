#####
# Functions that span the three input tables: dose table, events table, target table
#####

# Check if a drug has any non-zero doses in a dose table
drugHasNonZeroDoses <- function(dt, drug) {
  drugDoses <- dt[dt$Drug == drug & dt$Dose != "", ]
  any(suppressWarnings(as.numeric(drugDoses$Dose)) != 0, na.rm = TRUE)
}

# Coerce all columns to the correct data type and only keep full rows
cleanDoseTable <- function(DT) {
  DT$Drug    <- as.character(DT$Drug)
  DT$Units   <- as.character(DT$Units)
  DT$Dose    <- suppressWarnings(as.numeric(DT$Dose))
  DT$Time    <- as.character(DT$Time)
  DT <- DT[DT$Drug != "" & !is.na(DT$Dose) & DT$Time != "" & DT$Units != "", ]
  DT
}

validateDoseTableInput <- function(DT, drugDefaults = getDrugDefaultsGlobal()) {
  if (!is.data.frame(DT) || !all(c("Drug", "Time", "Dose", "Units") %in% names(DT))) {
    stop(shiny::safeError("Invalid dose table structure."))
  }
  if (nrow(DT) > MAX_DOSE_ROWS) stop(shiny::safeError("Dose table exceeds the permitted row limit."))

  DT <- cleanDoseTable(DT)
  if (nrow(DT) == 0L) return(invisible(TRUE))

  if (any(
    nchar(DT$Drug) > MAX_DRUGNAME_LENGTH |
    nchar(DT$Time) > MAX_TIME_STRING_LENGTH |
    nchar(DT$Units) > MAX_UNIT_STRING_LENGTH,
    na.rm = TRUE
  )) {
    stop(shiny::safeError("Dose table contains a value that's too long."))
  }

  if (any(!DT$Drug %in% drugDefaults$Drug)) stop(shiny::safeError("Dose table contains an unknown drug."))
  if (any(!DT$Units %in% allUnits)) stop(shiny::safeError("Dose table contains unknown dose units."))
  if (any(!is.finite(DT$Dose) | DT$Dose < 0 | DT$Dose > MAX_DOSE_VALUE)) {
    stop(shiny::safeError("Dose must be finite, non-negative, and within the permitted limit."))
  }
  if (any(vapply(DT$Time, function(x) !identical(validateTime(x), x), logical(1)))) {
    stop(shiny::safeError("Dose table contains an invalid time."))
  }
  invisible(TRUE)
}

validateEventTableInput <- function(ET, eventDefaults = getEventDefaults()) {
  if (!is.data.frame(ET) || !all(c("Time", "Event") %in% names(ET))) stop(shiny::safeError("Invalid event table structure."))
  if (nrow(ET) > MAX_EVENT_ROWS) stop(shiny::safeError("Event table exceeds the permitted row limit."))
  time <- as.character(ET$Time)
  event <- as.character(ET$Event)
  if (any(nchar(time) > MAX_TIME_STRING_LENGTH | nchar(event) > MAX_DRUGNAME_LENGTH, na.rm = TRUE)) {
    stop(shiny::safeError("Event table contains a value that's too long."))
  }
  if (any(!event %in% eventDefaults$Event)) stop(shiny::safeError("Event table contains an unknown event."))
  if (any(vapply(time, function(x) !identical(validateTime(x), x), logical(1)))) {
    stop(shiny::safeError("Event table contains an invalid time."))
  }
  invisible(TRUE)
}

validateTargetTableInput <- function(targetTable) {
  if (!is.data.frame(targetTable) || !all(c("Time", "Target") %in% names(targetTable))) {
    stop(shiny::safeError("Invalid target table structure."))
  }
  if (nrow(targetTable) > MAX_TARGET_ROWS) stop(shiny::safeError("Target table exceeds the permitted row limit."))
  time <- as.character(targetTable$Time)
  targetText <- as.character(targetTable$Target)
  target <- suppressWarnings(as.numeric(targetText))
  present <- nzchar(time) | nzchar(targetText)
  if (any(nchar(time) > MAX_TIME_STRING_LENGTH, na.rm = TRUE)) stop(shiny::safeError("Target table contains an overlong time."))
  if (any(nzchar(time) & vapply(time, function(x) !identical(validateTime(x), x), logical(1)), na.rm = TRUE)) {
    stop(shiny::safeError("Target table contains an invalid time."))
  }
  if (any(present & (!is.finite(target) | target < 0 | target > MAX_DOSE_VALUE), na.rm = TRUE)) {
    stop(shiny::safeError("Target concentrations must be finite and within the permitted limit."))
  }
  invisible(TRUE)
}
