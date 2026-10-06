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

# Guarantee a usable ventilation setting whenever a gas is in the dose table.
#
# Provenance: drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the
# request of Steven L. Shafer; run and verified on R 4.6.1 by
# tests/testthat/test-input-tables.R.
#
# Why: with no ventilation row the gas engine reads alveolar ventilation as
# 0 L/min, i.e. apnea.  Nothing then carries gas from the circuit to the
# alveoli, so every agent sits at 0% and oxygen is simply consumed.  The rule
# (Shafer): ventilation must be > 0 if a gas is present, so as soon as a gas is
# entered a ventilation row is added, and a ventilation dose that is blank,
# non-numeric or <= 0 is replaced by the default.
#
# Works on the RAW dose table (character columns, trailing blank rows), so the
# row it adds is visible and editable in the table rather than being a hidden
# engine default.  Returns DT untouched when there is nothing to do, so callers
# can use identicalTable() to see whether anything changed.
ensureGasVentilation <- function(DT, weight = 70) {
  if (!is.data.frame(DT) || !all(c("Drug", "Time", "Dose", "Units") %in% names(DT))) return(DT)

  drug <- as.character(DT$Drug)
  if (!any(drug %in% setdiff(gasDrugNames(), "ventilation"))) return(DT)

  default <- as.character(defaultGasVentilation(weight))
  vent <- which(drug == "ventilation")
  dose <- suppressWarnings(as.numeric(as.character(DT$Dose[vent])))
  bad  <- vent[is.na(dose) | dose <= 0]
  if (length(vent) > 0 && length(bad) == 0) return(DT)

  # Something has to be written, so make sure the columns can take it.
  for (col in c("Drug", "Time", "Dose", "Units")) DT[[col]] <- as.character(DT[[col]])

  if (length(vent) == 0) {
    # Insert directly below the last filled row, above the trailing blank rows.
    last <- max(which(drug != ""))
    new  <- DT[last, , drop = FALSE]
    new$Drug <- "ventilation"; new$Time <- "0"; new$Dose <- default; new$Units <- "L/min"
    DT <- rbind(DT[seq_len(last), , drop = FALSE], new, DT[-seq_len(last), , drop = FALSE])
    rownames(DT) <- NULL
  } else {
    DT$Dose[bad] <- default
    # A row with no time or units would be dropped by cleanDoseTable(), leaving
    # the patient apneic after all.
    DT$Time[bad][is.na(DT$Time[bad]) | DT$Time[bad] == ""]    <- "0"
    DT$Units[bad][is.na(DT$Units[bad]) | DT$Units[bad] == ""] <- "L/min"
  }
  DT
}

# Guarantee an oxygen row whenever nitrous oxide is in the dose table.
#
# Provenance: drafted by Claude Code (Claude Fable 5.1), 2026-10-05, at the
# request of Steven L. Shafer; run and verified on R 4.6.1 by
# tests/testthat/test-input-tables.R.
#
# The rule (Shafer): as soon as nitrous oxide is added, add oxygen if it is not
# already present, starting at 21% of total gas flow.
#
# "21% of total gas flow" is implemented as: the oxygen flow that makes the
# FRESH-GAS OXYGEN FRACTION 21%, counting the oxygen that any air flow already
# carries.  With nitrous oxide alone that is Q_O2 = 0.21 / 0.79 * Q_N2O.
#
# The nitrous oxide row usually arrives before its flow does (the drug name is
# typed first), and 21% of an unknown total cannot be computed.  So the oxygen
# row is added at once with a BLANK dose, and a blank oxygen dose is filled in
# when the nitrous oxide flow becomes known.  An oxygen dose the user has
# entered -- including an explicit 0 -- is never overwritten.
ensureGasOxygen <- function(DT) {
  if (!is.data.frame(DT) || !all(c("Drug", "Time", "Dose", "Units") %in% names(DT))) return(DT)

  drug <- as.character(DT$Drug)
  n2o  <- which(drug == "nitrousOxide")
  if (length(n2o) == 0) return(DT)

  dose <- suppressWarnings(as.numeric(as.character(DT$Dose)))
  firstFlow <- function(rows) {
    d <- dose[rows]
    d <- d[!is.na(d) & d > 0]
    if (length(d) == 0) 0 else d[1]
  }
  Q_N2O <- firstFlow(n2o)
  Q_air <- firstFlow(which(drug == "air"))

  # Oxygen flow giving a fresh-gas oxygen fraction of GAS_INITIAL_O2_FRACTION:
  #   (Q_O2 + AIR_FRACTION_O2 * Q_air) / (Q_O2 + Q_N2O + Q_air) = f
  # Blank until there is a nitrous oxide flow to take 21% of.
  f <- GAS_INITIAL_O2_FRACTION
  target <- if (Q_N2O > 0) {
    as.character(max(0, round((f * (Q_N2O + Q_air) - AIR_FRACTION_O2 * Q_air) / (1 - f), 1)))
  } else {
    ""
  }

  o2 <- which(drug == "oxygen")
  blank <- o2[is.na(DT$Dose[o2]) | as.character(DT$Dose[o2]) == ""]
  if (length(o2) > 0 && (length(blank) == 0 || target == "")) return(DT)

  for (col in c("Drug", "Time", "Dose", "Units")) DT[[col]] <- as.character(DT[[col]])

  if (length(o2) == 0) {
    # Directly below the last nitrous oxide row, starting when it starts.
    at  <- max(n2o)
    new <- DT[at, , drop = FALSE]
    start <- DT$Time[n2o[1]]
    new$Drug <- "oxygen"; new$Dose <- target; new$Units <- "L/min"
    new$Time <- if (is.na(start) || start == "") "0" else start
    DT <- rbind(DT[seq_len(at), , drop = FALSE], new, DT[-seq_len(at), , drop = FALSE])
    rownames(DT) <- NULL
  } else {
    DT$Dose[blank] <- target
  }
  DT
}

# Round the gas flows and the ventilation to the nearest 0.1 L/min (Shafer,
# 2026-10-05).  Flowmeters and ventilators are not set more finely than that.
# The vaporiser settings, in %, are left alone.  Only doses that actually change
# are rewritten, so "2" stays "2" rather than becoming "2.0" or vice versa.
roundGasFlows <- function(DT) {
  if (!is.data.frame(DT) || !all(c("Drug", "Dose", "Units") %in% names(DT))) return(DT)

  flow <- which(isGasDrug(as.character(DT$Drug)) & as.character(DT$Units) == "L/min")
  dose <- suppressWarnings(as.numeric(as.character(DT$Dose[flow])))
  change <- !is.na(dose) & abs(round(dose, 1) - dose) > 1e-9
  if (!any(change)) return(DT)

  DT$Dose <- as.character(DT$Dose)
  DT$Dose[flow[change]] <- as.character(round(dose[change], 1))
  DT
}

# Every rule the dose table must satisfy once a gas is in it, in the order they
# depend on one another: oxygen first (it is a gas, so it can trigger the
# ventilation row), then rounding (which can round a tiny ventilation to 0),
# then ventilation (which replaces a 0).  Returns DT untouched when no rule
# applies, so callers can detect a change with identicalTable().
applyGasTableRules <- function(DT, weight = 70) {
  DT <- ensureGasOxygen(DT)
  DT <- roundGasFlows(DT)
  ensureGasVentilation(DT, weight)
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
  if (any(!DT$Units %in% c(allUnits, gasUnits, tciUnits))) stop(shiny::safeError("Dose table contains unknown dose units."))
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
