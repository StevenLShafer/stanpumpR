# Split each drug's comma-separated Units into a vector, grouped by route (IV,
# then PO, IM, IN, RA) so the units dropdowns list each route's units together.
drugUnitsExpand <- function(units) {
  # trimws: a Drug Library edit typed as "mg, mcg/kg/min" must offer
  # "mcg/kg/min", not " mcg/kg/min", which the dose table would then refuse
  lapply(strsplit(units, ","), function(tokens) groupUnitsByRoute(trimws(tokens)))
}
drugUnitsSimplify <- function(units) {
  unlist(lapply(units, paste, collapse = ","))
}

# Can ggplot draw this colour?  A name or "#RRGGBB" col2rgb() reads.
isValidColor <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) return(FALSE)
  !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

DRUG_LIBRARY_NUMERIC_COLUMNS <- c("Lower", "Upper", "Typical", "MEAC")

# The table the Drug Library dialog shows.  endCe is managed via the Drug
# Thresholds dialog; Category only groups the startup menu.  The threshold
# columns go to the grid as TEXT, and come back as typed (tableGridRead()):
# read as numbers, a typed "bogus" would be NA, the same as a cleared cell,
# and drugLibraryProblems() could not tell them apart.  The
# input$drugEditsOK observer converts them to numbers after validation.
drugLibraryGridData <- function(drugDefaults) {
  x <- drugDefaults
  x$Units <- drugUnitsSimplify(x$Units)
  x <- x[, !names(x) %in% c("endCe", "Category")]
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) x[[col]] <- as.character(x[[col]])
  x
}

# What is wrong with an edited drug library (the Drug Library dialog), as
# sentences, or character(0).  Only entries that changed are judged, since a
# metabolite-only row has no colour or units of its own.  A Color ggplot cannot
# read reached the plot as a generic error in its place, and a Units entry the
# dose table does not accept (validateDoseTableInput()) was offered in the
# table and then refused as unknown units, with a blank plot (2026-10-31).
# Lower/Upper/Typical/MEAC had no check at all: a non-numeric entry (e.g.
# "bogus") became NA with no message, erasing the stored threshold
# (production audit, 2026-11-01).  The check below needs the text as typed,
# which is why drugLibraryGridData() hands those columns to the grid as text.
drugLibraryProblems <- function(edited, current) {
  # The Drug column is read-only and the grid adds and removes no rows, so
  # the edited table names the library's drugs, all of them, in its order.
  # Only the cells beside them were checked: a forged table of one row
  # replaced the whole library, and a renamed drug became a new one, each
  # leaving the dose table's drugs unknown and the plot gone (ChatGPT's
  # fourth review, F03).
  if (nrow(edited) != nrow(current) ||
      !identical(as.character(edited$Drug), as.character(current$Drug))) {
    return("The Drug Library cannot add, remove or rename drugs.")
  }
  problems <- character(0)
  # A refused entry is quoted back, but not at any length the browser sends
  shown <- function(x) ifelse(nchar(x) > 40, paste0(substr(x, 1, 40), "..."), x)
  at <- match(edited$Drug, current$Drug)
  color <- as.character(edited$Color)
  colorBefore <- as.character(current$Color)[at]
  badColor <- !is.na(at) & !identical2(color, colorBefore) & !vapply(color, isValidColor, logical(1))
  if (any(badColor)) {
    problems <- c(problems, paste0(
      "Color must be a colour name or #RRGGBB (",
      paste(sprintf("%s: \"%s\"", edited$Drug[badColor], shown(color[badColor])), collapse = ", "), ")."))
  }
  known <- c(allUnits, gasUnits, tciUnits, scheduledUnits)
  units <- drugUnitsSimplify(edited$Units)
  unitsBefore <- drugUnitsSimplify(current$Units)[at]
  unknown <- character(0)
  emptied <- character(0)
  for (i in seq_along(units)) {
    if (is.na(at[i]) || identical(units[i], unitsBefore[i])) next
    tokens <- trimws(strsplit(units[i], ",", fixed = TRUE)[[1]])
    # A changed list with no unit, or an empty entry between commas, would
    # offer a blank the dose table ignores: refused, as an unknown unit is.
    if (length(tokens) == 0 || any(!nzchar(tokens))) emptied <- c(emptied, edited$Drug[i])
    bad <- tokens[nzchar(tokens) & !tokens %in% known]
    if (length(bad)) unknown <- c(unknown, sprintf("%s: %s", edited$Drug[i], paste0("\"", shown(bad), "\"", collapse = ", ")))
  }
  if (length(unknown)) {
    problems <- c(problems, paste0(
      "Units must be units the dose table accepts (", paste(unknown, collapse = "; "), ")."))
  }
  if (length(emptied)) {
    problems <- c(problems, paste0(
      "Units must list at least one unit, with none left blank (", paste(emptied, collapse = ", "), ")."))
  }
  # The four unit dropdowns.  Delete empties a dropdown cell, which no check
  # caught: a blank Concentration.Units stopped every simulation ("Unsupported
  # Concentration.Units: "), and Suggest Dosing then ended the session
  # (adversarial UI review, 2026-11, finding 9).  A changed entry must be
  # one its dropdown offers, and a default unit one of the drug's own units.
  # Unchanged entries are left alone: some drugs have no bolus or infusion
  # unit, and a drug in %, mmol or mOsm has a read-only concentration unit.
  dropdowns <- list(
    Concentration.Units = c("mcg", "ng"),
    Bolus.Units = bolusUnits,
    Infusion.Units = infusionUnits
  )
  # A cell with no unit is NA from the library and "" once emptied in the
  # grid: the same, so emptying an empty cell changes nothing.
  unitText <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  badUnits <- character(0)
  for (col in names(dropdowns)) {
    value <- unitText(edited[[col]])
    before <- unitText(current[[col]])[at]
    # A changed entry must be one its list offers.  A concentration unit
    # must also have been one: the grid draws one outside mcg and ng
    # read-only (drugLibraryGridColumns()), which a forged value walked past,
    # and warfarin's INR made mcg stopped every simulation of it (Carbon's UI
    # audit, 2026-11-14, F4)
    readOnly <- col == "Concentration.Units" & !(before %in% dropdowns[[col]])
    bad <- !is.na(at) & value != before & (!(value %in% dropdowns[[col]]) | readOnly)
    if (any(bad)) badUnits <- c(badUnits, sprintf("%s %s", edited$Drug[bad], col))
  }
  # The bolus and infusion units are the units Suggest Dosing writes, so a
  # changed one must also be one of the drug's own: calcium given a bolus
  # unit of mg had its suggestion refused by the simulation (the same audit)
  for (col in c("Bolus.Units", "Infusion.Units")) {
    value <- unitText(edited[[col]])
    before <- unitText(current[[col]])[at]
    for (i in which(!is.na(at) & value != before & value %in% dropdowns[[col]])) {
      offered <- trimws(strsplit(units[i], ",", fixed = TRUE)[[1]])
      if (!value[i] %in% offered) badUnits <- c(badUnits, sprintf("%s %s", edited$Drug[i], col))
    }
  }
  default <- unitText(edited$Default.Units)
  defaultBefore <- unitText(current$Default.Units)[at]
  for (i in seq_along(default)) {
    if (is.na(at[i]) || (default[i] == defaultBefore[i] && identical(units[i], unitsBefore[i]))) next
    offered <- trimws(strsplit(units[i], ",", fixed = TRUE)[[1]])
    if (!default[i] %in% offered) {
      badUnits <- c(badUnits, sprintf("%s Default.Units", edited$Drug[i]))
    }
  }
  if (length(badUnits)) {
    problems <- c(problems, paste0(
      "Choose each unit from its list, and a Default.Units, Bolus.Units or Infusion.Units from the drug's Units (",
      paste(badUnits, collapse = ", "), ")."))
  }
  badNumbers <- character(0)
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) {
    valueChr <- trimws(as.character(edited[[col]]))
    blank <- is.na(edited[[col]]) | !nzchar(valueChr)
    parsed <- suppressWarnings(as.numeric(valueChr))
    # the grid's own limits (a number cell, validateDose())
    bad <- !is.na(at) & !blank & (is.na(parsed) | !is.finite(parsed) | parsed < 0 | parsed > MAX_DOSE_VALUE)
    if (any(bad)) {
      badNumbers <- c(badNumbers, sprintf("%s %s: \"%s\"", edited$Drug[bad], col, shown(valueChr[bad])))
    }
  }
  if (length(badNumbers)) {
    problems <- c(problems, paste0(
      "Lower, Upper, Typical and MEAC must be left blank or a number from 0 to 1,000,000,000 (",
      paste(badNumbers, collapse = ", "), ")."))
  }
  # Only once everything above is right, since it simulates
  if (length(problems) == 0) problems <- drugLibraryUnitProblems(edited, current)
  problems
}

# What is wrong with the units a Drug Library edit adds to a drug, as
# sentences, or character(0).  drugLibraryProblems() accepts any unit the
# dose table knows, but not every drug's model can take every unit: calcium
# given "mg" stopped every simulation of it once dosed ("Dose unit for
# calcium must name the product"), and a route its model has no absorption
# for drew nothing at all (Carbon's UI audit, 2026-11-14, F4).  So each new
# unit is simulated once, a dose of 1 at time 0 over a day at the reference
# adult, through the app's own pipeline, and refused with the engine's
# reason or for giving no concentration; an inhaled agent takes only the gas
# units and no other drug takes them.  Every unit of every drug in the
# library passes (test-ui-audit-carbon.R).  At most `maxNew` new units are
# simulated in one edit, since a forged table could add hundreds.
drugLibraryUnitProblems <- function(edited, current, maxNew = DRUG_LIBRARY_MAX_NEW_UNITS) {
  at <- match(edited$Drug, current$Drug)
  units <- drugUnitsSimplify(edited$Units)
  unitsBefore <- drugUnitsSimplify(current$Units)[at]
  tokensOf <- function(x) {
    if (is.na(x)) return(character(0))
    t <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    t[nzchar(t)]
  }
  added <- list()
  # identical2(): a metabolite-only row has no units (NA)
  for (i in which(!is.na(at) & !identical2(units, unitsBefore))) {
    new <- setdiff(tokensOf(units[i]), tokensOf(unitsBefore[i]))
    if (length(new)) added[[as.character(edited$Drug[i])]] <- new
  }
  if (length(added) == 0) return(character(0))
  if (length(unlist(added)) > maxNew) {
    return(sprintf("Add at most %d units in one edit of the Drug Library.", maxNew))
  }
  # The library the simulation reads: the current one, with the edited
  # concentration units where they are the dropdown's
  library <- current
  cu <- as.character(edited$Concentration.Units)
  move <- !is.na(at) & cu %in% c("mcg", "ng")
  library$Concentration.Units[at[move]] <- cu[move]
  noEvents <- data.frame(Time = numeric(0), Event = character(0))
  bad <- character(0)
  for (drug in names(added)) {
    row <- library[library$Drug == drug, , drop = FALSE]
    gas <- identical(as.character(row$Class), "gas")
    for (unit in added[[drug]]) {
      if (gas != (unit %in% gasUnits)) {
        bad <- c(bad, sprintf("%s \"%s\" (%s)", drug, unit,
                              if (gas) "an inhaled agent is set in L/min or %" else "a unit of the inhaled agents"))
        next
      }
      if (gas) next
      DT <- data.frame(Drug = drug, Time = 0, Dose = 1, Units = unit, stringsAsFactors = FALSE)
      why <- tryCatch({
        drugs <- recalculatePK(NULL, library, DT, age = 35, weight = 70, height = 170, sex = "male")
        plasma <- processdoseTable(DT, noEvents, drugs, MINS_PER_DAY, FALSE)[[drug]]$wide$Plasma
        # A drug plotted above a baseline (an INR, an ACT, an osmolality)
        # starts above zero whatever the unit does
        baseline <- row$Concentration.Units %in% c("INR", "ACT", "mOsm")
        if (!baseline && !any(is.finite(plasma) & plasma > 0)) "no concentration follows a dose" else ""
      }, error = function(e) conditionMessage(e))
      if (nzchar(why)) bad <- c(bad, sprintf("%s \"%s\" (%s)", drug, unit, why))
    }
  }
  if (length(bad) == 0) return(character(0))
  paste0("These units cannot be simulated for the drug they were added to: ",
         paste(bad, collapse = "; "), ".")
}

# A drug moved between mcg and ng in the Drug Library keeps its thresholds.
# Its range (Lower, Typical, Upper), MEAC and time-until-threshold value
# (endCe, which the dialog does not show) are concentrations in its units,
# so each that the same edit left as it was is converted, by 1000 either way;
# one the edit changed is taken as typed in the new units.  Left alone they
# were 1000-fold off the curve drawn beside them, as the metabolites the drug
# formed and the antiseizure interactions it drove were (found checking
# Carbon's UI audit, 2026-11-14, F4).
#
# `new` is the edited library with its columns converted (the dialog's
# Apply), `edited` the grid's text as read, `current` the library before.
# Returns a list: `table`, the library, and `drugs`, those converted.
drugLibraryRescaleThresholds <- function(new, edited, current) {
  at <- match(new$Drug, current$Drug)
  before <- as.character(current$Concentration.Units)[at]
  after <- as.character(new$Concentration.Units)
  perMl <- c("mcg", "ng")
  moved <- which(!is.na(at) & before %in% perMl & after %in% perMl & before != after)
  # What the grid showed: an entry that reads the same was left as it was
  shown <- drugLibraryGridData(current)
  cell <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  for (i in moved) {
    factor <- internalDoseScale(before[i]) / internalDoseScale(after[i])
    for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) {
      if (cell(edited[[col]][i]) == cell(shown[[col]][at[i]]))
        new[[col]][i] <- suppressWarnings(as.numeric(current[[col]][at[i]])) * factor
    }
    if ("endCe" %in% names(current))
      new$endCe[i] <- suppressWarnings(as.numeric(current$endCe[at[i]])) * factor
  }
  list(table = new, drugs = as.character(new$Drug[moved]))
}

# Elementwise identical() for two character vectors, NA equal to NA
identical2 <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

#' Return the stanpumpR drug defaults table
#'
#' @param expand If `TRUE`, the list in the Units column is expanded. Otherwise,
#' the Units column contains comma-separated strings.
#' @returns A data.frame containing concentrations and bolus units, suggested colors and plasma/effect levels for drug effect endpoints
#' @details The function is memoised so that the file is only read once per session.
#'
#' @export
getDrugDefaultsGlobal <- memoise::memoise(function(expand = TRUE)
{
  drugDefaultsDataset <- utils::read.csv(
    system.file("extdata", "drugDefaults_global.csv", package = "stanpumpR"),
    na.strings = ""
  )

  if (expand) {
    drugDefaultsDataset$Units <- drugUnitsExpand(drugDefaultsDataset$Units)
  }

  drugDefaultsDataset
})

#' Drug names in the order the app offers them
#'
#' Alphabetical, ignoring case, in the C collation so the order is the same
#' on every platform.  The dose table's Drug column, the Add a dose dialog and
#' Suggest Dosing list the drugs this way; the drug library itself, and so
#' everything indexed by its rows, keeps the order of the CSV.
#'
#' @param drugs drug names
#' @returns `drugs`, sorted
#' @keywords internal
sortDrugNames <- function(drugs) {
  drugs[order(tolower(drugs), method = "radix")]
}

getEventDefaults <- function() {
  utils::read.csv(
    system.file("extdata", "eventDefaults.csv", package = "stanpumpR")
  )
}

#' Returns the stanpumpR drug defaults table for a single drug
#'
#' @param drug The drug in question
#'
#' @returns A subsetted data.frame from getDrugDefaultsGlobal()
#'
#' @export
getDrugDefaults <- function(drug)
{
  drugDefaults_global <- getDrugDefaultsGlobal()
  drugDefaults_global[drugDefaults_global$Drug == drug, ]
}
