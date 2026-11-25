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
drugLibraryProblems <- function(edited, current, inUse = NULL) {
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
    readOnly <- col == "Concentration.Units" &
      concentrationUnitsLocked(as.character(edited$Drug), before)
    bad <- !is.na(at) & value != before & (!(value %in% dropdowns[[col]]) | readOnly)
    if (any(bad)) badUnits <- c(badUnits, sprintf("%s %s", edited$Drug[bad], col))
  }
  # The bolus and infusion units are the units Suggest Dosing writes, so a
  # changed one must also be one of the drug's own: calcium given a bolus
  # unit of mg had its suggestion refused by the simulation (the same audit)
  # Nor may an edit of the Units take away the drug's bolus or infusion
  # unit, which Suggest Dosing then wrote for rows the grid emptied of units
  # (Carbon's second UI audit, 2026-11-15, F7).  A unit that was not among
  # them before is not the edit's doing, and is left as it was.
  tokens <- function(x) if (is.na(x)) character(0) else trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  for (col in c("Bolus.Units", "Infusion.Units")) {
    value <- unitText(edited[[col]])
    before <- unitText(current[[col]])[at]
    for (i in which(!is.na(at) & value != before & value %in% dropdowns[[col]])) {
      offered <- trimws(strsplit(units[i], ",", fixed = TRUE)[[1]])
      if (!value[i] %in% offered) badUnits <- c(badUnits, sprintf("%s %s", edited$Drug[i], col))
    }
    for (i in which(!is.na(at) & value == before & nzchar(value) &
                    !identical2(units, unitsBefore))) {
      if (value[i] %in% tokens(unitsBefore[i]) && !value[i] %in% tokens(units[i]))
        badUnits <- c(badUnits, sprintf("%s %s", edited$Drug[i], col))
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
  # Nor may it take away a unit the dose table uses: the grid blanks a unit
  # its drug does not take, so the row left the table at the next edit of
  # any row (Carbon's third UI audit, 2026-11-15, C4).  `inUse` is the
  # tables' Drug and Units (dosedUnits()).
  if (!is.null(inUse) && nrow(inUse) > 0) {
    inUseText <- character(0)
    for (i in which(!is.na(at) & !identical2(units, unitsBefore))) {
      drug <- as.character(edited$Drug[i])
      removed <- setdiff(tokens(unitsBefore[i]), tokens(units[i]))
      used <- intersect(removed, inUse$Units[inUse$Drug == drug])
      if (length(used)) inUseText <- c(inUseText, sprintf("%s %s", drug, used))
    }
    if (length(inUseText)) {
      problems <- c(problems, paste0(
        "A unit the dose table uses cannot be removed; change or delete those doses first (",
        paste(unique(inUseText), collapse = ", "), ")."))
    }
  }
  badNumbers <- character(0)
  # Only an entry the edit changed is held to the grid's limits: one left as
  # the grid showed it is the library's own, and a library value past them
  # (a range converted from mcg to ng, drugLibraryRescaleThresholds()) made
  # every later Apply fail, whatever drug it edited (Carbon's second UI
  # audit, 2026-11-15, F5)
  shownNow <- drugLibraryGridData(current)
  cellText <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) {
    valueChr <- trimws(as.character(edited[[col]]))
    blank <- is.na(edited[[col]]) | !nzchar(valueChr)
    parsed <- suppressWarnings(as.numeric(valueChr))
    changed <- is.na(at) | cellText(edited[[col]]) != cellText(shownNow[[col]][at])
    # the grid's own limits (a number cell, validateDose())
    bad <- !is.na(at) & changed & !blank &
      (is.na(parsed) | !is.finite(parsed) | parsed < 0 | parsed > MAX_DOSE_VALUE)
    if (any(bad)) {
      badNumbers <- c(badNumbers, sprintf("%s %s: \"%s\"", edited$Drug[bad], col, shown(valueChr[bad])))
    }
  }
  # And a range that runs the right way, for a row the edit changed (Carbon's
  # third UI audit, 2026-11-15, M6); a number outside the limits is refused
  # above, and is not also called backwards
  inLimits <- function(x) {
    x <- suppressWarnings(as.numeric(as.character(x)))
    x[!is.finite(x) | x < 0 | x > MAX_DOSE_VALUE] <- NA
    x
  }
  lowerNow <- inLimits(edited$Lower)
  upperNow <- inLimits(edited$Upper)
  rangeChanged <- !is.na(at) & (cellText(edited$Lower) != cellText(shownNow$Lower[at]) |
                                  cellText(edited$Upper) != cellText(shownNow$Upper[at]))
  # A row whose units change is checked after the bounds it leaves are
  # converted (input$drugEditsOK): its old Upper read against a new Lower
  # refused 500-2000 ng/mL as above 2 (CodeRabbit on PR #730)
  unitChanged <- !is.na(at) & "Concentration.Units" %in% names(edited) &
    cellText(edited$Concentration.Units) != cellText(shownNow$Concentration.Units[at])
  inverted <- rangeChanged & !unitChanged & is.finite(lowerNow) & is.finite(upperNow) &
    lowerNow > upperNow
  if (any(inverted)) {
    problems <- c(problems, paste0("Lower must not be above Upper (",
                                   paste(edited$Drug[inverted], collapse = ", "), ")."))
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
      # A combination product (Coartem) is dosed only in its own unit word
      # ("tablet"), which expandCombinationProducts() turns into mg of each
      # component; a mass unit would hand each component the product's dose
      # (R/combination-products.R)
      product <- isCombinationProduct(drug)
      if (product && !combinationUnitOk(drug, unit)) {
        bad <- c(bad, sprintf("%s \"%s\" (a combination product is dosed in %s)", drug, unit,
                              combinationDosedIn(drug)))
        next
      }
      DT <- data.frame(Drug = drug, Time = 0, Dose = 1, Units = unit, stringsAsFactors = FALSE)
      why <- tryCatch({
        # and is simulated as its components, each of which must move
        if (product) combinationUnitProblem(drug, DT, library, noEvents) else {
        drugs <- recalculatePK(NULL, library, DT, age = 35, weight = 70, height = 170, sex = "male")
        # An effect-site target needs an effect site: without one the
        # controller targets the plasma, under the effect site's name
        # (Carbon's second UI audit, 2026-11-15, F6)
        if (unit == TCI_UNIT_EFFECT && !pkHasEffectSite(drugs[[drug]]))
          stop("the model has no effect site", call. = FALSE)
        # A target is checked here at age 35 with no events and no other
        # drug, where a model that clinical events or the antiseizure
        # interactions switch to another PK set passed, and its targets were
        # then missed by up to 99% (Carbon's second repository audit,
        # 2026-11-19, A2-F9)
        if (unit %in% tciUnits && length(setdiff(drugs[[drug]]$pkEvents, PK_EVENT_DEFAULT)) > 0)
          stop("its parameters change with clinical events", call. = FALSE)
        if (unit %in% tciUnits && drug %in% asmInteractionVictims())
          stop("its parameters change with the antiseizure interactions", call. = FALSE)
        plasma <- processdoseTable(DT, noEvents, drugs, MINS_PER_DAY, FALSE)[[drug]]$wide$Plasma
        # A drug plotted above a baseline (an INR, an ACT, an osmolality)
        # starts above zero whatever the unit does, so it must move from
        # there: mannitol's "mg PO" drew its baseline alone (the same audit)
        baseline <- row$Concentration.Units %in% c("INR", "ACT", "mOsm")
        moved <- if (baseline) any(is.finite(plasma) & plasma != plasma[1]) else
          any(is.finite(plasma) & plasma > 0)
        if (!moved) "no concentration follows a dose" else ""
        }
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
  # drug by drug: internalDoseScale() is a switch(), which takes one unit
  factors <- vapply(moved, function(i) internalDoseScale(before[i]) / internalDoseScale(after[i]),
                    numeric(1))
  list(table = new, drugs = as.character(new$Drug[moved]),
       factors = stats::setNames(factors, as.character(new$Drug[moved])))
}

# The target concentrations of a dose table's TCI rows for drugs moved
# between mcg and ng (drugLibraryRescaleThresholds()), converted as their
# thresholds are: typed in the drug's units, a target of 3 mcg/mL left as 3
# became 3 ng/mL, a thousandth of the drug (Carbon's second UI audit,
# 2026-11-15, F6).  `factors` is named by drug.  Returns the table, or NULL
# when a converted target is not one the dose table can hold
# (validateDose()).
doseTableRescaleTargets <- function(dt, factors) {
  if (length(factors) == 0 || is.null(dt) || nrow(dt) == 0) return(dt)
  rows <- which(as.character(dt$Drug) %in% names(factors) & isTciUnit(dt$Units))
  for (i in rows) {
    value <- suppressWarnings(as.numeric(as.character(dt$Dose[i])))
    if (is.na(value)) next   # an unreadable cell is the grid's to refuse
    converted <- validateDose(plainDecimal(value * factors[[as.character(dt$Drug[i])]]))
    if (!nzchar(converted)) return(NULL)
    dt$Dose[i] <- converted
  }
  dt
}

# Whether a drug's concentration unit is fixed in the Drug Library: one that
# is not mcg or ng (an INR, a %, mmol...), and the drugs on the saturable
# engine, whose Vmax, Km and binding constants are in the model's own unit:
# moved to ng, phenytoin's curve was 2,225 times the mcg curve rather than
# 1,000, linagliptin's 0.0034 times rather than 0.001 (Carbon's third UI
# audit, 2026-11-15, C5).
concentrationUnitsLocked <- function(drugs, units) {
  !(units %in% c("mcg", "ng")) | vapply(drugs, drugOnSaturableEngine, logical(1))
}

# Whether a drug is simulated on the Michaelis-Menten engine: a property of the
# model (its michaelisMenten block, R/advanceMichaelisMenten.R), listed.  It
# was read from getDrugPK() at a reference adult and remembered, which in a
# fresh worker built the PK of every drug in mcg or ng the first time the Drug
# Library was drawn: 11.8 s (Carbon's fourth UI audit, 2026-11-15, C3).  The
# list is held to getDrugPK() for every drug in the library, at the reference
# adult and at a child, by tests/testthat/test-ui-audit-carbon4-engine.R, so a
# drug that moves onto or off the engine fails there until it is listed.  A
# group whose model the program chooses by age (R/modelSelection.R) is listed
# when any of its candidates is on the engine (vincristine and
# cyclophosphamide, for a child), since its unit must not move under any of
# them.
drugOnSaturableEngine <- function(drug) {
  drug %in% c("cetagliptin", "exenatide", "GHB", "linagliptin", "phenytoin",
              "vildagliptin", "voriconazole",
              "cyclophosphamide", "cyclophosphamideMcCune", "fluorouracil",
              "fluorouracilBriki", "fluorouracilTan", "ifosfamide", "paclitaxel",
              "paclitaxelCrombag", "paclitaxelJoerger", "paclitaxelProteinBound", "thioguanine", "thioguanineIV", "vincristine",
              "vincristinePediatric", "suzetrigine", "macitentan", "evolocumab")
}

# The drugs and units a dose table uses, for drugLibraryProblems(inUse =)
dosedUnits <- function(dt) {
  if (is.null(dt) || nrow(dt) == 0) return(data.frame(Drug = character(0), Units = character(0)))
  drug <- as.character(dt$Drug)
  units <- as.character(dt$Units)
  keep <- !is.na(drug) & nzchar(drug) & !is.na(units) & nzchar(units)
  data.frame(Drug = drug[keep], Units = units[keep], stringsAsFactors = FALSE)
}

# The library an edit of the Drug Library makes (`edited`, the grid's text as
# drugLibraryProblems() accepted it; `current`, the library before): the
# columns typed back, a blank MEAC 0, and the columns the grid does not show or
# lets no one change (endCe, Category, Class) from `current`.  The dialog's
# Apply and a restored link share it.
drugLibraryTable <- function(edited, current) {
  new <- edited
  for (col in c("Drug", "Concentration.Units", "Bolus.Units", "Infusion.Units",
                "Default.Units", "Units", "Color"))
    new[[col]] <- as.character(new[[col]])
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) new[[col]] <- suppressWarnings(as.numeric(new[[col]]))
  # A blank MEAC is no MEAC, which the library writes 0: kept as NA it
  # stopped every simulation of the drug ("missing value where TRUE/FALSE
  # needed" in simCpCe(); Gemini's production audit, 2026-11-08, finding 2)
  new$MEAC[is.na(new$MEAC)] <- 0
  # Class is shown but read-only: it is what makes a drug an inhaled agent
  # (gasDrugNames()), which an edit could otherwise give propofol or take
  # from sevoflurane (the same audit, finding 4).
  at <- match(new$Drug, current$Drug)
  new$endCe <- current$endCe[at]
  new$Category <- current$Category[at]
  new$Class <- current$Class[at]
  # A row with no units (a metabolite's, which the grid shows as "NA",
  # drugUnitsSimplify()) keeps none, rather than a unit called "NA"
  none <- is.na(new$Units) | trimws(new$Units) %in% c("", "NA")
  new$Units <- drugUnitsExpand(new$Units)
  new$Units[none] <- list(NA_character_)
  new
}

# The columns of the Drug Library a bookmark carries
DRUG_LIBRARY_BOOKMARK_COLUMNS <- c("Concentration.Units", "Units", "Default.Units",
                                   "Bolus.Units", "Infusion.Units", "Color",
                                   DRUG_LIBRARY_NUMERIC_COLUMNS)

# The rows of the Drug Library changed from the package's, as the grid's text,
# for a bookmark; NULL when none is.  A link that carried the dose table but
# not the library reopened a drug moved to ng in mcg, its targets and
# measured levels, converted with it, then 1000 times too high (Carbon's
# third UI audit, 2026-11-15, C4).
changedDrugLibrary <- function(drugDefaults, defaults = getDrugDefaultsGlobal()) {
  now <- drugLibraryGridData(drugDefaults)
  was <- drugLibraryGridData(defaults)
  at <- match(now$Drug, was$Drug)
  cell <- function(x) { x <- trimws(as.character(x)); x[is.na(x)] <- ""; x }
  changed <- !is.na(at) & Reduce(`|`, lapply(DRUG_LIBRARY_BOOKMARK_COLUMNS, function(col)
    cell(now[[col]]) != cell(was[[col]][at])))
  if (!any(changed)) return(NULL)
  out <- now[changed, c("Drug", DRUG_LIBRARY_BOOKMARK_COLUMNS), drop = FALSE]
  for (col in DRUG_LIBRARY_BOOKMARK_COLUMNS) out[[col]] <- cell(out[[col]])
  rownames(out) <- NULL
  out
}

# The library a link carries (changedDrugLibrary()), read as untrusted input:
# applied to the package's library and held to the Drug Library dialog's own
# rules (drugLibraryProblems()), drug by drug.  Returns a list: `table`, the
# library; `problems`, sentences; and `linkUnits`, the concentration units the
# link carries for each drug whose row could not be applied, which its
# targets, thresholds and measured concentrations are in
# (restoredLinkFactors()).  Until Carbon's fourth UI audit (2026-11-15, C2)
# it was applied all or nothing: one row the rules refused (a range converted
# to ng past 1,000,000,000) dropped every drug's row, and a target saved as
# 3000 ng/mL was then read as 3000 mcg/mL.
restoredDrugLibrary <- function(saved, defaults = getDrugDefaultsGlobal()) {
  none <- list(table = defaults, problems = character(0), linkUnits = character(0))
  if (is.null(saved)) return(none)
  unread <- list(table = defaults, linkUnits = character(0), problems = paste(
    "The link's drug library could not be read; the standard library is used,",
    "so check the dose table's units and targets."))
  saved <- tryCatch(as.data.frame(saved, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(saved) || !"Drug" %in% names(saved) || nrow(saved) > nrow(defaults)) return(unread)
  edited <- drugLibraryGridData(defaults)
  rejected <- character(0)
  linkUnits <- character(0)
  for (i in seq_len(nrow(saved))) {
    drug <- asScalarString(saved$Drug[[i]])
    k <- match(drug, edited$Drug)
    if (is.na(k)) next
    candidate <- edited
    for (col in intersect(DRUG_LIBRARY_BOOKMARK_COLUMNS, names(saved))) {
      v <- saved[[col]][[i]]
      if (is.numeric(v) && length(v) == 1) v <- as.character(v)
      candidate[[col]][k] <- asScalarString(v)
    }
    problems <- tryCatch(drugLibraryProblems(candidate, defaults), error = function(e) "unreadable")
    # The dialog checks a row whose units change after converting the bound
    # it leaves (input$drugEditsOK), which a link's row skips: its bounds are
    # already in the units it carries, and are checked so (CodeRabbit on PR
    # #730)
    lower <- suppressWarnings(as.numeric(candidate$Lower[k]))
    upper <- suppressWarnings(as.numeric(candidate$Upper[k]))
    if (isTRUE(is.finite(lower) && is.finite(upper) && lower > upper))
      problems <- c(problems, "Lower above Upper")
    if (length(problems) == 0) {
      edited <- candidate
    } else {
      rejected <- c(rejected, drug)
      if ("Concentration.Units" %in% names(saved))
        linkUnits[[drug]] <- asScalarString(saved$Concentration.Units[[i]])
    }
  }
  table <- drugLibraryTable(edited, defaults)
  converted <- names(restoredLinkFactors(linkUnits, table))
  problems <- if (length(rejected) > 0) paste0(
    "The link's Drug Library entries for ", paste(unique(rejected), collapse = ", "),
    " could not be used, and the standard entries are",
    if (length(converted) > 0) paste0(
      "; the targets, thresholds and measured concentrations of ",
      paste(converted, collapse = ", "), " were converted to the standard units"),
    ".")
  list(table = table, problems = problems, linkUnits = linkUnits)
}

# The factors that bring a link's numbers for each drug from the units the
# link carries (restoredDrugLibrary()$linkUnits) into the units in force,
# named by drug: only mcg and ng, and only where they differ
restoredLinkFactors <- function(linkUnits, drugDefaults) {
  factors <- numeric(0)
  for (drug in names(linkUnits)) {
    from <- linkUnits[[drug]]
    to <- drugDefaults$Concentration.Units[match(drug, drugDefaults$Drug)]
    if (isTRUE(from %in% c("mcg", "ng")) && isTRUE(to %in% c("mcg", "ng")) && from != to)
      factors[[drug]] <- mcgPerMlScale(from) / mcgPerMlScale(to)
  }
  factors
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
