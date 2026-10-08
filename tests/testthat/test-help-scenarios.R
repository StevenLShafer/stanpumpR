# Teaching scenarios: definitions, pages and loading.  See R/help-scenarios.R.

scenarios <- helpScenarios()
drugDefaults <- getDrugDefaultsGlobal()

test_that("there are scenarios in every group, with unique slug ids", {
  expect_true(length(scenarios) >= 15)
  ids <- helpScenarioIds()
  expect_false(any(duplicated(ids)))
  expect_true(all(grepl("^[a-z0-9]+(-[a-z0-9]+)*$", ids)))
  groups <- vapply(scenarios, `[[`, character(1), "group")
  expect_setequal(unique(groups), HELP_SCENARIO_GROUPS)
})

test_that("every scenario is consistent with the drug library", {
  for (s in scenarios) {
    problems <- helpScenarioCheck(s, drugDefaults)
    expect_length(problems, 0)
    if (length(problems)) message(s$id, ": ", paste(problems, collapse = "; "))
  }
})

test_that("every scenario has a narrative, and every narrative a scenario", {
  ids <- helpScenarioIds()
  for (id in ids) {
    expect_true(helpHasMarkdown(paste0("scenarios/", id)), info = paste("missing inst/help/scenarios/", id, ".md"))
    text <- helpReadMarkdown(paste0("scenarios/", id))
    expect_match(text, "## What to look for", fixed = TRUE, info = id)
  }
  files <- sub("^scenarios/", "", grep("^scenarios/", helpMarkdownIds(), value = TRUE))
  expect_true(all(files %in% ids), info = paste("orphan:", setdiff(files, ids)))
})

test_that("the checker catches bad scenarios", {
  s <- helpScenario("bad-one", "Bad", "Opioids", "x",
                    doses = helpDoses(c("propofol", 0, 10, "mcg/kg/hr")))
  expect_match(helpScenarioCheck(s, drugDefaults), "unit not offered", all = FALSE)
  s <- helpScenario("bad-two", "Bad", "Opioids", "x", age = 200,
                    doses = helpDoses(c("propofol", 90, 10, "mg")), maximum = 60)
  problems <- helpScenarioCheck(s, drugDefaults)
  expect_match(problems, "age out of range", all = FALSE)
  expect_match(problems, "beyond Max time", all = FALSE)
  s <- helpScenario("Bad Id", "Bad", "Nope", "x", doses = helpDoses(c("nodrug", 0, 1, "mg")),
                    maximum = 61, normalization = "Peak plasma", showThreshold = TRUE)
  problems <- helpScenarioCheck(s, drugDefaults)
  expect_match(problems, "slug", all = FALSE)
  expect_match(problems, "unknown group", all = FALSE)
  expect_match(problems, "unknown drug", all = FALSE)
  expect_match(problems, "Max time choices", all = FALSE)
  expect_match(problems, "showThreshold needs", all = FALSE)
  s <- helpScenario("gas", "Gas", "Inhaled anesthetics", "x",
                    doses = helpDoses(c("sevoflurane", 0, 2, "%")))
  expect_match(helpScenarioCheck(s, drugDefaults), "ventilation", all = FALSE)
})

test_that("the dose table a scenario produces is what the app expects", {
  s <- helpScenarioById("propofol-induction-maintenance")
  dt <- helpScenarioDoseTable(s)
  expect_equal(names(dt), c("Drug", "Time", "Dose", "Units"))
  expect_true(all(vapply(dt, is.character, logical(1))))
  expect_equal(nrow(dt), nrow(s$doses) + 3)
  expect_equal(tail(dt$Drug, 3), c("", "", ""))
  # The app's own cleaner keeps exactly the dosed rows
  clean <- cleanDoseTable(dt)
  expect_equal(nrow(clean), nrow(s$doses))
  expect_equal(clean$Dose, s$doses$Dose[order(s$doses$Time, s$doses$Drug)])
  # Times and doses survive the app's cell validators unchanged
  expect_equal(as.numeric(vapply(clean$Time, validateTime, character(1))), as.numeric(clean$Time))
  expect_equal(as.numeric(vapply(as.character(clean$Dose), validateDose, character(1))), clean$Dose)
  expect_true(all(validateDoseTableInput(dt, drugDefaults)))
})

test_that("scenario dose tables in minutes are written exactly as before time units", {
  # minutesToDisplayTime() in minutes is as.character() for every time the
  # scenarios use (it differs only where as.character() would write 1e+05)
  for (s in Filter(function(s) s$options$timeUnits == "minutes", scenarios)) {
    d <- s$doses[order(s$doses$Time, s$doses$Drug), ]
    expect_identical(helpScenarioDoseTable(s, blankRows = 0)$Time, as.character(d$Time), info = s$id)
  }
})

test_that("every scenario's dose times survive being written in its own unit", {
  # The long-term scenarios are written in days; whatever the unit, the app
  # must read back exactly the minutes the scenario defines.
  for (s in scenarios) {
    d <- s$doses[order(s$doses$Time, s$doses$Drug), ]
    written <- helpScenarioDoseTable(s, blankRows = 0)$Time
    expect_identical(
      displayTimeToMinutes(written, REFERENCE_TIME_NONE, s$options$timeUnits),
      as.numeric(d$Time),
      info = s$id
    )
  }
})

test_that("a scenario in another time unit writes and checks its times in that unit", {
  s <- helpScenario("long-one", "Long", "Opioids", "x",
                    doses = helpDoses(c("morphine", 0, 10, "mg"), c("morphine", 30240, 10, "mg")),
                    timeUnits = "days", maximum = 28 * MINS_PER_DAY)
  expect_length(helpScenarioCheck(s, drugDefaults), 0)
  dt <- helpScenarioDoseTable(s)
  expect_equal(dt$Time[1:2], c("0", "21"))
  expect_identical(displayTimeToMinutes(dt$Time[1:2], REFERENCE_TIME_NONE, "days"), c(0, 30240))
  expect_true(all(validateDoseTableInput(dt, drugDefaults)))

  # Max time must be one of the scenario's unit's choices
  s$options$maximum <- 1440
  expect_match(helpScenarioCheck(s, drugDefaults), "Max time choices", all = FALSE)
  s$options$timeUnits <- "fortnights"
  expect_match(helpScenarioCheck(s, drugDefaults), "invalid timeUnits", all = FALSE)

  # The page: times in the unit, in full (three significant figures printed
  # 30240 minutes as 30,200), and the unit named
  s <- helpScenario("long-one", "Long", "Opioids", "x",
                    doses = helpDoses(c("morphine", 30240, 10, "mg")),
                    timeUnits = "days", maximum = 28 * MINS_PER_DAY)
  local_mocked_bindings(
    helpScenarioById = function(id) s,
    helpReadMarkdown = function(id) "## What to look for"
  )
  html <- helpScenarioPageHTML("long-one")
  expect_match(html, "Time (days)", fixed = TRUE)
  expect_match(html, "<td>21</td>", fixed = TRUE)
  expect_match(html, "28 days", fixed = TRUE)
  s$options$timeUnits <- "minutes"
  s$options$maximum <- 1440
  expect_match(helpScenarioPageHTML("long-one"), "<td>30240</td>", fixed = TRUE)
})

test_that("the checker mirrors the app's rule for TCI and inhaled agents", {
  # Simulated only on plots of 7 days or less (timeUnitViolation())
  tci <- helpScenario("tci-long", "TCI", "Opioids", "x",
                      doses = helpDoses(c("propofol", 0, 3, "Effect site target")),
                      timeUnits = "days", maximum = 14 * MINS_PER_DAY)
  expect_match(helpScenarioCheck(tci, drugDefaults), "7 days or less", all = FALSE)
  tci$options$maximum <- 7 * MINS_PER_DAY
  expect_length(helpScenarioCheck(tci, drugDefaults), 0)
  gas <- helpScenario("gas-long", "Gas", "Inhaled anesthetics", "x",
                      doses = helpDoses(c("sevoflurane", 0, 2, "%"), c("ventilation", 0, 5, "L/min")),
                      timeUnits = "weeks", maximum = 4 * MINS_PER_WEEK)
  expect_match(helpScenarioCheck(gas, drugDefaults), "7 days or less", all = FALSE)
  # events must fall on the plot too
  ev <- helpScenario("ev", "Ev", "Opioids", "x", doses = helpDoses(c("propofol", 0, 1, "mg")),
                     events = helpEvents(c(90, "Induction")), maximum = 60,
                     addedPlots = PLOT_ID_EVENTS)
  expect_match(helpScenarioCheck(ev, drugDefaults), "event at 90 is beyond Max time", all = FALSE)
})

test_that("event tables are produced for scenarios with and without events", {
  expect_identical(helpScenarioEventTable(helpScenarioById("propofol-bolus")), eventTableInit)
  et <- helpScenarioEventTable(helpScenarioById("tiva-remifentanil-propofol"))
  expect_equal(names(et), c("Time", "Event"))
  expect_true("Intubation" %in% et$Event)
})

test_that("every scenario runs through the simulation engine", {
  for (s in scenarios) {
    p <- s$patient
    d <- s$doses
    iv <- d[!isGasDrug(d$Drug), ]
    if (nrow(iv) > 0) {
      out <- simulateDrugsWithCovariates(iv, helpScenarioEventTable(s), p$weight, p$height, p$age, p$sex,
                                         s$options$maximum, s$options$showThreshold,
                                         cyp2d6 = p$cyp2d6, adjustToFFM = p$adjustToFFM)
      # every dosed drug has a row; a drug that forms an active metabolite adds
      # the metabolite's row too
      expect_true(all(unique(iv$Drug) %in% names(out)), info = s$id)
      expect_true(all(setdiff(names(out), iv$Drug) %in% drugDefaults$Drug), info = s$id)
      for (drug in names(out)) {
        res <- out[[drug]]$results
        # A prodrug (codeine, tramadol) has no effect site of its own, so its
        # effect-site series is NA by design; the effect is on its metabolite's
        # row. Require finite, positive values where the series is present, and
        # no non-finite values that are not NA (no Inf/NaN).
        expect_false(any(is.nan(res$Y) | is.infinite(res$Y)), info = paste(s$id, drug))
        finiteY <- res$Y[is.finite(res$Y)]
        expect_true(length(finiteY) > 0, info = paste(s$id, drug, "has no finite values"))
        expect_true(any(finiteY > 0), info = paste(s$id, drug, "is all zero"))
      }
    }
    gas <- d[isGasDrug(d$Drug), ]
    if (nrow(gas) > 0) {
      sim <- simulateGases(gas, weight = p$weight, age = p$age, maximum = s$options$maximum)
      expect_false(is.null(sim), info = s$id)
    }
  }
})

test_that("scenario pages and the index render with load buttons", {
  for (s in scenarios) {
    html <- helpScenarioPageHTML(s$id)
    expect_match(html, sprintf('data-help-scenario="%s"', s$id), fixed = TRUE, info = s$id)
    expect_match(html, htmltools::htmlEscape(s$summary), fixed = TRUE, info = s$id)
    expect_match(html, "What to look for", fixed = TRUE, info = s$id)
    # Max time as the unit's Max time list labels it, and the times' unit
    expect_match(html, maxTimeLabel(s$options$maximum, s$options$timeUnits), fixed = TRUE, info = s$id)
    expect_match(html, paste0("Time (", s$options$timeUnits, ")"), fixed = TRUE, info = s$id)
    for (drug in unique(s$doses$Drug)) {
      expect_match(html, sprintf('data-help-page="drugs/%s"', drug), fixed = TRUE, info = s$id)
    }
  }
  index <- helpScenarioIndexHTML()
  for (s in scenarios) {
    expect_match(index, sprintf('data-help-page="scenarios/%s"', s$id), fixed = TRUE)
    expect_match(index, sprintf('data-help-scenario="%s"', s$id), fixed = TRUE)
  }
  for (g in HELP_SCENARIO_GROUPS) expect_match(index, htmltools::htmlEscape(g), fixed = TRUE)
  expect_match(helpScenarioPageHTML("no-such"), "no scenario called")
  expect_null(helpScenarioById("no-such"))
  expect_null(helpScenarioById(NULL))
})

test_that("drug pages list the scenarios that use the drug", {
  html <- helpDrugPageHTML("propofol")
  expect_match(html, 'data-help-page="scenarios/propofol-bolus"', fixed = TRUE)
  expect_false(grepl("scenarios/naloxone-morphine", html, fixed = TRUE))
})

test_that("loading a scenario sets the tables and sends the input updates", {
  skip_if_not(exists("MockShinySession", asNamespace("shiny")), "MockShinySession not available")
  session <- shiny::MockShinySession$new()
  doseTable <- shiny::reactiveVal(doseTableInit)
  eventTable <- shiny::reactiveVal(eventTableInit)
  s <- helpScenarioById("tiva-remifentanil-propofol")

  applyHelpScenario(session, s, doseTable, eventTable)

  expect_identical(shiny::isolate(doseTable()), helpScenarioDoseTable(s))
  expect_identical(shiny::isolate(eventTable()), helpScenarioEventTable(s))
})

test_that("with the app's timeApi, a scenario sets its time settings and the table's format", {
  skip_if_not(exists("MockShinySession", asNamespace("shiny")), "MockShinySession not available")
  session <- shiny::MockShinySession$new()
  doseTable <- shiny::reactiveVal(doseTableInit)
  eventTable <- shiny::reactiveVal(eventTableInit)
  calls <- list()
  timeApi <- list(
    showTimeSettings = function(unit, mode, maximum) {
      calls$show <<- list(unit = unit, mode = mode, maximum = maximum)
    },
    setDoseTable = function(dt, format) calls$set <<- list(dt = dt, format = format)
  )
  s <- helpScenarioById("tiva-remifentanil-propofol")

  applyHelpScenario(session, s, doseTable, eventTable, timeApi)

  expect_equal(calls$show, list(unit = "minutes", mode = "relative", maximum = s$options$maximum))
  expect_identical(calls$set$dt, helpScenarioDoseTable(s))
  expect_equal(calls$set$format, timeFormat("minutes", "relative"))
  # the dose table goes through timeApi, not straight into doseTable()
  expect_identical(shiny::isolate(doseTable()), doseTableInit)
  expect_identical(shiny::isolate(eventTable()), helpScenarioEventTable(s))
})
