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
                                         s$options$maximum, s$options$showThreshold)
      expect_setequal(names(out), unique(iv$Drug))
      for (drug in names(out)) {
        res <- out[[drug]]$results
        expect_true(all(is.finite(res$Y)), info = paste(s$id, drug))
        expect_true(any(res$Y > 0), info = paste(s$id, drug, "is all zero"))
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
    expect_match(html, formatMinutes(s$options$maximum), fixed = TRUE, info = s$id)
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
