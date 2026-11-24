# The Patient Profile's Infection (R/infection.R; Steven L. Shafer,
# 2026-11-18).  It was built ahead of its first reader, imipenem and
# ceftolozane/tazobactam of the library expansion, so no drug in the library
# reads it yet: these tests give cefepime an infection term of their own (a
# mocked model that doubles V1 in any infection) and check that the field
# reaches exactly such a model, on every path a covariate takes.

# A mocked reader: cefepime's own model, with an infection term.  The mock
# is never shipped; the doubling stands for whatever a real source fits.
realCefepime <- cefepime
cefepimeReadingInfection <- function(weight, height, age, sex, adjustToFFM = TRUE,
                                     creatinine = NULL, pregnant = FALSE,
                                     infection = INFECTION_DEFAULT) {
  X <- realCefepime(weight, height, age, sex, adjustToFFM, creatinine, pregnant)
  if (!identical(infection, INFECTION_NONE))
    X$PK$default$v1 <- 2 * X$PK$default$v1
  X$reference <- paste0(X$reference, "; infection ", infection)
  X
}

# The readers of each field are cached by drug list, which a mock does not
# change, so the cache is emptied as the mock goes in and as it comes out
withInfectionReader <- function(env = parent.frame()) {
  patientFieldReaderCache$key <- NULL
  local_mocked_bindings(cefepime = cefepimeReadingInfection, .env = env)
  withr::defer(patientFieldReaderCache$key <- NULL, envir = env)
}

test_that("the categories are the union the two sources need, the default among them", {
  expect_identical(INFECTION_VALUES,
                   c("none", "cUTI", "cIAI", "pneumonia", "pneumoniaVentilated", "other"))
  expect_false(anyDuplicated(names(INFECTION_LABELS)) > 0)
  expect_true(INFECTION_DEFAULT %in% INFECTION_VALUES)
  # Shafer's decision of 2026-11-24 (R/infection.R, THE DEFAULT)
  expect_identical(INFECTION_DEFAULT, INFECTION_NONE)
  expect_identical(infectionLabel(INFECTION_PNEUMONIA_VENTILATED),
                   "Pneumonia, mechanically ventilated")
  # the header records the decision (the source is not in the built package
  # R CMD check tests, so this is checked from the checkout only)
  source <- test_path("..", "..", "R", "infection.R")
  skip_if_not(file.exists(source), "R/infection.R is not in the built package")
  header <- readLines(source)
  expect_true(any(grepl("THE DEFAULT (decided by Steven L. Shafer, 2026-11-24)", header,
                        fixed = TRUE)))
})

test_that("checkInfection() takes one category and nothing else", {
  for (x in INFECTION_VALUES) expect_identical(checkInfection(x), x)
  expect_identical(checkInfection(NULL), INFECTION_DEFAULT)
  for (bad in list("sepsis", "Pneumonia", "", NA_character_, NA, 1, TRUE,
                   INFECTION_VALUES, list("none"), character(0)))
    expect_error(checkInfection(bad), "Invalid infection")
})

test_that("getDrugPK() refuses an unknown infection and passes a known one only to a reader", {
  expect_error(getDrugPK("propofol", 70, 170, 50, "male", infection = "sepsis"),
               "Invalid infection")
  # a drug whose model does not read the field is unchanged by it
  for (x in INFECTION_VALUES)
    expect_identical(getDrugPK("propofol", 70, 170, 50, "male", infection = x)$PK,
                     getDrugPK("propofol", 70, 170, 50, "male")$PK)
  expect_identical(getDrugPK("cefepime", 70, 170, 50, "male", infection = "pneumonia")$PK,
                   getDrugPK("cefepime", 70, 170, 50, "male")$PK)

  withInfectionReader()
  none <- getDrugPK("cefepime", 70, 170, 50, "male")
  expect_match(none$reference, "infection none", fixed = TRUE)
  for (x in setdiff(INFECTION_VALUES, INFECTION_NONE)) {
    sick <- getDrugPK("cefepime", 70, 170, 50, "male", infection = x)
    expect_match(sick$reference, paste("infection", x), fixed = TRUE)
    expect_equal(sick$PK$default$v1, 2 * none$PK$default$v1)
  }
})

test_that("the field is shown and its tip names its readers only while a reader is in the table", {
  # no drug reads it yet: never shown, and the tip says so
  expect_length(patientFieldReaders("infection"), 0)
  expect_false(relevantFields(getDrugDefaultsGlobal()$Drug)[["infection"]])
  expect_match(patientFieldTooltip("infection"), "No drug in the library reads it.", fixed = TRUE)
  expect_true("infection" %in% RELEVANT_FIELDS)

  withInfectionReader()
  expect_identical(patientFieldReaders("infection"), "cefepime")
  expect_true(relevantFields("cefepime")[["infection"]])
  expect_true(relevantFields(c("propofol", "cefepime"))[["infection"]])
  expect_false(relevantFields(c("propofol", "fentanyl"))[["infection"]])
  tip <- patientFieldTooltip("infection")
  expect_match(tip, "Read by cefepime; no other drug is affected.", fixed = TRUE)
  # and the Cautions page's generated table
  table <- helpFieldReadersTableMarkdown()
  expect_match(table, "| Infection | 1 | [cefepime](help:drugs/cefepime) |", fixed = TRUE)
})

test_that("the Patient Profile offers every category, the default selected, in a hideable wrapper", {
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  expect_match(html, 'id="field_infection"', fixed = TRUE)
  for (i in seq_along(INFECTION_LABELS))
    expect_match(html, sprintf('<option value="%s"', INFECTION_LABELS[[i]]), fixed = TRUE)
  expect_match(html, sprintf('<option value="%s" selected>', INFECTION_DEFAULT), fixed = TRUE)
})

test_that("the server reads only a category the control offers (a link is untrusted)", {
  shiny::testServer(app_server, {
    expect_identical(infection(), INFECTION_DEFAULT)  # before the control reports
    session$setInputs(infection = "pneumoniaVentilated")
    expect_identical(infection(), "pneumoniaVentilated")
    for (forged in list("sepsis", c("cUTI", "cIAI"), list("cUTI"), 3, ""))
    {
      session$setInputs(infection = forged)
      expect_identical(infection(), INFECTION_DEFAULT)
    }
  })
})

test_that("a link carrying the field opens, whatever it carries", {
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  ui <- app_ui()
  build <- function(query) {
    ctx <- shiny:::RestoreContext$new(query)
    shiny:::withRestoreContext(ctx, ui(list(QUERY_STRING = query)))
  }
  page <- build("?_inputs_&infection=%22cIAI%22")
  expect_false(inherits(page, "httpResponse"))
  expect_match(as.character(htmltools::renderTags(page)$html),
               '<option value="cIAI" selected>', fixed = TRUE)
  # a value the page cannot be built from is answered as any such link is
  # (unreadableLinkResponse()); an unknown category builds the page, and the
  # server then reads the default
  bad <- build("?_inputs_&infection=%5B1%2C2%5D")
  if (inherits(bad, "httpResponse")) expect_equal(bad$status, 400L)
  expect_false(inherits(build("?_inputs_&infection=%22sepsis%22"), "httpResponse"))
})

test_that("simulateDrugsWithCovariates() and the Simulation API pass it to a reader", {
  withInfectionReader()
  dose <- data.frame(Drug = "cefepime", Time = 0, Dose = 2000, Units = "mg")
  at <- function(infection) {
    out <- simulateDrugsWithCovariates(dose, data.frame(Time = double(), Event = character()),
                                       70, 170, 50, "male", 60, FALSE,
                                       infection = infection)
    out$cefepime$wide$Plasma[match(1, out$cefepime$wide$Time)]
  }
  expect_lt(at("pneumonia"), at("none"))

  r <- apiSimulate(list(patient = list(weight = 70, height = 170, age = 50, sex = "male",
                                       infection = "cUTI"),
                        duration = 60, doses = dose, output = list(times = 1)))
  expect_identical(r$patient$infection, "cUTI")
  expect_false(any(grepl("infection", r$warnings)))
  healthy <- apiSimulate(list(patient = list(weight = 70, height = 170, age = 50, sex = "male"),
                              duration = 60, doses = dose, output = list(times = 1)))
  expect_identical(healthy$patient$infection, INFECTION_DEFAULT)
  expect_lt(r$timeseries$Plasma, healthy$timeseries$Plasma)
  err <- tryCatch(apiSimulate(list(patient = list(weight = 70, height = 170, age = 50,
                                                  sex = "male", infection = "sepsis"),
                                   duration = 60, doses = dose)),
                  stanpumpr_api_error = function(e) e)
  expect_s3_class(err, "stanpumpr_api_error")
  expect_match(conditionMessage(err), "patient.infection must be one of")
})

test_that("a teaching scenario carries it, checks it and sets it on load", {
  s <- helpScenario("x-y", "T", HELP_SCENARIO_GROUPS[1], "S",
                    doses = helpDoses(c("propofol", 0, 100, "mg")))
  expect_identical(s$patient$infection, INFECTION_DEFAULT)
  expect_length(helpScenarioCheck(s), 0)
  s$patient$infection <- "sepsis"
  expect_true(any(grepl("invalid infection", helpScenarioCheck(s))))

  session <- shiny::MockShinySession$new()
  session$messages <- list()
  unlockBinding("sendInputMessage", session)
  session$sendInputMessage <- function(inputId, message) {
    session$messages[[inputId]] <- message
    invisible()
  }
  lockBinding("sendInputMessage", session)
  scenario <- helpScenarioById("pregabalin-linear-absorption")
  scenario$patient$infection <- INFECTION_CIAI
  applyHelpScenario(session, scenario, shiny::reactiveVal(doseTableInit),
                    shiny::reactiveVal(eventTableInit))
  expect_identical(session$messages$infection$value, INFECTION_CIAI)
  # shown on the scenario's page only when it is not the default
  expect_false(grepl("Infection", helpScenarioPageHTML("pregabalin-linear-absorption"),
                     fixed = TRUE))
})
