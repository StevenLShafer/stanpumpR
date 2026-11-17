# The findings of Carbon's (Gemini) fourth adversarial UI audit, of PR #730
# at 118bfd4 (2026-11-15), in the app server, the Bayesian update, the Drug
# Library, the plot and Suggest Dosing.  Each was reproduced before it was
# fixed; each test here fails on 59094cf.  The browser's, Single Paper
# Review's and the API's, the gas engine's and the engines' are in
# test-ui-audit-carbon4-*.R.  Drafted by Claude Code, 2026-11-15, at the
# request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

noEvents <- data.frame(Time = numeric(0), Event = character(0))
G <- getDrugDefaultsGlobal()

c4Inputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "1440",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)
c4Start <- function(session, dt) {
  do.call(session$setInputs, c4Inputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}
c4Notices <- function(session) {
  sent <- new.env()
  sent$messages <- character(0)
  session$sendNotification <- function(type, message) {
    text <- if (is.list(message)) message$html else message
    sent$messages <- c(sent$messages, paste(as.character(text), collapse = " "))
  }
  sent
}
c4Cells <- function(x) lapply(seq_len(nrow(x)), function(i)
  lapply(x[i, ], function(v) if (is.na(v)) "" else as.character(v)))
c4Priors <- function(DT, asm = FALSE) {
  recalculatePK(NULL, G, DT, age = 50, weight = 70, height = 170, sex = "male",
                antiseizureInteractions = asm)
}
c4Rebuild <- function(DT) {
  function(drug, individual) {
    recalculatePK(NULL, G, DT[DT$Drug == drug, , drop = FALSE], age = 50, weight = 70,
                  height = 170, sex = "male", individual = individual)[[drug]]
  }
}
c4At <- function(out, name, times) out[[name]]$wide$Plasma[match(times, out[[name]]$wide$Time)]

# --- H3: the Procedure start moves the events as it moves the doses ----------

test_that("events keep their clock times as the Procedure start moves, and come back (P4-H3)", {
  # an event at 09:00 after an 08:00 start, and one past the first day
  expect_equal(eventTimesForStart(c(60, 2000), "08:00", "08:30"), c(30, 2000))
  expect_equal(eventTimesForStart(c(60, 2000), "08:00", "10:00"), c(1380, 2000))
  expect_equal(eventTimesForStart(c(1380, 2000), "10:00", "08:00"), c(60, 2000))
  expect_equal(eventTimesForStart(60, "08:00", ""), 60)
  shiny::testServer(app_server, {
    c4Start(session, data.frame(Drug = "propofol", Time = "0", Dose = "1", Units = "mg"))
    session$setInputs(timeMode = "clock", referenceTime = "08:00")
    eventTable(data.frame(Time = c(60, 2000), Event = c("Intubation", "Emergence"),
                          stringsAsFactors = FALSE))
    # typed a character at a time, each a readable start on the way
    for (r in c("08:3", "08:30", "08:45", "10:00")) session$setInputs(referenceTime = r)
    expect_equal(eventTable()$Time, c(1380, 2000))
    session$setInputs(referenceTime = "08:00")
    expect_equal(eventTable()$Time, c(60, 2000))
  })
})

# --- H4: a scenario in a session whose library shows a drug in other units ---

test_that("a scenario's targets are converted to the units the Drug Library shows (P4-H4)", {
  s <- helpScenarioById("tci-propofol")
  dd <- G
  dd$Concentration.Units[dd$Drug == "propofol"] <- "ng"
  out <- helpScenarioInSessionUnits(s, dd)
  tci <- isTciUnit(s$doses$Units)
  expect_equal(as.numeric(out$doses$Dose[tci]), 1000 * as.numeric(s$doses$Dose[tci]))
  expect_identical(out$converted, "propofol")
  expect_identical(helpScenarioInSessionUnits(s, G)$doses, s$doses)
  shiny::testServer(app_server, {
    c4Start(session, data.frame(Drug = "propofol", Time = "0", Dose = "1", Units = "mg"))
    drugDefaults(dd)
    expect_true(helpApi$loadScenario(s))
    targets <- doseTable()[isTciUnit(doseTable()$Units), ]
    expect_equal(as.numeric(targets$Dose), 1000 * as.numeric(s$doses$Dose[tci]))
  })
})

# --- C1: the Drug Library and the dose table's history ------------------------

test_that("a change of units ends the dose table's undo and redo (P4-C1)", {
  shiny::testServer(app_server, {
    sent <- c4Notices(session)
    c4Start(session, data.frame(Drug = "fentanyl", Time = "0", Dose = "1", Units = "mcg"))
    committed <- doseTable()
    target <- rbind(committed, data.frame(Drug = "propofol", Time = "0", Dose = "3",
                                          Units = TCI_UNIT_EFFECT))
    doseTableHistory()$do(target)
    doseTableHistory()$undo()
    expect_true(doseTableHistory()$can_redo)
    shown <- drugLibraryGridData(drugDefaults())
    shown$Concentration.Units[shown$Drug == "propofol"] <- "ng"
    session$setInputs(editDrugsHTML = tableGridInputValue(c4Cells(shown)), drugEditsOK = 1)
    expect_identical(drugDefaults()$Concentration.Units[drugDefaults()$Drug == "propofol"], "ng")
    # the 3 typed in mcg/mL cannot come back as 3 ng/mL
    expect_false(doseTableHistory()$can_redo)
    expect_false(session$isClosed())
  })
})

test_that("a unit switch is refused for a measured value without a drug, or a number past the ceiling (P4-C1, P4-C2)", {
  shiny::testServer(app_server, {
    sent <- c4Notices(session)
    c4Start(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    bayes$rows(list(rows = data.frame(Drug = "", Time = "30", Concentration = "3",
                                      stringsAsFactors = FALSE),
                    format = timeFormat("minutes", "relative")))
    shown <- drugLibraryGridData(drugDefaults())
    shown$Concentration.Units[shown$Drug == "propofol"] <- "ng"
    session$setInputs(editDrugsHTML = tableGridInputValue(c4Cells(shown)), drugEditsOK = 1)
    expect_identical(drugDefaults()$Concentration.Units[drugDefaults()$Drug == "propofol"], "mcg")
    expect_true(any(grepl("no drug chosen yet", sent$messages, fixed = TRUE)))
  })
  shiny::testServer(app_server, {
    sent <- c4Notices(session)
    c4Start(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    big <- drugDefaults()
    big$Upper[big$Drug == "propofol"] <- 5e6
    drugDefaults(big)
    shown <- drugLibraryGridData(drugDefaults())
    shown$Concentration.Units[shown$Drug == "propofol"] <- "ng"
    session$setInputs(editDrugsHTML = tableGridInputValue(c4Cells(shown)), drugEditsOK = 1)
    expect_identical(drugDefaults()$Concentration.Units[drugDefaults()$Drug == "propofol"], "mcg")
    expect_true(any(grepl("range, MEAC or threshold", sent$messages, fixed = TRUE)))
  })
})

# --- C2: a link's library, drug by drug ----------------------------------------

test_that("a link's library is applied drug by drug, and an unapplied drug's numbers are converted (P4-C2)", {
  edited <- G
  edited$Concentration.Units[edited$Drug == "propofol"] <- "ng"
  edited$Upper[edited$Drug == "propofol"] <- 5e9   # as an older link could carry
  edited$Color[edited$Drug == "fentanyl"] <- "#123456"
  saved <- changedDrugLibrary(edited)
  back <- restoredDrugLibrary(saved)
  # fentanyl's edit is kept although propofol's row is refused
  expect_identical(back$table$Color[back$table$Drug == "fentanyl"], "#123456")
  expect_identical(back$table$Concentration.Units[back$table$Drug == "propofol"], "mcg")
  expect_identical(back$linkUnits, c(propofol = "ng"))
  expect_match(back$problems, "propofol", fixed = TRUE)
  factors <- restoredLinkFactors(back$linkUnits, back$table)
  expect_equal(factors, c(propofol = 0.001))
  dt <- data.frame(Drug = "propofol", Time = "0", Dose = "3000", Units = TCI_UNIT_EFFECT)
  expect_identical(doseTableRescaleTargets(dt, factors)$Dose, "3")
})

# --- H7: a parent and its metabolite, both measured ------------------------------

test_that("a measured parent and metabolite are each drawn as fitted, in either order (P4-H7)", {
  obs <- data.frame(Drug = c("codeine", "codeine", "morphine", "morphine"),
                    Time = c(60, 240, 120, 240), Concentration = c(150, 60, 0.004, 0.003))
  for (ord in list(c("codeine", "morphine"), c("morphine", "codeine"))) {
    DT <- data.frame(Drug = ord, Time = 0, Dose = ifelse(ord == "codeine", 60, 0),
                     Units = ifelse(ord == "codeine", "mg PO", "mg"), stringsAsFactors = FALSE)
    res <- bayesianUpdateDrugs(c4Priors(DT), DT, noEvents, obs, rebuild = c4Rebuild(DT))
    out <- withOutputTimes(obs$Time, processdoseTable(DT, noEvents, res$drugs, 600, FALSE))
    pure <- withOutputTimes(obs$Time, processdoseTable(DT, noEvents, c4Priors(DT), 600, FALSE))
    f <- res$fits$morphine
    expect_lt(f$clearance, 0.9)
    expect_equal(c4At(out, "morphine", f$observations$Time), f$observations$Individual,
                 tolerance = 1e-6, label = paste(ord, collapse = " then "))
    expect_equal(c4At(out, "morphine (population)", f$observations$Time),
                 c4At(pure, "morphine", f$observations$Time), tolerance = 1e-6)
    expect_equal(f$observations$Population, c4At(pure, "morphine", f$observations$Time),
                 tolerance = 1e-6)
  }
})

# --- H8: the Bayesian update with the antiseizure interactions -------------------

test_that("interacting drugs measured together are drawn as fitted, their population lines the model's (P4-H8)", {
  DT <- data.frame(Drug = c("valproate", "lamotrigine"), Time = 0, Dose = c(500, 100),
                   Units = c("mg PO bid", "mg PO qd"), stringsAsFactors = FALSE)
  t14 <- 14 * 1440
  obs <- data.frame(Drug = c("valproate", "lamotrigine"), Time = t14, Concentration = c(60, 4))
  res <- bayesianUpdateDrugs(c4Priors(DT, TRUE), DT, noEvents, obs, rebuild = c4Rebuild(DT),
                             antiseizureInteractions = TRUE, maximum = 15 * 1440)
  out <- withOutputTimes(t14, processdoseTable(DT, noEvents, res$drugs, 15 * 1440, FALSE,
                                               antiseizureInteractions = TRUE))
  pure <- withOutputTimes(t14, processdoseTable(DT, noEvents, c4Priors(DT, TRUE), 15 * 1440, FALSE,
                                                antiseizureInteractions = TRUE))
  for (d in c("valproate", "lamotrigine")) {
    f <- res$fits[[d]]
    expect_false(isTRUE(all.equal(f$clearance, 1)), label = d)
    expect_equal(c4At(out, d, t14), f$observations$Individual, tolerance = 2e-3, label = d)
    expect_equal(c4At(out, bayesianPriorName(d), t14), c4At(pure, d, t14), tolerance = 1e-6,
                 label = paste(d, "population"))
  }
})

test_that("with the interactions on, another drug's dose is not the drug's, nor its TCI limit (P4-H8)", {
  DT <- data.frame(Drug = c("carbamazepine", "lamotrigine"), Time = 0, Dose = c(200, 0),
                   Units = c("mg PO bid", "mg PO qd"), stringsAsFactors = FALSE)
  obs <- data.frame(Drug = "lamotrigine", Time = 2 * 1440, Concentration = 2)
  res <- bayesianUpdateDrugs(c4Priors(DT, TRUE), DT, noEvents, obs, rebuild = c4Rebuild(DT),
                             antiseizureInteractions = TRUE, maximum = 3 * 1440)
  expect_match(res$problems, "lamotrigine has no dose above 0 yet", fixed = TRUE, all = FALSE)
  expect_null(res$fits$lamotrigine)
  expect_setequal(bayesianRelevantDrugs("lamotrigine", c4Priors(DT, TRUE),
                                        c("propofol", "carbamazepine", "lamotrigine"), TRUE),
                  c("lamotrigine", "carbamazepine"))
})

# --- M3: the measured levels follow the time format ------------------------------

test_that("measured levels are converted where they are stored, not only where drawn (P4-M3)", {
  shiny::testServer(app_server, {
    c4Start(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    session$setInputs(timeMode = "clock", referenceTime = "08:00")
    session$flushReact()
    bayes$rows(list(rows = data.frame(Drug = "propofol", Time = "08:30", Concentration = "2",
                                      stringsAsFactors = FALSE),
                    format = doseTableFormat()))
    session$setInputs(timeMode = "relative")
    session$flushReact()
    expect_identical(bayes$rows()$format, timeFormat("minutes", "relative"))
    expect_identical(bayes$rows()$rows$Time, "30")
    # and an emptied start, now legal, leaves the level where it is
    session$setInputs(referenceTime = "")
    expect_equal(bayesRowsToObservations(bayes$rows()$rows, bayes$rows()$format, "")$Time, 30)
  })
})

# --- M7: events in time order, the last of a tie in force -------------------------

test_that("the simulation takes events in time order, and Suggest Dosing the last of a tie (P4-M7)", {
  drugs <- recalculatePK(NULL, G, data.frame(Drug = "dexmedetomidineInfantCPB", Time = 0, Dose = 0,
                                             Units = "mcg/kg"),
                         age = 0.5, weight = 6, height = 65, sex = "male")
  PK <- drugs$dexmedetomidineInfantCPB
  DT <- data.frame(Time = 0, Dose = 1, Units = "mcg/kg")
  sorted <- data.frame(Time = c(10, 30), Event = c("CPB Start", "CPB End"))
  unsorted <- sorted[2:1, ]
  a <- simCpCe(DT, sorted, PK, 60, FALSE)$wide
  b <- simCpCe(DT, unsorted, PK, 60, FALSE)$wide
  expect_identical(b$Plasma, a$Plasma)
  # two events at the window's start: the simulation applies the second
  targets <- data.frame(Time = c(60, 120), Target = c(0.7, 0.7))
  tie <- data.frame(Time = c(60, 60), Event = c("CPB Start", "CPB End"))
  only <- data.frame(Time = 60, Event = "CPB End")
  r1 <- suggest("dexmedetomidineInfantCPB", targets, 130, drugs, G$Drug, tie, REFERENCE_TIME_NONE)
  r2 <- suggest("dexmedetomidineInfantCPB", targets, 130, drugs, G$Drug, only, REFERENCE_TIME_NONE)
  expect_equal(r1$Dose, r2$Dose)
})

# --- M2: a log axis with a panel that has nothing above zero ----------------------

test_that("a log axis keeps a panel with nothing above zero, and draws no band from log10(0) (P4-M2)", {
  DT <- data.frame(Drug = c("propofol", "fentanyl"), Time = 0, Dose = c(100, 0),
                   Units = c("mg", "mcg"), stringsAsFactors = FALSE)
  sim <- processdoseTable(DT, noEvents, c4Priors(DT), 60, FALSE)
  for (ty in c("none", "Range", "Mid")) {
    p <- simulationPlot(sim, noEvents, G, getEventDefaults(), logY = TRUE, typical = ty,
                        xBreaks = 0:6 * 10, xLabels = 0:6 * 10)
    b <- suppressWarnings(ggplot2::ggplot_build(p$plotObject))
    expect_equal(nrow(b$layout$layout), 2, label = ty)
    ranges <- unlist(lapply(b$layout$panel_params, function(pp) pp$y.range))
    expect_true(all(is.finite(ranges)), label = ty)
  }
})

.sprglobals$config <- oldConfig
