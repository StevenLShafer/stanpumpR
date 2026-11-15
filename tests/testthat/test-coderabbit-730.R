# CodeRabbit's review of PR #730 (2026-11-15): the leads in the app server,
# the Bayesian update, the Drug Library, the time display and the ke0
# solver, each reproduced before it was fixed; every test here fails on
# f197d63.  The gas engine's, the Visualizer's and Single Paper Review's are
# in test-coderabbit-730-*.R.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

G <- getDrugDefaultsGlobal()
noEvents <- data.frame(Time = numeric(0), Event = character(0))

crInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "1440",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
  bayesPKVariability = 25, bayesResidualVariability = 5
)
crStart <- function(session, dt) {
  do.call(session$setInputs, crInputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}
crNotices <- function(session) {
  sent <- new.env()
  sent$messages <- character(0)
  session$sendNotification <- function(type, message) {
    text <- if (is.list(message)) message$html else message
    sent$messages <- c(sent$messages, paste(as.character(text), collapse = " "))
  }
  sent
}
crCells <- function(x) lapply(seq_len(nrow(x)), function(i)
  lapply(x[i, ], function(v) if (is.na(v)) "" else as.character(v)))
crLibraryEdit <- function(session, edit) {
  shown <- drugLibraryGridData(session$env$drugDefaults())
  shown <- edit(shown)
  session$setInputs(editDrugsHTML = tableGridInputValue(crCells(shown)), drugEditsOK = 1)
}

test_that("an event's clock time survives a start moved by a fraction of its minute and back", {
  there <- eventTimesForStart(0.75, "08:00", "08:01")
  expect_equal(there, 1439.75)
  expect_equal(eventTimesForStart(there, "08:01", "08:00"), 0.75)
})

test_that("a link's threshold that is not one number is set aside, not an error", {
  expect_equal(linkNumbers(list(propofol = c(1, 2), fentanyl = 0.5, x = list(1))),
               c(propofol = NA, fentanyl = 0.5, x = NA))
  restored <- restoreThresholds(G, list(propofol = c(1, 2), fentanyl = 0.5))
  expect_equal(restored$drugDefaults$endCe[G$Drug == "fentanyl"], 0.5)
  expect_identical(restored$drugDefaults$endCe[G$Drug == "propofol"], G$endCe[G$Drug == "propofol"])
})

test_that("ke0 is solved for a fast oral curve, whose plasma peak a 0-4000 min search missed", {
  coef <- c(10 / 9, 0, 0)
  lambda <- c(1, 0, 0)
  ke0 <- ke0FromTPeak(5, coef, lambda, ka = 10)
  peak <- effectSitePeakTime(c(coef[1], -coef[1]), c(1, 10), ke0)
  expect_equal(peak, 5, tolerance = 1e-3)
})

test_that("a range switched to new units is checked in its final units (both ways)", {
  # Upper typed in ng/mL beside a Lower converted from mcg/mL: 1000-500
  shiny::testServer(app_server, {
    sent <- crNotices(session)
    crStart(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    lib <- drugDefaults()
    lib[lib$Drug == "propofol", c("Lower", "Upper", "Typical")] <- list(1, 2, 1.5)
    drugDefaults(lib)
    crLibraryEdit(session, function(x) {
      x$Concentration.Units[x$Drug == "propofol"] <- "ng"
      x$Upper[x$Drug == "propofol"] <- "500"
      x
    })
    expect_identical(drugDefaults()$Concentration.Units[drugDefaults()$Drug == "propofol"], "mcg")
    expect_true(any(grepl("Lower must not be above Upper in the new units", sent$messages, fixed = TRUE)))
  })
  # Lower typed in ng/mL beside an Upper converted: 500-2000, which is right
  shiny::testServer(app_server, {
    crStart(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    lib <- drugDefaults()
    lib[lib$Drug == "propofol", c("Lower", "Upper", "Typical")] <- list(1, 2, 1.5)
    drugDefaults(lib)
    crLibraryEdit(session, function(x) {
      x$Concentration.Units[x$Drug == "propofol"] <- "ng"
      x$Lower[x$Drug == "propofol"] <- "500"
      x
    })
    row <- drugDefaults()[drugDefaults()$Drug == "propofol", ]
    expect_identical(row$Concentration.Units, "ng")
    expect_equal(as.numeric(c(row$Lower, row$Upper)), c(500, 2000))
  })
})

test_that("a full table whose inhaled agent lacks its rows is not simulated, nor converted in part", {
  n <- MAX_DOSE_ROWS - 1L
  dt <- data.frame(Drug = "propofol", Time = as.character(seq_len(n)), Dose = "1", Units = "mg",
                   stringsAsFactors = FALSE)
  dt[n - 1, ] <- list("propofol", "0", "3", TCI_UNIT_EFFECT)
  dt[n, ] <- list("sevoflurane", "0", "2", "%")
  shiny::testServer(app_server, {
    sent <- crNotices(session)
    do.call(session$setInputs, crInputs)
    doseTable(dt)
    session$flushReact()
    expect_identical(doseTable(), dt)
    expect_match(plotMessage(), "needs its oxygen and ventilation rows", fixed = TRUE)
    crLibraryEdit(session, function(x) {
      x$Concentration.Units[x$Drug == "propofol"] <- "ng"
      x
    })
    # The converted target could not be written: nothing changed
    expect_identical(drugDefaults()$Concentration.Units[drugDefaults()$Drug == "propofol"], "mcg")
    expect_identical(doseTable(), dt)
    expect_true(any(grepl("could not be written to the dose table", sent$messages, fixed = TRUE)))
  })
})

test_that("an edit within the wait after a unit switch never brings back the old units' levels", {
  shiny::testServer(app_server, {
    crStart(session, data.frame(Drug = "propofol", Time = "0", Dose = "100", Units = "mg"))
    session$setInputs(bayesTable = list(rows = list(list("propofol", "30", "3"), list("", "", "")),
                                        tag = "minutes/relative", seq = 1))
    session$elapse(2000)
    expect_equal(bayes$observations()$Concentration, 3)
    crLibraryEdit(session, function(x) {
      x$Concentration.Units[x$Drug == "propofol"] <- "ng"
      x
    })
    expect_equal(bayes$observations()$Concentration, 3000)
    # a second level typed at once, within the 750 ms wait
    session$setInputs(bayesTable = list(rows = list(list("propofol", "30", "3000"),
                                                    list("propofol", "60", "2000"), list("", "", "")),
                                        tag = "minutes/relative", seq = 2))
    expect_false(3 %in% bayes$observations()$Concentration)
    session$elapse(2000)
    expect_equal(bayes$observations()$Concentration, c(3000, 2000))
  })
})

c730Priors <- function(DT) {
  recalculatePK(NULL, G, DT, age = 50, weight = 70, height = 170, sex = "male",
                antiseizureInteractions = TRUE)
}
c730Rebuild <- function(DT) {
  function(drug, individual) {
    recalculatePK(NULL, G, DT[DT$Drug == drug, , drop = FALSE], age = 50, weight = 70,
                  height = 170, sex = "male", individual = individual)[[drug]]
  }
}

test_that("a joint round cut short by the time limit gives back the last whole round", {
  DT <- data.frame(Drug = c("valproate", "lamotrigine"), Time = 0, Dose = c(500, 100),
                   Units = c("mg PO bid", "mg PO qd"), stringsAsFactors = FALSE)
  t14 <- 14 * 1440
  obs <- data.frame(Drug = c("valproate", "lamotrigine"), Time = t14, Concentration = c(60, 4))
  # The time limit reached in the second round, on its first fit
  calls <- 0
  real <- bayesianFitDrug
  local_mocked_bindings(bayesianFitDrug = function(...) {
    calls <<- calls + 1
    if (calls > 2) stop(structure(class = c("bayesTimeLimit", "error", "condition"),
                                  list(message = "out of time", call = NULL)))
    real(...)
  })
  res <- bayesianUpdateDrugs(c730Priors(DT), DT, noEvents, obs, rebuild = c730Rebuild(DT),
                             antiseizureInteractions = TRUE, maximum = 15 * 1440)
  # Both drugs as the first round left them: updated, with their fits
  expect_setequal(names(res$fits), c("valproate", "lamotrigine"))
  expect_length(res$failed, 0)
  for (d in c("valproate", "lamotrigine"))
    expect_equal(res$drugs[[d]]$bayesian$clearance, res$fits[[d]]$clearance, label = d)
  expect_match(res$problems, "stopped before the estimates settled", fixed = TRUE, all = FALSE)
})

test_that("a joint update that runs out of rounds says so", {
  # Phenobarbital and valproate act on each other, so their estimates move
  # in every round
  DT <- data.frame(Drug = c("phenobarbital", "valproate"), Time = 0, Dose = c(100, 500),
                   Units = c("mg PO qd", "mg PO DR bid"), stringsAsFactors = FALSE)
  obs <- data.frame(Drug = c("phenobarbital", "valproate"), Time = 14 * 1440, Concentration = c(30, 60))
  res <- bayesianUpdateDrugs(c730Priors(DT), DT, noEvents, obs, rebuild = c730Rebuild(DT),
                             antiseizureInteractions = TRUE, maximum = 15 * 1440,
                             jointPasses = 2L, jointTolerance = 1e-12, timeLimit = 600)
  expect_match(res$problems, "still changing after 2 rounds", fixed = TRUE, all = FALSE)
})

test_that("a time limit reached on a budget another drug spent is tried again", {
  DT <- data.frame(Drug = c("propofol", "fentanyl"), Time = 0, Dose = c(100, 100),
                   Units = c("mg", "mcg"), stringsAsFactors = FALSE)
  obs <- data.frame(Drug = c("propofol", "fentanyl"), Time = 30, Concentration = c(1, 0.5))
  priors <- recalculatePK(NULL, G, DT, age = 50, weight = 70, height = 170, sex = "male")
  rebuild <- function(drug, individual)
    recalculatePK(NULL, G, DT[DT$Drug == drug, , drop = FALSE], age = 50, weight = 70,
                  height = 170, sex = "male", individual = individual)[[drug]]
  # propofol spends the whole budget; fentanyl then has none
  real <- bayesianFitDrug
  local_mocked_bindings(bayesianFitDrug = function(drug, ...) {
    if (drug == "propofol") Sys.sleep(1.2)
    real(drug, ...)
  })
  # with time spent before each search (onFit), which the earlier rule took
  # for another drug's
  res <- bayesianUpdateDrugs(priors, DT, noEvents, obs, rebuild = rebuild, timeLimit = 1,
                             onFit = function(drug) Sys.sleep(0.05))
  expect_true("fentanyl" %in% names(res$failed))
  # fentanyl's failure was the shared budget's, and is not kept against it
  cachedFailures <- Filter(function(f) !is.null(f$failed) && identical(f$key$drug, "fentanyl"), res$cache)
  expect_length(cachedFailures, 0)
  # propofol's was its own, and is kept, so that a redraw does not search
  # again for the whole budget (CodeRabbit's second look: judged by the time
  # already gone, the setup made every failure "shared")
  ownFailures <- Filter(function(f) !is.null(f$failed) && identical(f$key$drug, "propofol"), res$cache)
  expect_length(ownFailures, 1)
})

test_that("a link's range is checked in the units it carries", {
  saved <- data.frame(Drug = "propofol", Concentration.Units = "ng", Lower = "2000", Upper = "1000",
                      stringsAsFactors = FALSE)
  back <- restoredDrugLibrary(saved)
  expect_identical(back$table$Concentration.Units[back$table$Drug == "propofol"], "mcg")
  expect_match(back$problems, "propofol", fixed = TRUE)
  ok <- saved
  ok$Lower <- "1000"; ok$Upper <- "2000"
  expect_identical(restoredDrugLibrary(ok)$table$Concentration.Units[G$Drug == "propofol"], "ng")
})

.sprglobals$config <- oldConfig
