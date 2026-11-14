# The findings of Carbon's (Gemini) adversarial UI audit of 275c1e8,
# 2026-11-14, each reproduced before it was fixed, and the defects found
# while reproducing them: the formed metabolites' and the interactions'
# units, the bookmark fields Carbon did not try, and the Single Paper Review
# upload path.  Drafted by Claude Code, 2026-11-14, at the request of
# Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

noEvents <- data.frame(Time = numeric(0), Event = character(0))
G <- getDrugDefaultsGlobal()

priorDrugsFor <- function(DT, defaults = G) {
  recalculatePK(NULL, defaults, DT, age = 50, weight = 70, height = 170, sex = "male")
}
rebuildFor <- function(DT, defaults = G) {
  function(drug, individual) {
    recalculatePK(NULL, defaults, DT[DT$Drug == drug, , drop = FALSE], age = 50, weight = 70,
                  height = 170, sex = "male", individual = stats::setNames(list(individual), drug))[[drug]]
  }
}
oneDose <- function(drug, dose, units, time = 0) {
  data.frame(Drug = drug, Time = time, Dose = dose, Units = units, stringsAsFactors = FALSE)
}
simulated <- function(DT, defaults = G, maximum = 1440, asm = FALSE) {
  drugs <- recalculatePK(NULL, defaults, DT, age = 50, weight = 70, height = 170, sex = "male",
                         antiseizureInteractions = asm)
  processdoseTable(DT, noEvents, drugs, maximum, FALSE, antiseizureInteractions = asm)
}
peakOf <- function(out, drug) max(out[[drug]]$wide$Plasma)
withUnits <- function(drug, units, defaults = G) {
  defaults$Concentration.Units[defaults$Drug == drug] <- units
  defaults
}

# --- F1: the Bayesian fit's horizon -----------------------------------------

test_that("a measurement past the longest plot is set aside at once, not simulated to (F1)", {
  DT <- oneDose("propofol", 3, TCI_UNIT_EFFECT)
  obs <- data.frame(Drug = "propofol", Time = 1e12, Concentration = 3)
  elapsed <- system.time(
    res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  )[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_length(res$fits, 0)
  expect_match(res$problems, "propofol: 1 measurement after 1 year, the longest plot, is not used.",
               fixed = TRUE)
})

test_that("a measurement after the end of the plot is set aside (F1)", {
  DT <- oneDose("morphine", 10, "mg")
  obs <- data.frame(Drug = "morphine", Time = c(60, 400), Concentration = c(0.02, 0.005))
  res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT),
                             maximum = 240)
  expect_match(res$problems, "after the end of the plot (4 hours)", fixed = TRUE)
  expect_equal(nrow(res$fits$morphine$observations), 1)
})

test_that("a TCI target is not simulated past a week, nor a schedule past its limit (F1)", {
  DT <- oneDose("propofol", 3, TCI_UNIT_EFFECT)
  obs <- data.frame(Drug = "propofol", Time = 2e4, Concentration = 3)
  res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  expect_length(res$fits, 0)
  expect_match(res$problems, "simulated only to 1 week", fixed = TRUE)

  expect_null(bayesianHorizonProblem("morphine", oneDose("morphine", 10, "mg PO q4h"), 1e4))
  expect_match(bayesianHorizonProblem("morphine", oneDose("morphine", 10, "mg PO q4h"), 1e7),
               "more than the 8,000 the app can simulate", fixed = TRUE)
})

test_that("a link's measurement past the longest plot is left out (F1)", {
  out <- restoredBayesObservations(list(Drug = c("propofol", "propofol"), Time = c(60, 1e12),
                                        Concentration = c(3, 3)), G$Drug)
  expect_equal(out$table$Time, 60)
  expect_true(out$changed)
})

# --- F2: enalapril's transit chain under the Bayesian update -----------------

test_that("the update moves a transit chain's kMetabolic with k10 (F2)", {
  X <- enalapril(70, 170, 35, "male")
  k10 <- function(X) X$PK$default$cl1 / X$PK$default$v1
  for (ind in list(list(clearance = 0.5, volume = 2), list(clearance = 3, volume = 0.5))) {
    Y <- individualizeModel(X, ind)
    expect_equal(Y$transitChain$kMetabolic / k10(Y), X$transitChain$kMetabolic / k10(X))
  }
  # A level that needs a clearance well below the model's: before, the search
  # stepped where kMetabolic exceeded k10 and the update failed
  DT <- oneDose("enalapril", 10, "mg PO")
  obs <- data.frame(Drug = "enalapril", Time = 360, Concentration = 25)
  res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT))
  expect_false(any(grepl("could not be updated", res$problems)))
  expect_lt(res$fits$enalapril$clearance / res$fits$enalapril$volume, 0.69)
})

# --- F3: the row limits ------------------------------------------------------

test_that("a dose table is held to its row limit, blank rows first (F3)", {
  limit <- MAX_DOSE_ROWS - 1L
  full <- data.frame(Drug = rep("propofol", limit), Time = as.character(seq_len(limit)),
                     Dose = "1", Units = "mg", stringsAsFactors = FALSE)
  expect_identical(doseTableWithinLimit(full), full)
  blank <- data.frame(Drug = "", Time = "", Dose = "", Units = "", stringsAsFactors = FALSE)
  # a trailing blank row past the limit is dropped
  expect_equal(nrow(doseTableWithinLimit(rbind(full, blank))), limit)
  # a dose past it cannot fit
  expect_null(doseTableWithinLimit(rbind(full, full[1, ])))
  # blank rows in the middle are dropped from the end first
  mixed <- rbind(full[1:3, ], blank, full[4:limit, ], blank)
  out <- doseTableWithinLimit(mixed)
  expect_equal(nrow(out), limit)
  expect_true(all(nzchar(out$Drug)))
})

carbonInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "1440",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)
startCarbon <- function(session, dt) {
  do.call(session$setInputs, carbonInputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}
recordCarbon <- function(session) {
  sent <- new.env()
  sent$notifications <- list()
  session$sendNotification <- function(type, message) {
    sent$notifications[[length(sent$notifications) + 1]] <- message
  }
  sent
}
errorsSent <- function(sent) {
  unlist(lapply(sent$notifications, function(n) {
    if (is.list(n) && identical(n$type, "error")) as.character(n$html) else NULL
  }))
}
# A table of `n` propofol doses, then `extra` rows (each list(drug, time,
# dose, units))
fullTable <- function(n, ...) {
  dt <- data.frame(Drug = rep("propofol", n), Time = as.character(seq_len(n)), Dose = "1",
                   Units = "mg", stringsAsFactors = FALSE)
  for (row in list(...)) dt[nrow(dt) + 1, ] <- row
  dt
}

test_that("Add a dose to a full table is refused and the table kept (F3, bypass A)", {
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(MAX_DOSE_ROWS - 1L))
    before <- doseTable()
    session$setInputs(addDoseDrug = "propofol", addDoseTime = "600", addDoseAmount = "20",
                      addDoseUnits = "mg", addDoseBtn = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    expect_true(any(grepl("holds at most 499 doses", errorsSent(sent))))
    expect_silent(validateDoseTableInput(doseTable()))
  })
})

test_that("Skip next dose and Stop with no room are refused (F3, bypasses B and C)", {
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(MAX_DOSE_ROWS - 2L, list("carbamazepine", "0", "200", "mg PO tid")))
    before <- doseTable()
    skipTarget(list(drug = "carbamazepine", time = 300))
    session$setInputs(skipNextDoseBtn = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    expect_true(any(grepl("holds at most 499 doses", errorsSent(sent))))
  })
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(MAX_DOSE_ROWS - 2L, list("fentanyl", "0", "1", "mcg/kg/hr")))
    before <- doseTable()
    skipTarget(list(drug = "fentanyl", time = 300))
    session$setInputs(stopAdministrationBtn = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    expect_true(any(grepl("holds at most 499 doses", errorsSent(sent))))
  })
})

test_that("the gas rules never take the dose table past its limit (F3, bypass D)", {
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(MAX_DOSE_ROWS - 1L))
    # nitrous oxide typed into the last row: the oxygen and ventilation rows
    # it needs do not fit, and the edit is refused
    edit <- fullTable(MAX_DOSE_ROWS - 2L, list("nitrousOxide", "0", "4", "L/min"))
    draft <- doseTableDraft()
    expect_false(storeGridEdit(edit, doseTableFormat()))
    expect_identical(doseTableDraft(), draft)
    expect_true(any(grepl("holds at most 499 doses", errorsSent(sent))))
    # a link's table with no room for them is kept as it came
    doseTable(edit)
    session$flushReact()
    expect_false(session$isClosed())
    expect_lte(nrow(doseTable()), MAX_DOSE_ROWS - 1L)
    expect_silent(validateDoseTableInput(doseTable()))
  })
})

test_that("Add event refuses a 101st event and an event not in the list (F3 bypass E, F6C)", {
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(1))
    eventTable(data.frame(Time = seq_len(MAX_EVENT_ROWS), Event = "Induction"))
    session$setInputs(clickTimeEvent = "5", clickEvent = "Induction", addEventBtn = 1)
    expect_false(session$isClosed())
    expect_equal(nrow(eventTable()), MAX_EVENT_ROWS)
    expect_true(any(grepl("holds at most 100 events", errorsSent(sent))))

    eventTable(data.frame(Time = numeric(0), Event = character(0)))
    n <- 1
    for (bad in list(NULL, character(0), c("Induction", "Intubation"), "not_an_event", list(1))) {
      session$setInputs(clickTimeEvent = "5", clickEvent = bad, addEventBtn = (n <- n + 1))
      expect_false(session$isClosed())
      expect_equal(nrow(eventTable()), 0)
    }
    # a time no text box sends is refused, not an error
    session$setInputs(clickTimeEvent = c("1", "2"), clickEvent = "Induction", addEventBtn = 99)
    expect_false(session$isClosed())
    expect_equal(nrow(eventTable()), 0)
    session$setInputs(clickTimeEvent = "5", clickEvent = "Induction", addEventBtn = 100)
    expect_equal(nrow(eventTable()), 1)
  })
})

# --- F4: the Drug Library's units ----------------------------------------------

test_that("the Drug Library refuses a unit the drug's model cannot simulate (F4a)", {
  current <- G
  edited <- drugLibraryGridData(G)
  add <- function(drug, unit, default = NULL) {
    e <- edited
    i <- e$Drug == drug
    e$Units[i] <- paste0(e$Units[i], ",", unit)
    if (!is.null(default)) e$Default.Units[i] <- default
    drugLibraryProblems(e, current)
  }
  expect_match(add("calcium", "mg", "mg"), "Dose unit for calcium must name the product", fixed = TRUE)
  expect_match(add("morphine", "mg IM"), "no concentration follows a dose", fixed = TRUE)
  expect_match(add("propofol", "%"), "a unit of the inhaled agents", fixed = TRUE)
  expect_match(add("sevoflurane", "mg"), "an inhaled agent is set in L/min or %", fixed = TRUE)
  expect_match(add("insulinRegular", "mg SC"), "must be in units (U)", fixed = TRUE)
  expect_length(add("fentanyl", "mcg/hr"), 0)
  expect_length(add("morphine", "mg PO bid"), 0)
  expect_length(drugLibraryProblems(edited, current), 0)
  # at most DRUG_LIBRARY_MAX_NEW_UNITS new units are simulated in one edit
  e <- edited
  dosed <- !is.na(G$Category) & nzchar(G$Category) & G$Class != "gas"
  e$Units[dosed] <- paste0(e$Units[dosed], ",mg/hr")
  expect_match(drugLibraryProblems(e, current), "Add at most 40 units", fixed = TRUE)
})

test_that("every unit of every drug passes the Drug Library's simulation of new units (F4a)", {
  skip_on_cran()
  # Each drug's units taken away and added back: the check must refuse none
  dosed <- G$Drug[!is.na(G$Category) & nzchar(G$Category)]
  current <- G
  for (drug in dosed) current$Units[[which(G$Drug == drug)]] <- character(0)
  problems <- drugLibraryUnitProblems(drugLibraryGridData(G), current, maxNew = Inf)
  expect_identical(problems, character(0))
})

test_that("the Drug Library keeps read-only concentration units and the bolus unit among the drug's (F4b)", {
  edited <- drugLibraryGridData(G)
  for (drug in c("warfarin", "calcium", "mannitol", "heparinCPB", "sevoflurane")) {
    e <- edited
    e$Concentration.Units[e$Drug == drug] <- "mcg"
    expect_match(drugLibraryProblems(e, G), paste(drug, "Concentration.Units"), fixed = TRUE)
  }
  e <- edited
  e$Bolus.Units[e$Drug == "calcium"] <- "mg"
  expect_match(drugLibraryProblems(e, G), "calcium Bolus.Units", fixed = TRUE)
  e <- edited
  e$Concentration.Units[e$Drug == "morphine"] <- "ng"
  expect_length(drugLibraryProblems(e, G), 0)
})

test_that("a metabolite is formed in the units the session's library gives it (F4c)", {
  codeine <- oneDose("codeine", 60, "mg PO")
  expect_equal(peakOf(simulated(codeine, withUnits("morphine", "ng")), "morphine") /
                 peakOf(simulated(codeine), "morphine"), 1000)
  enalapril <- oneDose("enalapril", 10, "mg PO")
  expect_equal(G$Concentration.Units[G$Drug == "enalaprilat"], "ng")
  expect_equal(peakOf(simulated(enalapril, withUnits("enalaprilat", "mcg")), "enalaprilat") /
                 peakOf(simulated(enalapril), "enalaprilat"), 0.001)
  # and a rebuild from the model keeps the library rows it was built with
  PK <- getDrugPK("codeine", 70, 170, 50, "male", drugDefaults = G[G$Drug == "codeine", ],
                  libraryDefaults = withUnits("morphine", "ng"))
  expect_equal(pkRebuildArgs(PK)$libraryDefaults$Concentration.Units, "ng")
})

test_that("the antiseizure interactions read a perpetrator in the registry's mcg/mL (F4d)", {
  DT <- oneDose("carbamazepine", 200, "mg PO bid")
  mcg <- simulated(DT, G, 30 * MINS_PER_DAY, asm = TRUE)
  ng <- simulated(DT, withUnits("carbamazepine", "ng"), 30 * MINS_PER_DAY, asm = TRUE)
  for (entry in c("carbamazepine", "carbamazepine (no interactions)"))
    expect_equal(peakOf(ng, entry) / peakOf(mcg, entry), 1000)
})

test_that("a drug moved between mcg and ng keeps its thresholds, converted (F4)", {
  current <- G
  edited <- drugLibraryGridData(G)
  i <- which(edited$Drug == "morphine")
  edited$Concentration.Units[i] <- "ng"
  edited$Lower[i] <- "7"    # typed in the new units
  new <- edited
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) new[[col]] <- as.numeric(new[[col]])
  new$endCe <- G$endCe
  out <- drugLibraryRescaleThresholds(new, edited, current)
  expect_equal(out$drugs, "morphine")
  expect_equal(out$table$Lower[i], 7)
  for (col in c("Upper", "Typical", "MEAC", "endCe"))
    expect_equal(out$table[[col]][i], G[[col]][i] * 1000)
  # every other drug as it was
  expect_equal(out$table$Upper[-i], new$Upper[-i])
})

test_that("the propofol-opioid surface reads its drugs in the units it was fitted in (F4)", {
  DT <- data.frame(Drug = c("propofol", "remifentanil"), Time = 0, Dose = c(150, 50),
                   Units = c("mg", "mcg"), stringsAsFactors = FALSE)
  # propofol in ng/mL, remifentanil in mcg/mL, with the thresholds converted
  # as the Drug Library converts them
  moved <- withUnits("remifentanil", "mcg", withUnits("propofol", "ng"))
  for (d in c("propofol", "remifentanil")) {
    i <- moved$Drug == d
    f <- if (d == "propofol") 1000 else 0.001
    for (col in c("MEAC", "Lower", "Typical", "Upper", "endCe")) moved[[col]][i] <- moved[[col]][i] * f
  }
  interaction <- function(defaults) {
    out <- simulationPlot(simulated(DT, defaults, 60), noEvents, defaults, getEventDefaults(),
                          plotMEAC = TRUE, plotInteraction = TRUE)
    out$plotResults$Y[out$plotResults$Drug == PLOT_NAME_INTERACTION]
  }
  base <- interaction(G)
  expect_gt(length(base), 0)
  expect_equal(interaction(moved), base, tolerance = 1e-6)
})

# --- F5: a bookmark link's values ------------------------------------------------

test_that("a link value that is not one number reads as none (F5)", {
  bad <- list(list(a = 1:2), list(1, 2), c(1, 2), NULL, "x", NA)
  for (x in bad) {
    expect_true(is.na(asScalarNumeric(x)))
    expect_equal(snapMaximum(x, "minutes"), MAX_TIMES$minutes$times[1])
    expect_equal(snapMinimum(x, 240, "minutes"), 0)
    expect_equal(legacyTimeUnit(x), "minutes")
    expect_equal(bayesSliderValue(x, 25, 5, 100), 25)
    expect_equal(debugLevel(x), DEBUG_LEVEL_OFF)
  }
  expect_equal(asScalarNumeric("120"), 120)
  expect_equal(snapMaximum("240", "minutes"), 240)
})

test_that("a link the page cannot be built from is answered with a page that says so (F5)", {
  ui <- app_ui()
  build <- function(query) {
    ctx <- shiny:::RestoreContext$new(query)
    shiny:::withRestoreContext(ctx, ui(list(QUERY_STRING = query)))
  }
  # Carbon's two now read as no value, and the page is built
  for (query in c("?_inputs_&maximum=%7B%22a%22%3A%5B1%2C2%5D%7D",
                  "?_inputs_&minimum=%7B%22a%22%3A%5B1%2C2%5D%7D"))
    expect_false(inherits(build(query), "httpResponse"))
  # The fields it did not try (a checkbox, a choice, a text area): Shiny's
  # own inputs restore them, so the link is answered with a page saying so
  for (query in c("?_inputs_&logY=%5B1%2C2%5D",
                  "?_inputs_&sex=%5B1%2C2%2C3%5D",
                  "?_inputs_&emailComments=%5B1%2C2%2C3%5D")) {
    page <- build(query)
    expect_s3_class(page, "httpResponse")
    expect_equal(page$status, 400L)
    expect_match(page$content, "This link could not be opened", fixed = TRUE)
  }
  # a readable link, and the plain page, are built as before
  expect_false(inherits(build("?_inputs_&maximum=240&logY=true"), "httpResponse"))
  expect_false(inherits(build(""), "httpResponse"))
  expect_true(isBookmarkQuery("?_inputs_&x=1"))
  expect_true(isBookmarkQuery("_values_&DT=1"))
  expect_false(isBookmarkQuery("?debug=1"))
  expect_false(isBookmarkQuery("?scenario=propofol_inputs_"))
})

# --- F6: values the browser could not have sent ----------------------------------

test_that("a plot click or hover that is no click is ignored (F6A)", {
  expect_null(plotEvent(list(x = 10, y = 1)))
  expect_null(plotEvent(list(x = c(1, 2), panelvar1 = "propofol")))
  expect_null(plotEvent(list(x = 10, panelvar1 = c("a", "b"))))
  expect_null(plotEvent("propofol"))
  expect_equal(plotEvent(list(x = "10", panelvar1 = "propofol"))$x, 10)
  shiny::testServer(app_server, {
    startCarbon(session, fullTable(1))
    # with a plot drawn, as in the browser: without one, imgDrugTime() stopped
    # at its req() before reading the panel
    expect_gt(nrow(allResultsReactive()), 0)
    for (e in list(list(x = 10, y = 1), list(x = 10, panelvar1 = c("a", "b")), "x", list())) {
      session$setInputs(plot_click = e)
      expect_false(session$isClosed())
      session$setInputs(plot_dblclick = e)
      expect_false(session$isClosed())
      session$setInputs(plot_hover = e)
      expect_false(session$isClosed())
    }
  })
})

test_that("the Edit doses buttons before any click do nothing (F6B)", {
  shiny::testServer(app_server, {
    startCarbon(session, fullTable(3))
    before <- doseTable()
    session$setInputs(deleteAllDosesBtn = 1)
    expect_false(session$isClosed())
    session$setInputs(confirmDeleteAllDoses = 1)
    expect_false(session$isClosed())
    session$setInputs(editDosesOK = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
  })
})

test_that("Email Slide and the patient fields read only single values (F6D, F6E)", {
  expect_false(isEmailValid(NULL))
  expect_false(isEmailValid(NA_character_))
  expect_false(isEmailValid(c("a@b.co", "c@d.co")))
  expect_true(isEmailValid("a@b.co"))
  shiny::testServer(app_server, {
    startCarbon(session, fullTable(1))
    session$setInputs(emailComments = NULL, recipient = "a@b.co", sendSlide = 1)
    expect_false(session$isClosed())
    session$setInputs(emailComments = c("x", "y"), commentSafe = NULL, sendSlide = 2)
    expect_false(session$isClosed())
    session$setInputs(recipient = c("a@b.co", "c@d.co"), sendSlide = 3)
    expect_false(session$isClosed())
    session$setInputs(age = c(10, 20))
    session$flushReact()
    expect_false(session$isClosed())
    session$setInputs(normalization = NULL, showThreshold = c(TRUE, FALSE), editThresholds = 1)
    expect_false(session$isClosed())
  })
  shiny::testServer(app_server, {
    session$setInputs(referenceTime = c("", "x"), client_time = "08:15")
    expect_false(session$isClosed())
  })
  shiny::testServer(app_server, {
    session$setInputs(referenceTime = "", client_time = c("08:15", "09:00"))
    expect_false(session$isClosed())
  })
})

test_that("Single Paper Review reads and deletes only what Shiny uploaded (F6G)", {
  # A file anywhere else, its path forged into the input, is neither read nor
  # deleted: before, a page that had uploaded nothing deleted it and its
  # directory with one Shiny.setInputValue()
  victimDir <- tempfile("victim-")
  dir.create(victimDir)
  victim <- file.path(victimDir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), victim)
  expect_false(paperReviewIsUploadPath(victim))
  expect_length(paperReviewReadUpload(victim), 0)
  expect_true(file.exists(victim))
  # nor by a path that climbs out of an upload's directory
  climb <- file.path(tempdir(), shinyUploadDirName(), "..", basename(victimDir), "0.pdf")
  expect_length(paperReviewReadUpload(climb), 0)
  expect_true(file.exists(victim))

  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf",
                                        datapath = victim))
    expect_true(file.exists(victim))
    expect_false(hasPdf())
    for (bad in list("bad", list(datapath = c(victim, victim)), list(name = "x"))) {
      session$setInputs(supplement = bad)
      expect_false(session$isClosed())
    }
    session$setInputs(timeUnit = "invalid_unit")
    session$setInputs(timeUnit = c("min", "h"))
    session$setInputs(timeUnit = "invalid_unit")
    expect_false(session$isClosed())
    expect_true(tableUnit() %in% PAPER_REVIEW_SIM_TIME_UNITS)
  })
  expect_true(file.exists(victim))
  unlink(victimDir, recursive = TRUE)

  # A real upload is read and deleted as before
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  bytes <- c(charToRaw("%PDF-1.7\n"), as.raw(1:20))
  writeBin(bytes, path)
  expect_true(paperReviewIsUploadPath(path))
  expect_equal(paperReviewReadUpload(path), bytes)
  expect_false(file.exists(path))
})

.sprglobals$config <- oldConfig
