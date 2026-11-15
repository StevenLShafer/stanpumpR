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

# --- Carbon's second audit, of PR #730 at 51f1276 (2026-11-15) ---------------
# Its findings are G1-G8 here, to keep them apart from the first audit's.

test_that("a dose-dependent drug's population curve is rebuilt from its own model (G1)", {
  for (case in list(c("insulinRegular", "U SC"), c("lixisenatide", "mcg SC"))) {
    DT <- oneDose(case[1], 10, case[2])
    drugs <- priorDrugsFor(DT)
    p <- withOutputTimes(60, processdoseTable(DT, noEvents, drugs, 600, FALSE))[[case[1]]]$wide
    p <- p$Plasma[p$Time == 60]
    obs <- data.frame(Drug = case[1], Time = 60, Concentration = 2 * p)
    res <- bayesianUpdateDrugs(drugs, DT, noEvents, obs, rebuild = rebuildFor(DT), maximum = 600)
    population <- bayesianPriorName(case[1])
    expect_true(population %in% names(res$drugs))
    out <- withOutputTimes(60, processdoseTable(DT, noEvents, res$drugs, 600, FALSE))
    # the population curve is the model's, the individual's moves toward the level
    w <- out[[population]]$wide
    expect_equal(w$Plasma[w$Time == 60], p, tolerance = 1e-9)
    w <- out[[case[1]]]$wide
    expect_gt(w$Plasma[w$Time == 60], 1.2 * p)
  }
  expect_identical(pkModelDrug(list(drug = "x (population)", companionOf = "x",
                                    companionGroup = "bayesian")), "x")
  expect_error(pkModelDrug(list(drug = "propofol (Schnider)", companionOf = "propofol",
                                companionGroup = "mintoSchnider")), "model of its own")
})

test_that("the adaptive step ends, refuses an unreadable step, and stops on an empty system (G2)", {
  # a step that fails at the smallest step stops, where it looped for ever
  elapsed <- system.time(expect_error(
    mmDormandPrince(function(t, y) -1e12 * y + sin(1e12 * t), x = 1, t0 = 0, t1 = 10, h = 1,
                    rtol = 1e-6, atol = 1e-12),
    "adaptive step failed"))[["elapsed"]]
  expect_lt(elapsed, 5)
  # a stage outside the derivative's domain (NaN) is a step refused
  f <- function(t, y) ifelse(is.na(y) | y < 0, NaN, -5 * y)
  r <- mmDormandPrince(f, x = 1, t0 = 0, t1 = 2, h = 2, rtol = 1e-6, atol = 1e-12)
  expect_equal(r$x, exp(-10), tolerance = 1e-5)
  # with nothing left and no input the interval is done at once
  calls <- 0
  g <- function(t, y) { calls <<- calls + 1; -5 * y }
  r <- mmDormandPrince(g, x = c(1e-12, 0), t0 = 0, t1 = 1e5, h = 1, rtol = 1e-6, atol = 1e-9)
  expect_identical(r$x, c(0, 0))
  expect_lte(calls, 2)
  # the search's corner in a neonate: stiff, and gone within hours
  pk <- getDrugPK("phenytoin", weight = 0.5, age = 0, height = 50, sex = "male",
                  individual = list(clearance = 20, volume = 0.05))
  DT <- oneDose("phenytoin", 10, "mg")
  invisible(processdoseTable(DT, noEvents, list(phenytoin = pk), 1440, FALSE))
  elapsed <- system.time(
    out <- processdoseTable(DT, noEvents, list(phenytoin = pk), 524160, FALSE))[["elapsed"]]
  expect_lt(elapsed, 1.5)
  expect_true(all(is.finite(out$phenytoin$wide$Plasma)))
})

test_that("an empty or vector entry, and a vector drug, leave the session open (G4)", {
  expect_identical(validateEntry(character(0), validateTime), "")
  expect_identical(validateEntry(numeric(0), validateDose), "")
  expect_identical(validateTime(character(0)), "")
  expect_identical(validateDose(character(0)), "")
  expect_identical(validateEntry(NULL, validateDose), "0")
  shiny::testServer(app_server, {
    sent <- recordCarbon(session)
    startCarbon(session, fullTable(3))
    before <- doseTable()
    session$setInputs(addDoseDrug = "propofol", addDoseTime = character(0), addDoseAmount = "1",
                      addDoseUnits = "mg", addDoseBtn = 1)
    expect_false(session$isClosed())
    session$setInputs(addDoseTime = "10", addDoseAmount = numeric(0), addDoseBtn = 2)
    expect_false(session$isClosed())
    session$setInputs(clickEvent = "Intubation", clickTimeEvent = character(0), addEventBtn = 1)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
    skipTarget(list(drug = "propofol", time = 1))
    session$setInputs(addDoseDrug = c("propofol", "fentanyl"))
    expect_false(session$isClosed())
    session$setInputs(skipNextDoseBtn = 1)
    expect_false(session$isClosed())
    session$setInputs(stopAdministrationBtn = 1)
    expect_false(session$isClosed())
    session$setInputs(addDoseDrug = c("propofol", "fentanyl"), addDoseBtn = 3)
    expect_false(session$isClosed())
    expect_identical(doseTable(), before)
  })
})

test_that("oral voriconazole stays finite and plausible, with either step (found checking G3)", {
  DT <- data.frame(Drug = "voriconazole", Time = c(0, 720), Dose = 200, Units = "mg PO",
                   stringsAsFactors = FALSE)
  drugs <- priorDrugsFor(DT)
  run <- function(adaptive) {
    d <- drugs
    d$voriconazole$michaelisMenten$adaptiveStep <- adaptive
    processdoseTable(DT, noEvents, d, 1440, FALSE)$voriconazole$wide
  }
  adaptive <- run(TRUE)
  fixed <- run(FALSE)
  for (w in list(adaptive, fixed)) {
    expect_true(all(is.finite(w$Plasma)))
    # after each peak the level falls steadily and is never zero: it fell to
    # exactly 0 two hours after the first dose, and the second reached 1e90
    for (window in list(c(0, 720), c(720, 1440))) {
      inWindow <- w$Time >= window[1] & w$Time < window[2]
      y <- w$Plasma[inWindow]
      after <- y[seq(which.max(y), length(y))]
      expect_true(all(after > 0))
      expect_true(all(diff(after) <= 1e-12))
    }
    expect_lt(max(w$Plasma), 5)
  }
  big <- adaptive$Plasma > 0.05 * max(adaptive$Plasma)
  expect_lt(max(abs(fixed$Plasma[big] / adaptive$Plasma[big] - 1)), 0.01)
})

test_that("an update stops at its time limit and does not try again until its inputs change (G3)", {
  DT <- oneDose("morphine", 10, "mg")
  obs <- data.frame(Drug = "morphine", Time = 60, Concentration = 0.02)
  fitted <- 0
  res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT),
                             timeLimit = 0, onFit = function(drug) fitted <<- fitted + 1)
  expect_equal(fitted, 1)
  expect_length(res$fits, 0)
  expect_match(res$problems, "morphine could not be updated within 0 seconds", fixed = TRUE)
  again <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT),
                               fits = c(res$fits, res$failed),
                               onFit = function(drug) fitted <<- fitted + 1)
  expect_equal(fitted, 1)
  expect_identical(again$problems, res$problems)
  # changed inputs are tried again, within the usual limit
  obs$Concentration <- 0.03
  third <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT),
                               fits = c(res$fits, res$failed),
                               onFit = function(drug) fitted <<- fitted + 1)
  expect_equal(fitted, 2)
  expect_named(third$fits, "morphine")
})

test_that("a threshold past the ceiling locks neither dialog (G5)", {
  # Drug Library: a range converted from mcg to ng past 1e9 is not an edit
  current <- G
  current$Upper[current$Drug == "propofol"] <- 5e9
  edited <- drugLibraryGridData(current)
  edited$Lower[edited$Drug == "fentanyl"] <- "0.5"
  expect_length(drugLibraryProblems(edited, current), 0)
  # an entry the edit changes is still held to it
  edited$Upper[edited$Drug == "fentanyl"] <- "5e9"
  expect_match(drugLibraryProblems(edited, current), "fentanyl Upper", fixed = TRUE)

  # Drug Thresholds: a stored value shown past the ceiling, and a volatile
  # agent's shown at another age, are not edits
  dd <- G
  dd$endCe[dd$Drug == "propofol"] <- 5e9
  dd$endCe[dd$Drug == "sevoflurane"] <- 9e8 / macForAge(1, 90)
  shown <- thresholdTableForDisplay(dd, age = 20)
  expect_gt(shown$Threshold[shown$Drug == "sevoflurane"], MAX_DOSE_VALUE)
  shown$Threshold[shown$Drug == "fentanyl"] <- 2
  back <- thresholdTableToDefaults(shown, dd, age = 20)
  expect_identical(back$invalid, character(0))
  expect_equal(back$drugDefaults$endCe[dd$Drug == "fentanyl"], 2)
  # rows left alone keep their stored value, unrounded
  untouched <- dd$Drug != "fentanyl"
  expect_identical(back$drugDefaults$endCe[untouched], dd$endCe[untouched])
  shown$Threshold[shown$Drug == "fentanyl"] <- 5e9
  expect_identical(thresholdTableToDefaults(shown, dd, age = 20)$invalid, "fentanyl")

  # a link's threshold past the ceiling is left out
  restored <- restoreThresholds(G, c(propofol = 5e9, fentanyl = 2), macThreshold = 5e9)
  expect_equal(restored$drugDefaults$endCe[G$Drug == "propofol"], G$endCe[G$Drug == "propofol"])
  expect_equal(restored$drugDefaults$endCe[G$Drug == "fentanyl"], 2)
  expect_equal(restored$macThreshold, GAS_MAC_THRESHOLD)
})

test_that("a drug moved between mcg and ng keeps its TCI targets and measured levels (G6A)", {
  dt <- data.frame(Drug = c("propofol", "propofol", "fentanyl"), Time = c("0", "10", "0"),
                   Dose = c("3", "2", "1"), Units = c(TCI_UNIT_EFFECT, "mg", TCI_UNIT_PLASMA),
                   stringsAsFactors = FALSE)
  out <- doseTableRescaleTargets(dt, c(propofol = 1000))
  expect_identical(out$Dose, c("3000", "2", "1"))
  expect_null(doseTableRescaleTargets(transform(dt, Dose = c("2e6", "2", "1")), c(propofol = 1000)))
  expect_identical(doseTableRescaleTargets(dt, numeric(0)), dt)
  # the factors are the thresholds'
  edited <- drugLibraryGridData(G)
  edited$Concentration.Units[edited$Drug == "propofol"] <- "ng"
  new <- edited
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) new[[col]] <- as.numeric(new[[col]])
  new$endCe <- G$endCe
  expect_equal(drugLibraryRescaleThresholds(new, edited, G)$factors, c(propofol = 1000))
  # and with no drug moved there are none (an empty switch() ended the session)
  same <- drugLibraryGridData(G)
  newSame <- same
  for (col in DRUG_LIBRARY_NUMERIC_COLUMNS) newSame[[col]] <- as.numeric(newSame[[col]])
  newSame$endCe <- G$endCe
  expect_length(drugLibraryRescaleThresholds(newSame, same, G)$factors, 0)
  # and the measured concentrations follow
  shiny::testServer(app_server, {
    startCarbon(session, oneDose("propofol", 100, "mg"))
    bayes$rows(list(rows = data.frame(Drug = c("propofol", "propofol"), Time = c("30", "60"),
                                      Concentration = c("3", "x"), stringsAsFactors = FALSE),
                    format = timeFormat("minutes", "relative")))
    bayes$rescale(c(propofol = 1000))
    expect_identical(bayes$rows()$rows$Concentration, c("3000", "x"))
  })
})

test_that("a drug not plotted as a concentration by mass takes no TCI, and a new unit must move the curve (G6B)", {
  DT <- oneDose("mannitol", 310, TCI_UNIT_PLASMA)
  expect_error(simulated(DT), "osmolality")
  current <- G
  edited <- drugLibraryGridData(G)
  add <- function(drug, unit) {
    e <- edited
    i <- e$Drug == drug
    e$Units[i] <- paste0(e$Units[i], ",", unit)
    drugLibraryProblems(e, current)
  }
  expect_match(add("mannitol", TCI_UNIT_PLASMA), "osmolality", fixed = TRUE)
  expect_match(add("mannitol", "mg PO"), "no concentration follows a dose", fixed = TRUE)
  expect_match(add("gabapentin", TCI_UNIT_EFFECT), "the model has no effect site", fixed = TRUE)
  expect_match(add("mannitol", TCI_UNIT_EFFECT), "the model has no effect site", fixed = TRUE)
  expect_length(add("mannitol", "g"), 0)
})

test_that("Suggest Dosing writes only units the drug takes, and the library keeps them (G7)", {
  choices <- suggestDrugChoices(G)
  expect_true("propofol" %in% choices)
  # given only as boluses, with an infusion unit of mg/hr in the library
  expect_false(any(c("atropine", "propranolol") %in% choices))
  for (drug in choices) {
    i <- G$Drug == drug
    expect_true(all(c(G$Bolus.Units[i], G$Infusion.Units[i]) %in% unlist(G$Units[i])), info = drug)
  }
  edited <- drugLibraryGridData(G)
  i <- edited$Drug == "propofol"
  edited$Units[i] <- "mg/kg, mg/kg/hr"
  edited$Default.Units[i] <- "mg/kg"
  problems <- drugLibraryProblems(edited, G)
  expect_match(problems, "propofol Bolus.Units", fixed = TRUE)
  expect_match(problems, "propofol Infusion.Units", fixed = TRUE)
  edited$Bolus.Units[i] <- "mg/kg"
  edited$Infusion.Units[i] <- "mg/kg/hr"
  expect_length(drugLibraryProblems(edited, G), 0)
  # a Units edit leaves alone a unit that was not among them before
  edited <- drugLibraryGridData(G)
  i <- edited$Drug == "atropine"
  edited$Units[i] <- paste0(edited$Units[i], ", mcg")
  expect_length(drugLibraryProblems(edited, G), 0)
})

test_that("a level before absorption begins is set aside, and a dose of 0 is no dose (G8)", {
  DT <- oneDose("gabapentin", 300, "mg PO")
  times <- c(6, 120, 240)
  p <- withOutputTimes(times, processdoseTable(DT, noEvents, priorDrugsFor(DT), 300, FALSE))
  p <- p$gabapentin$wide$Plasma[match(times, p$gabapentin$wide$Time)]
  expect_equal(p[1], 0)
  obs <- data.frame(Drug = "gabapentin", Time = times, Concentration = c(0.5, 1.5 * p[2:3]))
  res <- bayesianUpdateDrugs(priorDrugsFor(DT), DT, noEvents, obs, rebuild = rebuildFor(DT),
                             maximum = 300)
  expect_true(res$fits$gabapentin$converged)
  expect_equal(nrow(res$fits$gabapentin$observations), 2)
  o <- res$fits$gabapentin$observations
  expect_true(all(o$Individual > o$Population))
  expect_match(res$problems, "gabapentin: 1 measurement before the drug reaches the plasma",
               fixed = TRUE)

  zero <- oneDose("propofol", 0, "mg")
  res <- bayesianUpdateDrugs(priorDrugsFor(zero), zero, noEvents,
                             data.frame(Drug = "propofol", Time = 30, Concentration = 1),
                             rebuild = rebuildFor(zero))
  expect_length(res$fits, 0)
  expect_false(bayesianPriorName("propofol") %in% names(res$drugs))
  expect_match(res$problems, "propofol has no dose above 0 yet", fixed = TRUE)
})

.sprglobals$config <- oldConfig
