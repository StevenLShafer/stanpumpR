# The limits and guards added after the production UI review of 2026-10-31
# (an external audit replayed bookmark URLs against the app): an empty dose
# table, a schedule that expands past MAX_SCHEDULED_DOSES, a plot taller than
# MAX_PLOT_HEIGHT, the recovery labels of many panels, a bookmark longer than
# the server accepts, a Drug Library entry the plot cannot use, and the model
# guards for a 249 kg or 360 kg patient.  Drafted by Claude Code, 2026-10-31.

test_that("an empty dose table becomes one blank row, and can be drawn", {
  row <- blankDoseTableRow(doseTableBlank[0, ])
  expect_equal(nrow(row), 1L)
  expect_equal(names(row), names(doseTableBlank))
  expect_true(all(unlist(row) == ""))
  hot <- createHOT(doseTableBlank[0, ], getDrugDefaultsGlobal())
  expect_equal(length(hot$x$data), 1L)
})

test_that("scheduledDoseCount counts the repeats without building them", {
  dt <- data.frame(
    Drug = c("acetaminophen", "gabapentin", "oxycodone", "ibuprofen", "hydrocodone", "cefazolin", "pregabalin", "morphine"),
    Time = 0, Dose = 10,
    Units = c("mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg q4h", "mg PO q4h", "mg PO tablet q4h"),
    stringsAsFactors = FALSE
  )
  weeks52 <- 52 * MINS_PER_WEEK
  n <- scheduledDoseCount(dt, weeks52)
  built <- sum(vapply(unique(dt$Drug), function(d) {
    nrow(expandScheduledDoses(dt[dt$Drug == d, ], weeks52)$scheduled)
  }, numeric(1)))
  expect_equal(n, built)
  expect_gt(n, MAX_SCHEDULED_DOSES)                       # the case that disconnected
  expect_lt(scheduledDoseCount(dt[1:3, ], weeks52), MAX_SCHEDULED_DOSES)   # the one that drew
  expect_equal(scheduledDoseCount(dt, 60), 0L)            # no repeat within an hour
  expect_equal(scheduledDoseCount(doseTableInit[1:6, ], weeks52), 0L)
  expect_equal(scheduledDoseCount(NULL, weeks52), 0L)
})

test_that("a plot taller than the graphics device draws is refused with a message", {
  dd <- getDrugDefaultsGlobal()
  dt <- data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg", stringsAsFactors = FALSE)
  pk <- recalculatePK(NULL, dd, dt, age = 40, weight = 70, height = 170, sex = "male")
  sim <- processdoseTable(dt, eventTableInit, pk, 60, FALSE)
  draw <- function(yAxisHeight, pixelRatio = 1) simulationPlot(
    drugs = sim, events = eventTableInit, drugDefaults = dd, eventDefaults = getEventDefaults(),
    xBreaks = 0:6 * 10, xLabels = 0:6 * 10, xAxisLabel = "Time (minutes)", xMinimum = 0, xMaximum = 60,
    plasmaLinetype = "blank", effectsiteLinetype = "solid", normalization = NORMALIZE_NONE,
    plotCompartments = TRUE, typical = "Range", yAxisHeight = yAxisHeight, width = 1200,
    pixelRatio = pixelRatio
  )
  expect_s3_class(draw(200)$plotObject, "ggplot")
  # the devices' own limits
  expect_true(maxPlotDevicePixels() %in% c(MAX_PLOT_DEVICE_PIXELS_RAGG, MAX_PLOT_DEVICE_PIXELS_CAIRO))
  withr::with_options(list(shiny.useragg = FALSE),
                      expect_equal(maxPlotDevicePixels(), MAX_PLOT_DEVICE_PIXELS_CAIRO))
  # propofol with its compartments is three panels: 650 px at 200
  local_mocked_bindings(maxPlotDevicePixels = function() 1000L)
  expect_s3_class(draw(200)$plotObject, "ggplot")
  expect_error(draw(200, pixelRatio = 2), "650 pixels tall, more than the 500 the graphics device can draw on a screen of pixel ratio 2")
  expect_error(draw(350), "1,100 pixels tall, more than the 1,000 the graphics device can draw")
})

test_that("the recovery labels are not limited to 100 rows", {
  dd <- getDrugDefaultsGlobal()
  drugs <- dd$Drug[dd$Category %in% c("Hypnotics and sedatives", "Opioids")]
  units <- vapply(drugs, function(d) unlist(dd$Units[dd$Drug == d])[1], character(1))
  dt <- data.frame(Drug = drugs, Time = 0, Dose = 500, Units = units, stringsAsFactors = FALSE)
  dt <- dt[grepl("^(mg|mcg)( PO)?$", dt$Units), ]
  expect_gt(nrow(dt), 20)
  pk <- recalculatePK(NULL, dd, dt, age = 40, weight = 70, height = 170, sex = "male")
  sim <- processdoseTable(dt, eventTableInit, pk, 60, TRUE)
  p <- simulationPlot(
    drugs = sim, events = eventTableInit, drugDefaults = dd, eventDefaults = getEventDefaults(),
    xBreaks = 0:6 * 10, xLabels = 0:6 * 10, xAxisLabel = "Time (minutes)", xMinimum = 0, xMaximum = 60,
    plasmaLinetype = "blank", effectsiteLinetype = "solid", normalization = NORMALIZE_NONE,
    plotRecovery = TRUE, typical = "Range", yAxisHeight = 200, width = 1200
  )
  labels <- Filter(function(l) is.data.frame(l$data) && "new" %in% names(l$data), p$plotObject$layers)
  expect_gt(max(vapply(labels, function(l) nrow(l$data), integer(1))), 100)
})

test_that("a bookmark carries only the thresholds that were edited", {
  dd <- getDrugDefaultsGlobal()
  expect_null(changedThresholds(dd))
  edited <- dd
  edited$endCe[edited$Drug == "propofol"] <- 2.5
  edited$endCe[edited$Drug == "fentanyl"] <- 0
  saved <- changedThresholds(edited)
  expect_equal(saved, c(propofol = 2.5, fentanyl = 0))
  restored <- restoreThresholds(dd, saved)$drugDefaults
  expect_equal(restored$endCe, edited$endCe)
  # an old bookmark's full column still restores
  full <- stats::setNames(edited$endCe, edited$Drug)
  expect_equal(restoreThresholds(dd, full)$drugDefaults$endCe, edited$endCe)
})

test_that("the Drug Library refuses a colour ggplot cannot read and units the dose table does not accept", {
  dd <- getDrugDefaultsGlobal()
  expect_length(drugLibraryProblems(dd, dd), 0)
  edited <- dd
  i <- which(edited$Drug == "propofol")
  edited$Color[i] <- "notacolor"
  edited$Units[[i]] <- c(edited$Units[[i]], "bogus")
  problems <- drugLibraryProblems(edited, dd)
  expect_length(problems, 2)
  expect_match(problems[1], "propofol: \"notacolor\"")
  expect_match(problems[2], "propofol: \"bogus\"")
  # a valid change passes
  edited <- dd
  edited$Color[i] <- "#123456"
  edited$Units[[i]] <- c("mg", "mcg/kg/min")
  expect_length(drugLibraryProblems(edited, dd), 0)
  expect_true(isValidColor("steelblue"))
  expect_false(isValidColor(""))
  expect_false(isValidColor(NA_character_))
})

test_that("the James lean body mass is held at its peak past the peak", {
  weights <- seq(40, 500, by = 10)
  capped <- vapply(weights, lbmJamesCapped, numeric(1), height = 170, sex = "male")
  expect_true(all(diff(capped) >= 0))
  expect_true(all(capped > 0))
  # unchanged below the peak (BMI 43 for a man)
  expect_equal(lbmJamesCapped(100, 170, "male"), lbmJames(100, 170, "male"))
  expect_equal(lbmJamesCapped(60, 160, "female"), lbmJames(60, 160, "female"))
  # the patients the review found: a curve, not NaN or a negative volume
  pk <- istradefylline(249, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_true(all(is.finite(unlist(pk[c("v1", "v2", "cl1", "cl2")]))))
  expect_true(pk$cl1 > 0)
  expect_true(all(is.finite(unlist(tolcapone(400, 170, 40, "male", adjustToFFM = FALSE)$PK$default[c("v1", "cl1")]))))
})

test_that("sugammadex keeps a positive central volume in the heaviest patient", {
  heavy <- sugammadex(360, 170, 40, "male", adjustToFFM = FALSE)$PK$default
  expect_gt(heavy$v1, 0)
  expect_true(all(is.finite(unlist(heavy[c("v1", "v2", "cl1", "cl2")]))))
  # nothing changes below the cap
  expect_equal(sugammadex(150, 170, 40, "male", adjustToFFM = FALSE)$PK$default$v1,
               4.42 * (1 - 0.00354 * (150 - 74.5)) * (150 / 70))
  # above it the linear term is held at the cap; the (W / 70) scale is not
  expect_equal(sugammadex(250, 170, 40, "male", adjustToFFM = FALSE)$PK$default$v1,
               4.42 * (1 - 0.00354 * (SUGAMMADEX_WEIGHT_CAP - 74.5)) * (250 / 70))
})

test_that("every library model gives a curve at the extremes of the Patient Profile", {
  # The sweep in test-pk-finite-guard.R stops at 250 kg and 190 cm; the review
  # reached 249 kg at 170 cm and 360 kg.  Vancomycin says why it refuses a
  # creatinine clearance near zero, and is allowed to.
  dd <- getDrugDefaultsGlobal()
  drugs <- dd$Drug[!is.na(dd$Category) & vapply(dd$Drug, function(d) exists(d, mode = "function"), logical(1))]
  for (d in drugs) for (w in c(249, 360, 500)) for (ffm in c(TRUE, FALSE)) {
    r <- tryCatch({ getDrugPK(d, w, 170, 40, "male", adjustToFFM = ffm); "ok" },
                  error = function(e) conditionMessage(e))
    if (d == "vancomycin" && grepl("not defined at a creatinine clearance", r)) next
    expect_identical(r, "ok", info = paste(d, w, "kg, adjustToFFM", ffm))
  }
})

test_that("a scheduled dose of 0 for a transit-chain drug simulates", {
  # Among the 200 startup drugs at their startup dose of 0, opicapone's
  # "0 mg PO qd" stopped processdoseTable() with "missing value where
  # TRUE/FALSE needed" (liftCompartments()), and the whole plot was blank.
  dd <- getDrugDefaultsGlobal()
  rows <- data.frame(Drug = "opicapone", Time = 0, Dose = 0, Units = "mg PO qd", stringsAsFactors = FALSE)
  pk <- recalculatePK(NULL, dd, rows, age = 50, weight = 60, height = 168, sex = "female")
  sim <- processdoseTable(rows, eventTableInit, pk, 60, FALSE)
  expect_gt(nrow(sim$opicapone$results), 0)
  expect_true(all(sim$opicapone$results$Y == 0 | is.na(sim$opicapone$results$Y)))
  expect_equal(oralSaturationFraction(c(0, 50), list(form = "power", Dref = 50, exponent = -0.225)), c(1, 1))
  rows$Dose <- 50
  pk <- recalculatePK(NULL, dd, rows, age = 50, weight = 60, height = 168, sex = "female")
  sim <- processdoseTable(rows, eventTableInit, pk, 60, FALSE)
  expect_true("Depot" %in% names(sim$opicapone$compartments))
})

test_that("a bookmark longer than the server accepts is not put in the address bar", {
  short <- paste0("https://example.org/app/?_inputs_&", strrep("a", 100))
  expect_equal(bookmarkForAddressBar(short), list(url = short, problem = NULL))
  long <- paste0("https://example.org/app/?_inputs_&", strrep("a", MAX_BOOKMARK_URL_CHARS))
  link <- bookmarkForAddressBar(long)
  expect_equal(link$url, "https://example.org/app/")
  expect_match(link$problem, "too many settings to be saved in the address bar")
  expect_match(link$problem, format(nchar(long), big.mark = ","), fixed = TRUE)
})

# ---- In the running app (shiny::testServer(), as test-time-units-server.R) ----

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

# The inputs a browser reports when the app opens
uiInputs <- function(...) {
  utils::modifyList(list(
    timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60", minimum = "0",
    plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
    weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
    sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
  ), list(...))
}

test_that("removing every dose-table row leaves one blank row, not a dead session", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, uiInputs())
    startFormatFromInputs(session)
    doseTable(doseTableInit)
    session$flushReact()
    refresh <- doseTableRefresh()
    grid <- createHOT(doseTableInit, drugDefaults(), doseTableFormat())$x
    # What Handsontable reports after "Remove row" on the last row: no data
    expect_no_error(session$setInputs(doseTableHTML = list(
      data = list(), changes = list(event = "afterRemoveRow"), params = grid
    )))
    draft <- doseTableDraft()
    expect_equal(nrow(draft), 1L)
    expect_true(all(unlist(draft) == ""))
    expect_gt(doseTableRefresh(), refresh)                 # and the grid is redrawn
    expect_no_error(createHOT(draft, drugDefaults(), doseTableFormat()))
  })
})

test_that("an unreadable Procedure start is said in the plot area", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, uiInputs(timeMode = "clock", referenceTime = "99:99"))
    startFormatFromInputs(session)
    doseTable(doseTableInit)
    session$flushReact()
    expect_match(referenceTimeProblem(), "\"99:99\" is not a clock time")
    expect_match(plotMessage(), "not a clock time")
    session$setInputs(referenceTime = "08:30")
    expect_null(referenceTimeProblem())
    session$setInputs(referenceTime = "")       # waiting for the browser's clock: silent
    expect_null(referenceTimeProblem())
    session$setInputs(referenceTime = "99:99", timeMode = "relative")   # not read in elapsed time
    expect_null(referenceTimeProblem())
  })
})

test_that("a schedule past MAX_SCHEDULED_DOSES is refused before it is simulated", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, uiInputs(timeUnits = "weeks", maximum = as.character(52 * MINS_PER_WEEK)))
    startFormatFromInputs(session)
    dt <- data.frame(
      Drug = c("acetaminophen", "gabapentin", "oxycodone", "ibuprofen", "hydrocodone", "cefazolin", "pregabalin", "morphine", ""),
      Time = c(rep("0", 8), ""), Dose = c(rep("10", 8), ""),
      Units = c("mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg q4h", "mg PO q4h", "mg PO tablet q4h", ""),
      stringsAsFactors = FALSE
    )
    doseTable(dt)
    session$flushReact()
    expect_equal(plotMaximum(), 52 * MINS_PER_WEEK)
    expect_match(scheduleProblem(), "repeat 17,464 times")
    expect_match(plotMessage(), "scheduled doses repeat")
    expect_error(drugs(), class = "shiny.silent.error")   # nothing is simulated
    doseTable(dt[c(1:3, 9), ])                            # the three-drug case drew
    session$flushReact()
    expect_null(scheduleProblem())
  })
})

test_that("the time until threshold is computed only while it is drawn", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, uiInputs(showThreshold = TRUE))
    startFormatFromInputs(session)
    expect_true(plotRecovery())
    session$setInputs(normalization = "Peak plasma")   # the box is hidden, its value kept
    expect_false(plotRecovery())
    session$setInputs(normalization = "none")
    expect_true(plotRecovery())
  })
})

test_that("a model that fails for the patient is said in the plot area, not a blank plot", {
  shiny::testServer(app_server, {
    do.call(session$setInputs, uiInputs(weight = 20, age = 90, height = 150, creatinine = 15, adjustToFFM = FALSE))
    startFormatFromInputs(session)
    dt <- doseTableBlank
    dt[1, ] <- list("vancomycin", "0", "1000", "mg")
    doseTable(dt)
    session$flushReact()
    expect_match(plotMessage(), "not defined at a creatinine clearance", fixed = TRUE)
    expect_match(plotMessage(), PLOT_FAILURE_PREFIX, fixed = TRUE)
    session$setInputs(creatinine = 1)
    expect_null(plotMessage())
  })
})

.sprglobals$config <- oldConfig
