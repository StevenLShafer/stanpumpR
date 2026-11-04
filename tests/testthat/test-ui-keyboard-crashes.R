# What the 2026-11-02 adversarial keyboard review of the user interface
# reproduced: entries that ended the session ("Reload") or held the R
# process, each driven in a headless browser against the app at 4ed581d.
# Those that #669 (the external-LLM review merged the same night) also fixed
# are tested in test-ui-adversarial-review.R; this file holds the rest, and
# the Email Slide case that started it.  Drafted by Claude Code, 2026-11-02,
# at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

keyboardAppInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE,
  recipient = "someone@example.com", emailComments = "", commentSafe = FALSE
)

propofolTable <- function(dose = "2", time = "0") {
  dt <- doseTableInit[1:3, ]
  dt[1, ] <- list("propofol", time, dose, "mg/kg")
  dt[2:3, ] <- list("", "", "", "")
  dt
}

# --- Email Slide ------------------------------------------------------------

# Reproduced in the browser at 4ed581d: a dose of 2000000000 typed in the
# grid, Apply Changes, then Send; a 40-digit time the same way; and the same
# dose through Add a dose.  Each disconnected.  #669 (the external review of
# the same day) fixed them with simulationBlocked(); this holds the observer
# to it with the table set directly, as a bookmark can carry anything.
test_that("Email Slide with a dose table the simulation refuses says so and keeps the session", {
  alerts <- list()
  local_mocked_bindings(shinyalert = function(title, text = "", ...) {
    alerts[[length(alerts) + 1]] <<- list(title = title, text = text)
  }, .package = "shinyalert")
  local_mocked_bindings(sendSlide = function(...) stop("sendSlide must not be reached"))
  for (dt in list(propofolTable(dose = "2000000000"), propofolTable(time = strrep("1", 40)))) {
    alerts <- list()
    shiny::testServer(app_server, {
      do.call(session$setInputs, keyboardAppInputs)
      setDoseTable(dt, timeFormat("minutes", "relative"))
      session$flushReact()
      expect_no_error(session$setInputs(sendSlide = 1))
      expect_length(alerts, 1)
      expect_match(alerts[[1]]$text, "Dose")
      expect_equal(emailSendCount(), 0)
    })
  }
})

test_that("the SMTP send is bounded", {
  # emayili's own defaults are five attempts, with curl's unlimited transfer
  # and five-minute connect; a stalled send held every session of the worker
  expect_lte(EMAIL_SMTP_ATTEMPTS * (EMAIL_SMTP_TIMEOUT + 1), 180)
  expect_lte(EMAIL_SMTP_CONNECT_TIMEOUT, EMAIL_SMTP_TIMEOUT)
  body <- paste(deparse(sendSlide), collapse = "\n")
  for (option in c("max_times = EMAIL_SMTP_ATTEMPTS", "connecttimeout = EMAIL_SMTP_CONNECT_TIMEOUT",
                   "timeout = EMAIL_SMTP_TIMEOUT")) {
    expect_match(body, option, fixed = TRUE)
  }
  # the options reach curl: emayili passes them on to its handle
  expect_true("..." %in% names(formals(emayili::server)))
  expect_true(all(c("connecttimeout", "timeout") %in% names(curl::curl_options())))
})

# --- Add a dose -------------------------------------------------------------

# A race, reproduced here and not in the browser: the units list follows the
# drug a moment later, and Enter pressed in between sends no units.
test_that("Add a dose with no units chosen asks for them", {
  notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) notes <<- c(notes, as.character(ui)))
  shiny::testServer(app_server, {
    do.call(session$setInputs, keyboardAppInputs)
    setDoseTable(propofolTable(), timeFormat("minutes", "relative"))
    session$flushReact()
    before <- doseTable()
    session$setInputs(addDoseDrug = "fentanyl", addDoseTime = "5", addDoseAmount = "50")
    expect_no_error(session$setInputs(addDoseBtn = 1))
    expect_equal(notes, "Choose the units.")
    expect_identical(doseTable(), before)
    # a unit of another drug is refused the same way
    session$setInputs(addDoseUnits = "mg/kg/hr PO")
    expect_no_error(session$setInputs(addDoseBtn = 2))
    expect_identical(doseTable(), before)
    session$setInputs(addDoseUnits = "mcg")
    session$setInputs(addDoseBtn = 3)
    dt <- doseTable()
    expect_equal(dt$Units[dt$Drug == "fentanyl"], "mcg")
  })
})

# --- Suggest Dosing ---------------------------------------------------------

suggestInputs <- function(session, end = "10") {
  session$setInputs(targetDrug = "propofol", targetEndTime = end,
                    targetTableHTML = list(data = list()))
}

test_that("Suggest Dosing that fails, or finds doses the table cannot hold, leaves the table as it was", {
  notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = " "))
  })
  local_mocked_bindings(hot_to_r = function(...) data.frame(Time = c("0", ""), Target = c("3", "")),
                        .package = "rhandsontable")
  outcomes <- list(
    # nlm()'s error for a target of 1e-301, reproduced in the browser
    function(...) stop("probable coding error in analytic gradient"),
    # no target before the end time (issue #270)
    function(...) NULL,
    # the doses a target of 1e8 suggested, which then stopped every plot and
    # the Email Slide
    function(targetDrug, ...) data.frame(Drug = targetDrug, Time = c(0, 1), Dose = c(2e9, 5e8),
                                         Units = c("mg", "mg/min"))
  )
  messages <- c("probable coding error", "found no target", "cannot hold")
  for (i in seq_along(outcomes)) {
    notes <- character(0)
    local_mocked_bindings(suggest = outcomes[[i]])
    shiny::testServer(app_server, {
      do.call(session$setInputs, keyboardAppInputs)
      # fentanyl only, so the targeted drug's placeholder row is added
      dt <- propofolTable()
      dt$Drug[1] <- "fentanyl"
      dt$Units[1] <- "mcg"
      setDoseTable(dt, timeFormat("minutes", "relative"))
      session$flushReact()
      before <- doseTable()
      suggestInputs(session)
      expect_no_error(session$setInputs(targetOK = 1))
      expect_match(notes[length(notes)], messages[i])
      expect_identical(doseTable(), before)
    })
  }
})

test_that("Suggest Dosing for a drug not in the table keeps the draft and its undo history", {
  # The placeholder row it once wrote into doseTable() reset the draft, so a
  # suggestion that failed lost the unapplied edits and the undo history
  # (CodeRabbit on #674).
  notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = " "))
  })
  local_mocked_bindings(hot_to_r = function(...) data.frame(Time = c("0", ""), Target = c("3", "")),
                        .package = "rhandsontable")
  handed <- NULL
  outcomes <- list(
    function(targetDrug, targetTable, endTime, drugs, ...) {
      handed <<- drugs[[targetDrug]]
      stop("probable coding error in analytic gradient")
    },
    function(...) NULL,
    function(targetDrug, ...) data.frame(Drug = targetDrug, Time = c(0, 1), Dose = c(2e9, 5e8),
                                         Units = c("mg", "mg/min"))
  )
  for (i in seq_along(outcomes)) {
    local_mocked_bindings(suggest = outcomes[[i]])
    shiny::testServer(app_server, {
      do.call(session$setInputs, keyboardAppInputs)
      dt <- propofolTable()
      dt$Drug[1] <- "fentanyl"
      dt$Units[1] <- "mcg"
      setDoseTable(dt, timeFormat("minutes", "relative"))
      session$flushReact()
      committed <- doseTable()
      # an edit not yet applied
      edited <- committed
      edited$Dose[1] <- "75"
      doseTableHistory()$do(edited)
      session$flushReact()
      suggestInputs(session)
      expect_no_error(session$setInputs(targetOK = i))
      expect_identical(doseTable(), committed)
      expect_identical(doseTableDraft(), edited)
      expect_true(doseTableHistory()$can_undo)
    })
  }
  # suggest() was handed the targeted drug's own PK, built without a dose row
  expect_false(is.null(handed))
  expect_identical(handed$drug, "propofol")
})

test_that("Suggest Dosing for a drug not in the table writes its regimen", {
  local_mocked_bindings(showNotification = function(...) NULL)
  local_mocked_bindings(hot_to_r = function(...) data.frame(Time = c("0", ""), Target = c("3", "")),
                        .package = "rhandsontable")
  shiny::testServer(app_server, {
    do.call(session$setInputs, keyboardAppInputs)
    dt <- propofolTable()
    dt$Drug[1] <- "fentanyl"
    dt$Units[1] <- "mcg"
    setDoseTable(dt, timeFormat("minutes", "relative"))
    session$flushReact()
    suggestInputs(session)
    session$setInputs(targetOK = 1)
    out <- cleanDoseTable(doseTable())
    expect_true("propofol" %in% out$Drug)
    expect_true("fentanyl" %in% out$Drug)
    expect_true(all(out$Dose[out$Drug == "propofol"] >= 0))
  })
})

# --- What the grid and the dialogs accept ------------------------------------

test_that("the dose grid cannot be pasted past the row limit", {
  hot <- createHOT(doseTableInit, getDrugDefaultsGlobal())
  expect_equal(hot$x$maxRows, MAX_DOSE_ROWS - 1L)
  # the blank row the server adds below a full grid still fits
  full <- data.frame(Drug = rep("propofol", MAX_DOSE_ROWS - 1L), Time = "0", Dose = "1", Units = "mg")
  full[nrow(full) + 1, ] <- ""
  expect_true(validateDoseTableInput(full))
})

# The grid's own validateDose() and validateTime() (inst/www/hot_funs.js) must
# agree with R's: the server refuses a stored time that R's validateTime()
# would change, and a dose the grid accepted and R refuses is what hid the
# grid behind an error.
test_that("the grid's validators agree with R's, limits included", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- normalizePath(system.file("www", "hot_funs.js", package = "stanpumpR"), winslash = "/")
  src <- readLines(js)
  jsConstant <- function(name) {
    line <- grep(sprintf("^var %s = ", name), src, value = TRUE)
    expect_length(line, 1)
    as.numeric(sub(sprintf("^var %s = ([^;]+);.*$", name), "\\1", line))
  }
  expect_equal(jsConstant("MAX_DOSE_VALUE"), MAX_DOSE_VALUE)
  expect_equal(jsConstant("MAX_TIME_STRING_LENGTH"), MAX_TIME_STRING_LENGTH)
  entries <- c("2", "1000000000", "1000000001", "2000000000", "1e9", "1e10", "1e300", strrep("9", 40),
               strrep("1", 32), strrep("1", 33), "1e40", "0:80", "99:30", "12345678901:00",
               paste0(strrep("9", 40), ":00"), "9007199254740992:00", "-5", "abc", "1,000", "1,5", ".5")
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c(
    "const vm = require('vm'); const fs = require('fs');",
    sprintf("vm.runInThisContext(fs.readFileSync(%s, 'utf8'));", jsonlite::toJSON(js, auto_unbox = TRUE)),
    sprintf("const e = %s;", jsonlite::toJSON(entries)),
    "process.stdout.write(JSON.stringify({dose: e.map(x => String(validateDose(x))), time: e.map(validateTime)}));"
  ), script)
  out <- jsonlite::fromJSON(paste(system2(Sys.which("node"), shQuote(script), stdout = TRUE), collapse = ""))
  expect_equal(out$dose, vapply(entries, validateDose, character(1), USE.NAMES = FALSE))
  # the grid strips a leading zero R keeps; neither applies to these
  expect_equal(out$time, vapply(entries, validateTime, character(1), USE.NAMES = FALSE))
})

# --- The Debug section --------------------------------------------------------

test_that("debugLevel() is always one level", {
  expect_equal(debugLevel("1"), 1)
  expect_equal(debugLevel(2), 2)
  expect_equal(debugLevel("abc"), DEBUG_LEVEL_OFF)
  expect_equal(debugLevel(""), DEBUG_LEVEL_OFF)
  expect_equal(debugLevel(NULL, 1), 1)
  expect_equal(debugLevel(NA, 2), 2)
  expect_equal(debugLevel("1e400"), DEBUG_LEVEL_OFF)
  expect_equal(debugLevel("-5"), DEBUG_LEVEL_OFF)
  expect_equal(debugLevel("99"), DEBUG_LEVEL_VERBOSE)
  expect_equal(debugLevel("1.4"), 1)
})

test_that("an empty profiler threshold does not stop the profiled observers", {
  shiny::testServer(app_server, {
    session$userData$debug(DEBUG_LEVEL_NORMAL)
    for (threshold in list(NA, NULL, "abc", -1)) {
      session$setInputs(profiler_threshold = threshold)
      expect_equal(profileCode(1 + 1, "test"), 2)
    }
    expect_true(nrow(profileRecords()) >= 1)
    expect_no_error(output$profiling)
  })
})

.sprglobals$config <- oldConfig

# --- Pasting into the dose grid ------------------------------------------------

# Every pasted unit was dropped for the drug's default, so "morphine 0 10 mg PO
# tablet" became 10 mg intravenous: the Units dropdown validated it against
# the row's list before the drug was in place.  Reproduced in Chromium at
# 7491b43; #675 fixed the created row's lost time and dose, and this the unit
# and the Enter-twice reset.  The hooks run in Node on a stand-in grid, with
# the events in the order Chromium gave them.
test_that("a paste keeps every row's time, dose and unit", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- normalizePath(system.file("www", "hot_funs.js", package = "stanpumpR"), winslash = "/")
  dd <- getDrugDefaultsGlobal()
  defaults <- lapply(c("fentanyl", "morphine"), function(d) list(
    Drug = d, Units = unlist(dd$Units[dd$Drug == d]), Default.Units = dd$Default.Units[dd$Drug == d]
  ))
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c(
    "const vm = require('vm'); const fs = require('fs');",
    # the grid's warnings, collected
    "const notes = []; global.Shiny = { notifications: { show: n => notes.push(n.html) } }; global.window = { Shiny: global.Shiny };",
    sprintf("global.drug_defaults = %s;", jsonlite::toJSON(defaults, auto_unbox = TRUE)),
    sprintf("vm.runInThisContext(fs.readFileSync(%s, 'utf8'));", jsonlite::toJSON(js, auto_unbox = TRUE)),
    "function grid(rows) { return { data: rows, meta: {},",
    "  getSettings() { return { colHeaders: ['Drug', 'Time', 'Dose/Target', 'Units'] }; },",
    "  getDataAtRow(r) { return this.data[r]; }, getDataAtCell(r, c) { return this.data[r][c]; },",
    "  setCellMeta(r, c, k, v) { this.meta[r + ':' + c + ':' + k] = v; },",
    "  setDataAtCell(ch) { ch.forEach(x => { this.data[x[0]][x[1]] = x[2]; }); } }; }",
    # A paste as Handsontable delivers it: beforePaste with the clipboard
    # and the selection; a clipboard shorter than the selection repeated over
    # it; the Units cell never among the changes (the dropdown refuses it
    # against the row's old list); the created row's no-op edits as Chromium
    # logged them (#675), Time and Dose before the paste's own change, Drug
    # and Units after it.  Returns the grid and the warnings given.
    "function paste(hot, startRow, lines, opts) {",
    "  opts = opts || {}; const startCol = opts.startCol || 0;",
    "  const endRow = Math.max(startRow + lines.length - 1, opts.endRow === undefined ? -1 : opts.endRow);",
    "  notes.length = 0;",
    "  hookRememberPastedUnits.call(hot, lines, [{ startRow: startRow, startCol: startCol, endRow: opts.endRow }]);",
    "  const changes = [], created = [];",
    "  for (let r = startRow; r <= endRow; r++) { const l = lines[(r - startRow) % lines.length];",
    "    if (!hot.data[r]) { hot.data[r] = [null, null, null, null]; created.push(r); }",
    "    l.forEach((v, i) => { const c = startCol + i; if (c > 2) return;",
    "      changes.push([r, c, hot.data[r][c], v]); hot.data[r][c] = v; }); }",
    "  created.forEach(r => [1, 2].forEach(c => hookDoseTableUpdate.call(hot, [[r, c, null, null]], 'edit')));",
    "  hookDoseTableUpdate.call(hot, changes, 'CopyPaste.paste');",
    "  created.forEach(r => [0, 3].forEach(c => hookDoseTableUpdate.call(hot, [[r, c, null, null]], 'edit')));",
    "  return { data: hot.data, notes: notes.slice() }; }",
    "const blank = () => [['propofol', '0', '2', 'mg/kg'], ['', '', '', ''], ['', '', '', ''], ['', '', '', '']];",
    "process.stdout.write(JSON.stringify({",
    "  created: paste(grid(blank()), 2, [['fentanyl', '5', '50', 'mcg'], ['fentanyl', '10', '50', 'mcg'], ['fentanyl', '15', '2', 'mcg/kg']]),",
    "  oral: paste(grid(blank()), 1, [['morphine', '0', '10', 'mg PO tablet'], ['morphine', '60', '10', 'mg PO liquid']]),",
    "  refused: paste(grid(blank()), 1, [['morphine', '0', '10', 'mg PO']]),",
    "  noUnit: paste(grid(blank()), 1, [['morphine', '0', '10']]),",
    # Enter pressed twice on a Drug cell re-commits the drug it holds
    "  enterTwice: (function() { const hot = grid([['morphine', '30', '10', 'mg PO tablet'], ['', '', '', '']]);",
    "    hookDoseTableUpdate.call(hot, [[0, 0, 'morphine', 'morphine']], 'edit'); return hot.data; })(),",
    # a paste over a filled row that changes the drug and repeats its time and
    # dose: the repeated cells dropped out as no-ops, and the drug change
    # alone reset them to 0 (CodeRabbit on #676)
    "  drugOverRow: paste(grid([['propofol', '0', '2', 'mg/kg'], ['', '', '', '']]), 0, [['fentanyl', '0', '2', 'mcg/kg']]),",
    # the same with no unit: the old drug's mg/kg must not stay on fentanyl
    "  drugNoUnit: paste(grid([['propofol', '0', '2', 'mg/kg'], ['', '', '', '']]), 0, [['fentanyl', '0', '2']]),",
    # a refused unit over a row that held an offered one: not used, and said
    "  refusedOverUnit: paste(grid([['fentanyl', '5', '50', 'mcg/kg'], ['', '', '', '']]), 0, [['fentanyl', '10', '50', 'furlongs']]),",
    # Time, Dose and a refused unit, without the Drug column
    "  noDrugColumn: paste(grid([['fentanyl', '5', '50', 'mcg/kg'], ['', '', '', '']]), 0, [['10', '50', 'furlongs']], { startCol: 1 }),",
    # a Units cell alone
    "  unitsOnly: paste(grid([['fentanyl', '5', '50', 'mcg'], ['', '', '', '']]), 0, [['furlongs']], { startCol: 3 }),",
    # an offered unit with spaces round it: the clipboard text the dropdown
    # checks is the trimmed unit (CodeRabbit on #676)
    "  padded: (function() { const hot = grid([['morphine', '5', '10', 'mg PO liquid'], ['', '', '', '']]);",
    "    const lines = [[' mg PO tablet ']]; notes.length = 0;",
    "    hookRememberPastedUnits.call(hot, lines, [{ startRow: 0, startCol: 3 }]);",
    "    return { lines: lines, remembered: hot.pastedUnits[0], notes: notes.slice() }; })(),",
    # a refused unit in a row that is otherwise pasted as it was: no change
    # reaches afterChange, and the warning must still be given
    "  unchanged: paste(grid([['fentanyl', '5', '50', 'mcg/kg'], ['', '', '', '']]), 0, [['fentanyl', '5', '50', 'furlongs']]),",
    # one row pasted over three selected rows
    "  repeated: paste(grid(blank()), 1, [['morphine', '0', '10', 'mg PO tablet']], { endRow: 3 })",
    "}));"
  ), script)
  out <- jsonlite::fromJSON(paste(system2(Sys.which("node"), shQuote(script), stdout = TRUE), collapse = ""),
                            simplifyVector = FALSE)
  row <- function(x) unlist(lapply(x, function(v) if (is.null(v)) NA else as.character(v)))
  fentanylDefault <- dd$Default.Units[dd$Drug == "fentanyl"]
  morphineDefault <- dd$Default.Units[dd$Drug == "morphine"]
  expect_equal(row(out$created$data[[5]]), c("fentanyl", "15", "2", "mcg/kg"))
  expect_equal(row(out$created$data[[3]]), c("fentanyl", "5", "50", "mcg"))
  expect_equal(row(out$oral$data[[2]]), c("morphine", "0", "10", "mg PO tablet"))
  expect_equal(row(out$oral$data[[3]]), c("morphine", "60", "10", "mg PO liquid"))
  expect_length(out$oral$notes, 0)
  # a unit the drug does not offer is not taken, and the grid says so; the
  # row has the drug's default, as a row with no pasted unit does
  expect_equal(row(out$refused$data[[2]]), c("morphine", "0", "10", morphineDefault))
  expect_match(out$refused$notes[[1]], "mg PO.*morphine.*not used")
  expect_equal(row(out$noUnit$data[[2]]), c("morphine", "0", "10", morphineDefault))
  expect_length(out$noUnit$notes, 0)
  # was morphine 0 0 with its default unit
  expect_equal(row(out$enterTwice[[1]]), c("morphine", "30", "10", "mg PO tablet"))
  expect_equal(row(out$drugOverRow$data[[1]]), c("fentanyl", "0", "2", "mcg/kg"))
  # was fentanyl in mg/kg
  expect_equal(row(out$drugNoUnit$data[[1]]), c("fentanyl", "0", "2", fentanylDefault))
  # a refused unit leaves the row's own offered unit, and is said
  expect_equal(row(out$refusedOverUnit$data[[1]]), c("fentanyl", "10", "50", "mcg/kg"))
  expect_length(out$refusedOverUnit$notes, 1)
  expect_equal(row(out$noDrugColumn$data[[1]]), c("fentanyl", "10", "50", "mcg/kg"))
  expect_length(out$noDrugColumn$notes, 1)
  expect_equal(row(out$unitsOnly$data[[1]]), c("fentanyl", "5", "50", "mcg"))
  expect_match(out$unitsOnly$notes[[1]], "furlongs.*fentanyl.*not used")
  expect_equal(out$padded$lines[[1]][[1]], "mg PO tablet")
  expect_equal(out$padded$remembered, "mg PO tablet")
  expect_length(out$padded$notes, 0)
  expect_equal(row(out$unchanged$data[[1]]), c("fentanyl", "5", "50", "mcg/kg"))
  expect_length(out$unchanged$notes, 1)
  for (r in 2:4) {
    expect_equal(row(out$repeated$data[[r]]), c("morphine", "0", "10", "mg PO tablet"), info = r)
  }
})
