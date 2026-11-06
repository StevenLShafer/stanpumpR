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
                    targetTableHTML = tableGridInputValue(list(list("0", "3"), list("", ""))))
}

test_that("Suggest Dosing that fails, or finds doses the table cannot hold, leaves the table as it was", {
  notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) {
    notes <<- c(notes, paste(as.character(ui), collapse = " "))
  })
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

# --- What the grid and the dialogs accept ------------------------------------

test_that("the dose grid cannot be pasted past the row limit", {
  html <- as.character(doseGridInput("doseGrid"))
  expect_match(html, sprintf('data-max-rows="%d"', MAX_DOSE_ROWS - 1L), fixed = TRUE)
  # the blank row the server adds below a full grid still fits
  full <- data.frame(Drug = rep("propofol", MAX_DOSE_ROWS - 1L), Time = "0", Dose = "1", Units = "mg")
  full[nrow(full) + 1, ] <- ""
  expect_true(validateDoseTableInput(full))
})

# The grid's own validateDose() and validateTime() (inst/www/grid-funs.js) must
# agree with R's: the server refuses a stored time that R's validateTime()
# would change, and a dose the grid accepted and R refuses is what hid the
# grid behind an error.
test_that("the grid's validators agree with R's, limits included", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- normalizePath(system.file("www", "grid-funs.js", package = "stanpumpR"), winslash = "/")
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

# Pasting into the dose grid (whole rows keep their time, dose and offered
# unit; Enter twice on a drug keeps its row) is the browser suite's:
# tests/playwright/dose-grid.test.js, on the grid itself.
