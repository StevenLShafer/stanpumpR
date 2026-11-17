# The dose grid (R/dose-grid.R, inst/www/dose-grid.js;
# docs/grid-replacement.md): the server's reading of what the
# browser sends (untrusted: anyone can send their own session anything), the
# observer that stores it, and the browser code's own rules.  The behaviour in
# the browser is tests/playwright's.  Drafted by Claude Code, 2026-11-04, at
# the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

gridInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "240",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "male", showThreshold = FALSE, normalization = "none", typical = "Range", logY = FALSE
)

# What the grid sends: through the JSON Shiny reads it from
gridValue <- doseGridInputValue

lastRowsSent <- function(sent) {
  rows <- Filter(function(m) identical(m$id, "doseGrid") && !is.null(m$message$rows), sent$inputs)
  if (length(rows) == 0) NULL else rows[[length(rows)]]$message
}

recordGrid <- function(session) {
  sent <- new.env()
  sent$inputs <- list()
  session$sendInputMessage <- function(inputId, message) {
    sent$inputs[[length(sent$inputs) + 1]] <- list(id = inputId, message = message)
  }
  sent
}

startGrid <- function(session) {
  do.call(session$setInputs, gridInputs)
  session$env$setDoseTable(doseTableInit, timeFormat("minutes", "relative"))
  session$flushReact()
}

test_that("the grid's place in the page carries its header and row limit, and no input of its own", {
  html <- as.character(doseGridInput("doseGrid"))
  expect_match(html, 'class="stanpumpr-dose-grid"', fixed = TRUE)
  expect_match(html, sprintf('data-dose-header="%s"', DOSE_COLUMN_HEADER), fixed = TRUE)
  expect_match(html, sprintf('data-max-rows="%d"', MAX_DOSE_ROWS - 1L), fixed = TRUE)
  expect_false(grepl("<input|<select", html))
  # and its value is never written into a bookmark
  expect_true("doseGrid" %in% bookmarksToExclude)
})

test_that("what the browser sends is read as a table of strings, whatever it holds", {
  v <- doseGridValue(gridValue(list(list("propofol", "0", "2", "mg/kg"), list(NULL, NULL, NULL, NULL))))
  expect_equal(v$table, data.frame(Drug = c("propofol", ""), Time = c("0", ""), Dose = c("2", ""),
                                   Units = c("mg/kg", ""), stringsAsFactors = FALSE))
  expect_equal(v$format, timeFormat("minutes", "relative"))
  expect_equal(v$seq, 1)
  # a number is its decimal; a nested value, an object or a short row is blank
  v <- doseGridValue(gridValue(list(list("propofol", 0, 1e3, "mg"), list(list("a", "b"), list(x = 1), "x"))))
  expect_equal(v$table$Dose[1], "1000")
  expect_equal(unlist(v$table[2, ]), c(Drug = "", Time = "", Dose = "x", Units = ""))
  # rows past the grid's limit are cut to it, and that is said (the
  # observer shows a notification when `truncated` is set)
  many <- rep(list(list("fentanyl", "0", "1", "mcg")), MAX_DOSE_ROWS + 50)
  v <- doseGridValue(gridValue(many))
  expect_equal(nrow(v$table), MAX_DOSE_ROWS - 1L)
  expect_true(v$truncated)
  expect_false(doseGridValue(gridValue(many[seq_len(MAX_DOSE_ROWS - 1L)]))$truncated)
  # 499 doses and the blank row the grid keeps below them are not cut, nor said
  full <- c(many[seq_len(MAX_DOSE_ROWS - 1L)], list(list("", "", "", "")))
  v <- doseGridValue(gridValue(full))
  expect_false(v$truncated)
  expect_equal(nrow(v$table), MAX_DOSE_ROWS)
  # 500 doses are: the 500th is cut
  v <- doseGridValue(gridValue(many[seq_len(MAX_DOSE_ROWS)]))
  expect_true(v$truncated)
  expect_equal(sum(nzchar(v$table$Drug)), MAX_DOSE_ROWS - 1L)
  # not a grid's value at all
  for (bad in list(NULL, "x", 1, list(), list(rows = NULL), list(rows = list(a = function() 1)))) {
    expect_no_error(v <- doseGridValue(bad))
  }
  expect_null(doseGridValue("x"))
  # an unreadable format or seq
  v <- doseGridValue(gridValue(list(list("propofol", "0", "2", "mg")), format = "parsecs/x", seq = "abc"))
  expect_null(v$format)
  expect_equal(v$seq, 0)
})

test_that("the grid is told every drug's units, which are illicit, and what to offer", {
  dd <- getDrugDefaultsGlobal()
  msg <- doseGridDrugsMessage(dd, c("fentanyl", "propofol"))
  expect_length(msg$drugs, nrow(dd))
  fentanyl <- Filter(function(d) d$Drug == "fentanyl", msg$drugs)[[1]]
  expect_equal(as.character(fentanyl$Units), unlist(dd$Units[dd$Drug == "fentanyl"]))
  expect_equal(fentanyl$DefaultUnits, dd$Default.Units[dd$Drug == "fentanyl"])
  expect_false(fentanyl$Illicit)
  cocaine <- Filter(function(d) d$Drug == "cocaine", msg$drugs)[[1]]
  expect_true(cocaine$Illicit)
  # a one-unit drug still goes as an array, and so does a one-name list
  json <- jsonlite::toJSON(doseGridDrugsMessage(dd[dd$Drug == "sevoflurane", ], "sevoflurane"), auto_unbox = TRUE)
  expect_match(json, '"Units":["%"]', fixed = TRUE)
  expect_match(json, '"choices":["sevoflurane"]', fixed = TRUE)
})

test_that("an edit from the grid becomes the draft, and the grid is sent what the server holds", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    rows <- list(list("propofol", "0", "100", "mg"), list("fentanyl", "5", "50", "mcg"))
    session$setInputs(doseGrid = gridValue(rows, seq = 3))
    expect_false(session$isClosed())
    # the blank row below the last drug is added, as for the old grid
    expect_equal(doseTableDraft()$Drug, c("propofol", "fentanyl", ""))
    expect_true(doseTableHistory()$can_undo)
    msg <- lastRowsSent(sent)
    expect_equal(msg$ack, 3)
    expect_equal(msg$timeFormat, "minutes/relative")
    expect_equal(as.character(msg$rows[[2]]), c("fentanyl", "5", "50", "mcg"))
    # undo is the server's, and the grid is sent the result
    session$setInputs(dosetable_undo = 1)
    expect_equal(doseTableDraft()$Drug[1:6], doseTableInit$Drug[1:6])
    expect_equal(as.character(lastRowsSent(sent)$rows[[1]]), unlist(doseTableInit[1, ], use.names = FALSE))
  })
})

test_that("an edit from a grid drawn before a change of time unit is converted", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    session$setInputs(timeUnits = "hours")
    expect_equal(doseTableFormat(), timeFormat("hours", "relative"))
    # typed in minutes, in a grid not yet told of the change
    session$setInputs(doseGrid = gridValue(list(list("propofol", "90", "100", "mg")), "minutes/relative", 1))
    expect_equal(doseTableDraft()$Time[1], "1.5")
    expect_equal(lastRowsSent(sent)$timeFormat, "hours/relative")
    expect_equal(as.character(lastRowsSent(sent)$rows[[1]])[2], "1.5")
  })
})

test_that("a hostile or broken value from the browser leaves the session alive", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    before <- doseTableDraft()
    for (bad in list("x", list(rows = "abc"), list(rows = list(1, 2, 3)), list(seq = 4),
                     gridValue(list(list("<img src=x onerror=alert(1)>", "0", "1", "mg"))),
                     gridValue(list(list("propofol", "-5", "1e400", "furlongs"))))) {
      session$setInputs(doseGrid = bad)
      expect_false(session$isClosed())
    }
    # none of it reached the draft as it came: what the grid would refuse is
    # blanked first (doseGridAccepted()), so the draft passes the table's own
    # checks
    expect_true(isTRUE(validateDoseTableInput(doseTableDraft())))
    expect_false(any(doseTableDraft()$Units == "furlongs"))
    # every row removed: one blank row, as for the old grid
    session$setInputs(doseGrid = gridValue(list(), seq = 99))
    expect_false(session$isClosed())
    expect_equal(nrow(doseTableDraft()), 1L)
    expect_false(anyNA(doseTableDraft()))
  })
})

test_that("an edit the server leaves unchanged is sent back, so the grid shows the draft", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    n <- length(sent$inputs)
    draft <- doseTableDraft()
    rows <- lapply(seq_len(nrow(draft)), function(i) as.list(unname(unlist(draft[i, ]))))
    session$setInputs(doseGrid = gridValue(rows, seq = 7))
    session$flushReact()
    expect_gt(length(sent$inputs), n)
    expect_equal(lastRowsSent(sent)$ack, 7)
  })
})

test_that("Ctrl+Z and Ctrl+Y from the grid undo and redo the draft, and a broken request is ignored", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    session$setInputs(doseGrid = gridValue(list(list("propofol", "0", "100", "mg")), seq = 1))
    expect_equal(doseTableDraft()$Dose[1], "100")
    session$setInputs(doseGrid_history = list(action = "undo", n = 1))
    expect_equal(doseTableDraft()$Dose[1], "0")
    session$setInputs(doseGrid_history = list(action = "redo", n = 2))
    expect_equal(doseTableDraft()$Dose[1], "100")
    # nothing to redo, then requests that are not requests at all
    for (bad in list(list(action = "redo", n = 3), "undo", list(action = list("undo", "redo")), list(n = 4),
                     list(action = "drop table", n = 5), 42)) {
      session$setInputs(doseGrid_history = bad)
      expect_false(session$isClosed())
      expect_equal(doseTableDraft()$Dose[1], "100")
    }
    # undo past the start of the history does nothing
    session$setInputs(doseGrid_history = list(action = "undo", n = 6))
    session$setInputs(doseGrid_history = list(action = "undo", n = 7))
    session$setInputs(doseGrid_history = list(action = "undo", n = 8))
    expect_false(session$isClosed())
    expect_equal(doseTableDraft()$Dose[1], "0")
  })
  expect_true("doseGrid_history" %in% bookmarksToExclude)
})

# The values the external review of 2026-11-05 sent from the browser's
# console, each of which was stored, drawn or applied
test_that("a cell the grid would not accept is blanked before it is stored, and said", {
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    notes <- character(0)
    session$sendNotification <- function(type, message) notes <<- c(notes, paste(unlist(message), collapse = " "))
    startGrid(session)
    # a unit fentanyl does not offer: blanked, never swapped for its default
    session$setInputs(doseGrid = gridValue(list(list("fentanyl", "0", "50", "mg/kg")), seq = 20))
    expect_equal(unlist(doseTableDraft()[1, ], use.names = FALSE), c("fentanyl", "0", "50", ""))
    expect_true(any(grepl("could not be read", notes)))
    # a dose over the limit, a 1 MB string, an unknown drug
    session$setInputs(doseGrid = gridValue(list(
      list("fentanyl", "0", "1000000001", "mcg"),
      list("fentanyl", "0", strrep("x", 2^20), "mcg"),
      list("abc", "1", "2", "mg")
    ), seq = 21))
    expect_false(session$isClosed())
    expect_equal(doseTableDraft()$Dose[1:2], c("", ""))
    expect_equal(unlist(doseTableDraft()[3, ], use.names = FALSE), c("", "", "", ""))
    expect_true(isTRUE(validateDoseTableInput(doseTableDraft())))
    # a table the grid did send is stored as it is, without a word
    n <- length(notes)
    session$setInputs(doseGrid = gridValue(list(list("propofol", "0", "100", "mg")), seq = 22))
    expect_equal(unlist(doseTableDraft()[1, ], use.names = FALSE), c("propofol", "0", "100", "mg"))
    expect_equal(length(notes), n)
  })
})

test_that("a negative or unreadable seq cannot make the grid ignore the draft", {
  expect_equal(doseGridValue(gridValue(list(list("propofol", "0", "1", "mg")), seq = -1))$seq, 0)
  expect_equal(doseGridValue(gridValue(list(list("propofol", "0", "1", "mg")), seq = "x"))$seq, 0)
  shiny::testServer(app_server, {
    sent <- recordGrid(session)
    startGrid(session)
    session$setInputs(doseGrid = gridValue(list(list("propofol", "0", "1", "mg")), seq = 5))
    expect_equal(lastRowsSent(sent)$ack, 5)
    # the acknowledgement never falls back
    session$setInputs(doseGrid = gridValue(list(list("propofol", "1", "1", "mg")), seq = -1))
    expect_equal(lastRowsSent(sent)$ack, 5)
    expect_equal(as.character(lastRowsSent(sent)$rows[[1]])[2], "1")
  })
})

test_that("the grid's code draws nothing as HTML, and binds no cell as a Shiny input", {
  js <- readLines(system.file("www", "dose-grid.js", package = "stanpumpR"))
  code <- js[!grepl("^\\s*//", js)]
  expect_false(any(grepl("innerHTML|outerHTML|insertAdjacentHTML|document\\.write|\\.html\\(", code)))
  # the cells' controls carry the attribute Shiny skips, and no id or name
  expect_true(any(grepl("data-shiny-no-bind-input", code, fixed = TRUE)))
  expect_false(any(grepl("setAttribute\\('(name|id)'", code)))
})

test_that("the grid reads a spreadsheet's clipboard", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- normalizePath(system.file("www", "dose-grid.js", package = "stanpumpR"), winslash = "/")
  cases <- c("a\tb\n1\t2\n", "a\tb\r\n1\t2", "\"x\ty\"\t2", "\"say \"\"hi\"\"\"\t1", "one", "a\t\tb")
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c(
    sprintf("const g = require(%s);", jsonlite::toJSON(js, auto_unbox = TRUE)),
    sprintf("const c = %s;", jsonlite::toJSON(cases)),
    "process.stdout.write(JSON.stringify(c.map(g.parseTSV)));"
  ), script)
  out <- jsonlite::fromJSON(paste(system2(Sys.which("node"), shQuote(script), stdout = TRUE), collapse = ""),
                            simplifyVector = FALSE)
  out <- lapply(out, function(rows) lapply(rows, unlist))
  expect_equal(out[[1]], list(c("a", "b"), c("1", "2")))
  expect_equal(out[[2]], list(c("a", "b"), c("1", "2")))
  expect_equal(out[[3]], list(c("x\ty", "2")))
  expect_equal(out[[4]], list(c("say \"hi\"", "1")))
  expect_equal(out[[5]], list("one"))
  expect_equal(out[[6]], list(c("a", "", "b")))
})

.sprglobals$config <- oldConfig
