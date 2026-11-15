# The input findings of Carbon's (Gemini) third adversarial UI audit, of PR
# #730 at 4164743, 2026-11-15, each reproduced before it was fixed: a dose
# the grid took and the server blanked (M4), the Help search box and Email
# Slide (M5), Single Paper Review's Discard button (H9), and the small ones
# (L1): a regimen with no id, the report's download, the CSV export.  The
# grids' own findings (C3, and L1's timers) are checked in a browser by
# tests/playwright/dose-grid.test.js and table-grid.test.js.  Drafted by
# Claude Code, 2026-11-15, at the request of Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

# --- M4: a dose the grid took and the server blanked -------------------------

# Numbers within MAX_DOSE_VALUE whose stored string is longer than
# MAX_TIME_STRING_LENGTH, which restoredDoseTable() blanks in every Dose cell
# the grid sends; and their neighbours that fit.
longDoses <- c("1e-35", "100.00000000000000000000000000001", "000000000000000000000000000000100",
               "0.0000000000000000000000000000001", "1e-31")
fittingDoses <- c("1e-30", "0.000000000000000000000000000001", "100.0000000000000000000000000001",
                  "00000000000000000000000000000100", "2.5", "1e9", "1,000")

test_that("M4: validateDose() refuses a dose the dose grid's server would blank", {
  for (x in longDoses) expect_identical(validateDose(x), "", info = x)
  for (x in fittingDoses) expect_true(nzchar(validateDose(x)), info = x)
  # Whatever validateDose() takes, restoredDoseTable() keeps, unchanged: the
  # grid and the server hold the same table
  dd <- getDrugDefaultsGlobal()
  for (x in c(longDoses, fittingDoses)) {
    v <- validateDose(x)
    if (!nzchar(v)) next
    table <- data.frame(Drug = "propofol", Time = "0", Dose = v, Units = "mg", stringsAsFactors = FALSE)
    accepted <- doseGridAccepted(table, dd)
    expect_false(accepted$changed, info = x)
    expect_identical(accepted$table$Dose, v, info = x)
  }
  # A dialog's field is refused there too (validateEntry())
  expect_identical(validateEntry("1e-35", validateDose), "")
  # and a number arriving as a number
  expect_identical(validateDose(1e-35), "")
})

test_that("M4: the grid's validateDose() agrees with R's on the long doses", {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  js <- normalizePath(system.file("www", "grid-funs.js", package = "stanpumpR"), winslash = "/")
  entries <- c(longDoses, fittingDoses)
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  writeLines(c(
    "const vm = require('vm'); const fs = require('fs');",
    sprintf("vm.runInThisContext(fs.readFileSync(%s, 'utf8'));", jsonlite::toJSON(js, auto_unbox = TRUE)),
    sprintf("const e = %s;", jsonlite::toJSON(entries)),
    "process.stdout.write(JSON.stringify(e.map(x => String(validateDose(x)))));"
  ), script)
  out <- jsonlite::fromJSON(paste(system2(Sys.which("node"), shQuote(script), stdout = TRUE), collapse = ""))
  expect_equal(out, vapply(entries, validateDose, character(1), USE.NAMES = FALSE))
})

test_that("M4: the converter's decrement times refuse a longest infusion of Infinity minutes", {
  # The page's guard (decrement() in inst/www/pk-convert.js) sits beside the
  # one it slipped past; what it spares the page is the controller run to
  # its update limit, which an enormous finite infusion still meets, by
  # design, and says so ("not computed").
  src <- readLines(system.file("www", "pk-convert.js", package = "stanpumpR"))
  guard <- grep("until !== null && !isFinite(until * f)", src, fixed = TRUE)
  floor <- grep("until !== null && !(until * f >= TCI_UPDATE_MINUTES)", src, fixed = TRUE)
  expect_length(guard, 1)
  expect_length(floor, 1)
  expect_lt(guard, floor)
})

# --- M5: the Help search box ---------------------------------------------------

# What helpSearch() was asked to find, if anything (its mock writes it)
helpSearched <- new.env()

helpSearchServer <- function(search) {
  results <- new.env()
  helpSearched$term <- NULL
  # helpServer() takes app_server()'s arguments; testServer() passes them
  # only to a module
  module <- function(id, ...) shiny::moduleServer(id, function(input, output, session) {
    helpServer(input, output, session, ...)
  })
  shiny::testServer(module, args = list(
    id = "help", doseTable = shiny::reactiveVal(NULL), eventTable = shiny::reactiveVal(NULL),
    drugDefaults = shiny::reactiveVal(getDrugDefaultsGlobal())), {
    session$setInputs(help_search = search)
    session$elapse(400)
    results$html <- tryCatch(paste(as.character(output$help_search_results$html), collapse = ""),
                             error = function(e) paste("ERROR", conditionMessage(e)))
  })
  list(term = helpSearched$term, html = results$html)
}

test_that("M5: the Help search reads one string of at most the box's length", {
  local_mocked_bindings(helpSearch = function(term, ...) {
    helpSearched$term <- term
    data.frame(id = character(0), title = character(0), sectionTitle = character(0),
               hits = integer(0), snippet = character(0), stringsAsFactors = FALSE)
  })
  # A vector put a condition of length 2 in the results' if(), which showed
  # R's error in the results
  two <- helpSearchServer(c("ab", "cd"))
  expect_null(two$term)
  expect_false(grepl("ERROR", two$html))
  expect_null(helpSearchServer(list("ab"))$term)
  expect_null(helpSearchServer(character(0))$term)
  # A megabyte is cut to the box's maxlength before it is searched for
  long <- helpSearchServer(strrep("a", 1e6))
  expect_identical(nchar(long$term), HELP_SEARCH_MAX_CHARS)
  # and the box says the same
  html <- as.character(helpNavPanel())
  expect_match(html, sprintf('maxlength="%d"', HELP_SEARCH_MAX_CHARS), fixed = TRUE)
  # An ordinary search is untouched
  expect_identical(helpSearchServer("  propofol ")$term, "propofol")
})

# --- M5: Email Slide -------------------------------------------------------------

test_that("M5: the per-recipient limit counts a mailbox, not its +tags or Gmail's dots", {
  expect_identical(emailAddressKey(" Target+1@Gmail.com "), "target@gmail.com")
  expect_identical(emailAddressKey("t.a.r.g.e.t+x@googlemail.com"), "target@gmail.com")
  expect_identical(emailAddressKey("first.last+lab@example.org"), "first.last@example.org")
  expect_false(identical(emailAddressKey("a.b@example.org"), emailAddressKey("ab@example.org")))
  expect_identical(emailAddressKey(c("a+1@x.org", "b@y.org")), c("a@x.org", "b@y.org"))

  old <- .emailSends$log
  on.exit(.emailSends$log <- old)
  .emailSends$log <- NULL
  t0 <- Sys.time()
  for (i in seq_len(EMAIL_RECIPIENT_LIMIT_PER_HOUR)) {
    expect_null(emailRateProblem(paste0("target+", i, "@gmail.com"), t0))
    recordEmailSend(paste0("target+", i, "@gmail.com"), t0)
  }
  expect_match(emailRateProblem("target+99@gmail.com", t0), "That address")
  expect_match(emailRateProblem("Target@Gmail.com", t0), "That address")
  expect_null(emailRateProblem("someone.else@gmail.com", t0))
})

slideFiles <- function(dir) {
  files <- file.path(dir, c("a.pptx", "a.png", "a.xlsx"))
  for (f in files) writeBin(as.raw(1:10), f)
  list(title = "t", bodyText = "<p>b</p>", pptxfileName = files[1], pngfileName = files[2],
       xlsxfileName = files[3])
}

test_that("M5: a send that fails before the mail server is given back to the hour's limit", {
  old <- .emailSends$log
  on.exit(.emailSends$log <- old, add = TRUE)
  quietly <- function(expr) suppressMessages(expr)
  count <- function(to) sum(emailRecentSends()$to == emailAddressKey(to))

  # No mail server configured: blank is not configured
  .emailSends$log <- NULL
  recordEmailSend("a@example.org")
  expect_identical(quietly(sendSlide(list(), "a@example.org", NULL, NULL, NULL, 1, 1, 1, list(),
                                     email_username = "", email_password = "p")), EMAIL_FAILURE_MESSAGE)
  expect_equal(count("a@example.org"), 0)
  recordEmailSend("a@example.org")
  expect_identical(quietly(sendSlide(list(), "a@example.org", NULL, NULL, NULL, 1, 1, 1, list(),
                                     email_username = NULL, email_password = NULL)), EMAIL_FAILURE_MESSAGE)
  expect_equal(count("a@example.org"), 0)

  # The slide could not be made: given back, and only that one send
  .emailSends$log <- NULL
  recordEmailSend("b+x@example.org")
  recordEmailSend("b@example.org")
  local_mocked_bindings(generateEmail = function(...) stop("the slide could not be made"))
  expect_identical(quietly(sendSlide(list(), "b@example.org", NULL, NULL, NULL, 1, 1, 1, list(),
                                     email_username = "u@example.org", email_password = "p")),
                   EMAIL_FAILURE_MESSAGE)
  expect_equal(count("b@example.org"), 1)
})

test_that("M5: a send the mail server refused stays counted", {
  old <- .emailSends$log
  on.exit(.emailSends$log <- old, add = TRUE)
  .emailSends$log <- NULL
  dir <- tempfile("slides-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  local_mocked_bindings(generateEmail = function(...) slideFiles(dir))
  local_mocked_bindings(server = function(...) function(msg, verbose = FALSE) stop("535 refused"),
                        .package = "emayili")
  recordEmailSend("c@example.org")
  expect_identical(suppressMessages(sendSlide(list(), "c@example.org", NULL, NULL, NULL, 1, 1, 1, list(),
                                              email_username = "u@example.org", email_password = "p")),
                   EMAIL_FAILURE_MESSAGE)
  expect_equal(sum(emailRecentSends()$to == "c@example.org"), 1)
})

test_that("M5: the workbook's covariates are one cell each whatever a field held", {
  doseTable <- data.frame(Drug = "propofol", Time = 0, Dose = 2, Units = "mg/kg", stringsAsFactors = FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  defaults <- getDrugDefaultsGlobal(FALSE)
  drugs <- processdoseTable(doseTable, events, recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"),
                            60, FALSE)
  plot <- simulationPlot(drugs = drugs, events = events, drugDefaults = defaults,
                         eventDefaults = getEventDefaults(), plotRecovery = FALSE)
  values <- list(
    DT = doseTable, url = "http://example.com", comments = "",
    age = 50, ageUnit = 1, weight = 70, weightUnit = 1, height = 170, heightUnit = 1,
    sex = "male", pregnant = FALSE, adjustToFFM = TRUE, osmolality = NULL, creatinine = NULL,
    # what the inputs hand on unread on a run no model reads them in (a run
    # of inhaled agents alone): character(0) left the sheet a value short
    cyp2d6 = character(0), cyp2c19 = c("normal", "poor"), cyp2c9 = list("x"), vkorc1 = NA,
    cyp4f2 = "CT", timeUnit = "minutes", maximum = 60
  )
  dir <- tempfile("slides-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  email <- generateEmail(values, "a@b.c", plot$plotObject, plot$allResults, plot$plotResults,
                         height = plot$plotHeight, width = 800, slide = 1, drugs = drugs, dir = dir)
  sheet <- openxlsx::read.xlsx(email$xlsxfileName, sheet = "Covariates")
  value <- function(name) sheet$Value[sheet$Covariate == name]
  expect_identical(value("CYP2D6"), "unreadable")
  expect_identical(value("CYP2C19"), "unreadable")
  expect_identical(value("CYP2C9"), "unreadable")
  expect_identical(value("VKORC1"), "unreadable")
  expect_identical(value("CYP4F2"), "CT")
  expect_identical(value("CYP2B6"), CYP2B6_DEFAULT)
  expect_identical(value("Sex"), "male")
  expect_identical(value("Age"), "50")
  expect_identical(value("Serum creatinine (mg/dL)"), "not entered (assumed normal)")
})

# --- H9 and L1: Single Paper Review -----------------------------------------------

carbon3Upload <- function() {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path)
}
carbon3Settle <- function(session) {
  for (i in 1:200) {
    later::run_now(0.01)
    session$flushReact()
    if (later::loop_empty()) break
  }
}

test_that("H9: Discard is live while a supplement alone is held", {
  shiny::testServer(paperReviewServer, {
    expect_false(holdsPaper())
    session$setInputs(consent = TRUE, supplement = carbon3Upload())
    expect_true(exists("supplement", envir = store))
    expect_false(hasPdf())
    expect_true(holdsPaper())
    session$setInputs(discard = 1)
    expect_false(holdsPaper())
    expect_length(ls(store, all.names = TRUE), 0)
  })
})

test_that("H9: Discard is live while an extraction whose review failed is held", {
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ...) {
      promises::promise_resolve(list(extraction = paperReviewExample(),
                                     usage = list(input_tokens = 1000, output_tokens = 200)))
    },
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewRun = function(extraction) stop("the review could not be computed"))
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = carbon3Upload(), consent = TRUE)
    session$setInputs(review = 1)
    carbon3Settle(session)
    expect_null(review())
    expect_false(reviewing())
    expect_false(hasPdf())
    expect_true(exists("extraction", envir = store))
    expect_match(output$status, "could not be computed")
    expect_true(holdsPaper())
    session$setInputs(discard = 1)
    expect_false(holdsPaper())
    expect_length(ls(store, all.names = TRUE), 0)
  })
})

test_that("L1: the report's download says so when there is no report, and serves it when there is", {
  shiny::testServer(paperReviewServer, {
    page <- readLines(output$download)
    expect_true(any(grepl("There is no review to download", page, fixed = TRUE)))
    session$setInputs(example = 1)
    page <- paste(readLines(output$download), collapse = "\n")
    expect_match(page, "Single Paper Review", fixed = TRUE)
    expect_false(grepl("There is no review to download", page, fixed = TRUE))
    session$setInputs(discard = 1)
    expect_true(any(grepl("There is no review to download", readLines(output$download), fixed = TRUE)))
  })
})

test_that("L1: a regimen with no id, or a blank one, keeps its absorption", {
  # The example paper given an oral route, its ka, F and lag, and an oral
  # regimen (as test-paper-review-model.R's extravascular paper)
  ex <- paperReviewExample()
  ex$structure$routes <- list("oral")
  ex$parameters <- c(ex$parameters, list(
    list(name_in_paper = "ka", role = "ka", route = "oral", value = 1.2, value_text = "1.2",
         units = "1/h", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "F", role = "F", route = "oral", value = 60, value_text = "60",
         units = "percent", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2"),
    list(name_in_paper = "tlag", role = "tlag", route = "oral", value = 0.25, value_text = "0.25",
         units = "h", uncertainty = list(type = "not_reported"), status = "estimated", location = "T2")))
  ex$regimens <- c(ex$regimens, list(list(
    id = "R3", description = "100 mg by mouth",
    doses = list(list(route = "oral", time = 0, amount = 100, mass_unit = "mg", per_kg = "no",
                      per_time = "none", duration = NULL, time_units = "min")),
    location = "Methods")))
  oral <- which(vapply(ex$regimens, paperReviewNeedsAbsorption, logical(1)))
  expect_length(oral, 1)
  base <- paperReviewQuietly(function() paperReviewRun(ex))
  expect_true(isTRUE(base$drug$absorptions[["R3"]]$ok))
  for (blank in list("", NULL, "  ")) {
    x <- ex
    if (is.null(blank)) x$regimens[[oral[1]]]["id"] <- list(NULL) else x$regimens[[oral[1]]]$id <- blank
    named <- paperReviewNameRegimens(x)
    id <- named$regimens[[oral[1]]]$id
    expect_identical(id, paste("regimen", oral[1]))
    expect_match(named$extraction_limitations, paste0("\"regimen ", oral[1], "\""), fixed = TRUE)
    review <- paperReviewQuietly(function() paperReviewRun(x))
    # found by its new name, with the absorption the named regimen had
    expect_false(is.null(review$drug$absorptions[[id]]))
    expect_equal(review$drug$absorptions[[id]]$ka,
                 base$drug$absorptions[[ex$regimens[[oral[1]]]$id]]$ka)
    expect_false("" %in% names(review$drug$absorptions))
  }
  # An id already taken is not given twice
  y <- ex
  y$regimens[[oral[1]]]$id <- ""
  other <- setdiff(seq_along(y$regimens), oral[1])
  if (length(other)) {
    y$regimens[[other[1]]]$id <- paste("regimen", oral[1])
    ids <- vapply(paperReviewNameRegimens(y)$regimens, function(r) r$id, "")
    expect_false(anyDuplicated(ids) > 0)
  }
  # paperReviewAbsorptions() itself never stores an element under ""
  z <- ex
  z$regimens[[oral[1]]]$id <- ""
  expect_false("" %in% names(paperReviewAbsorptions(z, base$drug, base$reference)))
})

# --- L1: the API's CSV ---------------------------------------------------------------

test_that("L1: the API's CSV writes a text cell a spreadsheet would run after an apostrophe", {
  df <- data.frame(Drug = c("propofol", "=HYPERLINK(\"x\")", "+1", "-1", "@SUM(A1)", "\tx"),
                   Time = c(0, -1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  rows <- utils::read.csv(text = apiCsv(df), stringsAsFactors = FALSE)
  expect_identical(rows$Drug, c("propofol", "'=HYPERLINK(\"x\")", "'+1", "'-1", "'@SUM(A1)", "'\tx"))
  # numbers are left alone, negative ones included
  expect_identical(rows$Time, c(0L, -1L, 2L, 3L, 4L, 5L))
  # and an ordinary result is unchanged
  result <- apiSimulate(list(
    patient = list(age = 40, weight = 70, height = 170, sex = "male"),
    doses = list(list(Drug = "propofol", Time = 0, Dose = 2, Units = "mg/kg")),
    duration = 10, output = list(interval = 5)))
  csv <- apiFormatResult(result, "csv")$body
  expect_false(grepl("'", csv, fixed = TRUE))
})
