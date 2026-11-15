# The Export panel (named Email Slide until 2026-11-15): the email's three
# files -- the PowerPoint deck, the spreadsheet and the plot as an image --
# each downloaded on its own, and the email still offered beside them.  The
# files are written by R/sendSlide.R's writeSlideDeck(), writeWorkbook() and
# writePlotImage(), which the email uses too; the downloads are app_server()'s
# exportDownload().  Drafted by Claude Code, 2026-11-15, at the request of
# Steven L. Shafer.

local_mocked_bindings(outputComments = function(...) {})

oldConfig <- .sprglobals$config
.sprglobals$config <- DEFAULT_CONFIG

exportInputs <- list(
  timeUnits = "minutes", timeMode = "relative", referenceTime = "08:00", maximum = "60",
  plotWidth = 800, yaxisHeight = 200, plasmaLinetype = "blank", effectsiteLinetype = "solid",
  weight = 70, weightUnit = "1", height = 170, heightUnit = "1", age = 40, ageUnit = "1",
  sex = "female", pregnant = TRUE, showThreshold = FALSE, normalization = "none",
  typical = "Range", logY = FALSE
)

exportTable <- function(...) {
  rows <- list(...)
  dt <- doseTableInit[seq_along(rows), ]
  for (i in seq_along(rows)) dt[i, ] <- rows[[i]]
  dt[nrow(dt) + 1, ] <- list("", "", "", "")
  rownames(dt) <- NULL
  dt
}

startExport <- function(session, dt) {
  do.call(session$setInputs, exportInputs)
  session$env$setDoseTable(dt, timeFormat("minutes", "relative"))
  session$flushReact()
}

isPNG <- function(path) {
  identical(readBin(path, "raw", 8), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
}
# Its height and width in pixels, from the IHDR chunk that follows the
# signature (the png package is not a dependency)
pngSize <- function(path) {
  header <- readBin(path, "raw", 24)
  size <- readBin(header[17:24], "integer", n = 2, size = 4, endian = "big")
  c(height = size[2], width = size[1])
}

local_mocked_bindings(
  showPageSpinner = function(...) spinner$shown <- spinner$shown + 1,
  hidePageSpinner = function(...) spinner$hidden <- spinner$hidden + 1,
  .package = "shinycssloaders"
)
spinner <- new.env()
spinner$shown <- 0
spinner$hidden <- 0

test_that("the panel is Export, with the three downloads and the email beside them", {
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  expect_match(html, ">Export<", fixed = TRUE)
  expect_no_match(html, "Email Slide", fixed = TRUE)
  for (id in c("exportPowerPoint", "exportSpreadsheet", "exportImage")) {
    expect_match(html, sprintf("id=\"%s\"[^>]*shiny-download-link|shiny-download-link[^>]*id=\"%s\"", id, id))
  }
  # the PowerPoint and the spreadsheet are separate downloads, in that order
  expect_lt(regexpr("PowerPoint (.pptx)", html, fixed = TRUE),
            regexpr("Spreadsheet (.xlsx)", html, fixed = TRUE))
  # Without mail credentials the downloads are still offered, and the email
  # says why it is not
  expect_match(html, "Email is not configured", fixed = TRUE)
  expect_no_match(html, "id=\"sendSlide\"", fixed = TRUE)

  # With them, the email's inputs are the ones they always were (the
  # bookmark leaves them out by these names: bookmarksToExclude)
  .sprglobals$config <- modifyList(DEFAULT_CONFIG, list(email_username = "u@example.org", email_password = "p"))
  on.exit(.sprglobals$config <- DEFAULT_CONFIG, add = TRUE)
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  for (id in c("recipient", "emailComments", "commentSafe", "sendSlide")) {
    expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE)
    expect_true(id %in% bookmarksToExclude)
  }
  expect_lt(regexpr("Image (.png)", html, fixed = TRUE), regexpr("id=\"recipient\"", html, fixed = TRUE))
})

test_that("each file downloads on its own, the same as the email's", {
  alerts <- character(0)
  local_mocked_bindings(shinyalert = function(title, text = "", ...) alerts <<- c(alerts, text),
                        .package = "shinyalert")
  shiny::testServer(app_server, {
    startExport(session, exportTable(list("propofol", "0", "2", "mg/kg"),
                                      list("fentanyl", "0", "100", "mcg")))
    spinner$shown <- 0
    spinner$hidden <- 0

    # The PowerPoint: the whole plot, then one slide per drug
    deck <- output$exportPowerPoint
    expect_match(basename(deck), "^stanpumpR\\.[0-9]{6}-[0-9]{6}\\.pptx$")
    pptx <- officer::read_pptx(deck)
    expect_equal(length(pptx), 3)
    text <- officer::pptx_summary(pptx)
    text <- text[text$content_type == "paragraph", ]
    expect_match(text$text[text$slide_id == 1][1], "^stanpumpR simulation on ")
    expect_equal(c(text$text[text$slide_id == 2][1], text$text[text$slide_id == 3][1]),
                 c("propofol", "fentanyl"))

    # The spreadsheet, with no slide in it
    book <- output$exportSpreadsheet
    expect_match(basename(book), "^stanpumpR\\.[0-9]{6}-[0-9]{6}\\.xlsx$")
    expect_equal(openxlsx::getSheetNames(book),
                 c("Covariates", "Dose Table", "Simulation Results", "Results for Plotting",
                   "fentanyl PK", "propofol PK"))
    covariates <- openxlsx::read.xlsx(book, sheet = "Covariates")
    value <- function(name) covariates$Value[covariates$Covariate == name]
    expect_equal(value("Sex"), "female")
    expect_equal(value("Pregnant"), "yes")
    expect_equal(value("Max time (minutes)"), "60")
    doses <- openxlsx::read.xlsx(book, sheet = "Dose Table")
    expect_setequal(doses$Drug, c("propofol", "fentanyl"))

    # The image
    image <- output$exportImage
    expect_match(basename(image), "^stanpumpR\\.[0-9]{6}-[0-9]{6}\\.png$")
    expect_true(isPNG(image))
    expect_equal(unname(pngSize(image)), c(2 * plotHeight(), 2 * 800))

    expect_length(alerts, 0)
    expect_equal(spinner$shown, 3)
    expect_equal(spinner$hidden, 3)
    expect_false(session$isClosed())
  })
})

test_that("a download with nothing to export says why and keeps the session", {
  alerts <- character(0)
  local_mocked_bindings(shinyalert = function(title, text = "", ...) alerts <<- c(alerts, text),
                        .package = "shinyalert")
  local_mocked_bindings(writeSlideDeck = function(...) stop("writeSlideDeck must not be reached"))
  shiny::testServer(app_server, {
    # An empty dose table
    startExport(session, doseTableBlank)
    spinner$shown <- 0
    spinner$hidden <- 0
    expect_error(output$exportPowerPoint, "enter a dose first")
    expect_match(tail(alerts, 1), "There is no plot to export: enter a dose first.", fixed = TRUE)
    expect_false(session$isClosed())

    # A patient field the simulation cannot read
    session$env$setDoseTable(exportTable(list("propofol", "0", "2", "mg/kg")),
                             timeFormat("minutes", "relative"))
    session$setInputs(age = NA)
    expect_error(output$exportSpreadsheet)
    expect_match(tail(alerts, 1), "Age")
    expect_false(session$isClosed())
    # no spinner is left up
    expect_equal(spinner$shown, 0)
    expect_equal(spinner$hidden, 0)
  })
})

test_that("a download that fails while writing says so, and the detail goes only to the server's log", {
  alerts <- character(0)
  local_mocked_bindings(shinyalert = function(title, text = "", ...) alerts <<- c(alerts, text),
                        .package = "shinyalert")
  local_mocked_bindings(writeWorkbook = function(...) stop("disk full at /secret/path"))
  shiny::testServer(app_server, {
    startExport(session, exportTable(list("propofol", "0", "2", "mg/kg")))
    spinner$shown <- 0
    spinner$hidden <- 0
    expect_message(expect_error(output$exportSpreadsheet, EXPORT_FAILURE_MESSAGE, fixed = TRUE),
                   "Export \\(xlsx\\) failed: disk full")
    expect_equal(tail(alerts, 1), EXPORT_FAILURE_MESSAGE)
    expect_no_match(EXPORT_FAILURE_MESSAGE, "secret")
    expect_equal(spinner$shown, 1)
    expect_equal(spinner$hidden, 1)
    expect_false(session$isClosed())
  })
})

test_that("the email sends the same three files, and its slide is numbered by the send", {
  calls <- list()
  local_mocked_bindings(sendSlide = function(...) {
    calls[[length(calls) + 1]] <<- list(...)
    TRUE
  })
  local_mocked_bindings(shinyalert = function(...) NULL, .package = "shinyalert")
  shiny::testServer(app_server, {
    startExport(session, exportTable(list("propofol", "0", "2", "mg/kg")))
    session$setInputs(recipient = "user@example.com", emailComments = "teaching case", commentSafe = TRUE)
    session$setInputs(sendSlide = 1)
    expect_length(calls, 1)
    sent <- calls[[1]]
    expect_equal(sent$values$comments, "teaching case")
    expect_equal(sent$values$sex, "female")
    expect_true(sent$values$pregnant)
    expect_equal(sent$slide, 1)
    # the values a download writes are the email's, with no comment
    expect_equal(modifyList(sent$values, list(comments = "")), exportValues())
  })
})

test_that("the writers each write one file, and the email is built from them", {
  doseTable <- data.frame(Drug = "propofol", Time = 0, Dose = 2, Units = "mg/kg",
                          stringsAsFactors = FALSE)
  events <- data.frame(Time = numeric(0), Event = character(0))
  defaults <- getDrugDefaultsGlobal(FALSE)
  drugs <- processdoseTable(doseTable, events,
                            recalculatePK(NULL, defaults, doseTable, 50, 70, 170, "male"), 60, TRUE)
  built <- simulationPlot(drugs = drugs, events = events, drugDefaults = defaults,
                          eventDefaults = getEventDefaults(), plotRecovery = FALSE)
  values <- list(DT = doseTable, url = "http://example.com", comments = "",
                 age = 50, ageUnit = 1, weight = 70, weightUnit = 1, height = 170, heightUnit = 1,
                 sex = "male", pregnant = FALSE, adjustToFFM = TRUE,
                 timeUnit = "minutes", maximum = 60)

  dir <- tempfile("export-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  deck <- writeSlideDeck(built$plotObject, file.path(dir, "a.pptx"), title = "A title", slide = 1)
  expect_equal(deck, file.path(dir, "a.pptx"))
  pptx <- officer::read_pptx(deck)
  expect_equal(length(pptx), 2)
  text <- officer::pptx_summary(pptx)
  expect_true("A title" %in% text$text[text$slide_id == 1])

  book <- writeWorkbook(values, built$allResults, built$plotResults, drugs, file.path(dir, "a.xlsx"))
  expect_equal(openxlsx::getSheetNames(book)[c(1, 5)], c("Covariates", "propofol PK"))
  covariates <- openxlsx::read.xlsx(book, sheet = "Covariates")
  expect_equal(covariates$Value[covariates$Covariate == "Age Unit"], "years")
  expect_equal(covariates$Value[covariates$Covariate == "Pregnant"], "no")

  image <- writePlotImage(built$plotObject, file.path(dir, "a.png"), height = built$plotHeight, width = 800)
  expect_true(isPNG(image))
  # the app's drawing, at twice its pixels (writePlotImage())
  expect_equal(unname(pngSize(image)), c(2 * built$plotHeight, 1600))

  expect_equal(exportUnitNames(list(ageUnit = 1, weightUnit = 1, heightUnit = 1)),
               list(age = "years", weight = "kilograms", height = "cms"))
  expect_equal(exportUnitNames(list(ageUnit = 1 / 12, weightUnit = 0.453592, heightUnit = 2.54)),
               list(age = "months", weight = "pounds", height = "inches"))
  expect_equal(exportFileName("pptx", as.POSIXct("2026-11-15 14:22:33", tz = "UTC")),
               "stanpumpR.261115-142233.pptx")
  expect_equal(exportTitle(as.POSIXct("2026-11-15 14:22:33", tz = "UTC")),
               "stanpumpR simulation on 2026-11-15 14:22:33")

  # The email attaches the same three files, written by the same functions
  written <- character(0)
  local_mocked_bindings(
    writeSlideDeck = function(plotObject, file, ...) { written <<- c(written, "deck"); file.create(file); file },
    writeWorkbook = function(values, allResults, plotResults, drugs, file) { written <<- c(written, "book"); file.create(file); file },
    writePlotImage = function(plotObject, file, ...) { written <<- c(written, "image"); file.create(file); file }
  )
  email <- generateEmail(values, "a@b.c", built$plotObject, built$allResults, built$plotResults,
                         height = built$plotHeight, width = 800, slide = 2, drugs = drugs, dir = dir)
  expect_setequal(written, c("deck", "book", "image"))
  expect_true(all(file.exists(c(email$pptxfileName, email$xlsxfileName, email$pngfileName))))
  expect_match(email$bodyText, "50 years-old male weighing 70 kilograms", fixed = TRUE)
})

.sprglobals$config <- oldConfig
