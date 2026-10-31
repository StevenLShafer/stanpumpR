# The hardening from the external security review of 2026-10-31 (read-only
# source audit): every claim here was first reproduced against the source.
# Email Slide's files and failure message (R/sendSlide.R); the simulation
# API's bounds on expanded schedules and response rows, its refusal of a
# field given twice, its warning about unknown fields, finite times, the
# media type, the body limit on every launch path, and the build in the
# metadata (R/api.R, inst/plumber/plumber.R).  Drafted by Claude Code.

local_mocked_bindings(outputComments = function(...) {})

# ---- Email Slide ----------------------------------------------------------

made <- character(0)
stubEmail <- function(values, recipient, plotObject, allResults, plotResults, height, width, slide, drugs, dir) {
  made <<- c(made, dir)
  files <- file.path(dir, c("deck.pptx", "preview.png", "book.xlsx"))
  file.create(files)
  list(title = "t", bodyText = "<p>b</p>", pptxfileName = files[1], pngfileName = files[2], xlsxfileName = files[3])
}

sendStub <- function(username = "u@example.org") {
  sendSlide(values = list(), recipient = "a@b.c", plotObject = NULL, allResults = NULL, plotResults = NULL,
            height = 1, width = 1, slide = 1, drugs = list(),
            email_username = username, email_password = "p")
}

test_that("Email Slide writes to a private directory that is removed on success and on failure", {
  local_mocked_bindings(generateEmail = stubEmail)
  sent <- 0
  local_mocked_bindings(server = function(...) function(msg, verbose = FALSE) { sent <<- sent + 1; invisible(TRUE) },
                        .package = "emayili")
  expect_true(sendStub())
  expect_equal(sent, 1)
  expect_length(made, 1)
  expect_false(dir.exists(made[1]))
  expect_false(dir.exists("Slides"))

  # SMTP refuses: the files still go, and the browser is told only that it failed
  local_mocked_bindings(server = function(...) function(msg, verbose = FALSE) stop("535 Authentication failed for /secret/path"),
                        .package = "emayili")
  expect_message(r <- sendStub(), "Email Slide failed: 535 Authentication failed")
  expect_equal(r, EMAIL_FAILURE_MESSAGE)
  expect_no_match(r, "535")
  expect_length(made, 2)
  expect_false(dir.exists(made[2]))

  # two sends do not share a directory
  expect_false(identical(made[1], made[2]))
})

test_that("a missing credential is reported generically too", {
  expect_message(r <- sendStub(username = NULL), "Email username missing")
  expect_equal(r, EMAIL_FAILURE_MESSAGE)
})

# ---- The simulation API ---------------------------------------------------

apiReq <- function(...) {
  req <- list(
    patient = list(weight = 70, height = 170, age = 40, sex = "male"),
    doses = data.frame(Drug = "propofol", Time = 0, Dose = 100, Units = "mg", stringsAsFactors = FALSE),
    duration = 60
  )
  over <- list(...)
  for (n in names(over)) req[[n]] <- over[[n]]
  req
}

test_that("a field given twice is refused, not silently resolved", {
  expect_error(apiField(list(weight = 70, Weight = 90), "weight"), "`weight` is given more than once \\(`weight`, `Weight`\\)")
  expect_error(apiField(list(weightUnit = "kg", weight_unit = "lb"), "weightUnit"), "given more than once")
  # the same key twice in the JSON text
  text <- '{"patient": {"weight": 70, "weight": 90, "height": 170, "age": 40, "sex": "male"}, "doses": [{"Drug": "propofol", "Time": 0, "Dose": 100, "Units": "mg"}], "duration": 60}'
  expect_error(apiSimulate(apiParseRequest(text)), "`weight` is given more than once", class = "stanpumpr_api_error")
  expect_equal(apiField(list(weight = 70), "weight"), 70)
})

test_that("unknown fields are reported in the warnings, not defaulted in silence", {
  r <- apiSimulate(apiReq(patient = list(weight = 70, height = 170, age = 40, sex = "male", creatinin = 1.2, mrn = "12345"),
                          output = list(interval = 10, recovry = TRUE), extra = 1))
  expect_match(r$warnings, "Unknown fields in `patient` ignored: `creatinin`, `mrn`.", fixed = TRUE, all = FALSE)
  expect_match(r$warnings, "Unknown field in `output` ignored: `recovry`.", fixed = TRUE, all = FALSE)
  expect_match(r$warnings, "Unknown field in the request ignored: `extra`.", fixed = TRUE, all = FALSE)
  clean <- apiSimulate(apiReq(output = list(interval = 10)))
  expect_false(any(grepl("Unknown field", clean$warnings)))
  expect_null(apiUnknownFields(list(weight = 1, WEIGHT_UNIT = "kg"), API_PATIENT_FIELDS, "x"))
})

test_that("a time that reads as Inf is refused", {
  huge <- strrep("9", 400)
  expect_equal(as.numeric(huge), Inf)
  expect_error(apiTimes(huge, "minutes", "`doses`"), "is not a non-negative number", class = "stanpumpr_api_error")
  expect_error(apiSimulate(apiReq(doses = data.frame(Drug = "propofol", Time = huge, Dose = 100, Units = "mg"))),
               "row 1: time", class = "stanpumpr_api_error")
})

test_that("a schedule past MAX_SCHEDULED_DOSES is refused before it is simulated", {
  doses <- data.frame(
    Drug = c("acetaminophen", "gabapentin", "oxycodone", "ibuprofen", "hydrocodone", "cefazolin", "pregabalin", "morphine"),
    Time = 0, Dose = 10,
    Units = c("mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg PO q4h", "mg q4h", "mg PO q4h", "mg PO tablet q4h"),
    stringsAsFactors = FALSE
  )
  expect_error(apiSimulate(apiReq(doses = doses, duration = 52, timeUnit = "weeks")),
               "repeat 17,464 times over `duration`, more than the 8,000", class = "stanpumpr_api_error")
  # within the limit it simulates
  r <- apiSimulate(apiReq(doses = doses[1:3, ], duration = 52, timeUnit = "weeks", output = list(interval = 1)))
  expect_true("acetaminophen" %in% r$drugs$Drug)
})

test_that("the response's rows are bounded, on a requested grid and on the native one", {
  local_mocked_bindings(API_MAX_ROWS = 50L)
  expect_error(apiSimulate(apiReq(output = list(interval = 1))),
               "asks for 61 rows \\(1 drugs x 61 times\\), more than the 50", class = "stanpumpr_api_error")
  r <- apiSimulate(apiReq(output = list(interval = 2)))
  expect_equal(nrow(r$timeseries), 31)
  # a metabolite adds rows the request could not count: tramadol forms desmetramadol
  tramadol <- data.frame(Drug = "tramadol", Time = 0, Dose = 100, Units = "mg PO", stringsAsFactors = FALSE)
  err <- tryCatch(apiSimulate(apiReq(doses = tramadol, output = list(interval = 2))), error = function(e) e)
  expect_s3_class(err, "stanpumpr_api_error")
  expect_equal(err$status, 422L)
  expect_match(conditionMessage(err), "The response would have 62 rows over 2 drugs, more than the 50")
  # native: the per-drug limit applies after the simulation (a 60-minute
  # bolus has 28 native points)
  local_mocked_bindings(API_MAX_POINTS = 10L, API_MAX_ROWS = 500000L)
  err <- tryCatch(apiSimulate(apiReq(output = list(interval = "native"))), error = function(e) e)
  expect_s3_class(err, "stanpumpr_api_error")
  expect_equal(err$status, 422L)
  expect_match(conditionMessage(err), "native time line of propofol has more than 10 rows")
})

test_that("the metadata carries the build the service runs", {
  r <- apiSimulate(apiReq(output = list(interval = 10)))
  expect_true("build" %in% names(r$meta))
  expect_equal(r$meta$build, stanpumpRBuild()$commit)
  json <- jsonlite::fromJSON(apiFormatResult(r, "json")$body)
  expect_true("build" %in% names(json$meta))
  # the other formats take an unrecorded build (NA) in their stride
  expect_type(apiFormatResult(r, "csv")$body, "character")
  expect_type(apiFormatResult(r, "xml")$body, "character")
})

test_that("the HTTP service refuses a body that is not JSON, sets the body limit, and reports the build", {
  skip_if_not_installed("plumber")
  withr::local_options(plumber.maxRequestSize = NULL)
  api <- plumber::pr(system.file("plumber", "plumber.R", package = "stanpumpR"))
  expect_equal(getOption("plumber.maxRequestSize"), 1e6)   # set by the file, not only by run_api()
  call <- function(method, path, body = "", type = "application/json") {
    req <- new.env()
    req$REQUEST_METHOD <- method
    req$PATH_INFO <- path
    req$QUERY_STRING <- ""
    req$HTTP_ACCEPT <- "*/*"
    req$CONTENT_TYPE <- type
    req$HTTP_CONTENT_TYPE <- type
    req$HEADERS <- c(accept = "*/*", "content-type" = type)
    bodyRaw <- charToRaw(body)
    req$rook.input <- list(read = function(...) bodyRaw, rewind = function() invisible(0),
                           read_lines = function() body)
    api$call(req)
  }
  text <- function(res) if (is.raw(res$body)) rawToChar(res$body) else res$body
  body <- as.character(jsonlite::toJSON(apiReq(output = list(interval = 10)), dataframe = "rows", auto_unbox = TRUE))

  form <- call("POST", "/simulate", body, type = "application/x-www-form-urlencoded")
  expect_equal(form$status, 415L)
  expect_match(jsonlite::fromJSON(text(form))$error, "Send the request body as application/json")
  ok <- call("POST", "/simulate", body, type = "application/json; charset=utf-8")
  expect_equal(ok$status, 200L)
  expect_equal(call("POST", "/simulate", body, type = "")$status, 200L)   # undeclared is read as JSON

  health <- jsonlite::fromJSON(text(call("GET", "/health")))
  expect_true("build" %in% names(health))
})
