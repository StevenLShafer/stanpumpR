# Single Paper Review: confidentiality.  An upload leaves no file behind,
# discarding leaves nothing in the session, and nothing about the paper
# reaches the console (which is the server log on a host).  See
# docs/paper-review-privacy.md.

test_that("reading an upload deletes its file and the directory Shiny made for it", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  bytes <- c(charToRaw("%PDF-1.7\n"), as.raw(1:200))
  writeBin(bytes, path)
  expect_equal(paperReviewReadUpload(path), bytes)
  expect_false(file.exists(path))
  expect_false(dir.exists(dir))
  expect_true(dir.exists(tempdir()))
})

test_that("a non-PDF upload is still deleted", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(charToRaw("not a pdf"), path)
  bytes <- paperReviewReadUpload(path)
  expect_false(paperReviewIsPdf(bytes))
  expect_false(file.exists(path))
})

test_that("discarding empties the session's store", {
  store <- paperReviewStore()
  assign("pdf", as.raw(1:10), envir = store)
  assign("review", paperReviewRun(paperReviewExample()), envir = store)
  assign(".hidden", 1, envir = store)
  paperReviewDiscard(store)
  expect_length(ls(store, all.names = TRUE), 0)
})

test_that("reviewing, reporting and simulating write nothing to the console or to disk", {
  ex <- paperReviewExample()
  ex$drug$name <- "SENTINELDRUG"
  before <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  old <- options(ECHO_OUTPUT_COMMENTS = TRUE)
  on.exit(options(old), add = TRUE)
  out <- utils::capture.output(type = "output", {
    msg <- utils::capture.output(type = "message", {
      review <- paperReviewQuietly(function() paperReviewRun(ex))
      page <- paperReviewReportPage(review)
      ref <- review$reference
      sim <- paperReviewSimulate(review$drug, ref,
                                 data.frame(Time = 0, Dose = 1, Units = "mg/kg"), 60)
    })
  })
  expect_false(any(grepl("SENTINELDRUG", c(out, msg))))
  expect_length(c(out, msg), 0)
  expect_true(grepl("SENTINELDRUG", page))        # it is in the report, which stays in memory
  after <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  expect_setequal(after, before)
})

test_that("the app's server deletes uploads at once and discards on request and at session end", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "secret.pdf", size = 59, type = "application/pdf",
                                        datapath = path))
    expect_false(file.exists(path))
    expect_match(output$status, "held in this session's memory only")
    session$setInputs(example = 1)
    expect_false(is.null(review()))
    session$setInputs(discard = 1)
    expect_null(review())
    expect_match(output$status, "Discarded")
    # Reviewing with nothing uploaded sends nothing and says why.
    session$setInputs(review = 1)
    expect_match(output$status, "Upload a manuscript first")
  })
})

test_that("the app is started with bookmarking disabled", {
  old <- options(ECHO_OUTPUT_COMMENTS = getOption("ECHO_OUTPUT_COMMENTS"))
  oldConfig <- .sprglobals$config
  on.exit({ options(old); .sprglobals$config <- oldConfig }, add = TRUE)
  app <- run_paper_review_app(config_file = tempfile())
  expect_true(is.na(getOption("ECHO_OUTPUT_COMMENTS")))
  expect_equal(app$appOptions$bookmarkStore, "disable")
})

# Run the event loop until the review's promise settles and its observers run.
settle <- function(session) {
  for (i in 1:200) {
    later::run_now(0.01)
    session$flushReact()
    if (later::loop_empty()) break
  }
}

test_that("pressing Review runs the call, drops the manuscript and reports, with no network", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  calls <- 0
  sent <- NULL
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ...) {
      calls <<- calls + 1
      sent <<- key
      promises::promise_reject(paperReviewError("http_529", "The API is overloaded (529). Try again shortly."))
    },
    paperReviewUsageReport = function(record, ...) invisible(record))
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf",
                                        datapath = path))
    session$setInputs(consent = TRUE)
    session$setInputs(review = 1)
    # The manuscript is dropped as soon as it is handed to the request.
    expect_false(hasPdf())
    expect_false(exists("pdf", envir = store))
    settle(session)
    expect_equal(calls, 1)
    expect_equal(sent, "operator-key")
    expect_match(output$status, "overloaded")
    expect_match(output$status, "The manuscript has been discarded")
  })
})

test_that("a review that comes back is reported, and one discarded meanwhile is dropped", {
  upload <- function() {
    dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
    dir.create(dir)
    path <- file.path(dir, "0.pdf")
    writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
    list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path)
  }
  sent <- NULL
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ...) {
      sent <<- key
      promises::promise_resolve(list(extraction = paperReviewExample(),
                                     usage = list(input_tokens = 1000, output_tokens = 200)))
    },
    paperReviewUsageReport = function(record, ...) invisible(record))
  shiny::testServer(paperReviewServer, {
    session$setInputs(ownKey = "sk-ant-api03-userownkey1234")
    session$setInputs(manuscript = upload(), consent = TRUE)
    session$setInputs(review = 1)
    settle(session)
    expect_equal(sent, "sk-ant-api03-userownkey1234")   # the user's own key, when entered
    expect_false(is.null(review()))
    expect_match(output$status, "1,000 tokens read, 200 written")

    session$setInputs(discard = 1)
    session$setInputs(ownKey = "")
    session$setInputs(manuscript = upload())
    session$setInputs(review = 2)
    session$setInputs(discard = 2)                      # before the answer is read
    settle(session)
    expect_equal(sent, "operator-key")
    expect_null(review())
    expect_match(output$status, "Discarded")
  })
})

test_that("Single Paper Review is a tab of stanpumpR, between Simulator and Help", {
  # app_ui() uses numericInput(updateOn = ), which needs shiny 1.10
  skip_if_not_installed("shiny", "1.10.0")
  oldConfig <- .sprglobals$config
  .sprglobals$config <- DEFAULT_CONFIG
  on.exit(.sprglobals$config <- oldConfig)
  html <- as.character(htmltools::renderTags(app_ui()(list()))$html)
  at <- function(value) regexpr(paste0('data-value="', value, '"'), html, fixed = TRUE)[1]
  expect_true(at("Simulator") > 0 && at("Single Paper Review") > 0 && at("Help") > 0)
  expect_true(at("Simulator") < at("Single Paper Review") && at("Single Paper Review") < at("Help"))
  # Its inputs live in their own namespace, which app_server() keeps out of
  # the bookmark.
  expect_true(grepl(paste0('id="', PAPER_REVIEW_MODULE_ID, '-consent"'), html, fixed = TRUE))
  expect_true(grepl(paste0('id="', PAPER_REVIEW_MODULE_ID, '-manuscript"'), html, fixed = TRUE))
})

test_that("the tab's server is the review's own, in its namespace", {
  shiny::testServer(paperReviewModuleServer, args = list(id = PAPER_REVIEW_MODULE_ID), {
    session$setInputs(example = 1)
    expect_false(is.null(review()))
    expect_match(output$status, "Nothing was sent anywhere")
    expect_equal(session$ns("download"), paste0(PAPER_REVIEW_MODULE_ID, "-download"))
  })
})

test_that("a review still running after ten minutes is offered a quick one, at effort medium", {
  # Shafer, 2026-11-13: at PAPER_REVIEW_LATE_SEC the banner offers Keep
  # waiting or Stop and run a quick review; the quick review re-sends the
  # same manuscript at PAPER_REVIEW_QUICK_EFFORT, and its report says so.
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  clock <- as.POSIXct("2026-11-13 10:00:00", tz = "UTC")
  efforts <- character(0)
  pdfs <- list()
  rejectFull <- NULL
  stopped <- 0
  testthat::local_mocked_bindings(
    paperReviewNow = function() clock,
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewSharedKeyAdmit = function(...) promises::promise_resolve(TRUE),
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      pdfs[[length(pdfs) + 1]] <<- pdf
      if (effort == PAPER_REVIEW_EFFORT) {
        # The full review never answers until it is stopped.
        if (is.function(onJob)) onJob("the full review's job")
        promises::promise(function(resolve, reject) rejectFull <<- reject)
      } else {
        promises::promise_resolve(list(extraction = paperReviewExample(),
                                       usage = list(input_tokens = 1000, output_tokens = 200)))
      }
    })
  testthat::local_mocked_bindings(
    stop_mirai = function(x) {
      stopped <<- stopped + 1
      rejectFull(paperReviewError("background", "stopped"))
    }, .package = "mirai")
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    session$flushReact()
    expect_equal(efforts, PAPER_REVIEW_EFFORT)
    expect_false(exists("pdf", envir = store))        # out of the upload slot at once
    expect_true(exists("inFlight", envir = store))     # held only until the answer returns
    expect_false(lateOffered())
    # Ten minutes on, the quick review is offered.
    clock <<- clock + PAPER_REVIEW_LATE_SEC + 1
    session$elapse(PAPER_REVIEW_LATE_SEC * 1000 + 1000)
    expect_true(lateOffered())
    # Keep waiting only hides the offer; it is not made again.
    session$setInputs(keepWaiting = 1)
    expect_equal(stopped, 0)
    expect_true(reviewing())
    # The user takes it after all: the full review is stopped and the same
    # manuscript sent again at effort medium.
    session$setInputs(quickReview = 1)
    settle(session)
    expect_equal(stopped, 1)
    expect_equal(efforts, c(PAPER_REVIEW_EFFORT, PAPER_REVIEW_QUICK_EFFORT))
    expect_identical(pdfs[[2]], pdfs[[1]])
    expect_true(isTRUE(review()$quick))
    expect_match(output$status, "A quick review, done")
    expect_false(exists("inFlight", envir = store))    # dropped once the answer returned
    html <- as.character(htmltools::renderTags(paperReviewReportTags(review(), plots = FALSE))$html)
    expect_match(html, "A quick review.", fixed = TRUE)
  })
})

test_that("a full review that answers is not replaced, and is not marked quick", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  efforts <- character(0)
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewSharedKeyAdmit = function(...) promises::promise_resolve(TRUE),
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      promises::promise_resolve(list(extraction = paperReviewExample(), usage = list()))
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    settle(session)
    expect_equal(efforts, PAPER_REVIEW_EFFORT)
    expect_false(isTRUE(review()$quick))
    expect_false(lateOffered())
    expect_false(exists("inFlight", envir = store))
    html <- as.character(htmltools::renderTags(paperReviewReportTags(review(), plots = FALSE))$html)
    expect_false(grepl("A quick review.", html, fixed = TRUE))
  })
})
