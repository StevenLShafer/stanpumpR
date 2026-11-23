# Single Paper Review: confidentiality.  An upload leaves no file behind,
# discarding leaves nothing in the session, and nothing about the paper
# reaches the console (which is the server log on a host).  See
# docs/paper-review-privacy.md.

test_that("reading an upload deletes its file and the directory Shiny made for it", {
  dir <- file.path(tempdir(), shinyUploadDirName())
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
  dir <- file.path(tempdir(), shinyUploadDirName())
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
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  # Discard empties the file fields as well, which kept the discarded files'
  # names and "Upload complete" (Shafer's test of 2026-11-17).
  reset <- character(0)
  testthat::local_mocked_bindings(reset = function(id = "", ...) reset <<- c(reset, id),
                                  .package = "shinyjs")
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
    expect_setequal(reset, c("manuscript", "supplement"))
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
  dir <- file.path(tempdir(), shinyUploadDirName())
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
    dir <- file.path(tempdir(), shinyUploadDirName())
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
    session$setInputs(ownKeyUse = TRUE, ownKey = "sk-ant-api03-userownkey1234")
    session$setInputs(manuscript = upload(), consent = TRUE)
    session$setInputs(review = 1)
    settle(session)
    expect_equal(sent, "sk-ant-api03-userownkey1234")   # the user's own key, when entered
    expect_false(is.null(review()))
    expect_match(output$status, "1,000 tokens read, 200 written")

    session$setInputs(discard = 1)
    session$setInputs(ownKeyUse = FALSE, ownKey = "")
    session$setInputs(manuscriptChosen = 1)              # a browser reports the choice first
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
  dir <- file.path(tempdir(), shinyUploadDirName())
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
        if (is.function(onJob)) onJob(function() {
          stopped <<- stopped + 1
          rejectFull(paperReviewError("background", "stopped"))
          TRUE
        })
        promises::promise(function(resolve, reject) rejectFull <<- reject)
      } else {
        promises::promise_resolve(list(extraction = paperReviewExample(),
                                       usage = list(input_tokens = 1000, output_tokens = 200)))
      }
    })
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
  dir <- file.path(tempdir(), shinyUploadDirName())
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

test_that("with stanpumpR's own key, a quick review is sent only if the limit has room for it", {
  # CodeRabbit, PR #720: its place is reserved before the full review is
  # stopped, and with none left the full review goes on.
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  clock <- as.POSIXct("2026-11-13 10:00:00", tz = "UTC")
  room <- FALSE
  admits <- 0
  released <- 0
  efforts <- character(0)
  rejectFull <- NULL
  stopped <- 0
  exhausted <- FALSE
  keys <- list()
  testthat::local_mocked_bindings(
    paperReviewNow = function() clock,
    # As the real one: the limit, once reached, withdraws the operator's key
    # except from a review whose place is already reserved.
    paperReviewApiKey = function(sessionKey = NULL, config = NULL, admitted = FALSE)
      if (!is.null(sessionKey)) sessionKey else if (exhausted && !isTRUE(admitted)) NULL else "operator-key",
    paperReviewSharedKeyAdmit = function(...) {
      admits <<- admits + 1
      # The quick review's place is the last: another session then reaches
      # the limit before the quick review is sent (CodeRabbit, PR #720).
      if (room) exhausted <<- TRUE
      if (admits == 1 || room) promises::promise_resolve(TRUE)
      else promises::promise_reject(paperReviewError("shared_limit", "The key has made its reviews."))
    },
    paperReviewSharedKeyRelease = function() released <<- released + 1,
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      keys[length(keys) + 1] <<- list(key)         # a NULL key kept as one
      if (effort == PAPER_REVIEW_EFFORT) {
        onJob(function() { stopped <<- stopped + 1; rejectFull(paperReviewError("background", "stopped")); TRUE })
        promises::promise(function(resolve, reject) rejectFull <<- reject)
      } else promises::promise_resolve(list(extraction = paperReviewExample(), usage = list()))
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    session$flushReact()
    expect_equal(admits, 1)                           # the full review's own place
    clock <<- clock + PAPER_REVIEW_LATE_SEC + 1
    session$elapse(PAPER_REVIEW_LATE_SEC * 1000 + 1000)
    # No room: the full review is not stopped, and goes on.
    session$setInputs(quickReview = 1)
    settle(session)
    expect_equal(admits, 2)
    expect_equal(stopped, 0)
    expect_true(reviewing())
    expect_match(output$status, "cannot be sent")
    expect_match(output$status, "full review goes on")
    # Room: the place is reserved, the full review stopped, and the quick
    # review sent on that place without asking for another.
    room <<- TRUE
    session$setInputs(quickReview = 2)
    settle(session)
    expect_equal(admits, 3)
    expect_equal(stopped, 1)
    expect_equal(efforts, c(PAPER_REVIEW_EFFORT, PAPER_REVIEW_QUICK_EFFORT))
    expect_equal(keys[[2]], "operator-key")             # kept, the limit reached since
    expect_equal(released, 0)
    expect_true(isTRUE(review()$quick))
  })
})

test_that("stopping a review ends its background process, not only the wait for it", {
  # Without a dispatcher, mirai::stop_mirai() settles the job at once but
  # leaves its process running, still reading the paper and billed
  # (CodeRabbit, PR #720; measured with mirai 2.7.4).
  skip_on_cran()
  skip_on_os("windows")     # the check that the process is gone reads `ps`
  testthat::local_mocked_bindings(
    paperReviewTransport = function(json, key, fallbacks = TRUE, url, version, beta, timeout, pidFile = NULL) {
      writeLines(as.character(Sys.getpid()), pidFile)
      Sys.sleep(60)
      list(failure = "connection")
    })
  stop <- NULL
  outcome <- NULL
  p <- paperReviewExtractAsync(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), key = "sk-ant-test",
                               onJob = function(f) stop <<- f)
  promises::then(p, function(x) outcome <<- "answered", function(e) outcome <<- e$code)
  expect_true(is.function(stop))
  # Wait for the process to write its ID.
  pidFile <- NULL
  for (i in 1:200) {
    files <- list.files(tempdir(), "^paper-review-", full.names = TRUE)
    pidFile <- files[file.exists(files) & file.size(files) > 0]
    if (length(pidFile)) break
    Sys.sleep(0.05)
  }
  pid <- as.integer(readLines(pidFile[length(pidFile)]))
  # A process that has ended stays a zombie ("Z") until its parent reaps it.
  state <- function() substr(trimws(paste(suppressWarnings(
    system2("ps", c("-o", "stat=", "-p", pid), stdout = TRUE, stderr = FALSE)), collapse = "")), 1, 1)
  expect_true(state() %in% c("S", "R", "D"))          # running
  expect_true(stop())
  for (i in 1:100) { later::run_now(0.05); if (!is.null(outcome)) break }
  expect_equal(outcome, "background")
  Sys.sleep(0.5)
  expect_true(state() %in% c("", "Z"))                # and ended
  expect_false(stop())                                # a second stop does nothing
})

test_that("a stop that cannot end the process says so and leaves the review running", {
  # CodeRabbit, PR #720: without a readable process ID, or when the signal is
  # not delivered, the stop is not confirmed, so the job is not settled (its
  # answer is still read) and the stop returns FALSE.
  skip_on_cran()
  skip_on_os("windows")
  own <- tempfile("paper-review-test-")
  job <- paperReviewInBackground(function(f) { writeLines(as.character(Sys.getpid()), f); Sys.sleep(30); "done" },
                                 list(f = own), 60)
  on.exit({
    pid <- suppressWarnings(as.integer(readLines(own, warn = FALSE)[1]))
    if (length(pid) == 1 && !is.na(pid)) tools::pskill(pid)
    mirai::stop_mirai(job)
    unlink(own)
  }, add = TRUE)
  for (i in 1:200) { if (file.exists(own) && file.size(own) > 0) break; Sys.sleep(0.05) }
  # No process ID written.
  expect_false(paperReviewStopper(job, tempfile("paper-review-none-"))())
  expect_true(mirai::unresolved(job))
  # An unreadable one.
  junk <- tempfile("paper-review-junk-")
  writeLines("not a number", junk)
  expect_false(paperReviewStopper(job, junk)())
  expect_true(mirai::unresolved(job))
  # The ID of a process that has already ended: the signal is not delivered.
  dead <- as.integer(system2("sh", c("-c", shQuote("echo $$")), stdout = TRUE))
  writeLines(as.character(dead), junk)
  expect_false(paperReviewStopper(job, junk)())
  expect_true(mirai::unresolved(job))
  unlink(junk)
})

test_that("when the full review cannot be stopped, no quick review is sent and its place is given back", {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  clock <- as.POSIXct("2026-11-13 10:00:00", tz = "UTC")
  admits <- 0
  released <- 0
  stopped <- 0
  efforts <- character(0)
  resolveFull <- NULL
  testthat::local_mocked_bindings(
    paperReviewNow = function() clock,
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewSharedKeyAdmit = function(...) { admits <<- admits + 1; promises::promise_resolve(TRUE) },
    paperReviewSharedKeyRelease = function() released <<- released + 1,
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      onJob(function() { stopped <<- stopped + 1; FALSE })   # the stop is not confirmed
      promises::promise(function(resolve, reject) resolveFull <<- resolve)
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    session$flushReact()
    clock <<- clock + PAPER_REVIEW_LATE_SEC + 1
    session$elapse(PAPER_REVIEW_LATE_SEC * 1000 + 1000)
    session$setInputs(quickReview = 1)
    settle(session)
    expect_equal(stopped, 1)
    expect_equal(admits, 2)
    expect_equal(released, 1)                         # the quick review's place is given back
    expect_equal(efforts, PAPER_REVIEW_EFFORT)        # and no quick review was sent
    expect_false(stopForQuick())
    expect_true(reviewing())
    expect_match(output$status, "could not be stopped")
    # The full review's answer is still read.
    resolveFull(list(extraction = paperReviewExample(), usage = list()))
    settle(session)
    expect_false(reviewing())
    expect_false(isTRUE(review()$quick))
    expect_false(exists("inFlight", envir = store))
  })
})

test_that("the stop function holds no manuscript, and the app lets it go once the answer returns", {
  # CodeRabbit, PR #720: made inside paperReviewExtractAsync(), it kept that
  # call's frame, and the manuscript and key in it, alive.
  stop <- paperReviewStopper("a job", "a file")
  expect_setequal(ls(environment(stop)), c("job", "pidFile"))
  testthat::local_mocked_bindings(
    paperReviewTransport = function(...) list(failure = "connection"),
    paperReviewInBackground = function(fn, args, timeout) promises::promise_resolve(list(failure = "connection")))
  held <- NULL
  p <- paperReviewExtractAsync(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), key = "sk-ant-test",
                               onJob = function(f) held <<- f)
  promises::catch(p, function(e) NULL)
  expect_false(any(c("pdf", "key", "supplement") %in% ls(environment(held))))
})

test_that("a second click while a quick review's place is asked for does nothing", {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  clock <- as.POSIXct("2026-11-13 10:00:00", tz = "UTC")
  admits <- 0
  resolveSecond <- NULL
  stopped <- 0
  efforts <- character(0)
  rejectFull <- NULL
  testthat::local_mocked_bindings(
    paperReviewNow = function() clock,
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewSharedKeyAdmit = function(...) {
      admits <<- admits + 1
      if (admits == 1) promises::promise_resolve(TRUE)
      else promises::promise(function(resolve, reject) resolveSecond <<- resolve)
    },
    paperReviewSharedKeyRelease = function() NULL,
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      if (effort == PAPER_REVIEW_EFFORT) {
        onJob(function() { stopped <<- stopped + 1; rejectFull(paperReviewError("background", "stopped")); TRUE })
        promises::promise(function(resolve, reject) rejectFull <<- reject)
      } else promises::promise(function(resolve, reject) NULL)   # the quick review runs on
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    session$flushReact()
    clock <<- clock + PAPER_REVIEW_LATE_SEC + 1
    session$elapse(PAPER_REVIEW_LATE_SEC * 1000 + 1000)
    session$setInputs(quickReview = 1)                 # asks for a place...
    session$setInputs(quickReview = 2)                 # ...and a second click waits for it
    expect_equal(admits, 2)
    resolveSecond(TRUE)
    settle(session)
    expect_equal(stopped, 1)
    expect_equal(efforts, c(PAPER_REVIEW_EFFORT, PAPER_REVIEW_QUICK_EFFORT))
    expect_null(running$stop)                          # the quick review's mock gave no stop
  })
})

test_that("a quick review's place that comes back after its review has ended does not touch the next one", {
  # CodeRabbit, PR #720: the place is bound to the review that asked for it.
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  clock <- as.POSIXct("2026-11-13 10:00:00", tz = "UTC")
  admits <- 0
  released <- 0
  resolveLate <- NULL
  stopped <- 0
  efforts <- character(0)
  resolvers <- list()
  testthat::local_mocked_bindings(
    paperReviewNow = function() clock,
    paperReviewApiKey = function(sessionKey = NULL, ...) sessionKey %or% "operator-key",
    paperReviewSharedKeyAdmit = function(...) {
      admits <<- admits + 1
      if (admits == 2) promises::promise(function(resolve, reject) resolveLate <<- resolve)
      else promises::promise_resolve(TRUE)
    },
    paperReviewSharedKeyRelease = function() released <<- released + 1,
    paperReviewUsageReport = function(record, ...) invisible(record),
    paperReviewExtractAsync = function(pdf, supplement = NULL, key, ..., effort, onJob = NULL) {
      efforts <<- c(efforts, effort)
      onJob(function() { stopped <<- stopped + 1; TRUE })
      promises::promise(function(resolve, reject) resolvers[[length(resolvers) + 1]] <<- resolve)
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf", datapath = path),
                      consent = TRUE)
    session$setInputs(review = 1)
    session$flushReact()
    clock <<- clock + PAPER_REVIEW_LATE_SEC + 1
    session$elapse(PAPER_REVIEW_LATE_SEC * 1000 + 1000)
    session$setInputs(quickReview = 1)                 # its place is slow to come back
    expect_equal(admits, 2)
    # The full review answers meanwhile, and the user starts another.
    resolvers[[1]](list(extraction = paperReviewExample(), usage = list()))
    settle(session)
    expect_false(reviewing())
    dir.create(dir, showWarnings = FALSE)                 # the upload was deleted once read
    writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
    session$setInputs(manuscript = list(name = "y.pdf", size = 59, type = "application/pdf", datapath = path))
    session$setInputs(review = 2)
    settle(session)
    expect_true(reviewing())
    before <- released
    resolveLate(TRUE)                                  # the first review's place comes back
    settle(session)
    expect_equal(released, before + 1)                 # and is given back
    expect_equal(stopped, 0)                           # the second review is not stopped
    expect_equal(efforts, c(PAPER_REVIEW_EFFORT, PAPER_REVIEW_EFFORT))
    expect_true(reviewing())
    resolvers[[2]](list(extraction = paperReviewExample(), usage = list()))
    settle(session)
  })
})


test_that("an upload chosen before Discard and finished after it is dropped unread", {
  # CodeRabbit on #776: Discard emptied the store and the fields, and the
  # upload still in flight was then stored while its field showed nothing.
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  late <- file.path(dir, "0.pdf"); after <- file.path(dir, "1.pdf")
  for (p in c(late, after)) writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), p)
  testthat::local_mocked_bindings(reset = function(...) NULL, .package = "shinyjs")
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscriptChosen = 1)
    session$setInputs(discard = 1)
    session$setInputs(manuscript = list(name = "a.pdf", size = 59, type = "application/pdf",
                                        datapath = late))
    expect_false(file.exists(late))
    expect_false(exists("pdf", envir = store))
    expect_match(output$status, "dropped unread")
    # One chosen after Discard is taken.
    session$setInputs(manuscriptChosen = 2)
    session$setInputs(manuscript = list(name = "b.pdf", size = 59, type = "application/pdf",
                                        datapath = after))
    expect_true(exists("pdf", envir = store))
  })
})


# CodeRabbit on #776 (review comment 4635345129): the Discard mark was
# cleared by the next choice, so an upload chosen before Discard that
# finished after a new choice was stored, and held until the new file
# replaced it: a Review in that window sent the manuscript the user had
# discarded.  An upload is now tied to its choice: the browser numbers each
# choice of a field (`<id>Chosen`) and, just before an upload's end reaches
# the server, says which choice it belongs to (`<id>Arrived`,
# paperReviewChosenScript()); only the latest choice made after the last
# Discard is stored.
raceUpload <- function(name) {
  dir <- file.path(tempdir(), shinyUploadDirName())
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  list(name = name, size = 59, type = "application/pdf", datapath = path)
}

test_that("an upload chosen before Discard is dropped although another was chosen before it arrived", {
  testthat::local_mocked_bindings(reset = function(...) NULL, .package = "shinyjs")
  shiny::testServer(paperReviewServer, {
    a <- raceUpload("a.pdf"); b <- raceUpload("b.pdf")
    session$setInputs(consent = TRUE)
    session$setInputs(manuscriptChosen = 1)              # A chosen; it starts uploading
    session$setInputs(discard = 1)                       # Discard
    session$setInputs(manuscriptChosen = 2)              # B chosen before A has arrived
    session$setInputs(manuscriptArrived = 1)             # the browser: this upload is choice 1
    session$setInputs(manuscript = a)                    # A arrives
    expect_false(file.exists(a$datapath))                # read and deleted
    expect_false(exists("pdf", envir = store))           # and never held
    expect_false(hasPdf())
    expect_match(output$status, "dropped unread")
    session$setInputs(manuscriptArrived = 2)
    session$setInputs(manuscript = b)                    # B arrives and is held
    expect_true(exists("pdf", envir = store))
    expect_true(hasPdf())
  })
})

test_that("without the browser's word on an upload, one that may predate Discard is dropped", {
  # The marker depends on Shiny's upload script; should it ever not come,
  # an upload that may have been chosen before a Discard is not stored,
  # and the user is asked to choose the file again.  This is the sequence
  # CodeRabbit gave, as the server saw it before the marker existed.
  testthat::local_mocked_bindings(reset = function(...) NULL, .package = "shinyjs")
  shiny::testServer(paperReviewServer, {
    a <- raceUpload("a.pdf"); b <- raceUpload("b.pdf"); c <- raceUpload("c.pdf")
    session$setInputs(consent = TRUE)
    session$setInputs(manuscriptChosen = 1)
    session$setInputs(discard = 1)
    session$setInputs(manuscriptChosen = 2)
    session$setInputs(manuscript = a)                    # A arrives, unmarked
    expect_false(file.exists(a$datapath))
    expect_false(exists("pdf", envir = store))
    expect_false(hasPdf())
    expect_match(output$status, "choose")
    # The next upload is taken: either B, after A, or a new choice.
    session$setInputs(manuscript = b)
    expect_true(exists("pdf", envir = store))
    # A Discard with nothing in flight leaves the next choice's upload alone.
    session$setInputs(discard = 2)
    session$setInputs(manuscriptChosen = 3)
    session$setInputs(manuscript = c)
    expect_true(exists("pdf", envir = store))
  })
})

test_that("an upload superseded by a later choice is dropped, Discard or not", {
  testthat::local_mocked_bindings(reset = function(...) NULL, .package = "shinyjs")
  shiny::testServer(paperReviewServer, {
    a <- raceUpload("a.pdf"); b <- raceUpload("b.pdf")
    session$setInputs(manuscriptChosen = 1)
    session$setInputs(manuscriptChosen = 2)
    session$setInputs(manuscriptArrived = 1)
    session$setInputs(manuscript = a)
    expect_false(file.exists(a$datapath))
    expect_false(exists("pdf", envir = store))
    session$setInputs(manuscriptArrived = 2)
    session$setInputs(manuscript = b)
    expect_true(exists("pdf", envir = store))
  })
})

test_that("the supplement field is tied to its choices too", {
  testthat::local_mocked_bindings(reset = function(...) NULL, .package = "shinyjs")
  shiny::testServer(paperReviewServer, {
    a <- raceUpload("a.pdf"); b <- raceUpload("b.pdf")
    session$setInputs(supplementChosen = 1)
    session$setInputs(discard = 1)
    session$setInputs(supplementChosen = 2)
    session$setInputs(supplementArrived = 1)
    session$setInputs(supplement = a)
    expect_false(exists("supplement", envir = store))
    session$setInputs(supplementArrived = 2)
    session$setInputs(supplement = b)
    expect_true(exists("supplement", envir = store))
  })
})

test_that("the browser says which choice each upload belongs to, ahead of the upload", {
  # paperReviewChosenScript() run in Node against a stand-in for Shiny's
  # upload script that, unlike Shiny's, does not abandon an upload when
  # another file is chosen: the earlier upload still says it is choice 1.
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  html <- as.character(paperReviewChosenScript(shiny::NS("paperReview")))
  js <- sub("(?s)^<script>(.*)</script>$", "\\1", html, perl = TRUE)
  harness <- paste0('
    var handlers = [], sent = [];
    function jQuery(x) {
      return { on: function(ev, sel, fn) { handlers.push({ev: ev, sel: sel, fn: fn}); },
               data: function(k) { return (x.__data || {})[k]; } };
    }
    var document = {}, window = {};
    var Shiny = window.Shiny = { setInputValue: function(name, value, opts) {
      sent.push({name: name, value: value, priority: opts && opts.priority}); } };
    function Uploader(label) { this.label = label; }
    Uploader.prototype.onComplete = function() { sent.push({name: "uploadEnd", value: this.label}); };
    ', js, '
    // Shiny binds its own change handler on the field, so it runs first and
    // makes the uploader; then the delegated handlers run.
    function choose(el, label, withUploader) {
      el.files = [{}];
      el.__data = withUploader ? {currentUploader: new Uploader(label)} : {};
      handlers.forEach(function(h) { h.fn.call(el, {}); });
      return el.__data.currentUploader;
    }
    var ms = {id: "paperReview-manuscript"}, sp = {id: "paperReview-supplement"};
    var a = choose(ms, "A", true);
    var b = choose(ms, "B", true);
    a.onComplete();                     // A finishes after B was chosen
    b.onComplete();
    var s = choose(sp, "S", true);
    s.onComplete();
    choose(ms, "C", false);             // Shiny made no uploader we know: no marker
    process.stdout.write(JSON.stringify({sel: handlers[0].sel, sent: sent}));')
  file <- tempfile(fileext = ".js")
  on.exit(unlink(file))
  writeLines(harness, file)
  out <- jsonlite::fromJSON(paste(system2(Sys.which("node"), shQuote(file), stdout = TRUE),
                                  collapse = ""))
  expect_equal(out$sel, "#paperReview-manuscript, #paperReview-supplement")
  sent <- out$sent
  expect_equal(paste(sent$name, sent$value),
               c("paperReview-manuscriptChosen 1", "paperReview-manuscriptChosen 2",
                 "paperReview-manuscriptArrived 1", "uploadEnd A",
                 "paperReview-manuscriptArrived 2", "uploadEnd B",
                 "paperReview-supplementChosen 1",
                 "paperReview-supplementArrived 1", "uploadEnd S",
                 "paperReview-manuscriptChosen 3"))
  # Sent at once, so ahead of the upload's end, and never a name.
  expect_true(all(sent$priority[sent$name != "uploadEnd"] == "event"))
})
