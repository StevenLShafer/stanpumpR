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

test_that("pressing Review runs the call, drops the manuscript and reports, with no network", {
  dir <- file.path(tempdir(), paste0("upload-", paste(sample(letters, 12), collapse = "")))
  dir.create(dir)
  path <- file.path(dir, "0.pdf")
  writeBin(c(charToRaw("%PDF-1.7\n"), as.raw(1:50)), path)
  calls <- 0
  testthat::local_mocked_bindings(
    paperReviewApiKey = function(...) "test-key",
    paperReviewExtract = function(...) {
      calls <<- calls + 1
      stop(paperReviewError("http_529", "The API is overloaded (529). Try again shortly."))
    })
  shiny::testServer(paperReviewServer, {
    session$setInputs(manuscript = list(name = "x.pdf", size = 59, type = "application/pdf",
                                        datapath = path))
    session$setInputs(consent = TRUE)
    session$setInputs(review = 1)
    expect_equal(calls, 1)
    expect_match(output$status, "overloaded")
    expect_match(output$status, "The manuscript has been discarded")
    expect_false(hasPdf())
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
