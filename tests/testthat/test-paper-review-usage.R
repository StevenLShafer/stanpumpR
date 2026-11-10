# Single Paper Review: counting the uses of the operator's key (Shafer,
# 2026-11-10).  Coarse metadata only (docs/paper-review-privacy.md 1a).

resetUsage <- function() {
  old <- as.list(.paperReviewUsage)
  rm(list = setdiff(ls(.paperReviewUsage), "pending"), envir = .paperReviewUsage)
  old
}

test_that("each use is counted, with its tokens, time and outcome and nothing else", {
  old <- resetUsage()
  on.exit({ resetUsage(); for (n in names(old)) assign(n, old[[n]], envir = .paperReviewUsage) }, add = TRUE)
  t0 <- as.POSIXct("2026-11-10 14:02:00", tz = "UTC")
  r1 <- paperReviewUsageRecord("success", list(input_tokens = 50000, cache_read_input_tokens = 2341,
                                                output_tokens = 8120), 183.6, t0)
  r2 <- paperReviewUsageRecord("http_529", list(), NA, t0 + 60)
  expect_equal(c(r1$count, r2$count), 1:2)
  expect_equal(r2$since, t0)
  lines <- paperReviewUsageLines(r1)
  expect_true(any(grepl("Tokens read: 52,341", lines)))
  expect_true(any(grepl("Tokens written: 8,120", lines)))
  expect_true(any(grepl("Duration: 184 s", lines)))
  expect_true(any(grepl("since 2026-11-10 14:02 UTC: 1 \\(200 planned", lines)))
  expect_true(any(grepl("Tokens read: not reported", paperReviewUsageLines(r2))))
  # An outcome is a code, never an error's text.
  expect_equal(paperReviewUsageRecord("The API said: DRUGNAME", list())$outcome, "unexpected")
})

test_that("the log line goes to stderr, and no email is sent without the mail credentials", {
  old <- resetUsage()
  on.exit({ resetUsage(); for (n in names(old)) assign(n, old[[n]], envir = .paperReviewUsage) }, add = TRUE)
  sent <- list()
  testthat::local_mocked_bindings(paperReviewInBackground = function(fn, args, timeout) {
    sent[[length(sent) + 1]] <<- args
    promises::promise_resolve(TRUE)
  })
  record <- paperReviewUsageRecord("success", list(input_tokens = 10, output_tokens = 5), 1)
  expect_message(paperReviewUsageReport(record, list()), "Single Paper Review: Time:")
  expect_length(sent, 0)
  expect_null(paperReviewUsageNotify(record, list(email_username = "app@example.org")))
  suppressMessages(paperReviewUsageReport(record, list(email_username = "app@example.org", email_password = "pw")))
  expect_length(sent, 1)
  expect_equal(sent[[1]]$to, "app@example.org")
  expect_equal(sent[[1]]$subject, PAPER_REVIEW_USAGE_SUBJECT)
  expect_match(sent[[1]]$body, "paper_review_shared_key: false")
  suppressMessages(paperReviewUsageReport(record, list(email_username = "app@example.org", email_password = "pw",
                                                       paper_review_usage_email = "operator@example.org")))
  expect_equal(sent[[2]]$to, "operator@example.org")
  # A failure in the accounting never fails the review.
  testthat::local_mocked_bindings(paperReviewUsageNotify = function(...) stop("smtp down"))
  expect_no_error(suppressMessages(paperReviewUsageReport(record, list())))
})

test_that("a review is counted only when it used the operator's key and reached the API", {
  counted <- character(0)
  testthat::local_mocked_bindings(
    paperReviewUsageReport = function(record, ...) { counted <<- c(counted, record$outcome); invisible(record) })
  old <- resetUsage()
  on.exit({ resetUsage(); for (n in names(old)) assign(n, old[[n]], envir = .paperReviewUsage) }, add = TRUE)
  run <- function(p) { while (!later::loop_empty()) later::run_now(0.01) }
  ok <- promises::promise_resolve(list(usage = list(input_tokens = 1, output_tokens = 1)))
  fail <- function(code) promises::promise_reject(paperReviewError(code, "x"))
  value <- NULL
  promises::then(paperReviewTrackUse(ok, shared = TRUE), function(x) value <<- x); run()
  expect_equal(value$usage$input_tokens, 1)                    # the result passes through
  promises::catch(paperReviewTrackUse(fail("max_tokens"), shared = TRUE), function(e) value <<- e); run()
  expect_equal(value$code, "max_tokens")                       # and so does the error
  paperReviewTrackUse(ok, shared = FALSE); run()               # the user's own key
  promises::catch(paperReviewTrackUse(fail("connection"), shared = TRUE), function(e) NULL); run()
  promises::catch(paperReviewTrackUse(fail("too_large"), shared = TRUE), function(e) NULL); run()
  expect_equal(counted, c("success", "max_tokens"))
})
