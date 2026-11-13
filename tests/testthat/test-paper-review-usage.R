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
  expect_true(any(grepl("since 2026-11-10 14:02 UTC: 1 \\(the key stops after 200 in all\\)", lines)))
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

test_that("the operator's key stops after its limit, counted in the mailbox across processes", {
  old <- resetUsage()
  on.exit({ resetUsage(); for (n in names(old)) assign(n, old[[n]], envir = .paperReviewUsage) }, add = TRUE)
  run <- function() { while (!later::loop_empty()) later::run_now(0.01) }
  settle <- function(p) {
    out <- NULL
    promises::then(p, function(x) out <<- list(ok = x), function(e) out <<- list(error = e)); run()
    out
  }
  mail <- list(email_username = "app@example.org", email_password = "pw")
  inMailbox <- 0
  asked <- list()
  testthat::local_mocked_bindings(paperReviewInBackground = function(fn, args, timeout) {
    asked[[length(asked) + 1]] <<- args
    if (is.character(inMailbox)) promises::promise_reject(simpleError(inMailbox))
    else promises::promise_resolve(inMailbox)
  })
  expect_equal(paperReviewSharedKeyLimit(list()), 200L)
  expect_equal(paperReviewSharedKeyLimit(list(paper_review_shared_key_limit = 3)), 3L)
  expect_equal(paperReviewSharedKeyLimit(list(paper_review_shared_key_limit = "lots")), 200L)

  # 199 already in the mailbox: the 200th review is admitted, and the
  # mailbox is the account's Sent folder, searched for the usage subject.
  inMailbox <- 199
  expect_true(settle(paperReviewSharedKeyAdmit(mail))$ok)
  expect_equal(asked[[1]]$mailbox, PAPER_REVIEW_USAGE_MAILBOX)
  expect_equal(asked[[1]]$subject, PAPER_REVIEW_USAGE_SUBJECT)
  # Its email not yet sent, the next one would be the 201st.
  r <- suppressMessages(settle(paperReviewSharedKeyAdmit(mail)))
  expect_equal(r$error$code, "shared_limit")
  expect_match(conditionMessage(r$error), "own key")
  # From then on this process refuses at once, and offers no key to new sessions.
  expect_true(paperReviewSharedKeyExhausted())
  n <- length(asked)
  expect_equal(suppressMessages(settle(paperReviewSharedKeyAdmit(mail)))$error$code, "shared_limit")
  expect_length(asked, n)
  withr::local_envvar(STANPUMPR_ANTHROPIC_API_KEY = "sk-ant-test")
  expect_null(paperReviewApiKey(NULL, list()))
  expect_equal(paperReviewApiKey("sk-ant-own", list()), "sk-ant-own")

  # A count that cannot be read refuses the key (the limit fails closed),
  # but does not end it.
  resetUsage()
  inMailbox <- "IMAP down"
  r <- suppressMessages(settle(paperReviewSharedKeyAdmit(mail)))
  expect_equal(r$error$code, "shared_unverified")
  expect_false(paperReviewSharedKeyExhausted())
  expect_equal(paperReviewApiKey(NULL, list()), "sk-ant-test")

  # Without the mail credentials, this process's own count is the limit.
  resetUsage()
  three <- list(paper_review_shared_key_limit = 3)
  for (i in 1:3) expect_true(settle(paperReviewSharedKeyAdmit(three))$ok)
  paperReviewSharedKeyRelease()                       # one never reached the API
  expect_true(settle(paperReviewSharedKeyAdmit(three))$ok)
  expect_equal(suppressMessages(settle(paperReviewSharedKeyAdmit(three)))$error$code, "shared_limit")
})

test_that("a review's email moves it from this process's count to the mailbox's", {
  old <- resetUsage()
  on.exit({ resetUsage(); for (n in names(old)) assign(n, old[[n]], envir = .paperReviewUsage) }, add = TRUE)
  run <- function() { while (!later::loop_empty()) later::run_now(0.01) }
  testthat::local_mocked_bindings(paperReviewInBackground = function(fn, args, timeout) promises::promise_resolve(TRUE))
  .paperReviewUsage$started <- 1L
  record <- paperReviewUsageRecord("success", list(input_tokens = 1, output_tokens = 1), 1)
  suppressMessages(paperReviewUsageReport(record, list(email_username = "app@example.org", email_password = "pw")))
  run()
  expect_equal(paperReviewSharedKeyCounts()$emailed, 1L)
  # A review that never reached the API gives its reservation back.
  promises::catch(paperReviewTrackUse(promises::promise_reject(paperReviewError("connection", "x")),
                                      shared = TRUE), function(e) NULL)
  run()
  expect_equal(paperReviewSharedKeyCounts()$released, 1L)
})

test_that("the mailbox count reads the IMAP SEARCH response", {
  skip_if_not_installed("curl")
  reply <- "* SEARCH 3 17 42\r\nA003 OK SEARCH completed\r\n"
  seen <- NULL
  testthat::local_mocked_bindings(
    curl_fetch_memory = function(url, handle) { seen <<- url; list(content = charToRaw(reply)) },
    .package = "curl")
  n <- paperReviewCountUsageEmails(PAPER_REVIEW_IMAP_URL, "app@example.org", "pw",
                                   PAPER_REVIEW_USAGE_MAILBOX, PAPER_REVIEW_USAGE_SUBJECT, 1, 1)
  expect_equal(n, 3)
  expect_equal(seen, "imaps://imap.gmail.com:993/%5BGmail%5D%2FSent%20Mail")
  reply <- "* SEARCH\r\nA003 OK SEARCH completed\r\n"
  expect_equal(paperReviewCountUsageEmails(PAPER_REVIEW_IMAP_URL, "a", "b", "INBOX", "s", 1, 1), 0)
  reply <- "A003 NO [AUTHENTICATIONFAILED]\r\n"
  expect_error(paperReviewCountUsageEmails(PAPER_REVIEW_IMAP_URL, "a", "b", "INBOX", "s", 1, 1), "SEARCH")
})
