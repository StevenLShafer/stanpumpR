# Single Paper Review: counting the uses of the operator's API key.
#
# Shafer, 2026-11-10: the review uses his own Anthropic key by default "for
# now", he wants to know how often, and will probably require users' own keys
# after 200 uses.  So every review sent with the operator's key (not one a
# user brought) is counted, and reported in two places:
#   - one line on the server's log (stderr, which shinyapps.io keeps);
#   - one email to the operator, sent from a background process, whose
#     subject is always PAPER_REVIEW_USAGE_SUBJECT, so that searching the
#     mailbox for it counts the uses.
# The app has no persistent disk on shinyapps.io, so the count it prints is
# for its own R process only, which restarts when the app sleeps or is
# redeployed; the mailbox and the Anthropic Console's usage page for the key
# are the running totals.  `paper_review_shared_key: false` in config.yml
# withdraws the key (paperReviewApiKey()).
#
# Only the coarse metadata docs/paper-review-privacy.md section 1a defines is
# recorded: that a review was made and when, its token counts, how long it
# took, and its outcome class (an error code, never an error's text).  Never
# the manuscript, its name or size, the drug, any value, or a key.
# (Claude Code, 2026-11-10, at the request of Steven L. Shafer.)

PAPER_REVIEW_USAGE_SUBJECT <- "stanpumpR Single Paper Review: shared key used"
# The number of uses Shafer said he would probably allow (2026-11-10); the
# email says how far each one is toward it, by this process's count.
PAPER_REVIEW_SHARED_KEY_PLANNED_LIMIT <- 200L

# This R process's count, and when it began counting; and the usage emails
# still being sent.
.paperReviewUsage <- new.env(parent = emptyenv())
.paperReviewUsage$pending <- list()

# Outcomes of a review that never reached the API, so cost nothing and are
# not counted.
PAPER_REVIEW_UNSENT_CODES <- c("no_key", "no_pdf", "too_large", "no_httr2", "connection")

#' Record one review sent with the operator's key
#'
#' @param outcome "success", or the error code (\code{paper_review_error}'s
#'   \code{code})
#' @param usage the API's token counts, a list, possibly empty
#' @param seconds how long the review took
#' @param now the time
#' @returns the record, a list: \code{time}, \code{outcome},
#'   \code{tokensRead}, \code{tokensWritten}, \code{seconds}, \code{count}
#'   and \code{since}
#' @keywords internal
paperReviewUsageRecord <- function(outcome, usage = list(), seconds = NA_real_, now = Sys.time())
{
  if (is.null(.paperReviewUsage$since)) {
    .paperReviewUsage$since <- now
    .paperReviewUsage$count <- 0L
  }
  .paperReviewUsage$count <- .paperReviewUsage$count + 1L
  n <- function(x) suppressWarnings(as.numeric(x %or% NA))
  read <- sum(n(usage$input_tokens), n(usage$cache_read_input_tokens),
              n(usage$cache_creation_input_tokens), na.rm = TRUE)
  written <- n(usage$output_tokens)
  outcome <- if (is.character(outcome) && length(outcome) == 1 && grepl("^[a-z0-9_]+$", outcome))
    outcome else "unexpected"
  list(time = now, outcome = outcome,
       tokensRead = if (read > 0) read else NA_real_,
       tokensWritten = if (is.finite(written)) written else NA_real_,
       seconds = n(seconds), count = .paperReviewUsage$count, since = .paperReviewUsage$since)
}

# A time in UTC, which is what the host's log uses.
paperReviewUsageTime <- function(t) format(t, "%Y-%m-%d %H:%M UTC", tz = "UTC")

#' The usage record in words
#'
#' @param record from paperReviewUsageRecord()
#' @returns a character vector, one line each
#' @keywords internal
paperReviewUsageLines <- function(record)
{
  num <- function(x) if (is.na(x)) "not reported" else format(round(x), big.mark = ",")
  c(paste("Time:", paperReviewUsageTime(record$time)),
    paste("Outcome:", record$outcome),
    paste("Tokens read:", num(record$tokensRead)),
    paste("Tokens written:", num(record$tokensWritten)),
    paste("Duration:", if (is.na(record$seconds)) "not measured" else paste(round(record$seconds), "s")),
    sprintf("Shared-key reviews on this server process since %s: %d (%d planned in all)",
            paperReviewUsageTime(record$since), record$count, PAPER_REVIEW_SHARED_KEY_PLANNED_LIMIT))
}

#' Report one use of the operator's key
#'
#' A line on the server log, and an email to the operator when mail is
#' configured.  Never fails: a review is not lost to its own accounting.
#'
#' @param record from paperReviewUsageRecord()
#' @param config the app's configuration list
#' @returns invisibly, the record
#' @keywords internal
paperReviewUsageReport <- function(record, config = .sprglobals$config)
{
  tryCatch({
    base::message("stanpumpR Single Paper Review: ", paste(paperReviewUsageLines(record), collapse = "; "))
    paperReviewUsageNotify(record, config)
  }, error = function(e) NULL)
  invisible(record)
}

#' Email the operator one use of the shared key
#'
#' Sent from a background process (an SMTP send can stall for a minute), to
#' \code{paper_review_usage_email} in config.yml, or else to the app's own
#' mail account, \code{email_username}.  Nothing is sent without the mail
#' credentials.
#'
#' @inheritParams paperReviewUsageReport
#' @returns the background job, or NULL when nothing was sent
#' @keywords internal
paperReviewUsageNotify <- function(record, config = .sprglobals$config)
{
  if (!is.list(config)) return(NULL)
  from <- config$email_username
  password <- config$email_password
  to <- config$paper_review_usage_email %or% from
  ok <- function(x) is.character(x) && length(x) == 1 && nzchar(x)
  if (!ok(from) || !ok(password) || !ok(to)) return(NULL)
  body <- paste(c(paperReviewUsageLines(record), "",
    "Each review made with your Anthropic key sends one email with this subject, so searching",
    "your mailbox for the subject counts them; the Anthropic Console's usage page for the key",
    "is the other running total. The count above restarts with the server process.",
    "To require users' own keys, set paper_review_shared_key: false in the deployed config.yml",
    "(docs/paper-review.md, Setting up the API key)."), collapse = "\n")
  job <- paperReviewInBackground(paperReviewSendUsageEmail,
    list(from = from, password = password, to = to, subject = PAPER_REVIEW_USAGE_SUBJECT, body = body,
         attempts = EMAIL_SMTP_ATTEMPTS, connectTimeout = EMAIL_SMTP_CONNECT_TIMEOUT,
         timeout = EMAIL_SMTP_TIMEOUT),
    EMAIL_SMTP_ATTEMPTS * (EMAIL_SMTP_CONNECT_TIMEOUT + EMAIL_SMTP_TIMEOUT) + 60)
  # Held until it settles, so the job is not collected before it is sent;
  # a failure goes to the server log, as Email Slide's does.
  key <- format(record$count)
  .paperReviewUsage$pending[[key]] <- job
  promises::then(job,
    onFulfilled = function(value) .paperReviewUsage$pending[[key]] <- NULL,
    onRejected = function(e) {
      .paperReviewUsage$pending[[key]] <- NULL
      base::message("stanpumpR Single Paper Review: the usage email failed: ", conditionMessage(e))
    })
  job
}

# The send itself, in the background process: self-contained (emayili:: only).
paperReviewSendUsageEmail <- function(from, password, to, subject, body, attempts, connectTimeout, timeout)
{
  msg <- emayili::envelope()
  msg <- emayili::from(msg, paste0("stanpumpR <", from, ">"))
  msg <- emayili::to(msg, to)
  msg <- emayili::subject(msg, subject, interpolate = FALSE)
  msg <- emayili::text(msg, body, interpolate = FALSE)
  smtp <- emayili::server(host = "smtp.gmail.com", port = 587, username = from, password = password,
                          max_times = attempts, connecttimeout = connectTimeout, timeout = timeout)
  smtp(msg, verbose = FALSE)
  TRUE
}

#' Count a review when it settles, if it used the operator's key
#'
#' Chained onto the review's own promise, not left to the session's
#' observers, so a review is counted even when its page was closed while
#' Claude read the paper (it is billed all the same).
#'
#' @param promise the review, from paperReviewExtractAsync()
#' @param shared was it sent with the operator's key?
#' @param started when it was sent
#' @returns a promise that settles as \code{promise} does
#' @keywords internal
paperReviewTrackUse <- function(promise, shared, started = Sys.time())
{
  if (!isTRUE(shared)) return(promise)
  elapsed <- function() as.numeric(difftime(Sys.time(), started, units = "secs"))
  promises::then(promise,
    onFulfilled = function(result) {
      paperReviewUsageReport(paperReviewUsageRecord("success", result$usage %or% list(), elapsed()))
      result
    },
    onRejected = function(e) {
      code <- e$code %or% "unexpected"
      if (!code %in% PAPER_REVIEW_UNSENT_CODES)
        paperReviewUsageReport(paperReviewUsageRecord(code, e$usage %or% list(), elapsed()))
      stop(e)
    })
}
