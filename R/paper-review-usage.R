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
# The limit (Shafer, 2026-11-10: "stop provid[ing] access after 200 papers
# have been tested. I don't want a malicious actor to incur thousands of
# dollars"): before each review with the operator's key, the usage emails
# already sent are counted in the mail account's Sent folder over IMAP,
# which survives restarts and redeploys and is shared by every process, and
# the reviews this process has started whose email has not yet gone are
# added.  Past PAPER_REVIEW_SHARED_KEY_LIMIT (or `paper_review_shared_key_limit`
# in config.yml) the operator's key is no longer used and the user is asked
# for their own.  A count that cannot be read refuses the key too: the limit
# fails closed.  Without the mail credentials there is no mailbox to count,
# and the limit holds per R process only.  A spend limit on the key's
# workspace in the Anthropic Console is the backstop that no bug here can
# lift (docs/paper-review.md, Setting up the API key).
#
# Only the coarse metadata docs/paper-review-privacy.md section 1a defines is
# recorded: that a review was made and when, its token counts, how long it
# took, and its outcome class (an error code, never an error's text).  Never
# the manuscript, its name or size, the drug, any value, or a key.
# (Claude Code, 2026-11-10, at the request of Steven L. Shafer.)

PAPER_REVIEW_USAGE_SUBJECT <- "stanpumpR Single Paper Review: shared key used"
# The reviews the operator's key may make in all (Shafer, 2026-11-10).
PAPER_REVIEW_SHARED_KEY_LIMIT <- 200L
# Where the usage emails are counted: the sending account's Sent folder
# (Gmail keeps a copy of every message sent through its SMTP server).
PAPER_REVIEW_USAGE_MAILBOX <- "[Gmail]/Sent Mail"
PAPER_REVIEW_IMAP_URL <- "imaps://imap.gmail.com:993/"
PAPER_REVIEW_IMAP_CONNECT_TIMEOUT <- 10
PAPER_REVIEW_IMAP_TIMEOUT <- 30

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
    sprintf("Shared-key reviews on this server process since %s: %d (the key stops after %d in all)",
            paperReviewUsageTime(record$since), record$count, paperReviewSharedKeyLimit()))
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
    onFulfilled = function(value) {
      .paperReviewUsage$pending[[key]] <- NULL
      # Now in the Sent folder, where paperReviewSharedKeyAdmit() counts it.
      .paperReviewUsage$emailed <- (.paperReviewUsage$emailed %or% 0L) + 1L
    },
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
      else paperReviewSharedKeyRelease()
      stop(e)
    })
}

#' The number of reviews the operator's key may make
#'
#' \code{paper_review_shared_key_limit} in config.yml when it is a whole
#' number of 0 or more, else PAPER_REVIEW_SHARED_KEY_LIMIT.
#'
#' @param config the app's configuration list
#' @returns an integer
#' @keywords internal
paperReviewSharedKeyLimit <- function(config = .sprglobals$config)
{
  v <- if (is.list(config)) suppressWarnings(as.numeric(config$paper_review_shared_key_limit %or% NA)) else NA
  if (length(v) == 1 && is.finite(v) && v >= 0) as.integer(floor(v)) else PAPER_REVIEW_SHARED_KEY_LIMIT
}

#' Has this process seen the operator's key reach its limit?
#' @keywords internal
paperReviewSharedKeyExhausted <- function() isTRUE(.paperReviewUsage$exhausted)

# This process's reviews with the operator's key: started, and of those
# released (they never reached the API, or were refused here) and emailed
# (now in the Sent folder).
paperReviewSharedKeyCounts <- function() {
  u <- .paperReviewUsage
  list(started = u$started %or% 0L, released = u$released %or% 0L, emailed = u$emailed %or% 0L)
}

#' Give back a review that cost nothing
#' @keywords internal
paperReviewSharedKeyRelease <- function()
  .paperReviewUsage$released <- (.paperReviewUsage$released %or% 0L) + 1L

#' May the next review use the operator's key?
#'
#' Reserves a review at once (so that reviews started together all count),
#' then counts the usage emails in the mail account's Sent folder in a
#' background process and adds this process's reviews whose email has not
#' yet been sent.  Within the limit the promise resolves; past it, or when
#' the count cannot be read, the reservation is released and the promise is
#' rejected with a \code{paper_review_error} (\code{shared_limit} or
#' \code{shared_unverified}) that tells the user to enter their own key.
#'
#' @param config the app's configuration list
#' @returns a promise of TRUE
#' @keywords internal
paperReviewSharedKeyAdmit <- function(config = .sprglobals$config)
{
  limit <- paperReviewSharedKeyLimit(config)
  refuse <- function(code, log) {
    paperReviewSharedKeyRelease()
    if (code == "shared_limit") .paperReviewUsage$exhausted <- TRUE
    base::message("stanpumpR Single Paper Review: ", log)
    promises::promise_reject(paperReviewError(code, if (code == "shared_limit")
      paste0("stanpumpR's own Anthropic key has made the ", limit, " reviews it is allowed, so reviews ",
             "now need your own key. Enter it above and review again.")
    else paste0("stanpumpR could not confirm how many reviews its own Anthropic key has made, so the key ",
                "was not used. Enter your own key above, or try again later.")))
  }
  if (paperReviewSharedKeyExhausted() || limit == 0)
    return(refuse("shared_limit", "the shared key's limit has been reached; it was not used."))
  .paperReviewUsage$started <- (.paperReviewUsage$started %or% 0L) + 1L
  n <- paperReviewSharedKeyCounts()
  inProcess <- n$started - n$released
  from <- if (is.list(config)) config$email_username else NULL
  password <- if (is.list(config)) config$email_password else NULL
  ok <- function(x) is.character(x) && length(x) == 1 && nzchar(x) && !grepl("^<", x)
  if (!ok(from) || !ok(password)) {
    # No mailbox to count: this process's own count is the only one.
    if (inProcess > limit)
      return(refuse("shared_limit", paste0("the shared key's limit (", limit, ") has been reached by this ",
                                           "process's count; it was not used.")))
    return(promises::promise_resolve(TRUE))
  }
  mailbox <- config$paper_review_usage_mailbox %or% PAPER_REVIEW_USAGE_MAILBOX
  job <- paperReviewInBackground(paperReviewCountUsageEmails,
    list(url = PAPER_REVIEW_IMAP_URL, username = from, password = password, mailbox = mailbox,
         subject = PAPER_REVIEW_USAGE_SUBJECT, connectTimeout = PAPER_REVIEW_IMAP_CONNECT_TIMEOUT,
         timeout = PAPER_REVIEW_IMAP_TIMEOUT),
    PAPER_REVIEW_IMAP_CONNECT_TIMEOUT + PAPER_REVIEW_IMAP_TIMEOUT + 60)
  promises::then(job,
    onFulfilled = function(sent) {
      if (!is.numeric(sent) || length(sent) != 1 || !is.finite(sent))
        return(refuse("shared_unverified", "the shared key's uses could not be counted; it was not used."))
      n <- paperReviewSharedKeyCounts()
      used <- sent + (n$started - n$released - n$emailed)
      if (used > limit)
        return(refuse("shared_limit", paste0("the shared key's limit (", limit, ") has been reached: ",
                                             sent, " reviews counted in the mailbox; it was not used.")))
      TRUE
    },
    onRejected = function(e)
      refuse("shared_unverified", "the shared key's uses could not be counted; it was not used."))
}

# Count the usage emails in a mailbox, in the background process:
# self-contained (curl:: only).  Returns the number, or stops.
paperReviewCountUsageEmails <- function(url, username, password, mailbox, subject, connectTimeout, timeout)
{
  h <- curl::new_handle()
  curl::handle_setopt(h, username = username, password = password, use_ssl = 3L,
                      customrequest = paste0("SEARCH SUBJECT \"", gsub("\"", "", subject, fixed = TRUE), "\""),
                      connecttimeout = connectTimeout, timeout = timeout)
  res <- curl::curl_fetch_memory(paste0(url, utils::URLencode(mailbox, reserved = TRUE)), handle = h)
  text <- rawToChar(res$content)
  line <- regmatches(text, regexpr("\\* SEARCH[ 0-9]*", text))
  if (!length(line)) stop("the mailbox gave no SEARCH response")
  length(regmatches(line, gregexpr("[0-9]+", line))[[1]])
}
