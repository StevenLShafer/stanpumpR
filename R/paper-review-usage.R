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
# dollars"; made 200 a MONTH on 2026-11-17, with a countdown of the reviews
# left shown in the tab: "Dr. Shafer is providing 200 single paper reviews
# per month through stanpumpR").  The month is the calendar month in UTC,
# as the Anthropic Console's spend limits reset.  Before each review with
# the operator's key, the usage emails sent since the month began are
# counted in the mail account's Sent folder over IMAP (SEARCH SINCE, which
# compares the date the server gives each message, so a review within hours
# of midnight UTC at the month's turn can fall in either month), which
# survives restarts and redeploys and is shared by every process, and the
# reviews this process has started whose email has not yet gone are added.
# Past PAPER_REVIEW_SHARED_KEY_LIMIT (or `paper_review_shared_key_limit` in
# config.yml) the operator's key is no longer used until the month turns,
# and the user is asked for their own.  A count that cannot be read refuses the key too: the limit
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
# The reviews the operator's key may make in a calendar month, UTC (Shafer,
# 2026-11-10: 200 in all; 2026-11-17: 200 a month).
PAPER_REVIEW_SHARED_KEY_LIMIT <- 200L
# How long the countdown the tab shows may reuse a count of the mailbox, so
# that sessions opening the tab together log in to it once.  A review's own
# admission always counts afresh.
PAPER_REVIEW_COUNT_CACHE_SEC <- 60
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
    sprintf("Shared-key reviews on this server process since %s: %d (the key allows %d a month)",
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

# The month the limit counts in, "2026-11": the calendar month in UTC.
paperReviewMonth <- function(now = Sys.time()) format(now, "%Y-%m", tz = "UTC")

# The day the month's reviews renew: the first of the next month, UTC.
paperReviewMonthRenews <- function(now = Sys.time())
{
  lt <- as.POSIXlt(now, tz = "UTC")
  y <- lt$year + 1900 + (lt$mon == 11)
  m <- (lt$mon + 1) %% 12 + 1
  as.Date(sprintf("%04d-%02d-01", y, m))
}

# A date in words, "1 December", whatever the locale.
paperReviewDayMonth <- function(d)
  paste(as.integer(format(d, "%d")), month.name[as.integer(format(d, "%m"))])

# The first of the month as IMAP writes a date, "1-Nov-2026".
paperReviewImapSince <- function(now = Sys.time())
{
  lt <- as.POSIXlt(now, tz = "UTC")
  sprintf("1-%s-%04d", month.abb[lt$mon + 1], lt$year + 1900)
}

# A new month starts this process's counts afresh: the reviews in flight and
# the limit reached belonged to the month before.  (The first call only
# notes the month.)
paperReviewUsageRoll <- function(now = Sys.time())
{
  month <- paperReviewMonth(now)
  if (is.null(.paperReviewUsage$month)) .paperReviewUsage$month <- month
  if (identical(.paperReviewUsage$month, month)) return(invisible(FALSE))
  .paperReviewUsage$month <- month
  .paperReviewUsage$started <- 0L
  .paperReviewUsage$released <- 0L
  .paperReviewUsage$emailed <- 0L
  .paperReviewUsage$exhausted <- FALSE
  .paperReviewUsage$display <- NULL
  invisible(TRUE)
}

#' Has this process seen the operator's key reach this month's limit?
#' @param now the time
#' @keywords internal
paperReviewSharedKeyExhausted <- function(now = Sys.time())
{
  paperReviewUsageRoll(now)
  isTRUE(.paperReviewUsage$exhausted)
}

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
#' @param now the time
#' @returns a promise of TRUE
#' @keywords internal
paperReviewSharedKeyAdmit <- function(config = .sprglobals$config, now = Sys.time())
{
  limit <- paperReviewSharedKeyLimit(config)
  refuse <- function(code, log) {
    paperReviewSharedKeyRelease()
    if (code == "shared_limit") .paperReviewUsage$exhausted <- TRUE
    base::message("stanpumpR Single Paper Review: ", log)
    promises::promise_reject(paperReviewError(code, if (code == "shared_limit")
      paste0("stanpumpR's own Anthropic key has made this month's ", limit, " reviews, so until they ",
             "renew on ", paperReviewDayMonth(paperReviewMonthRenews(now)), " reviews need your own key. ",
             "Tick \"", PAPER_REVIEW_OWN_KEY_LABEL, "\" above, enter it and review again.")
    else paste0("stanpumpR could not confirm how many reviews its own Anthropic key has made this month, ",
                "so the key was not used. Tick \"", PAPER_REVIEW_OWN_KEY_LABEL, "\" above and enter yours, ",
                "or try again later.")))
  }
  if (paperReviewSharedKeyExhausted(now) || limit == 0)
    return(refuse("shared_limit", "the shared key's limit has been reached; it was not used."))
  .paperReviewUsage$started <- (.paperReviewUsage$started %or% 0L) + 1L
  n <- paperReviewSharedKeyCounts()
  inProcess <- n$started - n$released
  if (!paperReviewMailConfigured(config)) {
    # No mailbox to count: this process's own count is the only one.
    if (inProcess > limit)
      return(refuse("shared_limit", paste0("the shared key's limit (", limit, " a month) has been reached ",
                                           "by this process's count; it was not used.")))
    return(promises::promise_resolve(TRUE))
  }
  promises::then(paperReviewCountThisMonth(config, now),
    onFulfilled = function(sent) {
      if (!is.numeric(sent) || length(sent) != 1 || !is.finite(sent))
        return(refuse("shared_unverified", "the shared key's uses could not be counted; it was not used."))
      n <- paperReviewSharedKeyCounts()
      used <- sent + (n$started - n$released - n$emailed)
      if (used > limit)
        return(refuse("shared_limit", paste0("the shared key's limit (", limit, " a month) has been reached: ",
                                             sent, " reviews this month counted in the mailbox; it was not used.")))
      TRUE
    },
    onRejected = function(e)
      refuse("shared_unverified", "the shared key's uses could not be counted; it was not used."))
}

# Are the mail account's credentials there, so that the mailbox can be
# counted?
paperReviewMailConfigured <- function(config = .sprglobals$config)
{
  ok <- function(x) is.character(x) && length(x) == 1 && nzchar(x) && !grepl("^<", x)
  is.list(config) && ok(config$email_username) && ok(config$email_password)
}

# The usage emails sent this month, counted in the mailbox in a background
# process: a promise of the number.  Each count is kept for the countdown
# (paperReviewSharedKeyRemaining()), with the emails this process had sent
# by then.
paperReviewCountThisMonth <- function(config = .sprglobals$config, now = Sys.time())
{
  month <- paperReviewMonth(now)
  job <- paperReviewInBackground(paperReviewCountUsageEmails,
    list(url = PAPER_REVIEW_IMAP_URL, username = config$email_username, password = config$email_password,
         mailbox = config$paper_review_usage_mailbox %or% PAPER_REVIEW_USAGE_MAILBOX,
         subject = PAPER_REVIEW_USAGE_SUBJECT, connectTimeout = PAPER_REVIEW_IMAP_CONNECT_TIMEOUT,
         timeout = PAPER_REVIEW_IMAP_TIMEOUT, since = paperReviewImapSince(now)),
    PAPER_REVIEW_IMAP_CONNECT_TIMEOUT + PAPER_REVIEW_IMAP_TIMEOUT + 60)
  promises::then(job, function(sent) {
    if (is.numeric(sent) && length(sent) == 1 && is.finite(sent) && identical(paperReviewMonth(), month))
      .paperReviewUsage$display <- list(at = Sys.time(), month = month, sent = sent,
                                        emailed = paperReviewSharedKeyCounts()$emailed)
    sent
  })
}

#' The reviews the operator's key has left this month
#'
#' For the countdown the tab shows.  A count of the mailbox less than
#' PAPER_REVIEW_COUNT_CACHE_SEC old is reused, with the emails this process
#' has sent since and its reviews in flight added; without the mail
#' credentials, this process's own count.  It decides nothing: each review
#' is admitted on a fresh count (paperReviewSharedKeyAdmit()).
#'
#' @param config the app's configuration list
#' @param now the time
#' @returns a promise of a list: \code{remaining}, \code{limit} and
#'   \code{renews} (a Date); rejected when the mailbox cannot be counted
#' @keywords internal
paperReviewSharedKeyRemaining <- function(config = .sprglobals$config, now = Sys.time())
{
  paperReviewUsageRoll(now)
  limit <- paperReviewSharedKeyLimit(config)
  left <- function(used) list(remaining = max(0L, as.integer(limit - used)), limit = limit,
                              renews = paperReviewMonthRenews(now))
  inFlight <- function(n = paperReviewSharedKeyCounts()) max(0L, n$started - n$released - n$emailed)
  if (isTRUE(.paperReviewUsage$exhausted)) return(promises::promise_resolve(left(limit)))
  n <- paperReviewSharedKeyCounts()
  if (!paperReviewMailConfigured(config)) return(promises::promise_resolve(left(n$started - n$released)))
  d <- .paperReviewUsage$display
  if (is.list(d) && identical(d$month, paperReviewMonth(now)) &&
      as.numeric(difftime(now, d$at, units = "secs")) < PAPER_REVIEW_COUNT_CACHE_SEC)
    return(promises::promise_resolve(left(d$sent + max(0L, n$emailed - d$emailed) + inFlight(n))))
  promises::then(paperReviewCountThisMonth(config, now), function(sent) {
    if (!is.numeric(sent) || length(sent) != 1 || !is.finite(sent)) stop("the mailbox could not be counted")
    left(sent + inFlight())
  })
}

# Count the usage emails in a mailbox, in the background process:
# self-contained (curl:: only).  Returns the number, or stops.  `since`, an
# IMAP date ("1-Nov-2026"), counts only those from that day on.
paperReviewCountUsageEmails <- function(url, username, password, mailbox, subject, connectTimeout, timeout,
                                        since = NULL)
{
  h <- curl::new_handle()
  curl::handle_setopt(h, username = username, password = password, use_ssl = 3L,
                      customrequest = paste0("SEARCH ", if (!is.null(since)) paste0("SINCE ", since, " "),
                                             "SUBJECT \"", gsub("\"", "", subject, fixed = TRUE), "\""),
                      connecttimeout = connectTimeout, timeout = timeout)
  res <- curl::curl_fetch_memory(paste0(url, utils::URLencode(mailbox, reserved = TRUE)), handle = h)
  text <- rawToChar(res$content)
  line <- regmatches(text, regexpr("\\* SEARCH[ 0-9]*", text))
  if (!length(line)) stop("the mailbox gave no SEARCH response")
  length(regmatches(line, gregexpr("[0-9]+", line))[[1]])
}
