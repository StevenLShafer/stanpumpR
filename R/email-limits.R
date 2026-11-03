# How much mail one worker of the app may send (the external security audit
# of 2026-11-02).  Email Slide sends a branded message, with a link and three
# attachments, to any address the user types; EMAIL_SESSION_LIMIT bounds one
# session, and reloading the page starts another.  These bound the worker
# process: every session it serves shares the count, which lives in memory
# only, is never logged, and forgets each send after an hour.  A host that
# runs several workers multiplies the bound by their number; the mail
# provider's own quota stays the last line.
EMAIL_WORKER_LIMIT_PER_HOUR <- 30L
EMAIL_RECIPIENT_LIMIT_PER_HOUR <- 5L
EMAIL_LIMIT_WINDOW_SECONDS <- 3600

.emailSends <- new.env(parent = emptyenv())

# The sends of the last hour: a data frame of time and recipient
emailRecentSends <- function(now = Sys.time())
{
  sends <- .emailSends$log
  if (is.null(sends)) sends <- data.frame(time = as.POSIXct(character(0)), to = character(0))
  sends <- sends[as.numeric(difftime(now, sends$time, units = "secs")) < EMAIL_LIMIT_WINDOW_SECONDS, ,
                 drop = FALSE]
  .emailSends$log <- sends
  sends
}

emailAddressKey <- function(recipient) tolower(trimws(as.character(recipient)))

#' Why this send may not go now, or NULL
#' @noRd
emailRateProblem <- function(recipient, now = Sys.time())
{
  sends <- emailRecentSends(now)
  if (nrow(sends) >= EMAIL_WORKER_LIMIT_PER_HOUR) {
    return("The app has sent as much mail as it may this hour. Please try again later.")
  }
  if (sum(sends$to == emailAddressKey(recipient)) >= EMAIL_RECIPIENT_LIMIT_PER_HOUR) {
    return("That address has been sent as many slides as it may this hour. Please try again later.")
  }
  NULL
}

#' Count a send against the limits (an attempt, whether or not it succeeds:
#' each one costs the mail server a connection)
#' @noRd
recordEmailSend <- function(recipient, now = Sys.time())
{
  sends <- emailRecentSends(now)
  .emailSends$log <- rbind(sends, data.frame(time = now, to = emailAddressKey(recipient)))
  invisible(NULL)
}
