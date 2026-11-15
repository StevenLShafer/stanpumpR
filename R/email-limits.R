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

# The mailbox an address reaches, for the per-recipient count: a "+tag"
# after the local part is dropped, and Gmail's dots, since name+1@ and
# n.a.m.e@ reach the one inbox, so 30 tags sent one person six times their
# limit (Carbon's third UI audit, 2026-11-15, M5).  An address that drops
# neither for its own server is only counted more strictly, the safe side
# for a limit.
emailAddressKey <- function(recipient)
{
  address <- tolower(trimws(as.character(recipient)))
  at <- regexpr("@[^@]*$", address)
  local <- ifelse(at > 0, substr(address, 1, at - 1), address)
  domain <- ifelse(at > 0, substr(address, at + 1, nchar(address)), "")
  local <- sub("\\+.*$", "", local)
  gmail <- domain %in% c("gmail.com", "googlemail.com")
  local[gmail] <- gsub(".", "", local[gmail], fixed = TRUE)
  domain[gmail] <- "gmail.com"
  ifelse(at > 0, paste0(local, "@", domain), local)
}

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

#' Give back the send last counted for this address, when it failed before
#' reaching the mail server
#'
#' app_server() counts a send (recordEmailSend()) before sendSlide() runs,
#' and sendSlide() can fail before it connects: no mail server configured,
#' or a slide that could not be made (a genotype field holding a vector on a
#' run of inhaled agents alone, which no model read).  Such a send cost the
#' server no connection, the reason a failed attempt is counted, and it used
#' a slot of the hour's limit for nothing (Carbon's third UI audit,
#' 2026-11-15, M5).  R serves a worker's sessions on one thread, and nothing
#' yields between the count and the send, so the last send counted for the
#' address is this one.
#' @noRd
forgetEmailSend <- function(recipient, now = Sys.time())
{
  sends <- emailRecentSends(now)
  mine <- which(sends$to == emailAddressKey(recipient))
  if (length(mine)) .emailSends$log <- sends[-max(mine), , drop = FALSE]
  invisible(NULL)
}
