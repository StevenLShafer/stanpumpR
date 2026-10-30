# Single Paper Review: the one call to the Anthropic Messages API.
#
# The manuscript goes inline, base64, in the request body.  The Files API is
# deliberately not used: it stores the file on Anthropic's side until it is
# deleted.  No prompt caching (cache_control) is requested, no user metadata
# is sent, and nothing about the request or the response is written to the
# console, a log or the disk: the only things returned are the parsed
# extraction and the token counts.  R has no official Anthropic SDK, so this
# is raw HTTP through httr2, streamed (server-sent events) because a long
# manuscript with adaptive thinking can run for minutes.
#
# The key: PAPER_REVIEW_KEY_ENV in the environment, or `anthropic_api_key` in
# config.yml (git-ignored).  paperReviewApiKey() takes a session key first, the
# seam for a later bring-your-own-key option in which a user's key is held
# only in that session's memory.  See docs/paper-review.md and
# docs/paper-review-privacy.md.  (Claude Code, 2026-10-30, at the request of
# Steven L. Shafer.)

PAPER_REVIEW_API_URL      <- "https://api.anthropic.com/v1/messages"
PAPER_REVIEW_API_VERSION  <- "2026-10-21"
PAPER_REVIEW_MODEL        <- "claude-opus-5-5"
PAPER_REVIEW_EFFORT       <- "high"
PAPER_REVIEW_MAX_TOKENS   <- 64000
PAPER_REVIEW_TIMEOUT_SEC  <- 1200
PAPER_REVIEW_KEY_ENV      <- "ANTHROPIC_API_KEY"
# Server-side refusal fallback: on a policy decline the API re-runs the same
# request on another Anthropic model inside the same call.
PAPER_REVIEW_FALLBACK_BETA <- "server-side-fallback-2026-07-01"
# The API's request limit is 32 MB; base64 adds a third.
PAPER_REVIEW_MAX_PDF_BYTES <- 20 * 1024^2

#' The API key for this session
#'
#' A key supplied for the session (bring-your-own-key) wins; then the
#' environment; then config.yml.  The key is returned, never printed.
#'
#' @param sessionKey a key held in the session's memory, or NULL
#' @param config the app's configuration list
#' @returns the key, or NULL when none is configured
#' @keywords internal
paperReviewApiKey <- function(sessionKey = NULL, config = .sprglobals$config)
{
  if (is.character(sessionKey) && length(sessionKey) == 1 && nzchar(sessionKey))
    return(sessionKey)
  key <- Sys.getenv(PAPER_REVIEW_KEY_ENV, "")
  if (nzchar(key)) return(key)
  key <- if (is.list(config)) config$anthropic_api_key else NULL
  if (is.character(key) && length(key) == 1 && nzchar(key) &&
      !grepl("^<", key)) return(key)
  NULL
}

#' The request body for one manuscript
#'
#' @param pdf the manuscript, raw bytes
#' @param supplement an optional supplement, raw bytes, or NULL
#' @param model,effort the model and its effort level
#' @param inferenceGeo NULL for the API's default, or "us"
#' @param fallbacks request the server-side refusal fallback?
#' @returns a list that serialises to the JSON body
#' @keywords internal
paperReviewRequestBody <- function(pdf, supplement = NULL,
                                   model = PAPER_REVIEW_MODEL,
                                   effort = PAPER_REVIEW_EFFORT,
                                   inferenceGeo = NULL,
                                   fallbacks = TRUE)
{
  document <- function(bytes, title)
    list(type = "document",
         title = title,
         source = list(type = "base64", media_type = "application/pdf",
                       # The API refuses line breaks, which base64_enc() inserts.
                       data = gsub("[\r\n]", "", jsonlite::base64_enc(bytes))))
  content <- list(document(pdf, "Manuscript"))
  if (!is.null(supplement) && length(supplement) > 0)
    content[[2]] <- document(supplement, "Supplement")
  content[[length(content) + 1]] <- list(
    type = "text",
    text = paste("Extract the pharmacokinetic model from this manuscript",
                 if (length(content) > 1) "and its supplement" else "",
                 "as the schema defines."))

  body <- list(
    model = model,
    max_tokens = PAPER_REVIEW_MAX_TOKENS,
    stream = TRUE,
    system = paperReviewSystemPrompt(),
    thinking = list(type = "adaptive"),
    output_config = list(
      effort = effort,
      format = list(type = "json_schema", schema = paperReviewSchema())
    ),
    messages = list(list(role = "user", content = content))
  )
  if (isTRUE(fallbacks)) body$fallbacks <- "default"
  if (!is.null(inferenceGeo)) body$inference_geo <- inferenceGeo
  body
}

#' Accumulate a streamed response
#'
#' Pure: takes the parsed server-sent events in order, so the parsing is
#' tested without the network.
#'
#' @param events a list of lists, each with \code{type} and \code{data} (the
#'   event's JSON text)
#' @returns a list: \code{text} (the concatenated text output),
#'   \code{stopReason}, \code{usage}, \code{model} and \code{error} (NULL or
#'   a list of \code{type} and \code{message})
#' @keywords internal
paperReviewAccumulateEvents <- function(events)
{
  text <- character(0)
  out <- list(stopReason = NA_character_, usage = list(), model = NA_character_,
              error = NULL)
  for (event in events)
  {
    data <- tryCatch(jsonlite::fromJSON(event$data, simplifyVector = FALSE),
                     error = function(e) NULL)
    if (is.null(data)) next
    type <- if (!is.null(data$type)) data$type else event$type
    if (identical(type, "message_start")) {
      out$model <- data$message$model
      out$usage <- utils::modifyList(out$usage, as.list(data$message$usage))
    } else if (identical(type, "content_block_delta") &&
               identical(data$delta$type, "text_delta")) {
      text[length(text) + 1] <- data$delta$text
    } else if (identical(type, "message_delta")) {
      if (!is.null(data$delta$stop_reason)) out$stopReason <- data$delta$stop_reason
      if (!is.null(data$usage)) out$usage <- utils::modifyList(out$usage, as.list(data$usage))
    } else if (identical(type, "error")) {
      out$error <- list(type = data$error$type, message = data$error$message)
    }
  }
  out$text <- paste(text, collapse = "")
  out
}

# A classed condition whose message is safe to show the user and carries
# nothing from the manuscript.
paperReviewError <- function(code, message)
  structure(class = c("paper_review_error", "error", "condition"),
            list(message = message, call = NULL, code = code))

#' Parse the extraction out of an accumulated response
#'
#' @param accumulated the output of \code{paperReviewAccumulateEvents()}
#' @returns the extraction, a list
#' @keywords internal
paperReviewParseResponse <- function(accumulated)
{
  if (!is.null(accumulated$error))
    stop(paperReviewError("api_stream_error",
                          paste0("The API reported an error during the review (",
                                 accumulated$error$type, ").")))
  if (identical(accumulated$stopReason, "refusal"))
    stop(paperReviewError("refusal",
                          "The model declined to review this manuscript."))
  if (identical(accumulated$stopReason, "max_tokens"))
    stop(paperReviewError("max_tokens",
                          "The extraction ran past the output limit and is incomplete."))
  extraction <- tryCatch(jsonlite::fromJSON(accumulated$text, simplifyVector = FALSE),
                         error = function(e) NULL)
  if (!is.list(extraction))
    stop(paperReviewError("unparseable", "The extraction was not valid JSON."))
  extraction
}

#' Send one manuscript to the Anthropic API and return the extraction
#'
#' @param pdf,supplement raw bytes (supplement optional)
#' @param key the API key
#' @param progress optional function(text) called as the stream advances,
#'   with counts only
#' @param ... passed to \code{paperReviewRequestBody()}
#' @returns a list: \code{extraction}, \code{usage}, \code{model}
#' @keywords internal
paperReviewExtract <- function(pdf, supplement = NULL, key, progress = NULL, ...)
{
  if (!requireNamespace("httr2", quietly = TRUE))
    stop(paperReviewError("no_httr2", "The httr2 package is needed to call the Anthropic API."))
  if (is.null(key)) stop(paperReviewError("no_key", "No Anthropic API key is configured."))
  if (!is.raw(pdf) || length(pdf) == 0)
    stop(paperReviewError("no_pdf", "No manuscript was uploaded."))
  if (length(pdf) + length(supplement) > PAPER_REVIEW_MAX_PDF_BYTES)
    stop(paperReviewError("too_large", "The files are too large to send (20 MB together)."))

  body <- paperReviewRequestBody(pdf, supplement, ...)
  req <- httr2::request(PAPER_REVIEW_API_URL)
  req <- httr2::req_headers(req,
    `anthropic-version` = PAPER_REVIEW_API_VERSION,
    `content-type` = "application/json",
    .redact = "x-api-key")
  req <- httr2::req_headers(req, `x-api-key` = key, .redact = "x-api-key")
  if (!is.null(body$fallbacks))
    req <- httr2::req_headers(req, `anthropic-beta` = PAPER_REVIEW_FALLBACK_BETA)
  req <- httr2::req_body_raw(req, jsonlite::toJSON(body, auto_unbox = TRUE,
                                                   null = "null", digits = NA),
                             type = "application/json")
  req <- httr2::req_timeout(req, PAPER_REVIEW_TIMEOUT_SEC)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_retry(req, max_tries = 3,
                          is_transient = function(resp)
                            httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529))
  rm(body)

  resp <- tryCatch(httr2::req_perform_connection(req),
                   error = function(e) stop(paperReviewError(
                     "connection", "Could not reach the Anthropic API.")))
  on.exit(close(resp), add = TRUE)
  status <- httr2::resp_status(resp)
  if (status >= 400)
    stop(paperReviewError(paste0("http_", status), switch(
      as.character(status),
      "400" = "The API rejected the request (400). The PDF may be too long or unreadable.",
      "401" = "The API key was not accepted (401).",
      "403" = "The API key may not use this model (403).",
      "413" = "The request was too large for the API (413).",
      "429" = "The API's rate limit was reached (429). Try again in a minute.",
      "529" = "The API is overloaded (529). Try again shortly.",
      paste0("The API returned an error (", status, ")."))))

  events <- list()
  while (!httr2::resp_stream_is_complete(resp)) {
    event <- httr2::resp_stream_sse(resp)
    if (is.null(event)) next
    events[[length(events) + 1]] <- event
    if (is.function(progress) && length(events) %% 200 == 0)
      progress(paste(length(events), "events received"))
  }
  accumulated <- paperReviewAccumulateEvents(events)
  rm(events)
  list(extraction = paperReviewParseResponse(accumulated),
       usage = accumulated$usage,
       model = accumulated$model)
}
