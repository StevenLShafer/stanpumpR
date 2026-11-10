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
# config.yml (git-ignored; on shinyapps.io written by the production deploy
# from a repository secret).  That is the operator's own key, the default
# for every user, and `paper_review_shared_key: false` in config.yml turns it
# off.  A user may instead enter their own key, held only in that session's
# input (paperReviewOwnKey()), which paperReviewApiKey() takes first.  Uses
# of the operator's key are counted (R/paper-review-usage.R).  See
# docs/paper-review.md and docs/paper-review-privacy.md.  (Claude Code,
# 2026-10-30 and 2026-11-10, at the request of Steven L. Shafer.)
#
# The call takes minutes, and one R process serves every session of a
# shinyapps.io worker, so it runs in a background R process
# (paperReviewExtractAsync(), through mirai; the server's ExtendedTask):
# the request is built here, sent and streamed there by
# paperReviewTransport(), which uses nothing of stanpumpR, and read back here
# by paperReviewInterpret().  paperReviewExtract() is the same three steps in
# this process, for the live test and for scripts.

PAPER_REVIEW_API_URL      <- "https://api.anthropic.com/v1/messages"
PAPER_REVIEW_API_VERSION  <- "2026-10-21"
PAPER_REVIEW_MODEL        <- "claude-opus-5-5"
# Effort high and 128,000 output tokens, the model's most: Shafer's decision
# of 2026-11-10 on the calibration (docs/paper-review-calibration.md), where
# high used up to 56,348 tokens on an eight-page paper, 88% of the old
# 64,000. The limit costs nothing unless a review needs it; at about 125
# tokens a second, 128,000 is about 17 minutes, inside the time limit.
PAPER_REVIEW_EFFORT       <- "high"
PAPER_REVIEW_MAX_TOKENS   <- 128000
PAPER_REVIEW_TIMEOUT_SEC  <- 1200
# This app's own key first, so it can sit beside another app's
# ANTHROPIC_API_KEY in the same ~/.Renviron; then the standard name.
PAPER_REVIEW_KEY_ENV      <- c("STANPUMPR_ANTHROPIC_API_KEY", "ANTHROPIC_API_KEY")
# Server-side refusal fallback: on a policy decline the API re-runs the same
# request on another Anthropic model inside the same call.
PAPER_REVIEW_FALLBACK_BETA <- "server-side-fallback-2026-07-01"
# The API's request limit is 32 MB; base64 adds a third.
PAPER_REVIEW_MAX_PDF_BYTES <- 20 * 1024^2
# Shiny refuses an upload over shiny.maxRequestSize (5 MB by default) before
# the app sees it; run_app() and run_paper_review_app() raise it to this, the
# PDF limit plus room for the request's own encoding.
PAPER_REVIEW_MAX_UPLOAD_BYTES <- PAPER_REVIEW_MAX_PDF_BYTES + 1024^2
# The longest key the own-key field accepts (Anthropic's are about 110).
PAPER_REVIEW_OWN_KEY_MAX_CHARS <- 300
# The background process is given the request's own time limit and five
# minutes more (its start, the retries' waits); past that it is abandoned
# and the user told, rather than the session waiting for ever.
PAPER_REVIEW_BACKGROUND_TIMEOUT_SEC <- PAPER_REVIEW_TIMEOUT_SEC + 300

#' The API key for this session
#'
#' A key the user entered for the session (bring-your-own-key) wins; then the
#' operator's: the environment, then config.yml, unless
#' \code{paper_review_shared_key} is \code{false} there.  The key is
#' returned, never printed.
#'
#' @param sessionKey a key held in the session's memory, or NULL
#' @param config the app's configuration list
#' @returns the key, or NULL when none is configured
#' @keywords internal
paperReviewApiKey <- function(sessionKey = NULL, config = .sprglobals$config)
{
  if (is.character(sessionKey) && length(sessionKey) == 1 && nzchar(sessionKey))
    return(sessionKey)
  # The operator's key can be withdrawn without removing it (after the uses
  # Shafer allows it, users bring their own).
  if (is.list(config) && isFALSE(config$paper_review_shared_key)) return(NULL)
  # Nor is it once it has made the reviews it is allowed
  # (paperReviewSharedKeyAdmit(), R/paper-review-usage.R).
  if (paperReviewSharedKeyExhausted()) return(NULL)
  for (name in PAPER_REVIEW_KEY_ENV) {
    key <- Sys.getenv(name, "")
    if (nzchar(key)) return(key)
  }
  key <- if (is.list(config)) config$anthropic_api_key else NULL
  if (is.character(key) && length(key) == 1 && nzchar(key) &&
      !grepl("^<", key)) return(key)
  NULL
}

#' A key the user typed, if it can be one
#'
#' Anthropic API keys begin \code{sk-ant-}.  Anything else (a blank field, a
#' half-pasted key, text with spaces) is not used, so a review is never sent
#' with it.
#'
#' @param x the field's value
#' @returns the trimmed key, or NULL
#' @keywords internal
paperReviewOwnKey <- function(x)
{
  if (!is.character(x) || length(x) != 1 || is.na(x)) return(NULL)
  x <- trimws(x)
  if (nchar(x) > PAPER_REVIEW_OWN_KEY_MAX_CHARS || !grepl("^sk-ant-[A-Za-z0-9_-]{10,}$", x)) return(NULL)
  x
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
                 "as the schema defines. Reply with the JSON object only."))

  # The schema travels in the instructions, not as output_config.format: as a
  # structured output it was refused, first for its 42 nullable fields and
  # then, with those removed, because "the compiled grammar is too large".
  # paperReviewParseResponse() checks the reply instead.
  body <- list(
    model = model,
    max_tokens = PAPER_REVIEW_MAX_TOKENS,
    stream = TRUE,
    system = paste0(paperReviewSystemPrompt(), "\n\nThe JSON schema:\n",
                    jsonlite::toJSON(paperReviewSchema(), auto_unbox = TRUE)),
    thinking = list(type = "adaptive"),
    output_config = list(effort = effort),
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

#' The API's own explanation of a refused request
#'
#' The error body's type and message (which describe the request's shape,
#' not its content), cut to 300 characters.  It goes into the condition the
#' user sees, never into a log.
#'
#' @param error the error body's \code{error} object, a list of \code{type}
#'   and \code{message}, or NULL
#' @returns a string, empty when there is none
#' @keywords internal
paperReviewApiErrorText <- function(error)
{
  if (!is.list(error) || is.null(error$message)) return("")
  substr(paste0("API says (", error$type %or% "error", "): ", error$message %or% ""), 1, 300)
}

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
  # The reply should be the object alone; take it from the first "{" to the
  # last "}" in case it came wrapped in a code fence or a sentence.
  text <- accumulated$text %or% ""
  open <- regexpr("{", text, fixed = TRUE)
  close <- max(gregexpr("}", text, fixed = TRUE)[[1]])
  json <- if (open > 0 && close > open) substr(text, open, close) else ""
  extraction <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE),
                         error = function(e) NULL)
  if (!is.list(extraction) || is.null(names(extraction)))
    stop(paperReviewError("unparseable", "The extraction was not valid JSON."))
  paperReviewValidate(paperReviewNormalize(extraction))
}

#' Give an extraction the shape the checks expect
#'
#' The schema is sent as instructions, not enforced, so a section the reply
#' leaves out is filled with an empty value of the right kind (a list, an
#' empty object, an empty string) and named in \code{extraction_limitations},
#' which the report prints.  Sections the schema does not define are dropped.
#'
#' @param x the normalised extraction
#' @returns the extraction, every top-level section present
#' @keywords internal
paperReviewValidate <- function(x)
{
  schema <- paperReviewSchema()
  missing <- character(0)
  out <- list()
  for (name in unlist(schema$required)) {
    spec <- schema$properties[[name]]
    value <- x[[name]]
    ok <- switch(spec$type,
      array = is.list(value) && is.null(names(value)),
      object = is.list(value) && !is.null(names(value)),
      string = is.character(value) && length(value) == 1,
      TRUE)
    if (!ok) {
      missing <- c(missing, name)
      value <- switch(spec$type, array = list(), object = stats::setNames(list(), character(0)),
                      string = "")
    }
    out[[name]] <- value
  }
  if (length(missing))
    out$extraction_limitations <- paste0(
      out$extraction_limitations,
      if (nzchar(out$extraction_limitations)) " " else "",
      "The reply had no usable ", paste(missing, collapse = ", "),
      "; the review treats them as empty.")
  # Every field below the top, against its schema: the schema travels in the
  # prompt, not as an enforced output, so a field can arrive as anything.  One
  # unreadable field (a list where a number belongs, NA, a vector) cost the
  # whole paid review: 175 of 400 fuzzed extractions failed in the checks or
  # the report (Devin's stress test, 2026-11-08).  Now it costs that field,
  # which is set aside and named here.
  notes <- new.env()
  notes$paths <- character(0)
  for (name in unlist(schema$required))
    if (!is.null(out[[name]]))
      out[name] <- list(paperReviewConform(out[[name]], schema$properties[[name]], name, name, notes))
  if (length(notes$paths))
    out$extraction_limitations <- paste0(
      out$extraction_limitations,
      if (nzchar(out$extraction_limitations)) " " else "",
      "Some fields of the reply could not be read and were set aside: ",
      paste(utils::head(notes$paths, 8), collapse = "; "),
      if (length(notes$paths) > 8) paste0("; and ", length(notes$paths) - 8, " more") else "", ".")
  # The schema's closed choices are instructions too: a value outside them
  # (an unknown parameter role) is passed over by the checks without a word,
  # so each one is named here, which the report prints.
  stray <- paperReviewStrayChoices(out, schema)
  if (length(stray))
    out$extraction_limitations <- paste0(
      out$extraction_limitations,
      if (nzchar(out$extraction_limitations)) " " else "",
      "Some values are not among their field's choices and may be ignored: ",
      paste(utils::head(stray, 8), collapse = "; "),
      if (length(stray) > 8) paste0("; and ", length(stray) - 8, " more") else "", ".")
  out
}

#' Make one part of an extraction the shape its schema says
#'
#' A readable value is kept, or coerced where the reading is unambiguous (a
#' number sent as text, one string sent as a number); an unreadable one is set
#' aside (\code{NA} for a number, an empty string for a required string, the
#' field's "other" for a required choice, \code{NULL} for an optional field
#' or an object) and its path recorded in \code{notes}.  A number is kept in
#' place as \code{NA} rather than removed: removed, \code{d$time} would
#' partially match \code{d$time_units} and \code{dq$value} \code{dq$value_text},
#' and a check would multiply \code{"min"} by a number.  An array item that
#' cannot be read is dropped.  Fields the schema does not name are left as
#' they are: no check reads them.
#'
#' @param value part of the extraction (after \code{paperReviewNormalize()})
#' @param spec its schema
#' @param name its field name (decides a numeric or optional field)
#' @param path where it is, for the note
#' @param notes an environment whose \code{paths} collects what was set aside
#' @returns the value, conformed
#' @keywords internal
paperReviewConform <- function(value, spec, name, path, notes)
{
  if (is.null(value) || !is.list(spec)) return(value)
  setAside <- function(replacement = NULL) {
    notes$paths <- c(notes$paths, path)
    replacement
  }
  scalar <- function(v) length(v) == 1 && !is.list(v) && !is.na(v)
  choices <- if (!is.null(spec$enum)) unlist(spec$enum) else attr(spec, "choices")
  kind <- if (!is.null(choices)) "choice"
    else if (spec$type %in% c("object", "array")) spec$type
    else if (name %in% PAPER_REVIEW_NUMERIC_FIELDS || identical(spec$type, "number")) "number"
    else spec$type %or% "other"
  switch(kind,
    number = {
      if (is.numeric(value) && scalar(value) && is.finite(value)) return(value)
      if (is.character(value) && scalar(value)) {
        n <- suppressWarnings(as.numeric(trimws(value)))
        if (is.finite(n)) return(n)
      }
      setAside(NA_real_)
    },
    choice = {
      if (is.character(value) && scalar(value)) return(value)
      if ((is.numeric(value) || is.logical(value)) && scalar(value)) return(as.character(value))
      fallback <- intersect(c("other", "unstated", "none"), choices)
      setAside(if (name %in% PAPER_REVIEW_OPTIONAL_ENUMS || !length(fallback)) NULL else fallback[1])
    },
    string = {
      if (is.character(value) && scalar(value)) return(value)
      if ((is.numeric(value) || is.logical(value)) && scalar(value)) return(as.character(value))
      setAside(if (name %in% PAPER_REVIEW_OPTIONAL_STRINGS) NULL else "")
    },
    object = {
      if (is.list(value) && length(value) == 0) return(stats::setNames(list(), character(0)))
      if (!is.list(value) || is.null(names(value)) || any(!nzchar(names(value)))) return(setAside())
      for (field in names(spec$properties)) {
        if (is.null(value[[field]])) next
        conformed <- paperReviewConform(value[[field]], spec$properties[[field]], field,
                                        paste0(path, ".", field), notes)
        if (is.null(conformed)) value[[field]] <- NULL else value[[field]] <- conformed
      }
      value
    },
    array = {
      items <- if (is.list(value) && is.null(names(value))) value
        # One object sent where a list of them belongs: the one item.
        else if (is.list(value) && identical(spec$items$type, "object")) list(value)
        else if (!is.list(value) && !identical(spec$items$type, "object")) as.list(value)
        else return(setAside(list()))
      out <- list()
      for (i in seq_along(items)) {
        conformed <- paperReviewConform(items[[i]], spec$items, name, paste0(path, "[", i, "]"), notes)
        if (is.null(conformed)) {
          if (!is.null(items[[i]]) && !(paste0(path, "[", i, "]") %in% notes$paths))
            notes$paths <- c(notes$paths, paste0(path, "[", i, "]"))
        } else out[[length(out) + 1]] <- conformed
      }
      out
    },
    value)
}

#' The values of an extraction outside their field's closed choices
#'
#' @param value part of the extraction
#' @param spec the schema of that part
#' @param path where it is, for the message
#' @returns one entry, the path and the value, per value outside its field's
#'   enum
#' @keywords internal
paperReviewStrayChoices <- function(value, spec, path = "")
{
  if (is.null(value) || !is.list(spec)) return(character(0))
  choices <- if (!is.null(spec$enum)) unlist(spec$enum) else attr(spec, "choices")
  if (!is.null(choices)) {
    ok <- !is.character(value) || length(value) != 1 || value %in% choices
    return(if (ok) character(0) else paste0(path, " (\"", value, "\")"))
  }
  if (identical(spec$type, "object") && is.list(value) && is.list(spec$properties))
    return(unlist(lapply(names(spec$properties), function(name)
      paperReviewStrayChoices(value[[name]], spec$properties[[name]],
                              if (nzchar(path)) paste0(path, ".", name) else name)), use.names = FALSE))
  if (identical(spec$type, "array") && is.list(value) && is.null(names(value)))
    return(unlist(lapply(seq_along(value), function(i)
      paperReviewStrayChoices(value[[i]], spec$items, paste0(path, "[", i, "]"))), use.names = FALSE))
  character(0)
}

#' Check a manuscript and build its request
#'
#' Everything that can be refused without the network is refused here, in
#' the app's process, before anything is sent.
#'
#' @param pdf,supplement raw bytes (supplement optional)
#' @param key the API key
#' @param ... passed to \code{paperReviewRequestBody()}
#' @returns a list: \code{json} (the request body) and \code{fallbacks}
#'   (whether the refusal fallback was requested)
#' @keywords internal
paperReviewPrepare <- function(pdf, supplement = NULL, key, ...)
{
  if (is.null(key)) stop(paperReviewError("no_key", "No Anthropic API key is configured."))
  if (!is.raw(pdf) || length(pdf) == 0)
    stop(paperReviewError("no_pdf", "No manuscript was uploaded."))
  if (length(pdf) + length(supplement) > PAPER_REVIEW_MAX_PDF_BYTES)
    stop(paperReviewError("too_large", "The files are too large to send (20 MB together)."))
  body <- paperReviewRequestBody(pdf, supplement, ...)
  list(json = as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA)),
       fallbacks = !is.null(body$fallbacks))
}

#' Send a prepared request and collect the streamed reply
#'
#' Runs in a background R process (paperReviewExtractAsync()), so it refers
#' to nothing of stanpumpR: every constant is an argument and every call is
#' \code{httr2::} or \code{jsonlite::}.  It returns what came back, raw:
#' the HTTP status, the API's error object for a refused request, or the
#' server-sent events; paperReviewInterpret() reads them.  It writes nothing
#' anywhere, and the key is redacted from the request.
#'
#' @param json the request body
#' @param key the API key
#' @param fallbacks request the server-side refusal fallback?
#' @param url,version,beta,timeout the endpoint, API version, fallback beta
#'   header and time limit (seconds)
#' @returns a list: \code{failure} (NULL, \code{"no_httr2"} or
#'   \code{"connection"}), \code{status}, \code{error}, \code{events}
#' @keywords internal
paperReviewTransport <- function(json, key, fallbacks = TRUE,
                                 url = "https://api.anthropic.com/v1/messages",
                                 version, beta, timeout)
{
  if (!requireNamespace("httr2", quietly = TRUE)) return(list(failure = "no_httr2"))
  req <- httr2::request(url)
  req <- httr2::req_headers(req,
    `anthropic-version` = version,
    `content-type` = "application/json",
    .redact = "x-api-key")
  req <- httr2::req_headers(req, `x-api-key` = key, .redact = "x-api-key")
  if (isTRUE(fallbacks)) req <- httr2::req_headers(req, `anthropic-beta` = beta)
  req <- httr2::req_body_raw(req, json, type = "application/json")
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_retry(req, max_tries = 3,
                          is_transient = function(resp)
                            httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529))
  rm(json)
  resp <- tryCatch(httr2::req_perform_connection(req), error = function(e) NULL)
  if (is.null(resp)) return(list(failure = "connection"))
  on.exit(close(resp), add = TRUE)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    body <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
                     error = function(e) NULL)
    error <- if (is.list(body) && is.list(body$error)) body$error[intersect(c("type", "message"), names(body$error))]
    return(list(status = status, error = error))
  }
  events <- list()
  while (!httr2::resp_stream_is_complete(resp)) {
    event <- httr2::resp_stream_sse(resp)
    if (is.null(event)) next
    events[[length(events) + 1]] <- event
  }
  list(status = status, events = events)
}

#' Read what paperReviewTransport() brought back
#'
#' @param result its return value
#' @returns a list: \code{extraction}, \code{usage}, \code{model}; or an
#'   error of class \code{paper_review_error}, which carries the tokens the
#'   reply used in \code{$usage} when the API answered (a reply cut at the
#'   output limit is billed too)
#' @keywords internal
paperReviewInterpret <- function(result)
{
  failure <- result$failure
  if (identical(failure, "no_httr2"))
    stop(paperReviewError("no_httr2", "The httr2 package is needed to call the Anthropic API."))
  if (identical(failure, "connection"))
    stop(paperReviewError("connection", "Could not reach the Anthropic API."))
  status <- result$status
  if (!is.numeric(status) || length(status) != 1)
    stop(paperReviewError("unexpected", "The review failed unexpectedly."))
  if (status >= 400)
    stop(paperReviewError(paste0("http_", status), paste(switch(
      as.character(status),
      "400" = "The API rejected the request (400).",
      "401" = "The API key was not accepted (401).",
      "403" = "The API key may not use this model (403).",
      "413" = "The request was too large for the API (413).",
      "429" = "The API's rate limit was reached (429). Try again in a minute.",
      "529" = "The API is overloaded (529). Try again shortly.",
      paste0("The API returned an error (", status, ").")),
      paperReviewApiErrorText(result$error))))
  accumulated <- paperReviewAccumulateEvents(result$events %or% list())
  extraction <- tryCatch(paperReviewParseResponse(accumulated),
                         paper_review_error = function(e) {
                           e$usage <- accumulated$usage
                           stop(e)
                         })
  list(extraction = extraction, usage = accumulated$usage, model = accumulated$model)
}

# The arguments paperReviewTransport() takes from this package's constants.
paperReviewTransportArgs <- function(prepared, key)
  list(json = prepared$json, key = key, fallbacks = prepared$fallbacks,
       url = PAPER_REVIEW_API_URL, version = PAPER_REVIEW_API_VERSION,
       beta = PAPER_REVIEW_FALLBACK_BETA, timeout = PAPER_REVIEW_TIMEOUT_SEC)

#' Send one manuscript to the Anthropic API and return the extraction
#'
#' In this R process, which waits for it: for scripts and the live test.  The
#' app uses paperReviewExtractAsync().
#'
#' @param pdf,supplement raw bytes (supplement optional)
#' @param key the API key
#' @param ... passed to \code{paperReviewRequestBody()}
#' @returns a list: \code{extraction}, \code{usage}, \code{model}
#' @keywords internal
paperReviewExtract <- function(pdf, supplement = NULL, key, ...)
{
  if (!requireNamespace("httr2", quietly = TRUE))
    stop(paperReviewError("no_httr2", "The httr2 package is needed to call the Anthropic API."))
  prepared <- paperReviewPrepare(pdf, supplement, key = key, ...)
  paperReviewInterpret(do.call(paperReviewTransport, paperReviewTransportArgs(prepared, key)))
}

#' Run a function in a background R process
#'
#' A mirai, one short-lived R process per call (no daemons are set), so a
#' minutes-long API call or a mail send never holds the R process every
#' session of a worker shares.  \code{fn} is sent with the global
#' environment as its own, so it must be self-contained.  The process is
#' started with this one's library paths, which it would not otherwise find
#' where they are set only inside R (renv, a host's own library), and then
#' waits for ever for a package it cannot load; \code{timeout} bounds it.
#'
#' @param fn a self-contained function
#' @param args its arguments, a list
#' @param timeout seconds
#' @returns the mirai, which is also a promise
#' @keywords internal
paperReviewInBackground <- function(fn, args, timeout)
{
  environment(fn) <- globalenv()
  old <- Sys.getenv("R_LIBS", unset = NA)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(old)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old), add = TRUE)
  mirai::mirai(do.call(fn, args), .args = list(fn = fn, args = args), .timeout = timeout * 1000)
}

#' Send one manuscript to the Anthropic API without waiting for it
#'
#' The request is built and checked here, sent from a background R process,
#' and read here when it returns.
#'
#' @inheritParams paperReviewExtract
#' @returns a promise of what paperReviewExtract() returns; it is rejected
#'   with a \code{paper_review_error}
#' @keywords internal
paperReviewExtractAsync <- function(pdf, supplement = NULL, key, ...)
{
  prepared <- tryCatch(paperReviewPrepare(pdf, supplement, key = key, ...),
                       paper_review_error = function(e) e)
  if (inherits(prepared, "paper_review_error")) return(promises::promise_reject(prepared))
  job <- paperReviewInBackground(paperReviewTransport, paperReviewTransportArgs(prepared, key),
                                 PAPER_REVIEW_BACKGROUND_TIMEOUT_SEC)
  rm(prepared)
  promises::then(job,
    onFulfilled = function(result) {
      # A failure inside the process comes back as a value, not an error.
      if (inherits(result, "miraiError") || inherits(result, "errorValue"))
        stop(paperReviewError("background", "The review's background process failed. Try again."))
      paperReviewInterpret(result)
    },
    onRejected = function(e)
      stop(paperReviewError("background",
                            "The review did not finish (it timed out, or its background process failed). Try again.")))
}
