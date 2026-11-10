# Single Paper Review: the Anthropic API call (R/paper-review-api.R), tested
# without the network.

fakePdf <- function() c(charToRaw("%PDF-1.7\n"), as.raw(0:255))

test_that("the manuscript goes inline as base64, never through the Files API or a cache", {
  body <- paperReviewRequestBody(fakePdf(), supplement = fakePdf())
  content <- body$messages[[1]]$content
  expect_equal(content[[1]]$type, "document")
  expect_equal(content[[1]]$source$type, "base64")
  expect_equal(content[[1]]$source$media_type, "application/pdf")
  expect_equal(jsonlite::base64_dec(content[[1]]$source$data), fakePdf())
  expect_false(grepl("\n", content[[1]]$source$data))
  expect_equal(content[[2]]$title, "Supplement")
  expect_equal(content[[3]]$type, "text")
  json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null"))
  expect_false(grepl("file_id|\"type\":\"file\"|cache_control|metadata|user_id", json))
  expect_equal(body$model, "claude-opus-5-5")
  expect_equal(body$thinking$type, "adaptive")
  # The schema rides in the instructions, not as a structured output.
  expect_null(body$output_config$format)
  expect_equal(body$output_config$effort, "high")
  # The calibration's decision (docs/paper-review-calibration.md): high, with
  # the model's 128,000 output tokens, since high used up to 56,348.
  expect_equal(body$max_tokens, 128000)
  expect_match(body$system, "The JSON schema:", fixed = TRUE)
  expect_true(body$stream)
  expect_equal(body$fallbacks, "default")
  expect_null(body$inference_geo)
  expect_equal(paperReviewRequestBody(fakePdf(), inferenceGeo = "us")$inference_geo, "us")
})

test_that("the schema is strict: every object closed and every property required", {
  check <- function(node) {
    if (!is.list(node)) return(invisible())
    if (identical(node$type, "object")) {
      expect_false(node$additionalProperties)
      expect_setequal(unlist(node$required), names(node$properties))
    }
    for (child in node) check(child)
  }
  check(paperReviewSchema())
  # The example extraction has every top-level field the schema requires.
  expect_setequal(names(paperReviewExample()), unlist(paperReviewSchema()$required))
})

test_that("the schema has no union types, which the API caps at 16", {
  # The first live call was refused with 42 nullable parameters.
  unions <- 0
  count <- function(node) {
    if (!is.list(node)) return(invisible())
    # (A property may itself be named "type": only a character vector is a type.)
    if (!is.null(node$anyOf) || (is.character(node$type) && length(node$type) > 1))
      unions <<- unions + 1
    for (child in node) count(child)
  }
  count(paperReviewSchema())
  expect_equal(unions, 0)
})

test_that("an extraction's empty strings and 'none' come back as NULLs and numbers", {
  x <- list(paper = list(title = "", year = "2019", doi = "10.1/x"),
            parameters = list(list(role = "CL", route = "none", value = "1.2",
                                   uncertainty = list(type = "SE", value = "", lower = "0.9"))),
            regimens = list(list(doses = list(list(duration = "not printed", time = 0)))),
            figures = list(list(regimen_id = "", series = list(list(points = list(
              list(time = 1, value = 2, error_lower = "", error_upper = "2.5")))))))
  n <- paperReviewNormalize(x)
  expect_null(n$paper$title)
  expect_equal(n$paper$year, 2019)
  expect_equal(n$paper$doi, "10.1/x")
  expect_null(n$parameters[[1]]$route)
  expect_equal(n$parameters[[1]]$value, 1.2)
  expect_null(n$parameters[[1]]$uncertainty$value)
  expect_equal(n$parameters[[1]]$uncertainty$lower, 0.9)
  expect_null(n$regimens[[1]]$doses[[1]]$duration)
  expect_null(n$figures[[1]]$regimen_id)
  p <- n$figures[[1]]$series[[1]]$points[[1]]
  expect_null(p$error_lower)
  expect_equal(p$error_upper, 2.5)
  # Idempotent on an extraction already in R form.
  ex <- paperReviewExample()
  expect_equal(paperReviewNormalize(ex), paperReviewNormalize(paperReviewNormalize(ex)))
  expect_equal(paperReviewRun(paperReviewNormalize(ex))$consistency,
               paperReviewRun(ex)$consistency)
})

test_that("streamed events are accumulated into the extraction", {
  ev <- function(type, data) list(type = type, data = jsonlite::toJSON(data, auto_unbox = TRUE))
  events <- list(
    ev("message_start", list(type = "message_start",
                             message = list(model = "claude-opus-5-5", usage = list(input_tokens = 1000)))),
    ev("content_block_delta", list(type = "content_block_delta",
                                   delta = list(type = "thinking_delta", thinking = "hmm"))),
    ev("content_block_delta", list(type = "content_block_delta",
                                   delta = list(type = "text_delta", text = "{\"a\": "))),
    ev("content_block_delta", list(type = "content_block_delta",
                                   delta = list(type = "text_delta", text = "1}"))),
    ev("message_delta", list(type = "message_delta", delta = list(stop_reason = "end_turn"),
                             usage = list(output_tokens = 50))),
    list(type = "ping", data = "not json"))
  acc <- paperReviewAccumulateEvents(events)
  expect_equal(acc$text, "{\"a\": 1}")
  expect_equal(acc$stopReason, "end_turn")
  expect_equal(acc$usage$input_tokens, 1000)
  expect_equal(acc$usage$output_tokens, 50)
  parsed <- paperReviewParseResponse(acc)
  # Not a schema section, so dropped; every section is filled in, and the
  # report is told which were missing.
  expect_null(parsed$a)
  expect_setequal(names(parsed), unlist(paperReviewSchema()$required))
  expect_match(parsed$extraction_limitations, "no usable paper")
})

test_that("a reply wrapped in a code fence still parses, and a full one is kept whole", {
  json <- as.character(jsonlite::toJSON(paperReviewExample(), auto_unbox = TRUE,
                                        null = "null", digits = NA))
  acc <- list(text = paste0("Here it is:\n```json\n", json, "\n```"),
              stopReason = "end_turn", error = NULL)
  parsed <- paperReviewParseResponse(acc)
  expect_equal(parsed$structure$compartments, 3)
  expect_equal(parsed$extraction_limitations, paperReviewExample()$extraction_limitations)
  expect_equal(paperReviewRun(parsed)$consistency, paperReviewRun(paperReviewExample())$consistency)
})

test_that("a refusal, a truncation, a stream error and bad JSON are classed errors", {
  base <- list(text = "{}", stopReason = "end_turn", error = NULL)
  expect_error(paperReviewParseResponse(modifyList(base, list(stopReason = "refusal"))),
               class = "paper_review_error")
  expect_error(paperReviewParseResponse(modifyList(base, list(stopReason = "max_tokens"))),
               "incomplete")
  expect_error(paperReviewParseResponse(modifyList(base, list(error = list(type = "overloaded_error")))),
               "overloaded_error")
  expect_error(paperReviewParseResponse(modifyList(base, list(text = "{not json"))),
               class = "paper_review_error")
})

test_that("the key comes from the session, then the environment, then config, and is never echoed", {
  # Sets the two key variables for the block and restores whatever the
  # machine had (a real key, on a developer's machine) without printing it.
  withKeys <- function(own, standard, code) {
    old <- Sys.getenv(PAPER_REVIEW_KEY_ENV, NA, names = TRUE)
    set <- function(name, value)
      if (is.na(value)) Sys.unsetenv(name) else do.call(Sys.setenv, stats::setNames(list(value), name))
    on.exit(for (n in names(old)) set(n, old[[n]]), add = TRUE)
    set(PAPER_REVIEW_KEY_ENV[1], own)
    set(PAPER_REVIEW_KEY_ENV[2], standard)
    force(code)
  }
  withKeys(NA, "env-key", {
    expect_equal(paperReviewApiKey("session-key", list()), "session-key")
    expect_equal(paperReviewApiKey(NULL, list(anthropic_api_key = "config-key")), "env-key")
  })
  withKeys("own-key", "env-key", {
    expect_equal(paperReviewApiKey(NULL, list()), "own-key")
  })
  withKeys(NA, NA, {
    expect_equal(paperReviewApiKey(NULL, list(anthropic_api_key = "config-key")), "config-key")
    expect_null(paperReviewApiKey(NULL, list(anthropic_api_key = "<your key>")))
    expect_null(paperReviewApiKey(NULL, list()))
    expect_error(paperReviewExtract(fakePdf(), key = NULL), class = "paper_review_error")
  })
  expect_error(paperReviewExtract(raw(0), key = "secret-key-123"), "No manuscript")
  e <- tryCatch(paperReviewExtract(raw(0), key = "secret-key-123"), error = function(e) e)
  expect_false(grepl("secret-key-123", conditionMessage(e)))
  big <- c(fakePdf(), raw(PAPER_REVIEW_MAX_PDF_BYTES))
  expect_error(paperReviewExtract(big, key = "k"), "too large")
})

# Devin's stress test (2026-11-08): the schema travels in the prompt, not as an
# enforced output, and 175 of 400 extractions with one to three fields made
# wrong (NULL, NA, wrong types, empty lists, vectors, huge or tiny numbers,
# Unicode) failed in the checks or the report, losing the paid review.  One
# unreadable field must cost that field, said in the limitations.
test_that("an unreadable field of the reply is set aside and named, and the review still renders", {
  ex <- paperReviewExample()
  ex$derived_quantities[[1]]$value <- list(a = 1)         # a list where a number belongs
  ex$derived_quantities[[2]]$quantity <- NA                # NA where a choice belongs
  ex$parameters[[1]]$units <- c("L", "mL")                 # two strings where one belongs
  ex$covariate_effects[[1]]$form <- "zz"                   # a form outside the choices
  ex$regimens[[1]]$doses[[1]]$time <- "soon"               # text that is not a number
  ex$reviewer_notes <- list(list(topic = "x", finding = "y", severity = 123456789, location = "p. 1"))
  v <- paperReviewValidate(ex)
  # [[ ]], not $: a removed field would partially match value_text
  expect_identical(v$derived_quantities[[1]][["value"]], NA_real_)
  expect_equal(v$derived_quantities[[2]]$quantity, "other")
  expect_equal(v$parameters[[1]]$units, "other")
  expect_identical(v$regimens[[1]]$doses[[1]][["time"]], NA_real_)
  expect_match(v$extraction_limitations, "could not be read and were set aside")
  expect_match(v$extraction_limitations, "derived_quantities[1].value", fixed = TRUE)
  review <- paperReviewQuietly(function() paperReviewRun(v))
  expect_no_error(paperReviewReportTags(review, plots = FALSE))
  # A readable value is left alone: a number sent as text is read.
  ok <- paperReviewExample()
  ok$parameters[[1]]$value <- "12.5"
  expect_equal(paperReviewValidate(ok)$parameters[[1]]$value, 12.5)
  expect_false(grepl("set aside", paperReviewValidate(paperReviewExample())$extraction_limitations))
})

test_that("forty fuzzed extractions all give a report", {
  ex <- paperReviewExample()
  paths <- function(x, p = list()) {
    if (!is.list(x) || length(x) == 0) return(list(p))
    c(list(p), unlist(lapply(seq_along(x), function(i) paths(x[[i]], c(p, i))), recursive = FALSE))
  }
  all <- Filter(length, paths(ex))
  junk <- list(NULL, NA, "zz", 123456789, -5, 1e-300, list(), list(a = 1), c(1, 2, 3), TRUE,
               "é中", strrep("x", 2000), list(list()))
  setAt <- function(x, p, v) {
    if (length(p) == 1) { if (is.null(v)) x[p[[1]]] <- list(NULL) else x[[p[[1]]]] <- v; return(x) }
    x[[p[[1]]]] <- setAt(x[[p[[1]]]], p[-1], v)
    x
  }
  withr::local_seed(20261108)
  for (k in 1:40) {
    m <- ex
    for (j in seq_len(sample(1:3, 1)))
      m <- tryCatch(setAt(m, all[[sample(length(all), 1)]], junk[[sample(length(junk), 1)]]),
                    error = function(e) m)
    review <- paperReviewQuietly(function() paperReviewRun(paperReviewValidate(m)))
    expect_no_error(paperReviewReportTags(review, plots = FALSE))
  }
})

# 2026-11-10: the operator's key by default, a user's own key first, and a
# switch that withdraws the operator's (Shafer: users bring their own after
# about 200 uses).
test_that("a user's own key wins, and paper_review_shared_key: false withdraws the operator's", {
  old <- Sys.getenv(PAPER_REVIEW_KEY_ENV, unset = NA)
  on.exit(for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else
    do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)), add = TRUE)
  Sys.unsetenv(PAPER_REVIEW_KEY_ENV)
  config <- list(anthropic_api_key = "operator-key")
  expect_equal(paperReviewApiKey(NULL, config), "operator-key")
  expect_equal(paperReviewApiKey("sk-ant-own", config), "sk-ant-own")
  expect_null(paperReviewApiKey(NULL, c(config, paper_review_shared_key = FALSE)))
  expect_equal(paperReviewApiKey("sk-ant-own", c(config, paper_review_shared_key = FALSE)), "sk-ant-own")
  Sys.setenv(STANPUMPR_ANTHROPIC_API_KEY = "env-key")
  expect_null(paperReviewApiKey(NULL, list(paper_review_shared_key = FALSE)))
})

test_that("only text shaped like an Anthropic key is taken as the user's own", {
  expect_equal(paperReviewOwnKey("  sk-ant-api03-AbC_def-1234567 \n"), "sk-ant-api03-AbC_def-1234567")
  expect_null(paperReviewOwnKey(""))
  expect_null(paperReviewOwnKey(NULL))
  expect_null(paperReviewOwnKey(NA_character_))
  expect_null(paperReviewOwnKey("sk-ant-"))
  expect_null(paperReviewOwnKey("sk-proj-abcdefghijklmnop"))
  expect_null(paperReviewOwnKey("sk-ant-abc def ghijklmnop"))
  expect_null(paperReviewOwnKey(c("sk-ant-abcdefghijklmn", "sk-ant-abcdefghijklmn")))
  expect_null(paperReviewOwnKey(paste0("sk-ant-", strrep("a", PAPER_REVIEW_OWN_KEY_MAX_CHARS))))
})

test_that("what comes back from the API is read the way the call used to read it", {
  expect_error(paperReviewInterpret(list(failure = "connection")), "Could not reach", class = "paper_review_error")
  e <- tryCatch(paperReviewInterpret(list(status = 401L, error = list(type = "authentication_error",
                                                                      message = "invalid x-api-key"))),
                error = function(e) e)
  expect_equal(e$code, "http_401")
  expect_match(conditionMessage(e), "not accepted.*authentication_error.*invalid x-api-key")
  ev <- function(type, data) list(type = type, data = jsonlite::toJSON(data, auto_unbox = TRUE))
  start <- ev("message_start", list(type = "message_start",
                                    message = list(model = "claude-opus-5-5", usage = list(input_tokens = 1000))))
  text <- ev("content_block_delta", list(type = "content_block_delta",
    delta = list(type = "text_delta",
                 text = as.character(jsonlite::toJSON(paperReviewExample(), auto_unbox = TRUE, null = "null")))))
  stop <- function(reason) ev("message_delta", list(type = "message_delta", delta = list(stop_reason = reason),
                                                     usage = list(output_tokens = 50)))
  ok <- paperReviewInterpret(list(status = 200L, events = list(start, text, stop("end_turn"))))
  expect_equal(ok$usage$input_tokens, 1000)
  expect_equal(ok$extraction$drug$name, paperReviewExample()$drug$name)
  # A reply cut at the output limit is billed, so its error carries the tokens.
  cut <- tryCatch(paperReviewInterpret(list(status = 200L, events = list(start, stop("max_tokens")))),
                  error = function(e) e)
  expect_equal(cut$code, "max_tokens")
  expect_equal(cut$usage$output_tokens, 50)
})

test_that("the parts that run in the background call nothing of stanpumpR", {
  # Every function called by a bare name must be base R: the rest is pkg::.
  called <- function(e) {
    if (!is.call(e)) return(character(0))
    f <- e[[1]]
    if (is.call(f) && identical(f[[1]], as.name("::"))) f <- NULL
    c(if (is.name(f)) as.character(f), if (is.call(f)) called(f),
      unlist(lapply(as.list(e)[-1], called)))
  }
  for (fn in list(paperReviewTransport, paperReviewSendUsageEmail, paperReviewCountUsageEmails)) {
    names <- setdiff(unique(called(body(fn))), c(names(formals(fn)), "smtp"))
    expect_true(all(vapply(names, exists, logical(1), envir = baseenv())),
                info = paste(names[!vapply(names, exists, logical(1), envir = baseenv())], collapse = ", "))
  }
})

test_that("a review runs in a background process and its failure comes back as a paper_review_error", {
  skip_on_cran()
  testthat::local_mocked_bindings(paperReviewTransportArgs = function(prepared, key)
    list(json = prepared$json, key = key, fallbacks = FALSE, url = "http://127.0.0.1:9/v1/messages",
         version = PAPER_REVIEW_API_VERSION, beta = PAPER_REVIEW_FALLBACK_BETA, timeout = 5))
  outcome <- NULL
  p <- paperReviewExtractAsync(fakePdf(), key = "secret-key-123")
  promises::then(p, onFulfilled = function(x) outcome <<- x, onRejected = function(e) outcome <<- e)
  deadline <- Sys.time() + 60
  while (is.null(outcome) && Sys.time() < deadline) later::run_now(0.1)
  expect_s3_class(outcome, "paper_review_error")
  expect_equal(outcome$code, "connection")
  expect_false(grepl("secret-key-123", conditionMessage(outcome)))
  # Refused before anything is started: no key, no manuscript.
  refused <- NULL
  promises::then(paperReviewExtractAsync(raw(0), key = "k"), onRejected = function(e) refused <<- e)
  while (is.null(refused) && !later::loop_empty()) later::run_now(0.01)
  expect_equal(refused$code, "no_pdf")
})
