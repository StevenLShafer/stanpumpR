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
