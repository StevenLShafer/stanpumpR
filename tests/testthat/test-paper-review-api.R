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
  expect_equal(body$output_config$format$type, "json_schema")
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
  expect_equal(paperReviewParseResponse(acc), list(a = 1L))
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
  withr_env <- function(value, code) {
    old <- Sys.getenv(PAPER_REVIEW_KEY_ENV, NA)
    if (is.na(value)) Sys.unsetenv(PAPER_REVIEW_KEY_ENV) else Sys.setenv(ANTHROPIC_API_KEY = value)
    on.exit(if (is.na(old)) Sys.unsetenv(PAPER_REVIEW_KEY_ENV) else Sys.setenv(ANTHROPIC_API_KEY = old))
    force(code)
  }
  withr_env("env-key", {
    expect_equal(paperReviewApiKey("session-key", list()), "session-key")
    expect_equal(paperReviewApiKey(NULL, list(anthropic_api_key = "config-key")), "env-key")
  })
  withr_env(NA, {
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
