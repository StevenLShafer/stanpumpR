# The TCI API's HTTP layer (tci-api-server.R): docs/tci-api.md, section 8.3.
# Drafted by Claude Code, 2026-10-12, at the request of Steven L. Shafer; run
# on R 4.3.3 with plumber 1.2.1.
#
# Entirely in process: plumber's pr$call() answers a Rook request environment
# built by rookRequest() (helpers.R), so the router is exercised end to end --
# filters, body parser, handler, serializer, 404 and error handlers -- without
# a port, a subprocess or the network.  The computation itself is tested in
# test-tci-api.R; here each route is checked against the core function it
# wraps, and every status of section 6.3 against the error shape.

skip_if_not_installed("plumber")

local_mocked_bindings(outputComments = function(...) {})

router <- tciApiRouter()

# ---------------------------------------------------------------------------
# Helpers

# Call the router and parse the answer.
call <- function(method, path, body = NULL, query = "", contentType = "application/json")
{
  r <- router$call(rookRequest(method, path, body, query, contentType = contentType))
  r$text <- as.character(r$body)
  r$json <- jsonlite::fromJSON(r$text, simplifyVector = FALSE)
  r
}

# What a core result looks like after the serializer and a client's parser.
viaJson <- function(x)
  jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null"),
                     simplifyVector = FALSE)

nowStamp <- function(offsetSeconds = 0) tciApiFormatTime(as.numeric(Sys.time()) + offsetSeconds)

# A well-formed request at the server's own time, so that CLOCK_SKEW never
# fires by accident: propofol to 3 mcg/mL by default.
rateBody <- function(drug = "propofol", target = 3, units = "mcg/mL", site = "effect",
                     rateUnits = "mg/min", maxRate = 1e5, latency = 1.5, interval = 10,
                     history = list(), requestId = "ch1-000412", now = nowStamp())
{
  list(
    requestId = requestId, drug = drug, now = now,
    patient = list(weight = 70, height = 170, age = 50, sex = "male"),
    pump = list(rateUnits = rateUnits, maxRate = maxRate),
    target = list(site = site, value = target, units = units),
    timing = list(latency = latency, interval = interval),
    history = history
  )
}

expectJson <- function(r, status = 200L)
{
  expect_equal(r$status, status)
  expect_equal(r$headers[["Content-Type"]], "application/json")
  invisible(r)
}

# The error shape of section 6.3: one error with the code and pointer, the
# request id when the body carried one, the server block without
# computeSeconds.
expectError <- function(r, status, code, pointer = NULL, requestId = NULL)
{
  expectJson(r, status)
  b <- r$json
  expect_equal(names(b), c("apiVersion", "requestId", "errors", "server"), info = code)
  expect_equal(b$apiVersion, "1.0")
  expect_equal(b$requestId, requestId, info = code)
  expect_length(b$errors, 1)
  e <- b$errors[[1]]
  expect_equal(names(e), c("code", "message", "pointer"), info = code)
  expect_equal(e$code, code)
  expect_true(is.character(e$message) && nzchar(e$message), info = code)
  expect_equal(e$pointer, pointer, info = code)
  expect_equal(names(b$server), c("package", "version", "commit", "receivedAt", "respondedAt"), info = code)
  expect_equal(b$server$package, "stanpumpR")
  invisible(b)
}

timeRegex <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{3}Z$"


# ---------------------------------------------------------------------------
# The GET routes

test_that("GET /tci/v1/health is the liveness body", {
  r <- expectJson(call("GET", "/tci/v1/health"))
  b <- r$json
  expect_equal(names(b), c("apiVersion", "status", "package", "version", "commit", "serverTime", "tciDrugs"))
  expect_equal(b$apiVersion, "1.0")
  expect_equal(b$status, "ok")
  expect_match(b$serverTime, timeRegex)
  expect_equal(b$tciDrugs, length(tciApiDrugNames()))
  expect_match(r$text, '"commit":null')          # not installed from GitHub here
  h <- viaJson(tciApiHealth())
  b$serverTime <- NULL
  h$serverTime <- NULL
  expect_equal(b, h)
})

test_that("GET /tci/v1/drugs lists propofol with its units, the unit tokens and the limits", {
  r <- expectJson(call("GET", "/tci/v1/drugs"))
  b <- r$json
  expect_equal(b$apiVersion, "1.0")
  names(b$drugs) <- vapply(b$drugs, `[[`, "", "drug")
  expect_true("propofol" %in% names(b$drugs))
  p <- b$drugs$propofol
  expect_equal(p$concentrationUnits, "mcg/mL")
  expect_equal(p$massUnits, "mg")
  expect_setequal(unlist(p$sites), c("effect", "plasma"))
  expect_equal(p$typicalTarget, getDrugDefaults("propofol")$Typical)
  expect_equal(p$targetCeiling, TCI_API_TARGET_CEILING * getDrugDefaults("propofol")$Upper)
  expect_equal(unlist(b$units$rate), TCI_API_RATE_UNITS)
  expect_equal(unlist(b$units$concentration), TCI_API_CONCENTRATION_UNITS)
  expect_equal(b$limits$maxHistory, TCI_API_MAX_HISTORY)
  expect_equal(b$limits$weight, list(min = MIN_WEIGHT, max = MAX_WEIGHT))
  expect_equal(unlist(b$limits$sex), SEX_VALUES)
  # One-element vectors stay arrays: a client must never see a bare string.
  expect_match(r$text, '"volume":\\["mL"\\]')
  expect_equal(r$json, viaJson(tciApiDrugs()))
})

test_that("GET /tci/v1/drugs/propofol/model returns the model block for the patient", {
  r <- expectJson(call("GET", "/tci/v1/drugs/propofol/model",
                       query = "weight=70&height=170&age=50&sex=male"))
  b <- r$json
  expect_equal(b$drug, "propofol")
  expect_equal(b$concentrationUnits, "mcg/mL")
  expect_equal(b$massUnits, "mg")
  expect_true(is.numeric(b$tPeak) && b$tPeak > 0)
  expect_true(is.numeric(b$parameters$ke0) && b$parameters$ke0 > 0)
  expect_equal(names(b$parameters), c("v1", "v2", "v3", "cl1", "cl2", "cl3", "k10", "k12", "k13",
                                      "k21", "k31", "lambda_1", "lambda_2", "lambda_3", "ke0"))
  expect_match(b$hash, "^md5:[0-9a-f]{32}$")
  expect_true(b$adjustToFFM)
  expect_equal(b, viaJson(tciApiModel("propofol", 70, 170, 50, "male")))
  # The switch is read from the query and echoed; a decimal age is a number.
  r <- expectJson(call("GET", "/tci/v1/drugs/fentanyl/model",
                       query = "weight=100&height=170&age=50.5&sex=female&adjustToFFM=false"))
  expect_false(r$json$adjustToFFM)
  expect_equal(r$json, viaJson(tciApiModel("fentanyl", 100, 170, 50.5, "female", adjustToFFM = FALSE)))
  expect_false(identical(r$json$parameters,
                         viaJson(tciApiModel("fentanyl", 100, 170, 50.5, "female"))$parameters))
})

test_that("/model refuses what the body rules refuse: unknown, repeated, missing and bad parameters", {
  base <- "weight=70&height=170&age=50&sex=male"
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = paste0(base, "&adjustToFfm=false")),
              400L, "UNKNOWN_FIELD", "/adjustToFfm")
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = paste0(base, "&weight=80")),
              400L, "DUPLICATE_KEY", "/weight")
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = "height=170&age=50&sex=male"),
              400L, "MISSING_FIELD", "/weight")
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = "weight=seventy&height=170&age=50&sex=male"),
              400L, "INVALID_TYPE", "/weight")
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = "weight=600&height=170&age=50&sex=male"),
              422L, "INVALID_VALUE", "/weight")
  expectError(call("GET", "/tci/v1/drugs/propofol/model", query = "weight=70&height=170&age=50&sex=Male"),
              422L, "INVALID_VALUE", "/sex")
  expectError(call("GET", "/tci/v1/drugs/nosuchdrug/model", query = base),
              404L, "UNKNOWN_DRUG", "/drug")
  nonTci <- setdiff(getDrugDefaultsGlobal()$Drug, tciApiDrugNames())[1]
  expectError(call("GET", sprintf("/tci/v1/drugs/%s/model", nonTci), query = base),
              422L, "DRUG_NOT_TCI", "/drug")
})


# ---------------------------------------------------------------------------
# The controller

test_that("a /rate round trip equals tciNextRate() for the same request apart from server.*", {
  body <- rateBody(history = list(
    list(time = nowStamp(-120), rate = 100, units = "mg/min"),
    list(time = nowStamp(-110), rate = 0, units = "mg/min"),
    list(time = nowStamp(-30), rate = 8, units = "mg/min")
  ))
  r <- expectJson(call("POST", "/tci/v1/drugs/propofol/rate", body = body))
  direct <- viaJson(tciNextRate(body, drug = "propofol", receivedAt = Sys.time()))
  b <- r$json
  expect_equal(names(b), names(direct))
  expect_equal(b$requestId, "ch1-000412")
  expect_equal(b$drug, "propofol")
  expect_equal(b$rate$units, "mg/min")
  expect_true(b$rate$value >= 0)
  expect_match(b$timing$effectiveAt, timeRegex)
  # The server block is the router's own: its clock, and the compute time
  # measured from the preroute stamp.
  expect_equal(names(b$server), c("package", "version", "commit", "receivedAt", "respondedAt", "computeSeconds"))
  expect_match(b$server$receivedAt, timeRegex)
  expect_true(is.numeric(b$server$computeSeconds) && b$server$computeSeconds >= 0)
  expect_lte(tciApiParseTime(b$server$receivedAt, ""), tciApiParseTime(b$server$respondedAt, ""))
  b$server <- NULL
  direct$server <- NULL
  expect_equal(b, direct)
})

test_that("the path's drug is used when the body omits `drug`, and must agree with it when present", {
  body <- rateBody()
  body$drug <- NULL
  r <- expectJson(call("POST", "/tci/v1/drugs/propofol/rate", body = body))
  expect_equal(r$json$drug, "propofol")
  expect_equal(r$json$model$drug, "propofol")
  expectError(call("POST", "/tci/v1/drugs/remifentanil/rate", body = rateBody()),
              400L, "DRUG_MISMATCH", "/drug", requestId = "ch1-000412")
})

test_that("the body is understood whatever its Content-Type, as long as it is JSON", {
  body <- rateBody()
  json <- expectJson(call("POST", "/tci/v1/drugs/propofol/rate", body = body))$json
  plain <- suppressMessages(call("POST", "/tci/v1/drugs/propofol/rate", body = body, contentType = "text/plain"))
  none <- call("POST", "/tci/v1/drugs/propofol/rate", body = body, contentType = NULL)
  charset <- call("POST", "/tci/v1/drugs/propofol/rate", body = body,
                  contentType = "application/json; charset=utf-8")
  for (r in list(plain, none, charset)) {
    expectJson(r)
    a <- r$json
    a$server <- NULL
    json$server <- NULL
    expect_equal(a, json)
  }
})


# ---------------------------------------------------------------------------
# Every status of section 6.3, in the error shape

test_that("a body that is not JSON is 400 INVALID_JSON", {
  b <- expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = "{bad json"),
                   400L, "INVALID_JSON", "")
  expect_match(b$errors[[1]]$message, "not valid JSON")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = '{"drug": "propofol",'),
              400L, "INVALID_JSON", "")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = ""),
              400L, "INVALID_JSON", "")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate"),
              400L, "INVALID_JSON", "")
  # Unreadable JSON sent without the JSON Content-Type takes the same path.
  expectError(suppressMessages(call("POST", "/tci/v1/drugs/propofol/rate", body = "{bad json",
                                    contentType = "text/plain")),
              400L, "INVALID_JSON", "")
})

test_that("a body that is JSON but not an object, or with a key twice or unknown, is 400", {
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = "[1, 2]"),
              400L, "INVALID_TYPE", "")
  txt <- as.character(jsonlite::toJSON(rateBody(), auto_unbox = TRUE, digits = NA))
  expectError(call("POST", "/tci/v1/drugs/propofol/rate",
                   body = sub('"drug":', '"drug":"propofol","drug":', txt, fixed = TRUE)),
              400L, "DUPLICATE_KEY", "/drug", requestId = "ch1-000412")
  body <- rateBody()
  body$pump$maxrate <- 100
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = body),
              400L, "UNKNOWN_FIELD", "/pump/maxrate", requestId = "ch1-000412")
  body <- rateBody()
  body$timing$interval <- NULL
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = body),
              400L, "MISSING_FIELD", "/timing/interval", requestId = "ch1-000412")
})

test_that("an unknown drug is 404 UNKNOWN_DRUG and an unknown path 404 NOT_FOUND, inside and outside the mount", {
  expectError(call("POST", "/tci/v1/drugs/nosuchdrug/rate", body = rateBody(drug = "nosuchdrug")),
              404L, "UNKNOWN_DRUG", "/drug", requestId = "ch1-000412")
  for (path in c("/tci/v1/nothing", "/tci/v1/drugs/propofol/nothing", "/tci/v1", "/tci/v2/health",
                 "/nothing", "/", "/__docs__/", "/openapi.json")) {
    b <- expectError(call("GET", path), 404L, "NOT_FOUND")
    expect_match(b$errors[[1]]$message, path, fixed = TRUE)
  }
  expectError(call("POST", "/tci/v1/nothing", body = rateBody()), 404L, "NOT_FOUND")
})

test_that("a known path with another method is 405 METHOD_NOT_ALLOWED with an Allow header", {
  r <- call("GET", "/tci/v1/drugs/propofol/rate")
  expectError(r, 405L, "METHOD_NOT_ALLOWED")
  expect_equal(r$headers[["Allow"]], "POST")
  expect_match(r$json$errors[[1]]$message, "GET is not allowed on /tci/v1/drugs/propofol/rate")
  r <- call("PUT", "/tci/v1/drugs/propofol/rate", body = rateBody())
  expectError(r, 405L, "METHOD_NOT_ALLOWED")
  expect_equal(r$headers[["Allow"]], "POST")
  for (path in c("/tci/v1/health", "/tci/v1/drugs", "/tci/v1/drugs/propofol/model", "/tci/v1/openapi.json")) {
    r <- call("POST", path, body = list())
    expectError(r, 405L, "METHOD_NOT_ALLOWED")
    expect_equal(r$headers[["Allow"]], "GET")
    r <- call("DELETE", path)
    expectError(r, 405L, "METHOD_NOT_ALLOWED")
  }
})

test_that("a history of more than TCI_API_MAX_HISTORY events is 413 HISTORY_TOO_LONG", {
  event <- list(time = nowStamp(-60), rate = 0, units = "mg/min")
  body <- rateBody(history = rep(list(event), TCI_API_MAX_HISTORY + 1L))
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = body),
              413L, "HISTORY_TOO_LONG", "/history", requestId = "ch1-000412")
})

test_that("a domain error is 422 with the core's code and pointer", {
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody(units = "ng/mL")),
              422L, "UNIT_MISMATCH", "/target/units", requestId = "ch1-000412")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody(units = "mg/L")),
              422L, "UNIT_UNKNOWN", "/target/units", requestId = "ch1-000412")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody(latency = 10, interval = 10)),
              422L, "LATENCY_INVALID", "/timing/latency", requestId = "ch1-000412")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody(rateUnits = "mL/hr")),
              422L, "INFUSATE_REQUIRED", "/infusate", requestId = "ch1-000412")
  expectError(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody(target = 100)),
              422L, "TARGET_OUT_OF_RANGE", "/target/value", requestId = "ch1-000412")
})

test_that("an unexpected failure is 500 INTERNAL with a generic message, and the R error goes to the log", {
  test <- tciApiRouter()
  test <- plumber::pr_get(test, "/tci-test/boom", function(req, res) stop("the database is on fire"))
  expect_message(
    r <- test$call(rookRequest("GET", "/tci-test/boom")),
    "TCI API internal error on GET /tci-test/boom: the database is on fire")
  r$json <- jsonlite::fromJSON(as.character(r$body), simplifyVector = FALSE)
  b <- expectError(r, 500L, "INTERNAL")
  expect_false(grepl("fire", b$errors[[1]]$message))
})

test_that("a failure inside the controller is 500 INTERNAL even when its message says 'invalid'", {
  # The parse-error pattern of section 8.1 includes the bare word "invalid";
  # it applies to plumber's body parser, not to the controller, whose errors
  # are classified by stage.
  local_mocked_bindings(tciNextRate = function(...) stop("invalid state inside the controller"))
  test <- tciApiRouter()
  expect_message(
    r <- test$call(rookRequest("POST", "/tci/v1/drugs/propofol/rate", body = rateBody())),
    "TCI API internal error on POST .*/drugs/propofol/rate: invalid state inside the controller")
  r$json <- jsonlite::fromJSON(as.character(r$body), simplifyVector = FALSE)
  b <- expectError(r, 500L, "INTERNAL", requestId = "ch1-000412")
  expect_false(grepl("invalid", b$errors[[1]]$message))
})


# ---------------------------------------------------------------------------
# Serialization

test_that("numbers are serialized at full precision and scalars are unboxed", {
  # The default plumber serializer would round this to 0.0001 and box it.
  test <- tciApiRouter()
  test <- plumber::pr_get(test, "/tci-test/number", function(req, res)
    list(small = 0.000123456789, third = 1 / 3, one = I(0.1), none = list()))
  r <- test$call(rookRequest("GET", "/tci-test/number"))
  expect_equal(as.character(r$body), '{"small":0.000123456789,"third":0.333333333333333,"one":[0.1],"none":[]}')

  # A real response: the rate in the pump's units is a bare number with more
  # than ten significant digits, and it is tciNextRate()'s to 15 digits.
  body <- rateBody(drug = "sufentanil", target = 0.3, units = "ng/mL", rateUnits = "mcg/min",
                   latency = 0, history = list(list(time = nowStamp(-600), rate = 0.5, units = "mcg/min")))
  r <- expectJson(call("POST", "/tci/v1/drugs/sufentanil/rate", body = body))
  b <- r$json
  expect_true(is.numeric(b$rate$value) && length(b$rate$value) == 1L)
  expect_true(is.numeric(b$rate$ideal) && length(b$rate$ideal) == 1L)
  printed <- regmatches(r$text, regexpr('"ideal":[-0-9.eE+]+', r$text))
  digits <- gsub("[^0-9]", "", sub('"ideal":', "", printed))
  expect_gt(nchar(sub("^0+", "", digits)), 10)
  direct <- tciNextRate(body, drug = "sufentanil", receivedAt = Sys.time())
  expect_equal(b$rate$ideal, direct$rate$ideal, tolerance = 1e-14)
  expect_equal(b$state$now$ce, direct$state$now$ce, tolerance = 1e-14)
  expect_equal(b$model$parameters$ke0, direct$model$parameters$ke0, tolerance = 1e-14)
  expect_match(r$text, '"value":[0-9]')          # unboxed, not "value":[
})

test_that("empty lists are empty arrays, one-element vectors stay arrays, and NULL is null", {
  body <- rateBody()
  body$infusate <- list(concentration = 10, units = "mg/mL")
  body$pump <- list(rateUnits = "mL/hr", maxRate = 1200, rateResolution = 0.1)
  r <- expectJson(call("POST", "/tci/v1/drugs/propofol/rate", body = body))
  expect_match(r$text, '"warnings":\\[\\]')
  expect_true(is.list(r$json$warnings) && length(r$json$warnings) == 0)
  # A loading from zero wants far more than 1200 mL/hr of 1% propofol: the
  # one limit that applied is a one-element array, not a bare string.
  expect_match(r$text, '"limitedBy":\\["maxRate"\\]')
  expect_equal(r$json$rate$value, 1200)
  expect_match(r$text, '"commit":null')
  expect_match(r$text, '"reported":null')
  expect_true(is.list(r$json$state$now$p) && length(r$json$state$now$p) == 3)
  expect_true(is.list(r$json$state$now$e) && length(r$json$state$now$e) == 4)
  expect_null(r$json$delivered$reported)
  r <- expectJson(call("POST", "/tci/v1/drugs/propofol/rate", body = rateBody()))
  expect_match(r$text, '"limitedBy":\\[\\]')
})


# ---------------------------------------------------------------------------
# The OpenAPI document and the entry points

test_that("GET /tci/v1/openapi.json parses, lists the five paths and is the router's API spec", {
  r <- expectJson(call("GET", "/tci/v1/openapi.json"))
  spec <- r$json
  paths <- c("/tci/v1/health", "/tci/v1/drugs", "/tci/v1/drugs/{drug}/model",
             "/tci/v1/drugs/{drug}/rate", "/tci/v1/openapi.json")
  expect_equal(spec$openapi, "3.0.3")
  expect_equal(spec$info$version, TCI_API_VERSION)
  expect_setequal(names(spec$paths), paths)
  expect_equal(names(spec$paths[["/tci/v1/drugs/{drug}/rate"]]), "post")
  expect_equal(names(spec$paths[["/tci/v1/health"]]), "get")
  expect_setequal(names(spec$paths[["/tci/v1/drugs/{drug}/rate"]]$post$responses),
                  c("200", "400", "404", "405", "413", "422", "500"))
  req <- spec$components$schemas$RateRequest
  expect_false(req$additionalProperties)
  expect_setequal(unlist(req$required), c("now", "patient", "pump", "target", "timing", "history"))
  expect_setequal(names(req$properties),
                  c("requestId", "drug", "now", "patient", "infusate", "pump", "target", "timing",
                    "history", "lookAhead", "options"))
  expect_equal(req$properties$history$maxItems, TCI_API_MAX_HISTORY)
  expect_equal(unlist(spec$components$schemas$RateUnits$enum), TCI_API_RATE_UNITS)
  expect_equal(unlist(spec$components$schemas$PumpState$enum), TCI_API_PUMP_STATES)
  expect_setequal(unlist(spec$components$schemas$ErrorCode$enum),
                  c("INVALID_JSON", "INVALID_TYPE", "MISSING_FIELD", "UNKNOWN_FIELD", "DUPLICATE_KEY",
                    "DRUG_MISMATCH", "UNKNOWN_DRUG", "NOT_FOUND", "METHOD_NOT_ALLOWED", "HISTORY_TOO_LONG",
                    "DRUG_NOT_TCI", "INVALID_VALUE", "UNIT_UNKNOWN", "UNIT_MISMATCH", "INFUSATE_REQUIRED",
                    "INFUSATE_MISMATCH", "TIME_INVALID", "HISTORY_INVALID", "HISTORY_OVERLAP",
                    "HISTORY_IN_FUTURE", "LATENCY_INVALID", "TARGET_OUT_OF_RANGE", "INTERNAL"))
  expect_true(all(c("RateResponse", "ErrorResponse", "Model", "Drugs", "Health") %in%
                    names(spec$components$schemas)))
  # Every $ref resolves within the document.
  refs <- character(0)
  walk <- function(x) {
    if (!is.list(x)) return()
    if (!is.null(x[["$ref"]])) refs <<- c(refs, x[["$ref"]])
    for (el in x) walk(el)
  }
  walk(spec)
  resolves <- vapply(unique(refs), function(ref) {
    node <- spec
    for (p in strsplit(sub("^#/", "", ref), "/")[[1]]) {
      node <- node[[p]]
      if (is.null(node)) return(FALSE)
    }
    TRUE
  }, logical(1))
  expect_true(all(resolves))
  # The same document is what plumber would give its interactive docs.
  expect_setequal(names(router$getApiSpec()$paths), paths)
  # A one-element enum stays an array through the serializer.
  expect_match(r$text, '"enum":\\["mL"\\]')
})

test_that("the entry points are thin: a plumber router, docs off by default, and pr_run() with its defaults", {
  expect_s3_class(router, "Plumber")
  expect_s3_class(tciApiRouter(docs = TRUE), "Plumber")
  expect_equal(formals(tciApiRouter)$docs, FALSE)
  expect_equal(formals(run_tci_api), as.pairlist(alist(host = "127.0.0.1", port = 8000, docs = FALSE)))
  entry <- system.file("api", "entrypoint.R", package = "stanpumpR")
  expect_true(nzchar(entry))
  code <- parse(entry)
  expect_length(code, 1)
  expect_equal(deparse(code[[1]]), "stanpumpR::tciApiRouter()")
})
