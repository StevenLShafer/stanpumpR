# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/all.equal
# Default tolerance is 1.5e-8
expect_equal_rounded <- function (expected, actual, tolerance = 1.5e-6) {
  expect_equal(actual, expected, tolerance = tolerance)
}

# A Rook request environment for plumber's in-process pr$call(req): what
# httpuv would hand the router, built by hand so that the TCI API's router is
# tested without a port, a subprocess or the network (test-tci-api-http.R).
# `body` is a list (serialized as the API expects it), a character string
# (sent as is, so malformed JSON can be injected) or NULL; `query` is the
# query string without the "?".  `contentType` NULL sends no Content-Type.
rookRequest <- function(method, path, body = NULL, query = "",
                        contentType = "application/json")
{
  bytes <- if (is.null(body)) raw(0)
           else if (is.raw(body)) body
           else if (is.character(body)) charToRaw(enc2utf8(paste(body, collapse = "\n")))
           else charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA,
                                                         null = "null")))
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- method
  req$PATH_INFO <- path
  req$QUERY_STRING <- query
  req$SERVER_NAME <- "127.0.0.1"
  req$SERVER_PORT <- "8000"
  req$HTTP_HOST <- "127.0.0.1:8000"
  req$rook.version <- "1.1-0"
  req$rook.url_scheme <- "http"
  if (!is.null(body) && !is.null(contentType)) req$HTTP_CONTENT_TYPE <- contentType
  if (!is.null(body)) req$CONTENT_LENGTH <- as.character(length(bytes))
  req$rook.input <- list(
    read = function() bytes,
    read_lines = function() if (length(bytes)) strsplit(rawToChar(bytes), "\n", fixed = TRUE)[[1]] else character(0),
    rewind = function() invisible(NULL)
  )
  req
}


# shiny::testServer() starts the server before any input exists, where a
# browser reports its inputs first, so the dose table's format starts from the
# server's fallback (elapsed time, TIME_MODE_DEFAULT) whatever Time Display the
# test then sets.  Call after setting the opening inputs, to start the format
# as a browser opening with those inputs would.
startFormatFromInputs <- function(session) {
  env <- session$env
  shiny::isolate(env$doseTableFormat(env$selectorFormat()))
}

# Plasma (central) concentration of a mammillary model after a bolus at time
# 0 and a constant-rate infusion from 0 to tStop, by eigendecomposition of the
# compartmental rate matrix: independent of the package's closed-form
# coefficients.  `set` holds v1..v3 (L) and cl1..cl3 (L/min); a compartment
# with zero intercompartmental clearance is absent.  Returns amount per litre
# of v1 in the dose's amount unit.
mammillaryReference <- function(set, times, bolus = 0, rate = 0, tStop = Inf) {
  peripheral <- Filter(function(i) set[[paste0("cl", i)]] > 0, 2:3)
  n <- 1 + length(peripheral)
  K <- matrix(0, n, n)
  K[1, 1] <- -set$cl1 / set$v1
  for (j in seq_along(peripheral)) {
    i <- peripheral[j]
    q <- set[[paste0("cl", i)]]
    K[1, 1] <- K[1, 1] - q / set$v1
    K[1, j + 1] <- q / set[[paste0("v", i)]]
    K[j + 1, 1] <- q / set$v1
    K[j + 1, j + 1] <- -q / set[[paste0("v", i)]]
  }
  e <- eigen(K)
  mu <- Re(e$values); V <- Re(e$vectors); w <- solve(V)[, 1]
  vapply(times, function(t) {
    tau <- min(t, tStop)
    g <- bolus * exp(mu * t) + rate * (exp(mu * t) - exp(mu * (t - tau))) / mu
    sum(V[1, ] * g * w) / set$v1
  }, numeric(1))
}

# The plasma concentration after a first-order depot dose (F x dose absorbed
# at rate ka) into the same mammillary set, by convolving the depot's input
# with mammillaryReference()'s unit impulse response, for calibrated
# absorption rates (bupivacaine, ropivacaine, diazepam)
depotReference <- function(set, times, dose, ka, F = 1) {
  peripheral <- Filter(function(i) set[[paste0("cl", i)]] > 0, 2:3)
  n <- 1 + length(peripheral)
  K <- matrix(0, n, n)
  K[1, 1] <- -set$cl1 / set$v1
  for (j in seq_along(peripheral)) {
    i <- peripheral[j]
    q <- set[[paste0("cl", i)]]
    K[1, 1] <- K[1, 1] - q / set$v1
    K[1, j + 1] <- q / set[[paste0("v", i)]]
    K[j + 1, 1] <- q / set$v1
    K[j + 1, j + 1] <- -q / set[[paste0("v", i)]]
  }
  e <- eigen(K)
  mu <- Re(e$values); V <- Re(e$vectors); w <- solve(V)[, 1]
  coef <- V[1, ] * w / set$v1
  vapply(times, function(t) {
    F * dose * ka * sum(coef * (exp(mu * t) - exp(-ka * t)) / (ka + mu))
  }, numeric(1))
}

# The largest concentration depotReference() reaches, and when
depotPeakReference <- function(set, dose, ka, F = 1, upper = 1440) {
  o <- optimize(function(t) depotReference(set, t, dose, ka, F), c(0, upper),
                maximum = TRUE, tol = 1e-10)
  c(peak = o$objective, time = o$maximum)
}

# What the dose grid sends (input$doseGrid, inst/www/dose-grid.js), through
# the JSON Shiny reads it from: rows of four strings, the time format they
# were typed in, and the number of edits sent
doseGridInputValue <- function(rows, format = "minutes/relative", seq = 1) {
  json <- jsonlite::toJSON(list(rows = rows, timeFormat = format, seq = seq),
                           auto_unbox = TRUE, null = "null", na = "null")
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# What a table grid sends (inst/www/table-grid.js): its rows, a checkbox as
# TRUE or FALSE, and the tag it was drawn with
tableGridInputValue <- function(rows, tag = NULL, seq = 1) {
  value <- list(rows = rows, seq = seq)
  if (!is.null(tag)) value$tag <- tag
  json <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null")
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# The fat-free-mass ratio of a man to the library's reference man (70 kg,
# 170 cm, 35 years), written out from Al-Sallami 2015's equation rather than
# taken from pkSizeFactors(), so a drug test can pin its switch-on scaling
# independently of the code under test.  Volumes scale by the ratio,
# clearances by its 0.75 power, and a model's own weight term sees 70 x the
# ratio (pkWeight).  (Claude Code, 2026-11-13.)
ffmRatioMaleIndependent <- function(weight, height, age) {
  ffmMale <- function(w, h, a) {
    bmi <- w / (h / 100)^2
    (0.88 + 0.12 / (1 + (a / 13.4)^(-12.7))) * 9270 * w / (6680 + 216 * bmi)
  }
  ffmMale(weight, height, age) / ffmMale(70, 170, 35)
}

# The size factors of a source cohort's typical patient, written out from
# Al-Sallami 2015's equation rather than taken from cohortSizeFactors(), so a
# drug test can check a model anchored at its cohort independently of the
# code under test (docs/weight-adjustment.md, *Anchoring at the source's
# cohort*): the geometric mean of the cohort's man and woman, weighted by
# their numbers, against the reference man; clearances on each sex's volume
# ratio ^ 0.75.  `cohort` is the list a drug file writes: weight, age, men,
# women, and height or bmi.  (Claude Code, 2026-11-18.)
cohortFactorsIndependent <- function(cohort) {
  ffm <- function(w, h, a, male) {
    bmi <- w / (h / 100)^2
    if (male) (0.88 + 0.12 / (1 + (a / 13.4)^(-12.7))) * 9270 * w / (6680 + 216 * bmi)
    else (1.11 - 0.11 / (1 + (a / 7.1)^(-1.1))) * 9270 * w / (8780 + 244 * bmi)
  }
  ref <- ffm(70, 170, 35, TRUE)
  groups <- if (!is.null(cohort$groups)) cohort$groups else list(cohort)
  lv <- 0; n <- 0
  for (g in groups) {
    h <- if (!is.null(g$height)) g$height else 100 * sqrt(g$weight / g$bmi)
    if (g$men > 0) lv <- lv + g$men * log(ffm(g$weight, h, g$age, TRUE) / ref)
    if (g$women > 0) lv <- lv + g$women * log(ffm(g$weight, h, g$age, FALSE) / ref)
    n <- n + g$men + g$women
  }
  list(volume = exp(lv / n), clearance = exp(0.75 * lv / n))
}

# The name of a directory as Shiny makes one for an upload: 24 hex digits,
# directly in tempdir() (FileUploadContext, shiny 1.14).  The paper review
# reads, and deletes, only a file in such a directory
# (paperReviewIsUploadPath()).
shinyUploadDirName <- function() {
  paste(sample(c(0:9, letters[1:6]), 24, replace = TRUE), collapse = "")
}


# The gas engine with every volume taken litre for litre, as it was until
# 2026-11-28 and as GasMan takes them: for tests of the circuit's algebra,
# whose numbers are in the engine's own litres (gasFreshGasExpansion()).
gasAsStated <- function(...) advanceClosedFormGas(..., volumeConversion = FALSE)

# The body simulateGases() gives a patient of this weight with nothing else
# said: GasMan's scaling with the 10% shunt under anesthesia (2026-11-28,
# R/endobronchial.R).  For a reference run of advanceClosedFormGas(), which
# handed no body runs GasMan's, without a shunt.
gasShuntBody <- function(weight = 70) gasBodyEndobronchial(getGasBody(weight))
