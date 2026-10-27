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
