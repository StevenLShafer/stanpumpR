# The TCI API's HTTP layer (tci-api-server.R)
#
# Drafted by Claude Code, 2026-10-12, at the request of Steven L. Shafer.  The
# specification is docs/tci-api.md (version 1.0): sections 3 (endpoints), 6.3
# (errors), 7.1 (deployment) and 8.1 (this file's contract).  The tests are
# tests/testthat/test-tci-api-http.R.  Everything the API computes is in
# tci-api.R; this file only puts those functions behind plumber routes, so a
# request body that is logged by a pump replays identically in R with
# tciNextRate(body) and over HTTP.
#
# WHY plumber IS OPTIONAL
# =======================
# plumber is in Suggests, not Imports.  The Shiny app never serves the API,
# the shinyapps.io deploy installs Imports only, and renv.lock is unchanged.
# So nothing in this file runs when the package is loaded: both exported
# functions check requireNamespace("plumber") first, every plumber call is
# prefixed, and no route is built until tciApiRouter() is called.  The router
# shares nothing with run_app(): no config.yml, no .sprglobals, no ggplot
# theme.  Serve it with run_tci_api(), or with rsconnect::deployAPI() through
# inst/api/entrypoint.R.
#
# THE ROUTER
# ==========
# An inner router holds the five routes (health, drugs, model, rate,
# openapi.json) and is mounted at /tci/v1 on an outer one, so that the public
# paths are exactly /tci/v1/... and version 2 can be mounted beside it.
# plumber answers a mounted request with the inner router's own route(),
# which has its own not-found and error steps, so BOTH routers carry the
# API's 404 and error handlers: the outer one answers for paths outside the
# mount, the inner one for paths inside it.
#
# SERIALIZATION
# =============
# plumber's default JSON serializer rounds to four significant digits and
# boxes every scalar in an array.  A rate in mL/hr rounded to four digits is
# a dosing error, and a pump parser expects a number where the spec says a
# number, so the first thing set on each router -- before any route, because
# a route keeps the serializer in force when it is added -- is
# serializer_unboxed_json(digits = NA, null = "null"): full double precision,
# scalars unboxed, R NULL as JSON null.  The core builds its lists for exactly
# that call (I() keeps a one-element vector an array; list() is []).
#
# BODY PARSING
# ============
# plumber's default JSON parser simplifies: it would turn the history array
# into a data frame and lose the distinction between a missing key and a null
# one.  The /rate route therefore registers its own JSON parser with
# simplifyVector = FALSE, which hands the handler the list of lists that
# tciNextRate() is written for (section 8.1).  A body that reaches the handler
# unparsed -- sent without a JSON Content-Type, as curl -d does -- is parsed
# here the same way, so a controller that forgets the header is still
# understood; the specification says the header SHOULD be sent, not must.
#
# ERRORS
# ======
# Every refusal, whatever raised it, has the shape of section 6.3 and the
# status of its code.  A tci_api_error from the core keeps its own status.
# A body plumber's parser cannot read is thrown before the handler runs and
# reaches the error handler as an ordinary R error; the specification names
# the message pattern that identifies it (lexical error, parse error,
# premature EOF, invalid) and it becomes 400 INVALID_JSON.  Anything else is
# 500 INTERNAL with a generic message: the R error goes to the server log,
# never to the pump.  The pattern is applied only to errors raised before the
# handler ran: a failure inside the controller is marked by the handler
# (class tci_api_internal) and is INTERNAL whatever its message, so an R
# error that happens to say "invalid" can neither be mistaken for a parse
# error nor leak its text to the pump.  No such path is 404 NOT_FOUND, and a known path with
# another method is 405 METHOD_NOT_ALLOWED with an Allow header.  plumber's
# own 405 is not used: it does not fire inside a mount (it checks the outer
# router with the already-stripped path) and its body is not the API's shape,
# so the not-found handler looks the path up in the router's endpoints itself.
# The error bodies carry the server block of tci-api.R: a preroute hook on the
# outer router stamps the arrival time on the request, before the body is
# read, so that `receivedAt` and `computeSeconds` include the parsing of a
# 20000-event history, which is real server work.
#
# THE OPENAPI DOCUMENT
# ====================
# plumber generates almost nothing for programmatic routes, so the document is
# hand-written in inst/api/openapi.json (schemas for the request, the response
# and the error body), read with simplifyVector = FALSE so that a one-element
# array stays an array, installed with pr_set_api_spec() for the interactive
# documentation, and served at GET /tci/v1/openapi.json.  /__docs__/ is
# mounted by plumber only when the router is built with docs = TRUE, which a
# production deployment never does (section 7.1).


# ---------------------------------------------------------------------------
# Serialization and the error shape

# The one serializer of the API (see the header): full precision, scalars
# unboxed, NULL as null.
tciApiSerializer <- function()
  plumber::serializer_unboxed_json(digits = NA, null = "null")

# A tci_api_error built without being signalled, for the refusals the router
# raises itself (no such path, wrong method, unreadable JSON, an unexpected
# failure), so that every error body comes from tciApiErrorBody() and the
# condition is constructed in one place, tciApiStop().
tciApiCondition <- function(status, code, message, pointer = NULL, requestId = NULL)
{
  e <- tryCatch(tciApiStop(status, code, message, pointer), tci_api_error = function(e) e)
  e$requestId <- requestId
  e
}

# When the request reached the outer router (stamped by its preroute hook),
# or now if the stamp is missing.
tciApiReceivedAt <- function(req)
{
  t <- req$tciApiReceivedAt
  if (is.null(t)) Sys.time() else t
}

# Answer with an error: the status of the condition, the API's serializer and
# the body of section 6.3.  Returned, not sent: plumber serializes the value a
# handler returns with res$serializer.
tciApiErrorResponse <- function(res, e, receivedAt)
{
  res$status <- e$status
  res$serializer <- tciApiSerializer()
  tciApiErrorBody(e, receivedAt, Sys.time())
}

# The first line of an R error message, for a body the pump may display.
tciApiFirstLine <- function(msg) trimws(strsplit(msg, "\n", fixed = TRUE)[[1]][1])

# The pattern the specification (section 8.1) gives for a JSON parse error
# from jsonlite, which plumber throws before the handler runs.
tciApiIsJsonParseError <- function(msg)
  grepl("lexical error|parse error|premature EOF|invalid", msg)

# The error handler of both routers (pr_set_error).  `err` is a tci_api_error
# from the core or from a handler, a parse error from plumber's body parser,
# or anything else.
tciApiErrorHandler <- function(req, res, err)
{
  received <- tciApiReceivedAt(req)
  if (inherits(err, "tci_api_error")) {
    if (is.null(err$requestId)) err$requestId <- tciApiLenientRequestId(req$body)
    return(tciApiErrorResponse(res, err, received))
  }
  msg <- conditionMessage(err)
  if (!inherits(err, "tci_api_internal") && tciApiIsJsonParseError(msg)) {
    e <- tciApiCondition(400L, "INVALID_JSON",
                         paste("the request body is not valid JSON:", tciApiFirstLine(msg)), "")
    return(tciApiErrorResponse(res, e, received))
  }
  # The server's log gets the R error; the pump gets a generic body (7.1).
  message("TCI API internal error on ", req$REQUEST_METHOD, " ", req$PATH_INFO, ": ", msg)
  e <- tciApiCondition(500L, "INTERNAL", "the server could not process the request",
                       requestId = tciApiLenientRequestId(req$body))
  tciApiErrorResponse(res, e, received)
}

# The methods a router's endpoints accept on a path, from its endpoint table
# (plumber's public `endpoints` binding and PlumberEndpoint$matchesPath()).
# Empty when no endpoint has the path.
tciApiAllowedMethods <- function(router, path)
{
  verbs <- character(0)
  for (group in router$endpoints)
    for (endpoint in group)
      if (endpoint$matchesPath(path)) verbs <- c(verbs, endpoint$verbs)
  sort(unique(verbs))
}

# The not-found handler of a router (pr_set_404): 405 METHOD_NOT_ALLOWED with
# an Allow header when the path exists under another method, else 404
# NOT_FOUND.  `prefix` is the mount path of an inner router, whose PATH_INFO
# plumber has already stripped, so that the message names the public path.
tciApiNotFound <- function(router, prefix = "")
{
  force(router)
  force(prefix)
  function(req, res) {
    path <- paste0(prefix, req$PATH_INFO)
    allowed <- tciApiAllowedMethods(router, req$PATH_INFO)
    if (length(allowed)) {
      res$setHeader("Allow", paste(allowed, collapse = ", "))
      e <- tciApiCondition(405L, "METHOD_NOT_ALLOWED",
                           sprintf("%s is not allowed on %s; use %s", req$REQUEST_METHOD, path,
                                   paste(allowed, collapse = " or ")))
    } else {
      e <- tciApiCondition(404L, "NOT_FOUND", sprintf("no such path: %s", path))
    }
    tciApiErrorResponse(res, e, tciApiReceivedAt(req))
  }
}


# ---------------------------------------------------------------------------
# The handlers
#
# Each takes only (req, res): plumber then passes nothing else, so a body key
# or query parameter can never collide with a handler argument.  The path
# parameter is read from req$argsPath and the query from req$argsQuery.

# The parsed body of a /rate request.  plumber has parsed it when the
# Content-Type was JSON (or absent and the body looked like JSON); otherwise
# req$body is an empty unnamed list and the raw body is parsed here.  An empty
# body is INVALID_JSON, as it is for jsonlite.
tciApiRequestBody <- function(req)
{
  bytes <- req$bodyRaw
  if (is.null(bytes) || length(bytes) == 0L)
    tciApiStop(400L, "INVALID_JSON", "the request body is empty", "")
  body <- req$body
  if (is.list(body) && length(body) == 0L && is.null(names(body))) {
    txt <- tryCatch(rawToChar(bytes), error = function(e) NA_character_)
    if (is.na(txt))
      tciApiStop(400L, "INVALID_JSON", "the request body is not valid JSON: it is not text", "")
    Encoding(txt) <- "UTF-8"
    body <- tryCatch(
      jsonlite::fromJSON(txt, simplifyVector = FALSE),
      error = function(e)
        tciApiStop(400L, "INVALID_JSON",
                   paste("the request body is not valid JSON:", tciApiFirstLine(conditionMessage(e))), ""))
  }
  body
}

# POST /tci/v1/drugs/<drug>/rate (sections 4 to 6).  The arrival time is the
# outer router's stamp, so computeSeconds includes the body parsing.
tciApiRateHandler <- function(req, res)
{
  received <- tciApiReceivedAt(req)
  body <- tciApiRequestBody(req)
  # A failure inside the controller is a server fault whatever its message
  # says: it is marked here, by stage, so that the error handler never reads
  # an R message that happens to contain "invalid" as a JSON parse error and
  # answers 400 with the message in the body.
  # (One handler that looks at the class: with two, a tci_api_error rethrown
  # from the first would land in the second, which is still in scope.)
  out <- tryCatch(
    tciNextRate(body, drug = req$argsPath[["drug"]], receivedAt = received),
    error = function(e) {
      if (inherits(e, "tci_api_error")) stop(e)
      stop(structure(class = c("tci_api_internal", "error", "condition"),
                     list(message = conditionMessage(e), call = conditionCall(e))))
    })
  res$status <- 200L
  out
}

# GET /tci/v1/drugs/<drug>/model?weight=&height=&age=&sex=&adjustToFFM=
# (section 3).  The same rules as the body (section 3.1): a parameter the
# API does not know is refused rather than ignored, so that a typo can never
# fall back to a default, and a parameter given twice is contradictory.
tciApiModelHandler <- function(req, res)
{
  known <- c("weight", "height", "age", "sex", "adjustToFFM")   # the query of section 3
  q <- req$argsQuery
  if (is.null(q)) q <- list()
  unknown <- setdiff(names(q), known)
  if (length(unknown))
    tciApiStop(400L, "UNKNOWN_FIELD",
               sprintf("unknown query parameter '%s'; the parameters are %s", unknown[1],
                       paste(known, collapse = ", ")),
               paste0("/", unknown[1]))
  twice <- names(q)[lengths(q) > 1L]
  if (length(twice))
    tciApiStop(400L, "DUPLICATE_KEY",
               sprintf("query parameter '%s' is given more than once", twice[1]),
               paste0("/", twice[1]))
  tciApiModel(req$argsPath[["drug"]], q$weight, q$height, q$age, q$sex,
              adjustToFFM = q$adjustToFFM)
}

# The hand-written OpenAPI document, read with simplifyVector = FALSE so that
# a one-element array (a `required` list of one name, an enum of one token)
# stays an array when it is serialized again.
tciApiOpenApi <- function()
{
  path <- system.file("api", "openapi.json", package = "stanpumpR")
  if (!nzchar(path))
    stop("the TCI API's OpenAPI document (inst/api/openapi.json) is not installed")
  jsonlite::read_json(path, simplifyVector = FALSE)
}


# ---------------------------------------------------------------------------
# The exported entry points

#' Build the TCI API router
#'
#' The plumber router of the stanpumpR TCI API (version 1), with the public
#' paths \code{/tci/v1/health}, \code{/tci/v1/drugs},
#' \code{/tci/v1/drugs/{drug}/model}, \code{/tci/v1/drugs/{drug}/rate} and
#' \code{/tci/v1/openapi.json}.  Each route calls the matching function of
#' the core -- \code{\link{tciApiHealth}()}, \code{\link{tciApiDrugs}()},
#' \code{\link{tciApiModel}()}, \code{\link{tciNextRate}()} -- so a request
#' answered over HTTP and the same body given to \code{tciNextRate()} agree
#' apart from the \code{server} block.  Responses are serialized at full
#' double precision with scalars unboxed; every refusal has the error shape
#' of section 6.3 of \code{docs/tci-api.md}, including a body that is not
#' JSON (400 \code{INVALID_JSON}), an unknown path (404 \code{NOT_FOUND}) and
#' a known path with another method (405 \code{METHOD_NOT_ALLOWED}).
#'
#' This is the pure builder: nothing is served until the router is run, by
#' \code{\link{run_tci_api}()} or by \code{plumber::pr_run()}, or deployed
#' through \code{inst/api/entrypoint.R}.  The plumber package is needed and
#' is only suggested by stanpumpR: install it with
#' \code{install.packages("plumber")}.
#'
#' @param docs whether \code{plumber::pr_run()} should also serve the
#'   interactive documentation at \code{/__docs__/}.  \code{FALSE} by default
#'   and in any production deployment (section 7.1); the OpenAPI document
#'   itself is always served at \code{/tci/v1/openapi.json}.
#'
#' @return a \code{plumber::Plumber} router, with the API mounted at
#'   \code{/tci/v1}.  \code{router$call(req)} answers a Rook request in
#'   process; \code{plumber::pr_run(router)} serves it.
#'
#' @examples
#' if (requireNamespace("plumber", quietly = TRUE)) {
#'   router <- tciApiRouter()
#'   req <- new.env()
#'   req$REQUEST_METHOD <- "GET"
#'   req$PATH_INFO <- "/tci/v1/health"
#'   req$QUERY_STRING <- ""
#'   req$rook.input <- list(read = function() raw(0), read_lines = function() character(0),
#'                          rewind = function() NULL)
#'   router$call(req)$body
#' }
#'
#' @export
tciApiRouter <- function(docs = FALSE)
{
  if (!requireNamespace("plumber", quietly = TRUE))
    stop('The TCI API needs the plumber package: install.packages("plumber")')
  spec <- tciApiOpenApi()

  # The inner router: the serializer first (a route keeps the serializer in
  # force when it is added), then the routes, then its own 404 and error
  # handlers, because plumber routes a mounted request with this router's
  # route() and not the outer one's.
  api <- plumber::pr()
  api <- plumber::pr_set_serializer(api, tciApiSerializer())
  api <- plumber::pr_get(api, "/health", function(req, res) tciApiHealth())
  api <- plumber::pr_get(api, "/drugs", function(req, res) tciApiDrugs())
  api <- plumber::pr_get(api, "/drugs/<drug>/model", tciApiModelHandler)
  api <- plumber::pr_post(api, "/drugs/<drug>/rate", tciApiRateHandler,
                          parsers = list(json = list(simplifyVector = FALSE)))
  api <- plumber::pr_get(api, "/openapi.json", function(req, res) spec)
  api <- plumber::pr_set_404(api, tciApiNotFound(api, prefix = "/tci/v1"))
  api <- plumber::pr_set_error(api, tciApiErrorHandler)

  # The outer router: stamps the arrival time before the body is read, mounts
  # the API, and answers for everything outside the mount.
  root <- plumber::pr()
  root <- plumber::pr_set_serializer(root, tciApiSerializer())
  root <- plumber::pr_hook(root, "preroute", function(req) {
    req$tciApiReceivedAt <- Sys.time()
    invisible(NULL)
  })
  root <- plumber::pr_mount(root, "/tci/v1", api)
  root <- plumber::pr_set_404(root, tciApiNotFound(root))
  root <- plumber::pr_set_error(root, tciApiErrorHandler)
  root <- plumber::pr_set_api_spec(root, spec)
  root <- plumber::pr_set_docs(root, docs)
  root
}

#' Serve the TCI API
#'
#' Builds the router with \code{\link{tciApiRouter}()} and serves it with
#' \code{plumber::pr_run()} until interrupted.  This is the entry point of a
#' deployment (section 7.1 of \code{docs/tci-api.md}): run it behind an
#' ingress that terminates TLS and authenticates the pump controller, one
#' process per worker, since plumber serves one request at a time.  It shares
#' nothing with \code{\link{run_app}()}: no \code{config.yml}, no Shiny.
#'
#' @param host the interface to listen on.  The loopback address by default:
#'   the API is meant to sit behind an ingress, not on an open port.
#' @param port the TCP port.
#' @param docs whether to serve the interactive documentation at
#'   \code{/__docs__/}; never in production.
#'
#' @return does not return while the server runs; \code{NULL} (invisibly)
#'   when it stops.
#'
#' @examples
#' \dontrun{
#' run_tci_api()
#' # Rscript -e 'stanpumpR::run_tci_api(host = "127.0.0.1", port = 8000)'
#' }
#'
#' @export
run_tci_api <- function(host = "127.0.0.1", port = 8000, docs = FALSE)
{
  if (!requireNamespace("plumber", quietly = TRUE))
    stop('The TCI API needs the plumber package: install.packages("plumber")')
  plumber::pr_run(tciApiRouter(docs = docs), host = host, port = port)
}
