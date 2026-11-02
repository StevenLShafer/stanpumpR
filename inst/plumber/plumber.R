# The stanpumpR simulation API as an HTTP service.  Start it with
# stanpumpR::run_api(); the request and the result are specified in
# inst/apidocs/api.md (served at / here and at https://stanpumpr.io/api), and
# the work is done by apiSimulate() and apiFormatResult() in R/api.R, so this
# file only maps HTTP onto them.

#* @apiTitle stanpumpR simulation API
#* @apiDescription Pharmacokinetic simulation: POST a dose table, the patient's covariates and the length of the simulation to /simulate, and get back the predicted plasma and effect-site concentrations as JSON, CSV, Excel or XML. The full documentation is at https://stanpumpr.io/api and at / on this server.
#* @apiVersion 1

# The body limit is set here, where plumber::pr() reads this file, so that a
# service hosted directly (Posit Connect, Docker) has it as run_api() does.
# A request is a dose table of at most MAX_DOSE_ROWS rows: 1 MB is ample.
plumber::options_plumber(maxRequestSize = 1e6)

# Errors in the request are 400 (or the status apiError() gave), with the
# message as JSON; anything else is a 500 that does not leak internals.
apiFail <- function(res, e) {
  res$status <- if (inherits(e, "stanpumpr_api_error")) e$status else 500L
  res$setHeader("Content-Type", "application/json")
  message <- if (inherits(e, "stanpumpr_api_error")) {
    conditionMessage(e)
  } else {
    # The detail goes to the server's log, not to the client.
    base::message("stanpumpR API error: ", conditionMessage(e))
    "Internal error while simulating; please report the request that caused it."
  }
  res$body <- as.character(jsonlite::toJSON(list(error = message), auto_unbox = TRUE))
  res
}

# A body sent as application/json is parsed by plumber before /simulate runs,
# so a malformed one fails there and would reach the client as plumber's bare
# 500.  The router's error handler parses it again with apiParseRequest(),
# whose 400 says what is wrong.  (`@parser none` would avoid the first parse,
# but plumber registers that parser with the regex "*", which is not a valid
# pattern, and then fails on every request with a content type.)
#* @plumber
function(pr) {
  plumber::pr_set_error(pr, function(req, res, err) {
    if (identical(req$PATH_INFO, "/simulate")) {
      parsed <- tryCatch(stanpumpR::apiParseRequest(req$postBody), error = function(e) e)
      if (inherits(parsed, "stanpumpr_api_error")) err <- parsed
    }
    apiFail(res, err)
  })
}

apiSend <- function(res, out) {
  res$status <- 200L
  res$setHeader("Content-Type", out$contentType)
  if (is.raw(out$body)) {
    res$setHeader("Content-Disposition", paste0("attachment; filename=\"", out$filename, "\""))
  }
  res$body <- out$body
  res
}

#* This documentation as a web page (inst/apidocs/api.md)
#* @get /
#* @serializer html
function() {
  stanpumpR::apiDocsHtml()
}

#* Liveness check, with the package and API versions
#* @get /health
#* @serializer unboxedJSON
function() {
  list(status = "ok",
       apiVersion = stanpumpR:::API_VERSION,
       stanpumpRVersion = as.character(utils::packageVersion("stanpumpR")),
       build = stanpumpR:::stanpumpRBuild()$commit)
}

#* The drugs the API can simulate and the dose units each takes
#* @param format json (default) or csv
#* @param allowIllicitDrugs include the opt-in Illicit drugs category (true/false)
#* @get /drugs
function(res, format = "json", allowIllicitDrugs = "false") {
  tryCatch({
    allow <- stanpumpR:::apiFlag(allowIllicitDrugs, "allowIllicitDrugs")
    catalog <- stanpumpR::apiDrugCatalog(allowIllicit = allow)
    format <- stanpumpR:::apiChoice(tolower(format), "format", c("json", "csv"))
    out <- if (format == "csv") {
      list(body = stanpumpR:::apiCsv(catalog), contentType = "text/csv")
    } else {
      catalog$Units <- strsplit(catalog$Units, ",", fixed = TRUE)
      list(body = as.character(jsonlite::toJSON(catalog, dataframe = "rows", auto_unbox = TRUE)),
           contentType = "application/json")
    }
    apiSend(res, out)
  }, error = function(e) apiFail(res, e))
}

#* Simulate a dose table for one patient
#* @param format json (default), csv, xlsx or xml; overrides output.format and the Accept header
#* @param layout long (default: one row per drug and time) or wide (one row per time); CSV and Excel only; overrides output.layout
#* @post /simulate
function(req, res, format = "", layout = "") {
  tryCatch({
    # The body is JSON (the documentation's requests say so); a body declared
    # as anything else -- a browser form's, say -- is refused before it is
    # read.  No declaration at all is still read as JSON.
    type <- req$HTTP_CONTENT_TYPE
    if (is.null(type) || !nzchar(type)) type <- req$CONTENT_TYPE
    if (!is.null(type) && nzchar(type) && !grepl("^\\s*application/json\\s*(;|$)", tolower(type))) {
      stanpumpR:::apiError("Send the request body as application/json (its Content-Type was '",
                           type, "').", status = 415L)
    }
    request <- stanpumpR::apiParseRequest(req$postBody)
    format <- stanpumpR:::apiNegotiateFormat(format, request, req$HTTP_ACCEPT)
    # As for the format, the query parameter wins over the body.
    if (!nzchar(layout)) layout <- stanpumpR:::apiField(request$output, "layout", "long")
    # Check the format before simulating, so a typo costs nothing.
    stanpumpR:::apiChoice(format, "format", stanpumpR:::API_FORMATS)
    result <- stanpumpR::apiSimulate(request)
    apiSend(res, stanpumpR::apiFormatResult(result, format, layout))
  }, error = function(e) apiFail(res, e))
}
