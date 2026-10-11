# The stanpumpR simulation API as an HTTP service.  Start it with
# stanpumpR::run_api(); the request and the result are specified in
# docs/api.md, and the work is done by apiSimulate() and apiFormatResult() in
# R/api.R, so this file only maps HTTP onto them.

#* @apiTitle stanpumpR simulation API
#* @apiDescription Pharmacokinetic simulation: POST a dose table, the patient's covariates and the length of the simulation to /simulate, and get back the predicted plasma and effect-site concentrations as JSON, CSV, Excel or XML. See docs/api.md in the stanpumpR repository.
#* @apiVersion 1

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

apiSend <- function(res, out) {
  res$status <- 200L
  res$setHeader("Content-Type", out$contentType)
  if (is.raw(out$body)) {
    res$setHeader("Content-Disposition", paste0("attachment; filename=\"", out$filename, "\""))
  }
  res$body <- out$body
  res
}

#* Liveness check, with the package and API versions
#* @get /health
#* @serializer unboxedJSON
function() {
  list(status = "ok",
       apiVersion = stanpumpR:::API_VERSION,
       stanpumpRVersion = as.character(utils::packageVersion("stanpumpR")))
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
#* @param layout long (default: one row per drug and time) or wide (one row per time); CSV and Excel only
#* @post /simulate
function(req, res, format = "", layout = "long") {
  tryCatch({
    request <- stanpumpR::apiParseRequest(req$postBody)
    format <- stanpumpR:::apiNegotiateFormat(format, request, req$HTTP_ACCEPT)
    layout <- stanpumpR:::apiField(request$output, "layout", layout)
    # Check the format before simulating, so a typo costs nothing.
    stanpumpR:::apiChoice(format, "format", stanpumpR:::API_FORMATS)
    result <- stanpumpR::apiSimulate(request)
    apiSend(res, stanpumpR::apiFormatResult(result, format, layout))
  }, error = function(e) apiFail(res, e))
}
