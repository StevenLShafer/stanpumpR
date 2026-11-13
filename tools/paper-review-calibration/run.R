# Single Paper Review calibration: run the reviews.
#
# Sends each paper in papers.csv to the Anthropic Messages API with the app's
# own prompt and schema (paperReviewRequestBody()), once per effort level and
# replicate in the design below, and saves what came back, one JSON file per
# run, in CALIB_RESULTS. Resumable: a run whose file exists is skipped, so the
# script can be stopped and started again.
#
# Differences from the app, on purpose, so the calibration measures what each
# setting NEEDS rather than what the app's limits allow: max_tokens is the
# model's 128,000 (the app's is 64,000) and the time limit is an hour (the
# app's is 20 minutes). score.R reports which runs would have failed under the
# app's limits.
#
#   STANPUMPR_ANTHROPIC_API_KEY  the key (read by paperReviewApiKey())
#   CALIB_PDFS     directory holding <stem>.pdf for every row of papers.csv
#   CALIB_RESULTS  directory for the results (created; never commit it: the
#                  extractions hold the papers' content)
#   CALIB_WORKERS  reviews at a time (default 4)
#   CALIB_PLAN     "full" (default): every level once, then medium and high
#                  again; "smoke": the first paper at low, to check the setup;
#                  "custom": the papers in CALIB_PAPERS (comma-separated stems;
#                  default all) at the levels in CALIB_LEVELS (default high),
#                  saved as replicate CALIB_REP (default 3), e.g. to check a
#                  change to the prompt or the checks on the papers it touches
#   CALIB_SKIP     stems to leave out, comma-separated (a PDF that could not be
#                  fetched: the Drive connector refuses a file over 10 MB); the
#                  runs can be resumed with them once their PDFs are in place
#
#   Rscript tools/paper-review-calibration/run.R
#
# (Claude Code, 2026-11-10, at the request of Steven L. Shafer.)

suppressMessages(devtools::load_all(".", quiet = TRUE))

LEVELS <- c("low", "medium", "high", "xhigh", "max")
MAX_TOKENS <- 128000
TIMEOUT_SEC <- 3600

here <- "tools/paper-review-calibration"
papers <- utils::read.csv(file.path(here, "papers.csv"), stringsAsFactors = FALSE)
pdfDir <- Sys.getenv("CALIB_PDFS")
outDir <- Sys.getenv("CALIB_RESULTS")
workers <- as.integer(Sys.getenv("CALIB_WORKERS", "4"))
plan <- Sys.getenv("CALIB_PLAN", "full")
skip <- trimws(strsplit(Sys.getenv("CALIB_SKIP"), ",")[[1]])
if (length(skip)) {
  cat("Leaving out:", paste(skip, collapse = ", "), "\n")
  papers <- papers[!papers$stem %in% skip, ]
}
stopifnot(nzchar(pdfDir), nzchar(outDir), dir.exists(pdfDir))
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)
key <- paperReviewApiKey(NULL, list())
if (is.null(key)) stop("No key: set STANPUMPR_ANTHROPIC_API_KEY.")
missing <- papers$stem[!file.exists(file.path(pdfDir, paste0(papers$stem, ".pdf")))]
if (length(missing)) stop("PDFs missing from CALIB_PDFS: ", paste(missing, collapse = ", "))

# The design: every paper at every level once (replicate 1), then medium and
# high a second time (replicate 2), to measure run-to-run variation. Runs are
# ordered by replicate, then level, then paper, so a stopped run leaves whole
# levels done rather than whole papers.
split <- function(x, default) { v <- trimws(strsplit(x, ",")[[1]]); if (length(v)) v else default }
design <- if (identical(plan, "smoke")) {
  data.frame(stem = papers$stem[1], effort = "low", rep = 1L)
} else if (identical(plan, "custom")) {
  stems <- split(Sys.getenv("CALIB_PAPERS"), papers$stem)
  stopifnot(all(stems %in% papers$stem))
  expand.grid(stem = stems, effort = split(Sys.getenv("CALIB_LEVELS"), "high"),
              rep = as.integer(Sys.getenv("CALIB_REP", "3")), stringsAsFactors = FALSE)
} else {
  rbind(expand.grid(stem = papers$stem, effort = LEVELS, rep = 1L, stringsAsFactors = FALSE),
        expand.grid(stem = papers$stem, effort = c("medium", "high"), rep = 2L, stringsAsFactors = FALSE))
}
design$file <- file.path(outDir, sprintf("%s_%s_%d.json", design$stem, design$effort, design$rep))
todo <- design[!file.exists(design$file), ]
cat(sprintf("%d runs in the design, %d to do, %d at a time\n", nrow(design), nrow(todo), workers))

# One run, in a background process: the transport is self-contained, and the
# time is measured around the request itself.
timedTransport <- function(transport, args) {
  started <- Sys.time()
  result <- tryCatch(do.call(transport, args), error = function(e) list(failure = "transport_error",
                                                                         message = conditionMessage(e)))
  list(result = result, started = format(started, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
       seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
}
transport <- paperReviewTransport
environment(transport) <- globalenv()
environment(timedTransport) <- globalenv()

launch <- function(row) {
  pdf <- readBin(file.path(pdfDir, paste0(row$stem, ".pdf")), "raw",
                 file.info(file.path(pdfDir, paste0(row$stem, ".pdf")))$size)
  body <- paperReviewRequestBody(pdf, effort = row$effort)
  body$max_tokens <- MAX_TOKENS
  json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA))
  args <- list(json = json, key = key, fallbacks = !is.null(body$fallbacks),
               url = PAPER_REVIEW_API_URL, version = PAPER_REVIEW_API_VERSION,
               beta = PAPER_REVIEW_FALLBACK_BETA, timeout = TIMEOUT_SEC)
  mirai::mirai(timedTransport(transport, args), .args = list(timedTransport = timedTransport,
                                                  transport = transport, args = args))
}

# Read a finished run and save it: the usage and stop reason even when the
# extraction fails, since a failed review is billed too.
save <- function(row, value) {
  out <- list(stem = row$stem, effort = row$effort, rep = row$rep, max_tokens = MAX_TOKENS)
  if (inherits(value, "miraiError") || inherits(value, "errorValue")) {
    out$outcome <- "background"
    out$message <- as.character(value)
  } else {
    out$started <- value$started
    out$seconds <- value$seconds
    r <- value$result
    if (!is.null(r$failure)) {
      out$outcome <- r$failure
      out$message <- r$message %or% ""
    } else if (r$status >= 400) {
      out$outcome <- paste0("http_", r$status)
      out$message <- paperReviewApiErrorText(r$error)
    } else {
      acc <- paperReviewAccumulateEvents(r$events %or% list())
      out$model <- acc$model
      out$stop_reason <- acc$stopReason
      out$usage <- acc$usage
      parsed <- tryCatch(paperReviewParseResponse(acc), error = function(e) e)
      if (inherits(parsed, "error")) {
        out$outcome <- parsed$code %or% "unparseable"
        out$text <- acc$text
      } else {
        out$outcome <- "success"
        out$extraction <- parsed
      }
    }
  }
  tmp <- paste0(row$file, ".tmp")
  writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE), tmp,
             useBytes = TRUE)
  file.rename(tmp, row$file)
  cat(sprintf("%s  %-14s %-6s rep %d  %-12s %6.0f s  out %s\n", format(Sys.time(), "%H:%M:%S"),
              row$stem, row$effort, row$rep, out$outcome, out$seconds %or% NA,
              format(out$usage$output_tokens %or% NA, big.mark = ",")))
}

# The daemons are started with this process's library paths (see
# paperReviewInBackground()).
Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep))
mirai::daemons(workers)
on.exit(mirai::daemons(0), add = TRUE)
running <- list()
nextRow <- 1L
while (nextRow <= nrow(todo) || length(running)) {
  while (length(running) < workers && nextRow <= nrow(todo)) {
    row <- todo[nextRow, ]
    running[[length(running) + 1]] <- list(row = row, job = launch(row))
    nextRow <- nextRow + 1L
  }
  done <- vapply(running, function(x) !mirai::unresolved(x$job), logical(1))
  for (x in running[done]) save(x$row, x$job$data)
  running <- running[!done]
  if (!any(done)) Sys.sleep(5)
}
cat("All runs done.\n")
