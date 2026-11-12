# Single Paper Review calibration: score the runs.
#
# For each run file in CALIB_RESULTS, compares the extraction with the
# parameters block of the paper's digest card (papers/digests/<stem>.md),
# whose values were read from the PDF and checked against stanpumpR
# (verification: matches). Writes one row per run to scores.csv and a summary
# by effort level to summary.csv, both in CALIB_RESULTS.
#
# The measures:
#   key_items      numbers on the card (an equation contributes every number
#                  in it)
#   found_struct   of those, how many appear in the extraction's structured
#                  fields: a parameter's value or printed text, a covariate
#                  coefficient or reference value, a derived quantity
#   found_any      how many appear anywhere in the extraction, notes included
#   found_scaled   misses in the structured fields that are there at a power
#                  of ten (a table headed "k, 10^2 min^-1" recorded as printed)
#   key_supplement card numbers whose source is a supplement, which the run
#                  never saw (only the main PDF is sent); recall_main is the
#                  recall over the rest
#   recall_struct  found_struct / key_items: the headline accuracy measure
#   findings_*     the review's findings by severity (all of them in
#                  findings.csv): on a verified paper an
#                  error or concern is either a real problem the paper has or
#                  a false alarm, which is read by hand
#   seconds, input/output tokens, cost (USD at $4 / $20 per million), and
#   whether the run would have failed under the app's limits (64,000 output
#   tokens, 20 minutes)
#
# Matching: a card number counts as found when an extracted number agrees with
# it to within half a unit in its last printed digit, or to within 0.5%,
# whichever is larger. This is a matching of numbers, not of meanings: a
# number found under the wrong role still counts, so score.R lists the misses
# and the review's findings for reading by hand, and the report is written from
# that reading, not from these counts alone.
#
#   Rscript tools/paper-review-calibration/score.R
#
# (Claude Code, 2026-11-10, at the request of Steven L. Shafer.)

suppressMessages(devtools::load_all(".", quiet = TRUE))

outDir <- Sys.getenv("CALIB_RESULTS")
stopifnot(nzchar(outDir), dir.exists(outDir))
PRICE_IN <- 4 / 1e6
PRICE_OUT <- 20 / 1e6
# The app's limit when the calibration ran; 128,000 since (Shafer, 2026-11-10).
APP_MAX_TOKENS <- 64000
APP_TIMEOUT_SEC <- 1200

numbersIn <- function(x) {
  # The minus sign and en dash as escapes, matched as bytes, so the script
  # runs in a C locale (where enc2utf8() would mangle them).
  x <- as.character(x)
  for (dash in c("\u2212", "\u2013")) x <- gsub(dash, "-", x, fixed = TRUE, useBytes = TRUE)
  m <- regmatches(x, gregexpr("-?[0-9]+(\\.[0-9]+)?([eE][-+]?[0-9]+)?", x))[[1]]
  m
}

# Each card number with its tolerance from its printed digits.
cardKey <- function(stem) {
  s <- paste(readLines(file.path("papers/digests", paste0(stem, ".md")), warn = FALSE, encoding = "UTF-8"),
             collapse = "\n")
  yamlText <- regmatches(s, regexpr("```yaml\n(.*?)```", s))
  y <- yaml::yaml.load(sub("```$", "", sub("^```yaml\n", "", yamlText)))
  rows <- lapply(y$parameters, function(p) {
    texts <- numbersIn(p$value)
    if (!length(texts)) return(NULL)
    data.frame(name = p$name %or% "", printed = texts, value = as.numeric(texts),
               supplement = grepl("sup|supplement|ESM|Table S", p$source %or% "", ignore.case = TRUE),
               tol = sapply(texts, function(t) {
                 d <- if (grepl("\\.", t)) nchar(sub(".*\\.", "", sub("[eE].*", "", t))) else 0
                 0.5 * 10^(-d)
               }), stringsAsFactors = FALSE)
  })
  k <- do.call(rbind, rows)
  # Bare small integers (exponent 1, a count of 2) match too easily to mean
  # anything; they are kept but flagged.
  k$trivial <- abs(k$value) %in% c(0, 1, 2, 3) & !grepl("\\.", k$printed)
  k
}

# Numbers in the extraction's structured fields, and everywhere.
extractionNumbers <- function(x) {
  structured <- c(
    unlist(lapply(x$parameters, function(p) c(p$value, numbersIn(p$value_text %or% "")))),
    unlist(lapply(x$covariate_effects, function(c) c(c$coefficient, c$reference_value,
                                                      numbersIn(c$equation_as_printed %or% "")))),
    unlist(lapply(x$derived_quantities, function(d) c(d$value, numbersIn(d$value_text %or% "")))))
  everywhere <- numbersIn(paste(unlist(x), collapse = " "))
  list(structured = suppressWarnings(as.numeric(structured)),
       everywhere = suppressWarnings(as.numeric(everywhere)))
}

found <- function(key, values, scale = 1) {
  values <- values[is.finite(values)]
  vapply(seq_len(nrow(key)), function(i) {
    tol <- max(key$tol[i], 0.005 * abs(key$value[i]))
    any(vapply(scale, function(k)
      any(abs(values * k - key$value[i]) <= tol | abs(-values * k - key$value[i]) <= tol), logical(1)))
  }, logical(1))
}
POWERS_OF_TEN <- 10^c(-6:-1, 1:6)

files <- list.files(outDir, "\\.json$", full.names = TRUE)
keys <- list()
rows <- list()
misses <- list()
findings <- list()
for (f in files) {
  r <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  if (is.null(keys[[r$stem]])) keys[[r$stem]] <- cardKey(r$stem)
  key <- keys[[r$stem]]
  u <- r$usage %or% list()
  input <- sum(unlist(u[c("input_tokens", "cache_read_input_tokens", "cache_creation_input_tokens")]), na.rm = TRUE)
  output <- u$output_tokens %or% NA_real_
  row <- data.frame(stem = r$stem, effort = r$effort, rep = r$rep, outcome = r$outcome,
                    model = r$model %or% NA, stop_reason = r$stop_reason %or% NA,
                    seconds = r$seconds %or% NA, input_tokens = input, output_tokens = output,
                    cost = input * PRICE_IN + (if (is.na(output)) 0 else output) * PRICE_OUT,
                    fails_app_tokens = isTRUE(output > APP_MAX_TOKENS),
                    fails_app_time = isTRUE((r$seconds %or% 0) > APP_TIMEOUT_SEC),
                    key_items = nrow(key), key_nontrivial = sum(!key$trivial),
                    key_supplement = sum(key$supplement),
                    found_struct = NA, found_any = NA, found_scaled = NA, recall_struct = NA,
                    recall_struct_nontrivial = NA, recall_main = NA,
                    findings_error = NA, findings_concern = NA, findings_ok = NA,
                    stringsAsFactors = FALSE)
  if (identical(r$outcome, "success")) {
    x <- r$extraction
    nums <- extractionNumbers(x)
    fs <- found(key, nums$structured)
    fa <- found(key, nums$everywhere)
    row$found_struct <- sum(fs)
    row$found_any <- sum(fa)
    row$recall_struct <- mean(fs)
    row$recall_struct_nontrivial <- mean(fs[!key$trivial])
    row$recall_main <- if (any(!key$supplement)) mean(fs[!key$supplement]) else NA
    fsc <- !fs & found(key, nums$structured, POWERS_OF_TEN)
    row$found_scaled <- sum(fsc)
    if (any(!fs)) misses[[length(misses) + 1]] <- data.frame(
      stem = r$stem, effort = r$effort, rep = r$rep, name = key$name[!fs], printed = key$printed[!fs],
      anywhere = fa[!fs], scaled = fsc[!fs], supplement = key$supplement[!fs], stringsAsFactors = FALSE)
    review <- tryCatch(paperReviewQuietly(function() paperReviewRun(x)), error = function(e) NULL)
    # review$findings holds every finding: with several models, paperReviewRun()
    # gathers each model's findings and the paper's into it, so the models'
    # own lists are not counted again.
    fl <- if (is.null(review)) list() else review$findings %or% list()
    sev <- vapply(fl, function(f) f$severity %or% "", character(1))
    if (length(fl)) findings[[length(findings) + 1]] <- data.frame(
      stem = r$stem, effort = r$effort, rep = r$rep,
      group = vapply(fl, function(f) f$group %or% "", character(1)),
      section = vapply(fl, function(f) f$section %or% "", character(1)), severity = sev,
      title = vapply(fl, function(f) f$title %or% "", character(1)),
      detail = vapply(fl, function(f) paste(f$detail %or% "", collapse = " "), character(1)),
      stringsAsFactors = FALSE)
    row$findings_error <- sum(sev == "error")
    row$findings_concern <- sum(sev == "concern")
    row$findings_ok <- sum(sev == "ok")
  }
  rows[[length(rows) + 1]] <- row
}
scores <- do.call(rbind, rows)
scores <- scores[order(scores$stem, match(scores$effort, c("low", "medium", "high", "xhigh", "max")), scores$rep), ]
utils::write.csv(scores, file.path(outDir, "scores.csv"), row.names = FALSE)
if (length(misses)) utils::write.csv(do.call(rbind, misses), file.path(outDir, "misses.csv"), row.names = FALSE)
# Every finding of every review, for reading by hand.
if (length(findings)) utils::write.csv(do.call(rbind, findings), file.path(outDir, "findings.csv"), row.names = FALSE)

by <- split(scores, scores$effort)
summary <- do.call(rbind, lapply(names(by), function(e) {
  s <- by[[e]]
  ok <- s$outcome == "success"
  data.frame(effort = e, runs = nrow(s), succeeded = sum(ok),
             recall_struct_mean = mean(s$recall_struct[ok]),
             recall_nontrivial_mean = mean(s$recall_struct_nontrivial[ok]),
             recall_main_mean = mean(s$recall_main[ok], na.rm = TRUE),
             scaled_per_review = mean(s$found_scaled[ok]),
             recall_struct_min = if (any(ok)) min(s$recall_struct[ok]) else NA,
             seconds_median = stats::median(s$seconds, na.rm = TRUE),
             seconds_max = max(s$seconds, na.rm = TRUE),
             output_tokens_median = stats::median(s$output_tokens, na.rm = TRUE),
             cost_mean = mean(s$cost), cost_total = sum(s$cost),
             would_fail_app = sum(s$fails_app_tokens | s$fails_app_time),
             errors_per_review = mean(s$findings_error[ok]), concerns_per_review = mean(s$findings_concern[ok]))
}))
summary <- summary[order(match(summary$effort, c("low", "medium", "high", "xhigh", "max"))), ]
utils::write.csv(summary, file.path(outDir, "summary.csv"), row.names = FALSE)
print(summary, row.names = FALSE)
