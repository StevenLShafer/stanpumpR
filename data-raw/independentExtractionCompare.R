# Compare independent readings of stanpumpR's source papers with stanpumpR.
#
# Reads every reply in docs/independent-extraction/replies/ (<stem>.<reader>.md,
# the answer of Gemini, ChatGPT or another reader to
# docs/independent-extraction/PROMPT.md), takes the CSV block the prompt asks
# for, and looks up each number it reports:
#   - among the numbers stanpumpR's digest card and model code print for that
#     paper (the same matcher as data-raw/nlmixr2libCrossCheck.R);
#   - among the numbers each other reader reported for the paper;
#   - among the numbers nlmixr2lib transcribed from it, where it did.
# A number that at least two readers report (nlmixr2lib counting as one) and
# stanpumpR prints nowhere is a STRONG LEAD: stanpumpR probably missed or
# misread it.  A number one reader alone reports is weak: a misreading, or a
# part of the paper stanpumpR did not use.  Residual error and variability are
# counted but never leads, since stanpumpR carries them for only a few models.
#
# The replies quote the papers and stay out of the repository (.gitignore);
# the comparisons written here hold numbers, parameter names and locations
# only, never a quote.
#
# Usage (base R only):  Rscript --vanilla data-raw/independentExtractionCompare.R [<repo>]

args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1) args[1] else ".", mustWork = TRUE)
scriptDir <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(scriptDir, "crossCheckHelpers.R"))

base <- file.path(repo, "docs", "independent-extraction")
replyDir <- file.path(base, "replies")
outDir <- file.path(base, "comparisons")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

replyFiles <- list.files(replyDir, pattern = "\\.md$", full.names = TRUE)
if (!length(replyFiles)) {
  message("no replies in ", replyDir)
  quit(save = "no")
}

# The CSV block of a reply: the first fenced block whose first line is the
# prompt's header.
readReply <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  fences <- grep("^\\s*```", lines)
  for (k in seq_len(length(fences) - 1)) {
    block <- lines[(fences[k] + 1):(fences[k + 1] - 1)]
    if (length(block) && grepl("value_as_printed", block[1])) {
      df <- tryCatch(utils::read.csv(text = paste(block, collapse = "\n"),
                                     stringsAsFactors = FALSE, check.names = FALSE,
                                     colClasses = "character"),
                     error = function(e) NULL)
      if (!is.null(df)) return(df)
    }
  }
  NULL
}

firstNumber <- function(x) {
  hit <- regmatches(x, regexpr("-?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", x, perl = TRUE))
  if (length(hit)) as.numeric(hit) else NA_real_
}

cards <- readDigestCards(repo)
nlmx <- tryCatch(utils::read.csv(file.path(repo, "docs", "nlmixr2lib", "shared-parameters.csv"),
                                 stringsAsFactors = FALSE),
                 error = function(e) data.frame(card = character(), printed = numeric()))

stems <- unique(sub("\\.[^.]+\\.md$", "", basename(replyFiles)))
summaryRows <- list()
for (stem in stems) {
  card <- cards[cards$stem == stem, ]
  files <- replyFiles[startsWith(basename(replyFiles), paste0(stem, "."))]
  readings <- list()
  for (f in files) {
    reader <- sub(paste0("^", stem, "\\.(.+)\\.md$"), "\\1", basename(f))
    df <- readReply(f)
    if (is.null(df) || !"value_as_printed" %in% names(df)) {
      message(basename(f), ": no CSV block with the prompt's header; skipped")
      next
    }
    df$reader <- reader
    df$value <- vapply(df$value_as_printed, firstNumber, numeric(1))
    readings[[reader]] <- df
  }
  if (!length(readings)) next
  if (!nrow(card)) {
    message(stem, ": no digest card of that name; compared with the other readers only")
    cardNumbers <- codeNumbers <- numeric()
    codeFiles <- character()
  } else {
    codeFiles <- strsplit(card$codeFiles, ";")[[1]]
    codeFiles <- codeFiles[nzchar(codeFiles)]
    readText <- function(p) { p <- p[file.exists(p)]
      paste(unlist(lapply(p, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n") }
    cardNumbers <- numbersIn(readText(file.path(repo, "papers", "digests", paste0(stem, ".md"))))
    codeNumbers <- numbersIn(readText(file.path(repo, codeFiles)))
  }
  nlmxNumbers <- unique(nlmx$printed[nlmx$card == stem & is.finite(nlmx$printed)])
  hasNlmx <- length(nlmxNumbers) > 0

  rows <- do.call(rbind, lapply(names(readings), function(reader) {
    df <- readings[[reader]]
    others <- setdiff(names(readings), reader)
    type <- tolower(trimws(if ("estimate_type" %in% names(df)) df$estimate_type else ""))
    df$inCard <- vapply(df$value, matchValue, character(1), numbers = cardNumbers)
    df$inCode <- vapply(df$value, matchValue, character(1), numbers = codeNumbers)
    df$otherReaders <- vapply(df$value, function(v) sum(vapply(others, function(o)
      nzchar(matchValue(v, unique(readings[[o]]$value[is.finite(readings[[o]]$value)]))),
      logical(1))), integer(1))
    df$inNlmixr2lib <- if (hasNlmx) nzchar(vapply(df$value, matchValue, character(1),
                                                  numbers = nlmxNumbers)) else NA
    agree <- df$otherReaders + ifelse(is.na(df$inNlmixr2lib), 0L, as.integer(df$inNlmixr2lib))
    inStanpumpR <- grepl("exact$", df$inCard) | grepl("exact$", df$inCode)
    converted <- !inStanpumpR & (nzchar(df$inCard) | nzchar(df$inCode))
    variability <- type %in% c("iiv", "iov", "residual")
    df$verdict <- ifelse(!is.finite(df$value), "no number",
                  ifelse(inStanpumpR, "in stanpumpR",
                  ifelse(converted, "in stanpumpR only after a unit factor (check)",
                  ifelse(variability, "variability (not compared)",
                  ifelse(agree >= 1, "LEAD: another reader agrees, not in stanpumpR",
                         "one reading only, not in stanpumpR")))))
    df
  }))
  keep <- intersect(c("reader", "model", "parameter", "symbol_as_printed", "value_as_printed",
                      "units", "estimate_type", "pdf_page", "location", "inCard", "inCode",
                      "otherReaders", "inNlmixr2lib", "verdict"), names(rows))
  rows <- rows[order(rows$verdict != "LEAD: another reader agrees, not in stanpumpR",
                     rows$verdict != "in stanpumpR only after a unit factor (check)",
                     rows$verdict != "one reading only, not in stanpumpR", rows$reader), keep]
  utils::write.csv(rows, file.path(outDir, paste0(stem, ".csv")), row.names = FALSE, na = "")
  count <- function(v) sum(rows$verdict == v)
  summaryRows[[stem]] <- data.frame(
    stem = stem, readers = paste(names(readings), collapse = ";"),
    nlmixr2lib = hasNlmx, numbers = nrow(rows),
    inStanpumpR = count("in stanpumpR"),
    afterUnitFactor = count("in stanpumpR only after a unit factor (check)"),
    strongLeads = count("LEAD: another reader agrees, not in stanpumpR"),
    singleReadings = count("one reading only, not in stanpumpR"),
    variability = count("variability (not compared)"),
    codeFiles = paste(codeFiles, collapse = ";"), stringsAsFactors = FALSE)
}
summary <- do.call(rbind, summaryRows)
summary <- summary[order(-summary$strongLeads, summary$stem), ]
utils::write.csv(summary, file.path(outDir, "summary.csv"), row.names = FALSE, na = "")
message("compared ", nrow(summary), " papers; strong leads in ",
        sum(summary$strongLeads > 0), "; see docs/independent-extraction/comparisons/")
