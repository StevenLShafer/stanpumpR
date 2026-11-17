# -----------------------------------------------------------------------------
# Check, and optionally refresh, the FDA labels in papers/labels/
# -----------------------------------------------------------------------------
# The index is inst/extdata/fdaLabels.csv (see R/fda-labels.R and
# papers/labels/README.md).  Run from the repository root:
#
#   source("data-raw/fdaLabels.R")
#   checkFdaLabels()                  # every file present, checksums match
#
# CI runs checkFdaLabels() on every push and pull request
# (.github/workflows/fda-labels.yaml), because R CMD check cannot: it tests the
# built package, which leaves papers/ out, so test-fda-labels.R skips its file
# checks there.
#   refreshFdaLabels()                # list DailyMed labels with a newer version
#   refreshFdaLabels(download = TRUE) # download them and update the index
#
# Only `source == "dailymed"` rows are refreshed.  An `archive` file is the
# version its digest card in papers/digests/ reads page by page, so it is never
# replaced here: a newer label for such a drug goes into the archive, gets its
# card, and is then copied in.
#
# DailyMed (dailymed.nlm.nih.gov) serves the labels FDA holds, current version
# only, and permits scripted downloads; Drugs@FDA (accessdata.fda.gov) keeps
# every version but blocks them.  A cloud session needs dailymed.nlm.nih.gov in
# its environment's allowed hosts.
# -----------------------------------------------------------------------------

FDA_LABELS_INDEX <- "inst/extdata/fdaLabels.csv"
FDA_LABELS_DIR <- "papers/labels"

readFdaLabelIndex <- function(path = FDA_LABELS_INDEX) {
  x <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                       na.strings = character(0), encoding = "UTF-8")
  x[is.na(x)] <- ""
  x
}

writeFdaLabelIndex <- function(x, path = FDA_LABELS_INDEX) {
  x <- x[order(tolower(x$drug), x$published), ]
  utils::write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(x)
}

#' Every file in the index is in papers/labels/ with its checksum, every PDF
#' there is in the index, and every digest card named exists
checkFdaLabels <- function(index = readFdaLabelIndex(), dir = FDA_LABELS_DIR) {
  rows <- index[index$status == "label", ]
  path <- file.path(dir, rows$file)
  missing <- unique(rows$file[!file.exists(path)])
  present <- file.exists(path)
  bad <- unique(rows$file[present][unname(tools::md5sum(path[present])) != rows$md5[present]])
  stray <- setdiff(list.files(dir, pattern = "\\.pdf$"), rows$file)
  cards <- unique(rows$card[nzchar(rows$card)])
  noCard <- cards[!file.exists(file.path("papers", "digests", paste0(cards, ".md")))]
  if (length(missing)) message("Missing: ", paste(missing, collapse = ", "))
  if (length(bad)) message("Checksum differs: ", paste(bad, collapse = ", "))
  if (length(stray)) message("Not in the index: ", paste(stray, collapse = ", "))
  if (length(noCard)) message("Digest card not found: ", paste(noCard, collapse = ", "))
  ok <- !length(missing) && !length(bad) && !length(stray) && !length(noCard)
  message(if (ok) sprintf("%d label rows, %d files: all present and unchanged.",
                          nrow(rows), length(unique(rows$file))) else "Problems found.")
  invisible(ok)
}

dailymedLatest <- function(setid) {
  h <- jsonlite::fromJSON(sprintf(
    "https://dailymed.nlm.nih.gov/dailymed/services/v2/spls/%s/history.json", setid))
  latest <- h$data$history[1, ]
  list(title = h$data$spl$title,
       published = format(as.Date(latest$published_date, format = "%b %d, %Y"), "%Y-%m-%d"))
}

#' List (and with download = TRUE, fetch) DailyMed labels revised since they
#' were downloaded.  A refreshed label gets a new file name (its date is in
#' the name) and the old file is removed.
refreshFdaLabels <- function(download = FALSE, index = readFdaLabelIndex(), dir = FDA_LABELS_DIR) {
  dm <- unique(index[index$source == "dailymed", c("file", "setid", "published")])
  for (i in seq_len(nrow(dm))) {
    latest <- tryCatch(dailymedLatest(dm$setid[i]), error = function(e) NULL)
    if (is.null(latest)) {
      message(dm$file[i], ": set id no longer on DailyMed")
      next
    }
    if (latest$published <= dm$published[i]) next
    newFile <- sub("_\\d{4}-\\d{2}-\\d{2}\\.pdf$", paste0("_", latest$published, ".pdf"), dm$file[i])
    message(dm$file[i], " -> ", newFile)
    if (!download) next
    tmp <- tempfile(fileext = ".pdf")
    utils::download.file(sprintf(
      "https://dailymed.nlm.nih.gov/dailymed/downloadpdffile.cfm?setId=%s", dm$setid[i]),
      tmp, mode = "wb", quiet = TRUE)
    if (!identical(readBin(tmp, "raw", 4), charToRaw("%PDF"))) {
      message("  not a PDF; skipped")
      next
    }
    file.copy(tmp, file.path(dir, newFile), overwrite = TRUE)
    if (newFile != dm$file[i]) unlink(file.path(dir, dm$file[i]))
    rows <- index$file == dm$file[i]
    index$file[rows] <- newFile
    index$published[rows] <- latest$published
    index$title[rows] <- latest$title
    index$md5[rows] <- unname(tools::md5sum(file.path(dir, newFile)))
  }
  if (download) writeFdaLabelIndex(index)
  invisible(index)
}
