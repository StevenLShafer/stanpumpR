# -----------------------------------------------------------------------------
# FDA prescribing information (the product labels) for the library's drugs
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Written by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# asked that every FDA label behind a stanpumpR drug be collected and, being a
# public document, kept on GitHub where the documentation can reach it.
#
# The PDFs are in papers/labels/ (not in the package: papers/ is in
# .Rbuildignore, and a few hundred megabytes of labels have no place in an
# installed library).  What the package carries is the index,
# inst/extdata/fdaLabels.csv, one row per drug and label, from which each
# drug's help page lists its labels as links to the files on GitHub.
#
# A row's `status` is one of
#   label        a US label is in papers/labels/ (`file`)
#   no_us_label  there is no FDA prescribing information for this drug as
#                modelled (not approved in the US, an OTC monograph product,
#                a Schedule I substance); `note` says which
#   not_obtained an FDA label exists but is not yet in papers/labels/; `note`
#                says where it is
# A label's `source` says where the file came from: `archive` (a copy of the
# PDF in the private stanpumpRPapers archive, which its digest card in
# papers/digests/ cites page by page, so it is kept as that card read it) or
# `dailymed` (the current label as FDA's Structured Product Labeling,
# downloaded from DailyMed, the National Library of Medicine's distribution of
# the labels FDA holds, identified by its `setid`).
#
# data-raw/fdaLabels.R checks the files against the index and can refresh the
# DailyMed rows.  Tests: tests/testthat/test-fda-labels.R.
# -----------------------------------------------------------------------------

#' Where the label PDFs are published
#' @noRd
FDA_LABELS_URL <- "https://github.com/StevenLShafer/stanpumpR/blob/master/papers/labels/"

#' Where the digest cards are published
#' @noRd
DIGESTS_URL <- "https://github.com/StevenLShafer/stanpumpR/blob/master/papers/digests/"

#' The statuses a row of the label index may have
#' @noRd
FDA_LABEL_STATUSES <- c("label", "no_us_label", "not_obtained")

#' The index of FDA labels
#'
#' @returns a data frame, one row per drug and label: `drug`, `status`,
#'   `file`, `title`, `application`, `published`, `source`, `setid`, `card`,
#'   `md5` and `note` (empty strings where a field does not apply)
#' @keywords internal
fdaLabels <- function() {
  x <- utils::read.csv(
    system.file("extdata", "fdaLabels.csv", package = "stanpumpR"),
    stringsAsFactors = FALSE, na.strings = character(0), encoding = "UTF-8",
    colClasses = "character")
  x[is.na(x)] <- ""
  x
}

#' The FDA prescribing information section of a drug's help page
#'
#' Lists the drug's labels, newest first, each a link to its PDF on GitHub with
#' its date and application number, and a link to the digest card that reads
#' it where there is one; or says why there is none.
#'
#' @param drug the drug's name
#' @param labels the label index (fdaLabels())
#' @returns HTML, or "" for a drug with no row
#' @noRd
helpDrugFdaLabelHTML <- function(drug, labels = fdaLabels()) {
  r <- labels[labels$drug == drug, , drop = FALSE]
  if (nrow(r) == 0) return("")
  esc <- htmltools::htmlEscape
  link <- function(href, text)
    sprintf('<a href="%s" target="_blank" rel="noreferrer">%s</a>', esc(href, attribute = TRUE), esc(text))
  have <- r[r$status == "label", , drop = FALSE]
  have <- have[order(have$published, decreasing = TRUE), , drop = FALSE]
  items <- vapply(seq_len(nrow(have)), function(i) {
    h <- have[i, ]
    meta <- c(if (nzchar(h$published)) h$published,
              if (nzchar(h$application)) h$application,
              if (h$source == "dailymed") "from DailyMed")
    sprintf("<li>%s%s%s</li>",
            link(paste0(FDA_LABELS_URL, utils::URLencode(h$file)), h$title),
            if (length(meta)) sprintf(" (%s)", esc(paste(meta, collapse = "; "))) else "",
            if (nzchar(h$card)) paste0(" &middot; ", link(paste0(DIGESTS_URL, h$card, ".md"), "digest")) else "")
  }, character(1))
  notes <- unique(r$note[r$status != "label" & nzchar(r$note)])
  paste0(
    helpH2("FDA prescribing information"),
    if (length(items)) paste0("<ul>", paste(items, collapse = ""), "</ul>") else "",
    if (length(notes)) paste0("<p>", esc(notes), "</p>", collapse = "") else "",
    "<p>The labels are public documents, kept in the repository's ",
    link("https://github.com/StevenLShafer/stanpumpR/tree/master/papers/labels", "papers/labels"),
    " folder. A label is frozen at approval: where a later study disagrees with it, the model follows ",
    "the study (see the drug's model source above).</p>"
  )
}
