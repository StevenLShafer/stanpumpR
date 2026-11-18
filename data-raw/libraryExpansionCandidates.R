# Candidate source papers for the drugs stanpumpR plans to add.
#
# docs/library-expansion/plan.csv lists the drugs, by category, with the
# nlmixr2lib names each is filed under.  This script finds every nlmixr2lib
# model of each (docs/nlmixr2lib/models-outside-library.csv, written by
# data-raw/nlmixr2libCrossCheck.R), ranks them, and marks as PICKS the papers to
# obtain first: the largest study, and the most recent if that is another
# paper.  Where docs/library-expansion/archive-status.csv says whether a paper
# is already in the stanpumpR paper archive (by PMID or DOI file name), it is
# joined in, and docs/library-expansion/downloads.md lists the picks still to
# download, with their DOI and PubMed links.
#
# nlmixr2lib only finds the papers; every number in a model is read in the
# paper itself (docs/nlmixr2lib/README.md, AGENTS.md).  A drug nlmixr2lib does
# not model is listed for a PubMed search.
#
# Usage (base R only):  Rscript --vanilla data-raw/libraryExpansionCandidates.R [<repo>]

args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1) args[1] else ".", mustWork = TRUE)
dir <- file.path(repo, "docs", "library-expansion")

plan <- utils::read.csv(file.path(dir, "plan.csv"), stringsAsFactors = FALSE)
# A drug already in stanpumpR needs no papers: once it is implemented its
# models leave models-outside-library.csv, and it would come back as a drug
# to search PubMed for.
plan <- plan[plan$status != "done", , drop = FALSE]
models <- utils::read.csv(file.path(repo, "docs", "nlmixr2lib", "models-outside-library.csv"),
                          stringsAsFactors = FALSE, colClasses = c(pmid = "character"))
statusFile <- file.path(dir, "archive-status.csv")
status <- if (file.exists(statusFile))
  utils::read.csv(statusFile, stringsAsFactors = FALSE, colClasses = "character") else NULL

leadingNumber <- function(x) {
  n <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", x)))
  ifelse(is.na(n), 0, n)
}

rows <- list()
for (i in seq_len(nrow(plan))) {
  keys <- strsplit(plan$nlmixr2libKeys[i], ";")[[1]]
  keys <- keys[!is.na(keys) & nzchar(keys)]
  m <- models[models$drug %in% keys, , drop = FALSE]
  if (!nrow(m)) {
    rows[[length(rows) + 1]] <- data.frame(
      category = plan$category[i], drug = plan$drug[i], rank = NA, pick = FALSE,
      model = "", year = "", n = NA, ageRange = "", disease = "", doi = "", pmid = "",
      reference = "No nlmixr2lib model: search PubMed.", stringsAsFactors = FALSE)
    next
  }
  m <- m[!duplicated(m$model), ]
  m$n <- leadingNumber(m$nSubjects)
  m$yearNum <- suppressWarnings(as.numeric(sub("[a-z]$", "", m$year)))
  m <- m[order(-m$n, -ifelse(is.na(m$yearNum), 0, m$yearNum)), ]
  m$rank <- seq_len(nrow(m))
  # The largest study, and the most recent if that is another paper; two
  # models of one paper count once.
  pick <- m$rank == 1
  newest <- which.max(ifelse(is.na(m$yearNum), -Inf, m$yearNum))
  if (length(newest)) pick[newest] <- TRUE
  pickedDois <- unique(m$doi[pick & nzchar(m$doi)])
  pick <- pick | (nzchar(m$doi) & m$doi %in% pickedDois)
  rows[[length(rows) + 1]] <- data.frame(
    category = plan$category[i], drug = plan$drug[i], rank = m$rank, pick = pick,
    model = m$model, year = m$year, n = m$n, ageRange = m$ageRange,
    disease = substr(m$disease, 1, 160), doi = m$doi, pmid = m$pmid,
    reference = m$reference, stringsAsFactors = FALSE)
}
CANDIDATE_COLUMNS <- c("category", "drug", "rank", "pick", "model", "year", "n", "ageRange",
                       "disease", "doi", "pmid", "reference")
cand <- if (length(rows)) do.call(rbind, rows) else
  as.data.frame(setNames(replicate(length(CANDIDATE_COLUMNS), character(), simplify = FALSE),
                         CANDIDATE_COLUMNS), stringsAsFactors = FALSE)
cand$pick <- as.logical(cand$pick)
cand$n <- as.numeric(cand$n)
cand$rank <- as.numeric(cand$rank)
cand$doi[is.na(cand$doi)] <- ""
cand$pmid[is.na(cand$pmid)] <- ""

if (!is.null(status) && nrow(cand)) {
  # A paper is found in archive-status.csv by its DOI, or by its PMID where
  # it has no DOI.
  idx <- match(tolower(cand$doi), tolower(status$doi), incomparables = "")
  byPmid <- match(cand$pmid, status$pmid, incomparables = "")
  idx[is.na(idx)] <- byPmid[is.na(idx)]
  fill <- !is.na(idx)
  cand$pmid[fill & !nzchar(cand$pmid)] <- status$pmid[idx[fill & !nzchar(cand$pmid)]]
  cand$pmcid <- ifelse(fill, status$pmcid[idx], "")
  cand$inArchive <- ifelse(fill, status$inArchive[idx], "")
  cand$archiveFile <- ifelse(fill, status$archiveFile[idx], "")
} else {
  cand$pmcid <- rep("", nrow(cand))
  cand$inArchive <- rep("", nrow(cand))
  cand$archiveFile <- rep("", nrow(cand))
}
utils::write.csv(cand, file.path(dir, "candidates.csv"), row.names = FALSE, na = "")

# The file name the archive gives a paper: PMID_<pmid>.pdf, or, for one with
# no PMID, DOI_<doi>.pdf with every character but letters, digits and dots
# made "_" (DOI_10.2133_dmpk.13.376.pdf).
archiveName <- function(pmid, doi)
  ifelse(nzchar(pmid), paste0("PMID_", pmid, ".pdf"),
         ifelse(nzchar(doi), paste0("DOI_", gsub("[^A-Za-z0-9.]", "_", doi), ".pdf"), ""))

# The download list: the picks not in the archive, by category.  One paper may
# be the pick of two drugs, so they are made unique by DOI, then by PMID; a
# pick with neither is listed to be looked up.
picks <- cand[cand$pick, , drop = FALSE]
dupDoi <- nzchar(picks$doi) & duplicated(tolower(picks$doi))
dupPmid <- nzchar(picks$pmid) & duplicated(picks$pmid)
picks <- picks[!(dupDoi | dupPmid), , drop = FALSE]
unidentified <- picks[!nzchar(picks$doi) & !nzchar(picks$pmid), , drop = FALSE]
picks <- picks[nzchar(picks$doi) | nzchar(picks$pmid), , drop = FALSE]
missing <- picks[picks$inArchive != "TRUE", , drop = FALSE]
searches <- cand[is.na(cand$rank), c("category", "drug")]
lines <- c(
  "# Papers to download for the library expansion",
  "",
  "Generated by `data-raw/libraryExpansionCandidates.R` from `plan.csv` and nlmixr2lib's list of models",
  "(`docs/nlmixr2lib/`). Do not edit by hand.",
  "",
  "For each planned drug, the picks are the largest population model nlmixr2lib holds, and the most",
  "recent where that is another paper. All its other models are in `candidates.csv`.",
  sprintf("Picks: %d papers, of which %d are already in the archive and %d are listed below; %d of those are free in PubMed Central (no library login needed).",
          nrow(picks), sum(picks$inArchive == "TRUE"), nrow(missing), sum(nzchar(missing$pmcid))),
  "Save each under the name given (`PMID_<pmid>.pdf`, or `DOI_<doi>.pdf` for a paper with no PMID) so the archive's pipeline files it.",
  "Supplements matter as much as the paper: many models print their parameters only in a supplement.",
  "Save a supplement under the paper's name with `_sup1`, `_sup2` ... added (`PMID_<pmid>_sup1.pdf`), or the publisher's own suffix (`_mmc1`, `_ESM1`, `_SDC1`), never the paper's own name.",
  ""
)
for (cat in unique(plan$category)) {
  sub <- missing[missing$category == cat, ]
  if (!nrow(sub)) next
  lines <- c(lines, paste0("## ", cat), "")
  for (k in seq_len(nrow(sub))) {
    ref <- sub$reference[k]
    ref <- if (nchar(ref) > 220) paste0(substr(ref, 1, 217), "...") else ref
    links <- c(if (nzchar(sub$doi[k])) paste0("[DOI](https://doi.org/", sub$doi[k], ")"),
               if (nzchar(sub$pmcid[k])) paste0("[free in PMC](https://pmc.ncbi.nlm.nih.gov/articles/",
                                               sub$pmcid[k], "/)"),
               if (nzchar(sub$pmid[k])) paste0("[PubMed](https://pubmed.ncbi.nlm.nih.gov/",
                                              sub$pmid[k], "/)"),
               paste0("`", archiveName(sub$pmid[k], sub$doi[k]), "`"))
    links <- paste(links, collapse = " · ")
    lines <- c(lines, sprintf("- **%s** (n = %s): %s %s", sub$drug[k],
                              ifelse(sub$n[k] > 0, sub$n[k], "?"), ref, links))
  }
  lines <- c(lines, "")
}
if (nrow(unidentified)) {
  lines <- c(lines, "## Picks with no DOI or PMID: look up first", "",
             paste0("- **", unidentified$drug, "**: ", unidentified$reference), "")
}
if (nrow(searches)) {
  lines <- c(lines, "## Not in nlmixr2lib: search PubMed", "",
             paste0("- ", searches$drug, " (", searches$category, ")"), "")
}
writeLines(lines, file.path(dir, "downloads.md"), useBytes = TRUE)
# The same papers as a bare list, one per line, to paste into a session that
# downloads papers: each paper's PMID, or its DOI where it has none.
writeLines(as.character(ifelse(nzchar(missing$pmid), missing$pmid, missing$doi)),
           file.path(dir, "download-ids.txt"), useBytes = TRUE)
message("wrote docs/library-expansion/candidates.csv (", nrow(cand), " rows), download-ids.txt and downloads.md (",
        nrow(missing), " papers to download, ", nrow(searches), " drugs to search)")
