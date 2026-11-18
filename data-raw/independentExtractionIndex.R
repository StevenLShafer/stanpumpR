# The list of stanpumpR's source papers for independent readings by other
# language models (Gemini, ChatGPT): docs/independent-extraction/index.csv.
#
# stanpumpR's models were transcribed from their papers by Claude, and the
# repository's rule is that every number is read in the PDF.  A second and
# third reading by models from other companies, made BLIND (the prompt,
# docs/independent-extraction/PROMPT.md, never shows stanpumpR's numbers),
# checks the reading itself: where two independent readers agree on a number
# stanpumpR does not print, stanpumpR probably misread or missed it.
# docs/independent-extraction/README.md is the protocol, and
# data-raw/independentExtractionCompare.R reads the replies back.
#
# A source paper is a digest card of kind pk-model or pkpd-model that names a
# model file of a drug in the Drug Library.  The papers nlmixr2lib also
# transcribed come first, since their readings can be compared three ways
# (run data-raw/nlmixr2libCrossCheck.R first to mark them).
#
# Usage (base R only):  Rscript --vanilla data-raw/independentExtractionIndex.R [<repo>]

args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args) >= 1) args[1] else ".", mustWork = TRUE)
scriptDir <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(scriptDir, "crossCheckHelpers.R"))

outDir <- file.path(repo, "docs", "independent-extraction")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

cards <- readDigestCards(repo)
library <- readLibraryDrugs(repo)
modelFiles <- paste0("R/drugs_", library$drug, ".R")

isSource <- cards$kind %in% c("pk-model", "pkpd-model") & vapply(
  strsplit(cards$codeFiles, ";"), function(f) any(f %in% modelFiles), logical(1))
sources <- cards[isSource, ]

shared <- tryCatch(utils::read.csv(file.path(repo, "docs", "nlmixr2lib", "shared-papers.csv"),
                                   stringsAsFactors = FALSE),
                   error = function(e) data.frame(card = character()))
sources$nlmixr2lib <- sources$stem %in% shared$card
sources$drugs <- vapply(strsplit(sources$codeFiles, ";"), function(f)
  paste(gsub("R/drugs_|\\.R", "", f[f %in% modelFiles]), collapse = "; "), character(1))

# Replies already saved (docs/independent-extraction/replies/<stem>.<reader>.md).
replyFiles <- list.files(file.path(outDir, "replies"), pattern = "\\.md$")
readers <- function(stem) {
  r <- sub(paste0("^", stem, "\\.(.+)\\.md$"), "\\1",
           replyFiles[startsWith(replyFiles, paste0(stem, "."))])
  paste(sort(r), collapse = ";")
}
sources$repliesSaved <- vapply(sources$stem, readers, character(1))

sources <- sources[order(!sources$nlmixr2lib, sources$stem), ]
index <- data.frame(
  stem = sources$stem, pdf = sources$file, citation = sources$citation,
  pmid = sources$pmid, doi = sources$doi, stanpumpRDrugs = sources$drugs,
  alsoInNlmixr2lib = sources$nlmixr2lib, repliesSaved = sources$repliesSaved,
  stringsAsFactors = FALSE)
utils::write.csv(index, file.path(outDir, "index.csv"), row.names = FALSE, na = "")
message("wrote docs/independent-extraction/index.csv: ", nrow(index), " source papers (",
        sum(index$alsoInNlmixr2lib), " also transcribed by nlmixr2lib; ",
        sum(nzchar(index$repliesSaved)), " with replies saved)")
