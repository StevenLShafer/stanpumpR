# Cross-check stanpumpR against the nlmixr2lib model library.
#
# nlmixr2lib (Hooijmaijers R, Fidler M, Denney W, Vedantham K; GPL >= 2;
# https://github.com/nlmixr2/nlmixr2lib) is a library of several thousand
# published population PK/PD models, each transcribed from its paper with the
# paper's reference, the population it was fitted to and its parameter
# estimates.  stanpumpR uses its DATA only -- which papers exist, which models
# they hold, and the numbers nlmixr2lib transcribed from them -- and none of
# its code: the model files are PARSED here, never sourced or run.  Every
# number that reaches a stanpumpR model is still read in the paper itself
# (AGENTS.md).  docs/nlmixr2lib/README.md acknowledges the program and its
# authors.
#
# Three questions, three outputs (docs/nlmixr2lib/):
#   1. Shared papers.  For every paper that both libraries transcribed, is each
#      number nlmixr2lib took from it also among the numbers stanpumpR's
#      digest card and model code print?  A number found in neither is a LEAD
#      for the PDF: one of the two transcriptions may be wrong, or a unit or a
#      parameterisation differs.  (shared-papers.csv, shared-parameters.csv)
#   2. Papers not considered.  For the drugs stanpumpR already has, which
#      papers in nlmixr2lib have no digest card?  (candidate-papers.csv)
#   3. Drugs not in the library.  Which drugs does nlmixr2lib model that
#      stanpumpR does not, and is each at least mentioned in one of
#      stanpumpR's registries?  (candidate-drugs.csv, and one row per model
#      in models-outside-library.csv)
#
# Both transcriptions are AI-assisted (stanpumpR's by Claude; much of
# nlmixr2lib's by an extraction skill), so they can share a failure mode: a
# DISAGREEMENT is strong evidence, an agreement is weaker confirmation than two
# independent human readings would be.
#
# Usage (base R only, from any directory):
#   git clone --depth 1 https://github.com/nlmixr2/nlmixr2lib <dir>
#   Rscript --vanilla data-raw/nlmixr2libCrossCheck.R <dir> [<stanpumpR repo>]
# The repository defaults to the working directory.  The outputs are rewritten
# in full on every run.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("usage: nlmixr2libCrossCheck.R <nlmixr2lib clone> [<stanpumpR repo>]")
libDir <- normalizePath(args[1], mustWork = TRUE)
repo <- normalizePath(if (length(args) >= 2) args[2] else ".", mustWork = TRUE)
scriptDir <- dirname(normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
source(file.path(scriptDir, "crossCheckHelpers.R"))
outDir <- file.path(repo, "docs", "nlmixr2lib")
dir.create(outDir, showWarnings = FALSE, recursive = TRUE)

# A git that fails returns character(0) with a warning, not an error, and an
# empty value would drop the whole provenance line from summary.md.
gitOne <- function(dir, format) {
  out <- tryCatch(suppressWarnings(system2("git", c("-C", shQuote(dir), "log", "-1",
                                                   shQuote(format)), stdout = TRUE,
                                           stderr = FALSE)),
                  error = function(e) character())
  if (length(out) && nzchar(out[1])) out[1] else "?"
}
libCommit <- gitOne(libDir, "--format=%h %cs")
libVersion <- tryCatch(read.dcf(file.path(libDir, "DESCRIPTION"), "Version")[1, 1],
                       error = function(e) NA_character_)

# ---- reading nlmixr2lib without running it -----------------------------------

# Evaluates a literal expression: numbers, strings, c(), list(), paste() and
# arithmetic, and the transforms the model files write a value with.  Anything
# else (a name, a call to anything not listed) is refused, so no code from the
# library is ever executed.
SAFE_FUNCTIONS <- list(
  c = c, list = list, paste = paste, paste0 = paste0, log = log, exp = exp,
  sqrt = sqrt, `-` = `-`, `+` = `+`, `*` = `*`, `/` = `/`, `^` = `^`,
  `(` = function(x) x, fixed = function(...) c(...), as.integer = as.integer,
  as.numeric = as.numeric, logit = function(p) log(p / (1 - p)),
  expit = function(x) 1 / (1 + exp(-x)), log10 = log10
)
safeEval <- function(expr) {
  if (is.null(expr) || is.atomic(expr)) return(expr)
  if (is.name(expr)) {
    name <- as.character(expr)
    if (name %in% c("TRUE", "FALSE", "T", "F")) return(name %in% c("TRUE", "T"))
    if (name %in% c("NA", "NULL")) return(NULL)
    if (name == "pi") return(pi)
    if (name == "Inf") return(Inf)
    stop("name ", name)
  }
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])[1]
    if (!fn %in% names(SAFE_FUNCTIONS)) stop("call ", fn)
    argList <- lapply(as.list(expr)[-1], function(a)
      if (missing(a)) NULL else safeEval(a))
    return(do.call(SAFE_FUNCTIONS[[fn]], argList))
  }
  stop("unsupported")
}
tryEval <- function(expr) tryCatch(safeEval(expr), error = function(e) NULL)

# The value an ini() assignment prints, and the value it means: log(127) is
# printed as 127 and means 127; fixed(0.1) is fixed at 0.1; a bare number in
# a log-named parameter is left as written (the transform is not knowable).
iniValue <- function(expr) {
  fixed <- FALSE
  if (is.call(expr) && identical(as.character(expr[[1]])[1], "fixed")) {
    fixed <- TRUE
    if (length(expr) >= 2) expr <- expr[[2]]
  }
  # A bounded estimate is c(lower, estimate, upper) or c(lower, estimate)
  # (kga <- c(0, 1.08, 10)); the estimate is the second argument, and may
  # itself be log(...).
  if (is.call(expr) && identical(as.character(expr[[1]])[1], "c") &&
      length(expr) %in% c(3, 4))
    expr <- expr[[3]]
  transform <- "none"
  printed <- NULL
  if (is.call(expr) && as.character(expr[[1]])[1] %in% c("log", "logit") &&
      length(expr) == 2) {
    transform <- as.character(expr[[1]])[1]
    printed <- tryEval(expr[[2]])
  } else {
    printed <- tryEval(expr)
  }
  if (!is.numeric(printed) || length(printed) != 1) printed <- NA_real_
  list(printed = as.numeric(printed), transform = transform, fixed = fixed)
}

# One model file: its reference, population and ini() estimates.
readModelFile <- function(path) {
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
  if (is.null(exprs) || !length(exprs)) return(NULL)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  fun <- exprs[[1]]
  if (!is.call(fun) || length(fun) < 3) return(NULL)
  fnDef <- fun[[3]]
  if (!is.call(fnDef) || !identical(as.character(fnDef[[1]]), "function")) return(NULL)
  body <- fnDef[[3]]
  fields <- list()
  iniBlock <- NULL
  for (stmt in as.list(body)[-1]) {
    if (is.call(stmt) && as.character(stmt[[1]])[1] %in% c("<-", "=") &&
        is.name(stmt[[2]])) {
      fields[[as.character(stmt[[2]])]] <- tryEval(stmt[[3]])
    } else if (is.call(stmt) && identical(as.character(stmt[[1]])[1], "ini")) {
      iniBlock <- stmt[[2]]
    }
  }
  params <- NULL
  if (!is.null(iniBlock) && is.call(iniBlock) &&
      identical(as.character(iniBlock[[1]]), "{")) {
    refs <- attr(iniBlock, "srcref")
    stmts <- as.list(iniBlock)[-1]
    rows <- list()
    for (i in seq_along(stmts)) {
      s <- stmts[[i]]
      if (!is.call(s)) next
      op <- as.character(s[[1]])[1]
      lineNo <- if (!is.null(refs) && length(refs) >= i + 1) refs[[i + 1]][1] else NA
      text <- if (is.na(lineNo)) "" else lines[lineNo]
      label <- sub('.*label\\("([^"]*)"\\).*', "\\1", text)
      if (identical(label, text)) label <- ""
      comment <- if (grepl("#", text)) trimws(sub("^[^#]*#", "", text)) else ""
      if (op %in% c("<-", "=") && is.name(s[[2]])) {
        v <- iniValue(s[[3]])
        rows[[length(rows) + 1]] <- data.frame(
          parameter = as.character(s[[2]]), kind = "estimate",
          printed = v$printed, transform = v$transform, fixed = v$fixed,
          label = label, source = comment, line = lineNo, stringsAsFactors = FALSE)
      } else if (op == "~" && length(s) == 3) {
        v <- tryEval(s[[3]])
        if (is.call(s[[3]]) && identical(as.character(s[[3]][[1]])[1], "fixed"))
          v <- tryEval(s[[3]][[2]])
        if (is.numeric(v) && length(v) == 1)
          rows[[length(rows) + 1]] <- data.frame(
            parameter = paste(deparse(s[[2]]), collapse = ""), kind = "variability",
            printed = v, transform = "none", fixed = FALSE, label = label,
            source = comment, line = lineNo, stringsAsFactors = FALSE)
      }
    }
    if (length(rows)) params <- do.call(rbind, rows)
  }
  list(fields = fields, params = params)
}

asText <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- unlist(x)
  if (!length(x)) return(NA_character_)
  paste(as.character(x), collapse = "; ")
}

# ---- read both sides ---------------------------------------------------------

modelDir <- file.path(libDir, "inst", "modeldb", "specificDrugs")
files <- sort(list.files(modelDir, pattern = "\\.R$", full.names = TRUE))
message("reading ", length(files), " nlmixr2lib models from ", modelDir)

models <- list()
paramRows <- list()
for (f in files) {
  stem <- sub("\\.R$", "", basename(f))
  m <- readModelFile(f)
  parts <- strsplit(stem, "_")[[1]]
  yearAt <- which(grepl("^[0-9]{4}[a-z]?$", parts))[1]
  author <- if (is.na(yearAt)) parts[1] else paste(parts[seq_len(yearAt - 1)], collapse = " ")
  year <- if (is.na(yearAt)) NA_character_ else parts[yearAt]
  rest <- if (is.na(yearAt)) parts[-1] else parts[-seq_len(yearAt)]
  reference <- if (is.null(m)) NA_character_ else asText(m$fields$reference)
  doi <- if (is.na(reference)) NA_character_ else
    regmatches(reference, regexpr("10\\.[0-9]{4,9}/[^[:space:]\"]+", reference))
  if (!length(doi)) doi <- NA_character_
  pmid <- if (is.na(reference)) character() else
    regmatches(reference, regexpr("(?i)PMID:?\\s*[0-9]+", reference, perl = TRUE))
  pmid <- if (length(pmid)) gsub("[^0-9]", "", pmid) else NA_character_
  pop <- if (is.null(m)) list() else m$fields$population
  if (!is.list(pop)) pop <- list()
  models[[stem]] <- data.frame(
    model = stem, author = author, year = year,
    drugTokens = paste(rest, collapse = "_"),
    reference = reference, doi = normaliseDoi(doi), pmid = pmid,
    species = asText(pop$species), nSubjects = asText(pop$n_subjects),
    ageRange = asText(pop$age_range), weightRange = asText(pop$weight_range),
    disease = substr(asText(pop$disease_state), 1, 300),
    description = substr(asText(if (is.null(m)) NULL else m$fields$description), 1, 300),
    nParameters = if (is.null(m$params)) 0L else nrow(m$params),
    stringsAsFactors = FALSE)
  if (!is.null(m$params)) paramRows[[stem]] <- cbind(model = stem, m$params)
}
models <- do.call(rbind, models)
params <- do.call(rbind, paramRows)
rownames(models) <- NULL
rownames(params) <- NULL

cards <- readDigestCards(repo)
library <- readLibraryDrugs(repo)
keys <- libraryDrugKeys(library$drug)
registries <- readRegistryText(repo)

# ---- drug matching -----------------------------------------------------------

# A model matches a library drug when one of its file-name tokens after the
# year, or two adjacent tokens run together (tranexamic_acid), is a key of a
# library drug.
matchLibraryDrug <- function(tokens) {
  tok <- strsplit(tokens, "_")[[1]]
  candidates <- c(tok, if (length(tok) > 1) paste0(tok[-length(tok)], tok[-1]))
  candidates <- normaliseDrugToken(candidates)
  hit <- keys$drug[keys$key %in% candidates]
  if (length(hit)) paste(unique(hit), collapse = ";") else NA_character_
}
models$libraryDrug <- vapply(models$drugTokens, matchLibraryDrug, character(1))
models$isHuman <- is.na(models$species) | grepl("human", tolower(models$species))

# ---- matching papers ---------------------------------------------------------

cardByDoi <- cards[!is.na(cards$doi), ]
cardByPmid <- cards[!is.na(cards$pmid), ]
models$card <- NA_character_
for (i in seq_len(nrow(models))) {
  hit <- character()
  if (!is.na(models$doi[i])) hit <- cardByDoi$stem[cardByDoi$doi == models$doi[i]]
  if (!length(hit) && !is.na(models$pmid[i])) hit <- cardByPmid$stem[cardByPmid$pmid == models$pmid[i]]
  if (length(hit)) models$card[i] <- hit[1]
}

# A paper the model code cites by DOI but which has no digest card is still the
# source (or a stated cross-check) of a model stanpumpR runs, and its numbers
# can be compared with that code even with no card to compare them with.
# Found 2026-11-23: 24 such papers, piperacillin's own source (Kong 2025) among
# them, had never been compared, because only carded papers were.
codeFilesAll <- list.files(file.path(repo, "R"), pattern = "\\.R$", full.names = TRUE)
codeTextLower <- vapply(codeFilesAll, function(f)
  tolower(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")), character(1))
# Each path relative to the repository, cut literally: the checkout's own
# name may hold characters a regular expression reads (CodeRabbit on #851).
codeFilesRelative <- substring(codeFilesAll, nchar(file.path(repo, "")) + 1L)
# A DOI is matched whole: what follows it may not continue a DOI (a letter
# or digit, or one of . - _ / before one), so 10.1234/model.1 does not match
# a file citing only 10.1234/model.10 or 10.1234/model.1.2, while a full stop
# ending a sentence still ends it (CodeRabbit on #851).
citesDoi <- function(text, doi) {
  at <- gregexpr(doi, text, fixed = TRUE)[[1]]
  if (at[1] < 0) return(FALSE)
  after <- substring(text, at + nchar(doi), at + nchar(doi) + 1L)
  any(!grepl("^([0-9a-z]|[-._/][0-9a-z])", after))
}
models$citedIn <- NA_character_
for (i in which(is.na(models$card) & !is.na(models$doi))) {
  where <- vapply(codeTextLower, citesDoi, logical(1), doi = tolower(models$doi[i]))
  if (any(where))
    models$citedIn[i] <- paste(codeFilesRelative[where], collapse = ";")
}

# ---- 1. shared papers: do the numbers agree? -----------------------------------

readText <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return("")
  paste(vapply(paths, function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"),
                                        collapse = "\n"), character(1)), collapse = "\n")
}

shared <- models[!is.na(models$card) | !is.na(models$citedIn), ]
sharedParams <- list()
sharedSummary <- list()
for (i in seq_len(nrow(shared))) {
  if (!is.na(shared$card[i])) {
    card <- cards[cards$stem == shared$card[i], ]
    cardNumbers <- numbersIn(readText(file.path(repo, "papers", "digests", paste0(card$stem, ".md"))))
    codeFiles <- if (nzchar(card$codeFiles)) strsplit(card$codeFiles, ";")[[1]] else character()
  } else {
    # Cited in the code, no card: compared with the files that cite it.
    card <- data.frame(stem = "", citation = paste0("doi:", shared$doi[i]), stringsAsFactors = FALSE)
    cardNumbers <- numeric()
    codeFiles <- strsplit(shared$citedIn[i], ";")[[1]]
  }
  # Every R file the card names, not only R/drugs_*: a card names a file
  # because the model's numbers live there, and several models keep them
  # outside a drugs_ file (R/insulinTham.R builds the Tham insulins,
  # R/antibioticMbc.R holds the MIC and MBC tables).  A large shared file can
  # match a number by coincidence and hide a lead, which is why a match here
  # is checked on the page before it is trusted.
  codeFiles <- codeFiles[grepl("^R/.+\\.R$", codeFiles)]
  codeNumbers <- numbersIn(readText(file.path(repo, codeFiles)))
  p <- params[params$model == shared$model[i] & params$kind == "estimate", , drop = FALSE]
  if (!nrow(p)) next
  # Residual error is a property of the fit, not of the drug, and stanpumpR
  # carries it for only a few models, so it is reported but never a lead.
  # (propSdPhase1, the edoxaban model's residual for its phase 1 data, has a
  # suffix after Sd.)
  p$residual <- grepl("(^|_)(prop|add|exp|log)Sd|^sigma|Sd_|^lprop|^ladd",
                      p$parameter)
  findIn <- function(numbers) vapply(seq_len(nrow(p)), function(k) {
    hit <- matchValue(p$printed[k], numbers)
    # A logit-named parameter (logitfdepot) is printed by stanpumpR as the
    # fraction itself.  nlmixr2lib writes it as log(p/(1-p)), whose printed
    # argument is the odds, or as a bare log-odds; logit(p) is read as p
    # already.
    if (!nzchar(hit) && grepl("^logit", p$parameter[k]) && is.finite(p$printed[k])) {
      fraction <- switch(p$transform[k],
                         log = p$printed[k] / (1 + p$printed[k]),
                         none = 1 / (1 + exp(-p$printed[k])),
                         NA_real_)
      hit <- matchValue(fraction, numbers)
      if (nzchar(hit)) hit <- paste0("fraction ", hit)
    }
    # A bare number on a log-named parameter (lcl <- 4.84) may be the log of
    # what stanpumpR printed.
    if (!nzchar(hit) && p$transform[k] == "none" && grepl("^l", p$parameter[k]) &&
        is.finite(p$printed[k]) && abs(p$printed[k]) < 20) {
      hit <- matchValue(exp(p$printed[k]), numbers)
      if (nzchar(hit)) hit <- paste0("exp ", hit)
    }
    hit
  }, character(1))
  p$inCard <- findIn(cardNumbers)
  p$inCode <- findIn(codeNumbers)
  p$card <- card$stem
  p$codeFiles <- paste(codeFiles, collapse = ";")
  sharedParams[[i]] <- p
  readable <- !is.na(p$printed) & !p$residual
  lead <- readable & !nzchar(p$inCard) & !nzchar(p$inCode)
  # A match only after a unit factor is weaker: a card prints hundreds of
  # numbers, and some agree with x100 or /1000 of a value by coincidence.
  exact <- grepl("exact$", p$inCard) | grepl("exact$", p$inCode)
  converted <- readable & !lead & !exact
  sharedSummary[[i]] <- data.frame(
    model = shared$model[i], card = card$stem, citation = card$citation,
    libraryDrug = shared$libraryDrug[i], codeFiles = paste(codeFiles, collapse = ";"),
    estimates = sum(readable),
    foundInCard = sum(readable & nzchar(p$inCard)),
    foundInCode = sum(readable & nzchar(p$inCode)),
    foundInNeither = sum(lead),
    foundOnlyConverted = sum(converted),
    residualTerms = sum(p$residual),
    notFound = paste(p$parameter[lead], collapse = "; "),
    onlyConverted = paste(p$parameter[converted], collapse = "; "),
    stringsAsFactors = FALSE)
}
sharedParams <- do.call(rbind, sharedParams)
sharedSummary <- do.call(rbind, sharedSummary)
if (!is.null(sharedSummary)) {
  # A paper often holds several models (a reference fit, a pooled fit, one per
  # formulation), and stanpumpR implements at most one of them.  The model of
  # each paper whose numbers stanpumpR prints most of is the one to compare;
  # its siblings are listed apart, where an unmatched number usually means
  # only that stanpumpR did not take that model.
  share <- (sharedSummary$estimates - sharedSummary$foundInNeither) /
    pmax(sharedSummary$estimates, 1)
  sharedSummary$matchedShare <- round(share, 3)
  paperKey <- ifelse(nzchar(sharedSummary$card), sharedSummary$card, sharedSummary$citation)
  best <- ave(share, paperKey, FUN = function(x) seq_along(x) == which.max(x))
  sharedSummary$closestModelOfPaper <- as.logical(best)
  sharedSummary$implementedDrug <- !is.na(sharedSummary$libraryDrug) &
    grepl("R/drugs_", sharedSummary$codeFiles)
  sharedSummary <- sharedSummary[order(!sharedSummary$closestModelOfPaper,
                                       !sharedSummary$implementedDrug,
                                       sharedSummary$matchedShare, sharedSummary$model), ]
}

# ---- 2. papers not considered, for drugs the library has -------------------------

candidatePapers <- models[!is.na(models$libraryDrug) & is.na(models$card) & models$isHuman,
                          c("libraryDrug", "model", "author", "year", "nSubjects",
                            "ageRange", "weightRange", "disease", "doi", "pmid", "reference")]
nNum <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", candidatePapers$nSubjects)))
candidatePapers <- candidatePapers[order(candidatePapers$libraryDrug, -ifelse(is.na(nNum), 0, nNum)), ]

# ---- 3. drugs not in the library ---------------------------------------------------

outside <- models[is.na(models$libraryDrug) & models$isHuman, ]
# The drug is the first token, joined to the second where that is the rest of
# its name (mycophenolic_acid, tranexamic_acid, factor_viii).
NAME_CONTINUATIONS <- c("acid", "sodium", "potassium", "alfa", "beta", "viii", "ix",
                        "vii", "viia", "xiii", "hydrochloride", "mesylate", "phosphate")
outside$drug <- vapply(strsplit(outside$drugTokens, "_"), function(t) {
  if (!length(t)) return(NA_character_)
  if (length(t) > 1 && tolower(t[2]) %in% NAME_CONTINUATIONS) t <- paste0(t[1], t[2])
  normaliseDrugToken(t[1])
}, character(1))
outside <- outside[!is.na(outside$drug) & nzchar(outside$drug), ]
nOut <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", outside$nSubjects)))
outside$n <- ifelse(is.na(nOut), 0, nOut)
candidateDrugs <- do.call(rbind, lapply(split(outside, outside$drug), function(d) {
  best <- d[which.max(d$n), ]
  inRegistry <- names(registries)[vapply(registries, function(txt)
    grepl(paste0("\\b", d$drug[1], "\\b"), txt, perl = TRUE), logical(1))]
  data.frame(
    drug = d$drug[1], models = nrow(d), largestStudyN = best$n,
    largestStudy = best$model, largestStudyReference = best$reference,
    alsoDigested = sum(!is.na(d$card)),
    mentionedIn = paste(sub("\\.csv$", "", inRegistry), collapse = ";"),
    allModels = paste(d$model, collapse = "; "),
    stringsAsFactors = FALSE)
}))
candidateDrugs <- candidateDrugs[order(-candidateDrugs$models, candidateDrugs$drug), ]
# A component given only in a combination product is not a drug of its own
# (Steven L. Shafer, 2026-11-18 and 2026-11-23: tazobactam, clavulanate).  The
# register is docs/library-expansion/combination-only.csv, written by hand from
# DailyMed; a component listed there is flagged, not dropped.
comboFile <- file.path(repo, "docs", "library-expansion", "combination-only.csv")
combo <- if (file.exists(comboFile))
  utils::read.csv(comboFile, stringsAsFactors = FALSE) else
    data.frame(component = character(), nlmixr2libNames = character(), status = character())
comboNames <- data.frame(
  name = unlist(strsplit(combo$nlmixr2libNames, ";")),
  status = rep(combo$status, lengths(strsplit(combo$nlmixr2libNames, ";"))),
  stringsAsFactors = FALSE)
candidateDrugs$combinationOnly <- comboNames$status[match(candidateDrugs$drug, comboNames$name)]
# A component the register keeps as a drug of its own (ritonavir, 2026-11-23)
# is a candidate like any other.
candidateDrugs$combinationOnly[is.na(candidateDrugs$combinationOnly) |
                                 candidateDrugs$combinationOnly == "included"] <- ""

# ---- write ---------------------------------------------------------------------

writeCsv <- function(x, name) {
  utils::write.csv(x, file.path(outDir, name), row.names = FALSE, na = "")
  message("wrote ", file.path("docs", "nlmixr2lib", name), " (", NROW(x), " rows)")
}
writeCsv(sharedSummary, "shared-papers.csv")
writeCsv(sharedParams[, c("model", "card", "parameter", "label", "printed", "transform",
                          "fixed", "residual", "source", "inCard", "inCode", "codeFiles")],
         "shared-parameters.csv")
writeCsv(candidatePapers, "candidate-papers.csv")
writeCsv(candidateDrugs, "candidate-drugs.csv")
# One row per model of a drug not in the Drug Library, for choosing sources
# (data-raw/libraryExpansionCandidates.R reads it).
writeCsv(outside[order(outside$drug, -outside$n),
                 c("drug", "model", "author", "year", "nSubjects", "ageRange", "weightRange",
                   "disease", "doi", "pmid", "card", "reference")],
         "models-outside-library.csv")

# The summary the README links to.
mdTable <- function(df, cols, n = nrow(df)) {
  df <- utils::head(df[, cols, drop = FALSE], n)
  esc <- function(x) gsub("\\|", "/", ifelse(is.na(x), "", as.character(x)))
  c(paste0("| ", paste(cols, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(esc(r), collapse = " | "), " |")))
}
human <- sum(models$isHuman)
closest <- sharedSummary[sharedSummary$closestModelOfPaper, ]
leads <- closest[closest$implementedDrug & closest$foundInNeither > 0, ]
leads <- leads[order(leads$matchedShare, leads$model), ]
summaryLines <- c(
  "# nlmixr2lib cross-check: summary",
  "",
  sprintf("Generated by `data-raw/nlmixr2libCrossCheck.R` against nlmixr2lib %s (commit %s) and stanpumpR %s.",
          libVersion, libCommit,
          gitOne(repo, "--format=%h")),
  "Do not edit by hand; rerun the script.  What the columns mean, and the acknowledgement, are in [README.md](README.md).",
  "",
  "## Counts",
  "",
  sprintf("- nlmixr2lib models read: %d (%d with a reference DOI; %d in humans or with no species given).",
          nrow(models), sum(!is.na(models$doi)), human),
  sprintf("- Models of a drug stanpumpR's Drug Library holds: %d, of %d library drugs.",
          sum(!is.na(models$libraryDrug)), length(unique(unlist(strsplit(na.omit(models$libraryDrug), ";"))))),
  sprintf("- Shared papers (a nlmixr2lib model whose paper has a stanpumpR digest card, or which stanpumpR's code cites by DOI): %d, %d of them cited with no card.",
          NROW(sharedSummary), sum(!nzchar(sharedSummary$card))),
  sprintf("- Of their %d estimates (residual error left out), found in stanpumpR's card or code: %d; in neither: %d.",
          sum(sharedSummary$estimates), sum(sharedSummary$estimates - sharedSummary$foundInNeither),
          sum(sharedSummary$foundInNeither)),
  sprintf("- Of those found, %d only after a unit factor (x60, /1000, x100 ...), a weaker match: some are coincidences.",
          sum(sharedSummary$foundOnlyConverted)),
  sprintf("- For the model of each paper closest to stanpumpR's, where the drug is implemented: %d papers, %d of %d estimates found.",
          sum(closest$implementedDrug), sum((closest$estimates - closest$foundInNeither)[closest$implementedDrug]),
          sum(closest$estimates[closest$implementedDrug])),
  sprintf("- Papers on drugs stanpumpR has, with no digest card: %d (candidate-papers.csv).",
          nrow(candidatePapers)),
  sprintf("- Drugs nlmixr2lib models in humans that stanpumpR's Drug Library does not hold: %d (candidate-drugs.csv).",
          nrow(candidateDrugs)),
  "",
  "## Leads: the closest model of each paper behind an implemented drug, with numbers unmatched",
  "",
  "Each row is the nlmixr2lib model of a shared paper whose numbers stanpumpR prints most of, for a",
  "drug stanpumpR implements.  An unmatched number is a lead to check in the PDF, not an error: the",
  "libraries may parameterise differently (a rate constant against a clearance, a reference patient,",
  "a unit stanpumpR converted by a factor not tried), or stanpumpR may have applied the number only",
  "through a covariate it does not carry.  Residual-error terms are left out.  Every row of every",
  "model, siblings included, is in shared-papers.csv and shared-parameters.csv.",
  "",
  mdTable(leads, c("model", "card", "libraryDrug", "estimates", "foundInCard",
                   "foundInCode", "foundInNeither", "notFound", "onlyConverted")),
  "",
  "## Shared papers behind no implemented drug",
  "",
  "Read for stanpumpR (a digest card exists) but not the source of a model it runs, or its drug not in",
  "the library: the numbers can only be checked against the card.",
  "",
  mdTable(closest[!closest$implementedDrug, ], c("model", "card", "estimates", "foundInCard", "foundInNeither")),
  "",
  "## Drugs not in the Drug Library with the most nlmixr2lib models (first 40)",
  "",
  mdTable(candidateDrugs, c("drug", "models", "largestStudyN", "largestStudy", "mentionedIn"), 40)
)
writeLines(summaryLines, file.path(outDir, "summary.md"), useBytes = TRUE)
message("wrote docs/nlmixr2lib/summary.md")

# Every drug nlmixr2lib models in humans that the Drug Library lacks, largest
# study first, for reading by eye; the combination-only components apart.
missing <- candidateDrugs[order(-ifelse(is.na(candidateDrugs$largestStudyN), 0,
                                        candidateDrugs$largestStudyN), candidateDrugs$drug), ]
missingLines <- c(
  "# Drugs nlmixr2lib models that stanpumpR does not have",
  "",
  sprintf("Generated by `data-raw/nlmixr2libCrossCheck.R` against nlmixr2lib %s (commit %s).  Do not edit by hand.",
          libVersion, libCommit),
  "",
  "One row per drug nlmixr2lib models in humans that stanpumpR's Drug Library does not hold, largest study first.",
  "A drug's name is nlmixr2lib's file-name token, so a combination can appear under each component.",
  "`n` is the largest study's number of subjects as nlmixr2lib records it (a simulation's virtual subjects included).",
  "Components that are given only in a combination product are listed apart, from",
  "`docs/library-expansion/combination-only.csv`: they are not drugs of their own in stanpumpR.",
  "Whether a drug is wanted is decided in `docs/library-expansion/plan.csv`.",
  "",
  sprintf("## Candidates (%d)", sum(!nzchar(missing$combinationOnly))),
  "",
  mdTable(missing[!nzchar(missing$combinationOnly), ],
          c("drug", "models", "largestStudyN", "largestStudy", "mentionedIn")),
  "",
  sprintf("## Combination-only components (%d)", sum(nzchar(missing$combinationOnly))),
  "",
  "`not_included`: no single-ingredient US product, so not a drug of its own. `decision`: labelled alone but useful only with another drug; Shafer decides.",
  "",
  mdTable(missing[nzchar(missing$combinationOnly), ],
          c("drug", "combinationOnly", "models", "largestStudyN", "largestStudy"))
)
writeLines(missingLines, file.path(outDir, "missing-drugs.md"), useBytes = TRUE)
message("wrote docs/nlmixr2lib/missing-drugs.md")
