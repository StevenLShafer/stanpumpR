# Helpers shared by data-raw/nlmixr2libCrossCheck.R and
# data-raw/independentExtractionCompare.R.
#
# Base R only, so they run with `Rscript --vanilla` from any directory and need
# nothing from the package or renv.  They read stanpumpR's own files (the
# digest cards, the model code, the Drug Library) and compare NUMBERS: a value
# transcribed elsewhere from a paper is looked for among the numbers stanpumpR
# itself printed from the same paper.

# ---- stanpumpR's side ------------------------------------------------------

# The front matter of every digest card in papers/digests: one row per card.
readDigestCards <- function(repo) {
  files <- list.files(file.path(repo, "papers", "digests"), pattern = "\\.md$",
                      full.names = TRUE)
  files <- files[basename(files) != "README.md"]
  rows <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    bounds <- which(lines == "---")
    front <- if (length(bounds) >= 2) lines[(bounds[1] + 1):(bounds[2] - 1)] else character()
    field <- function(name) {
      hit <- grep(paste0("^", name, ":"), front, value = TRUE)
      if (!length(hit)) return(NA_character_)
      value <- trimws(sub(paste0("^", name, ":"), "", hit[1]))
      value <- gsub('^"|"$', "", value)
      if (!nzchar(value) || value %in% c("null", "~")) NA_character_ else value
    }
    drugs <- field("drugs")
    drugs <- if (is.na(drugs)) character() else
      trimws(strsplit(gsub("^\\[|\\]$", "", drugs), ",")[[1]])
    body <- paste(lines, collapse = "\n")
    codeFiles <- unique(regmatches(body, gregexpr("R/[A-Za-z0-9_.-]+\\.R", body))[[1]])
    data.frame(
      stem = sub("\\.md$", "", basename(f)),
      file = field("file"),
      pmid = field("pmid"),
      doi = normaliseDoi(field("doi")),
      citation = field("citation"),
      kind = field("kind"),
      drugs = paste(drugs[nzchar(drugs)], collapse = ";"),
      codeFiles = paste(codeFiles, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

normaliseDoi <- function(doi) {
  doi <- tolower(trimws(doi))
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi)
  doi <- sub("^doi:\\s*", "", doi)
  doi <- sub("[.,;)]+$", "", doi)
  ifelse(is.na(doi) | !nzchar(doi), NA_character_, doi)
}

# Every drug in the Drug Library, with the names it can be matched by.
readLibraryDrugs <- function(repo) {
  csv <- utils::read.csv(file.path(repo, "inst", "extdata", "drugDefaults_global.csv"),
                         stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    drug = csv$Drug,
    doseable = !is.na(csv$Category) & nzchar(csv$Category),
    category = ifelse(is.na(csv$Category), "", csv$Category),
    stringsAsFactors = FALSE
  )
}

# The text of every registry under inst/extdata, lower case, for a check of
# whether a drug the library does not dose is at least catalogued.
readRegistryText <- function(repo) {
  files <- list.files(file.path(repo, "inst", "extdata"),
                      pattern = "(Registry|Sources|Products)\\.csv$", full.names = TRUE)
  stats::setNames(vapply(files, function(f)
    tolower(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")),
    character(1)), basename(files))
}

# ---- drug names --------------------------------------------------------------

# Names used for one drug in different places.  The library's spelling is on
# the right.
DRUG_SYNONYMS <- c(
  paracetamol = "acetaminophen", adrenaline = "epinephrine",
  noradrenaline = "norepinephrine", meperidine = "pethidine",
  glibenclamide = "glyburide", ciclosporin = "cyclosporine",
  cyclosporin = "cyclosporine", cephalexin = "cefalexin",
  heroin = "diamorphine", `5fu` = "fluorouracil",
  lignocaine = "lidocaine", frusemide = "furosemide", salbutamol = "albuterol",
  valproicacid = "valproate", sodiumvalproate = "valproate",
  oxybate = "ghb", sodiumoxybate = "ghb", gammahydroxybutyrate = "ghb",
  ecstasy = "mdma", magnesium = "magnesiumsulfate", caffeine = "caffeineneonatal",
  unfractionatedheparin = "heparin", ufh = "heparin"
)

# The base names a library drug answers to: its own name in lower case, and
# the first word of a camel-case name (apixabanVTE -> apixaban,
# levodopaIntestinalGel -> levodopa), which is how the library names a second
# model of one drug.
libraryDrugKeys <- function(drugs) {
  full <- tolower(drugs)
  first <- tolower(sub("^([a-z0-9]+).*$", "\\1", drugs))
  unique(data.frame(drug = rep(drugs, 2), key = c(full, first),
                    stringsAsFactors = FALSE))
}

normaliseDrugToken <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]", "", x))
  ifelse(x %in% names(DRUG_SYNONYMS), DRUG_SYNONYMS[x], x)
}

# ---- numbers ---------------------------------------------------------------

# Every number written in a text, as numbers.  A thousands separator is not
# read (1,033 is two numbers), which errs toward a lead, never a false match.
numbersIn <- function(text) {
  tokens <- regmatches(text, gregexpr("(?<![A-Za-z_])-?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?",
                                      text, perl = TRUE))[[1]]
  values <- suppressWarnings(as.numeric(tokens))
  unique(values[is.finite(values)])
}

# Unit factors a value may have been printed under: none, minutes against
# hours, mass prefixes, and percent against a fraction.
MATCH_FACTORS <- c(exact = 1, `/60` = 1 / 60, `x60` = 60, `/1000` = 1 / 1000,
                   `x1000` = 1000, `/100` = 1 / 100, `x100` = 100)

# Where a value turns up among a set of numbers, and under which factor.
# Exact means equal to the printed digits of the shorter of the two; a
# converted match allows 0.5% (stanpumpR often prints a converted value
# rounded).  Returns "" when the value is not there.  A match is evidence that
# the number was transcribed, not proof that it sits in the right place: a
# small integer or 0.75 can match by coincidence.
matchValue <- function(value, numbers) {
  if (!is.finite(value) || !length(numbers)) return("")
  if (value == 0) return(if (any(numbers == 0)) "exact" else "")
  for (name in names(MATCH_FACTORS)) {
    target <- value * MATCH_FACTORS[[name]]
    tol <- if (name == "exact") 5e-7 else 5e-3
    if (any(abs(numbers - target) <= tol * abs(target) + 1e-12)) return(name)
  }
  ""
}
