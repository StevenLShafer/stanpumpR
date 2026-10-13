# -----------------------------------------------------------------------------
# The investigators: a registry of the people named in inst/help/investigators.md,
# links to them wherever they are named, and the pop-up that shows what of
# theirs the program uses
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code (Claude Fable 5.1), 2026-10-13, at the request of
# Steven L. Shafer.  Verified by tests/testthat/test-help-investigators.R.
#
# How it fits together
# --------------------
# inst/help/investigators.md names, in bold, every investigator whose work the
# program uses.  That file is the registry: helpInvestigatorRegistry() reads
# the bold names out of it, splits each into given names and surname, and
# gives each person a key (surname plus first initial, "eleveld-d").  Two
# spellings of one person ("Thomas Bouillon", "Thomas W. Bouillon") share a
# key and are one entry; two people with one surname ("Tae Kyun Kim",
# "Hyun Chul Kim") are two entries, and a bare "Kim" in the text is left
# alone because it could be either.
#
# helpLinkInvestigators() takes rendered HTML and wraps every mention of a
# registered investigator in <a class="investigator-link"
# data-investigator="KEY">.  It is run on every help page (helpPageUI()), on
# the citation strings the drug models return (citationItemHTML(), which the
# References panel under the plot and the drug pages use) and on the
# bibliography.  A mention is the surname with its given names or initials
# ("Douglas J. Eleveld", "Eleveld DJ", "Jim Philip"), or, when the surname
# belongs to one person only, the surname alone ("Eleveld's model").  Text
# already inside a link, or in code, is left alone.  Surnames in
# HELP_INVESTIGATORS_CITATION_ONLY (Shafer, who wrote the program and is
# named all through the help) are linked only in citations: context =
# "citation" is the bibliography and the citation strings, context = "prose"
# is everything else.  The same pass links every "Gas Man" to the Gas Man
# website (HELP_TERM_LINKS).
#
# Clicking a link sends the key to Shiny as input$investigator_show (one
# delegated handler in inst/www/app.js); helpServer() answers with
# helpInvestigatorModal(), whose body is every paragraph, list item and table
# row of investigators.md that names the person, grouped by section, followed
# by their entries in inst/help/references.md.
#
# Adding an investigator: write the name in bold in investigators.md.  Nothing
# else is needed; the tests check that every bold name parses and that each
# surname is either unique or distinguished by the first initial.
# -----------------------------------------------------------------------------

# Surnames linked only in citations, never in prose.
HELP_INVESTIGATORS_CITATION_ONLY <- "Shafer"

# Lower-case particles that begin a surname ("van Steveninck", "da Silva Neto",
# "De Smet"), matched without regard to case.
HELP_NAME_PARTICLES <- c("van", "von", "de", "da", "dos", "del", "della", "di",
                         "du", "la", "le", "ten", "ter")

# Generation suffixes, dropped from the surname ("Edmond I. Eger II").
HELP_NAME_SUFFIXES <- c("II", "III", "IV", "Jr", "Jr.", "Sr", "Sr.")

# Capitalised words that are never a given name, so that "The Schnider model"
# links "Schnider" and not "The Schnider".
HELP_NAME_STOPWORDS <- c(
  "The", "This", "That", "These", "Those", "There", "Then", "Thus", "Than",
  "A", "An", "And", "As", "At", "By", "For", "From", "If", "In", "Into", "Is",
  "It", "Its", "Of", "On", "Or", "So", "To", "With", "Without", "When",
  "Where", "While", "Which", "Who", "Whose", "Why", "But", "Both", "Because",
  "Before", "After", "Since", "Until", "See", "Also", "Although", "However",
  "Here", "How", "Are", "Was", "Were", "Not", "Nor", "Our", "Their", "His",
  "Her", "They", "We", "You", "Only", "Even", "Each", "Every", "Like", "Unlike",
  "Dr", "Drs", "Prof", "Professor", "Mr", "Mrs", "Ms", "Sir", "Lord", "Dame",
  "Saint", "St", "Team", "Group", "Lab", "Laboratory", "University", "Hospital",
  "Model", "Models", "Table", "Figure", "Page", "Section", "Chapter", "Reference"
)

# Terms linked to an external site wherever they appear in the help.
HELP_TERM_LINKS <- list(
  list(pattern = "Gas ?Man(?:\u00ae)?", href = "https://www.gasmanweb.com/")
)

# --- The registry -------------------------------------------------------------

#' Split a written name into given names and surname
#'
#' @param name "Douglas J. Eleveld", "M. J. J. da Silva Neto", "Edmond I. Eger II",
#'   "Liu"
#' @returns list(given = character, surname = character(1), initial = the first
#'   letter of the first given name, or NA when the name is a surname alone)
#' @noRd
helpSplitName <- function(name) {
  tokens <- strsplit(trimws(name), "\\s+")[[1]]
  while (length(tokens) > 1 && tokens[length(tokens)] %in% HELP_NAME_SUFFIXES) {
    tokens <- tokens[-length(tokens)]
  }
  start <- length(tokens)
  particles <- which(tolower(tokens) %in% HELP_NAME_PARTICLES)
  particles <- particles[particles > 1]
  if (length(particles) > 0) start <- min(particles)
  given <- if (start > 1) tokens[seq_len(start - 1)] else character(0)
  list(
    given = given,
    surname = paste(tokens[start:length(tokens)], collapse = " "),
    initial = if (length(given) > 0) toupper(substr(given[1], 1, 1)) else NA_character_
  )
}

#' An investigator's key: the surname and first initial as a slug
#'
#' Accents are folded to the plain letter ("Poyhia R" gives "poyhia-r") by
#' helpFoldAccents(), the same on every platform; iconv's transliteration
#' differs between glibc and macOS.
#' @noRd
helpInvestigatorKey <- function(surname, initial) {
  helpSlug(helpFoldAccents(paste(surname, if (!is.na(initial)) initial)))
}

#' Replace the accented Latin letters with their plain letter
#'
#' Covers the Latin-1 and the common Latin Extended-A letters.  Anything else
#' outside ASCII is left for helpSlug() to drop.
#' @noRd
helpFoldAccents <- function(x) {
  x <- enc2utf8(x)
  pairs <- c(
    "\u00e0a", "\u00e1a", "\u00e2a", "\u00e3a", "\u00e4a", "\u00e5a", "\u00e7c",
    "\u00e8e", "\u00e9e", "\u00eae", "\u00ebe", "\u00eci", "\u00edi", "\u00eei",
    "\u00efi", "\u00f1n", "\u00f2o", "\u00f3o", "\u00f4o", "\u00f5o", "\u00f6o",
    "\u00f8o", "\u00f9u", "\u00fau", "\u00fbu", "\u00fcu", "\u00fdy", "\u00ffy",
    "\u00c0A", "\u00c1A", "\u00c2A", "\u00c3A", "\u00c4A", "\u00c5A", "\u00c7C",
    "\u00c8E", "\u00c9E", "\u00caE", "\u00cbE", "\u00ccI", "\u00cdI", "\u00ceI",
    "\u00cfI", "\u00d1N", "\u00d2O", "\u00d3O", "\u00d4O", "\u00d5O", "\u00d6O",
    "\u00d8O", "\u00d9U", "\u00daU", "\u00dbU", "\u00dcU", "\u00ddY",
    "\u0107c", "\u010dc", "\u0111d", "\u0119e", "\u011fg", "\u0131i", "\u0142l",
    "\u0144n", "\u0151o", "\u0159r", "\u015bs", "\u015fs", "\u0161s", "\u0163t",
    "\u016fu", "\u0171u", "\u017az", "\u017cz", "\u017ez",
    "\u0106C", "\u010cC", "\u0110D", "\u0118E", "\u011eG", "\u0130I", "\u0141L",
    "\u0143N", "\u0150O", "\u0158R", "\u015aS", "\u015eS", "\u0160S", "\u0162T",
    "\u016eU", "\u0170U", "\u0179Z", "\u017bZ", "\u017dZ"
  )
  from <- paste(substr(pairs, 1, 1), collapse = "")
  to <- paste(substr(pairs, 2, 2), collapse = "")
  x <- gsub("\u00df", "ss", x)
  x <- gsub("\u00e6", "ae", x)
  x <- gsub("\u00c6", "AE", x)
  x <- gsub("\u0153", "oe", x)
  x <- gsub("\u0152", "OE", x)
  chartr(from, to, x)
}

#' The bold names in a Markdown text
#' @noRd
helpBoldNames <- function(text) {
  m <- regmatches(text, gregexpr("\\*\\*[^*\n]+\\*\\*", text))[[1]]
  trimws(gsub("^\\*\\*|\\*\\*$", "", m))
}

#' Does a bold token look like a person's name?
#'
#' One to six words, each starting with a letter, none all lower case except a
#' particle.  "Gas Man" would pass; the registry is read only from
#' investigators.md, where every bold token is a name.
#' @noRd
helpLooksLikeName <- function(x) {
  tokens <- strsplit(trimws(x), "\\s+")[[1]]
  if (length(tokens) < 1 || length(tokens) > 6) return(FALSE)
  all(grepl("^\\p{L}", tokens, perl = TRUE)) &&
    all(grepl("^\\p{Lu}", tokens, perl = TRUE) | tolower(tokens) %in% HELP_NAME_PARTICLES |
          tokens %in% HELP_NAME_SUFFIXES)
}

#' The investigators named in bold in a Markdown text
#'
#' @param text the Markdown of investigators.md
#' @returns a data frame, one row per person, in order of first mention: key,
#'   name (the longest spelling), surname, initial, ambiguous (another person
#'   shares the surname), citationOnly
#' @noRd
helpInvestigatorRegistry <- function(text = helpReadMarkdown("investigators")) {
  names <- helpBoldNames(text)
  names <- names[vapply(names, helpLooksLikeName, logical(1))]
  if (length(names) == 0) {
    return(data.frame(key = character(0), name = character(0), surname = character(0),
                      initial = character(0), ambiguous = logical(0),
                      citationOnly = logical(0), stringsAsFactors = FALSE))
  }
  parts <- lapply(names, helpSplitName)
  surname <- vapply(parts, `[[`, character(1), "surname")
  initial <- vapply(parts, `[[`, character(1), "initial")
  key <- vapply(seq_along(names), function(i) {
    helpInvestigatorKey(surname[i], initial[i])
  }, character(1))
  reg <- data.frame(key = key, name = names, surname = surname, initial = initial,
                    stringsAsFactors = FALSE)
  # One row per key, keeping the fullest spelling of the name
  reg <- reg[order(match(reg$key, unique(reg$key)), -nchar(reg$name)), ]
  reg <- reg[!duplicated(reg$key), ]
  reg$ambiguous <- reg$surname %in% reg$surname[duplicated(reg$surname)]
  reg$citationOnly <- reg$surname %in% HELP_INVESTIGATORS_CITATION_ONLY
  rownames(reg) <- NULL
  reg
}

# --- Matching a name in text --------------------------------------------------

#' The regular expression for a surname
#'
#' A particle may be capitalised at the start of a sentence or a citation
#' ("Van Steveninck AL").
#' @noRd
helpSurnameRegex <- function(surname) {
  tokens <- strsplit(surname, " ")[[1]]
  tokens <- vapply(tokens, function(t) {
    if (tolower(t) %in% HELP_NAME_PARTICLES) {
      first <- substr(t, 1, 1)
      paste0("[", toupper(first), tolower(first), "]", helpEscapeRegex(substring(t, 2)))
    } else {
      helpEscapeRegex(t)
    }
  }, character(1))
  paste(tokens, collapse = "\\s+")
}

#' The regular expressions that find one investigator in plain text
#'
#' @returns list(full = the name with given names or initials, which is the
#'   only form matched when the surname is shared; bare = the surname alone,
#'   or NULL when the name has no given names (then full is the bare form))
#' @noRd
helpNamePatterns <- function(surname, initial) {
  letters <- "[\\p{L}'.\\-]"
  before <- "(?<!\\p{L})"
  # Not followed by the end of a placeholder before the start of another: a
  # name already linked by an earlier pattern is not linked again inside
  # (helpLinkInvestigators()).
  after <- "(?!\\p{L})(?![^\\x01\\x03\\x04\\x06]*[\\x03\\x06])"
  suffix <- "(?:\\s+(?:II|III|IV|Jr\\.?|Sr\\.?))?"
  sur <- helpSurnameRegex(surname)
  bare <- paste0(before, sur, suffix, after)
  if (is.na(initial)) return(list(full = bare, bare = NULL))
  stop <- paste0("(?!(?:", paste(HELP_NAME_STOPWORDS, collapse = "|"), ")(?!\\p{L}))")
  # A given name starts with the initial and is not an acronym: "Douglas",
  # "D.", "Jung-Ryul"; up to three more given names or initials may follow
  # ("Michel M. R. F. Struys").
  given <- paste0(stop, initial, "(?![A-Z])", letters, "*",
                  "(?:\\s+[A-Z](?![A-Z])", letters, "*){0,3}\\s+")
  initials <- paste0("\\s+", initial, "[A-Z]{0,3}")
  full <- paste0(before, "(?:", given, sur, suffix, "|", sur, initials, ")", after)
  list(full = full, bare = bare)
}

#' Does a plain text name this investigator?
#' @noRd
helpMentions <- function(text, inv) {
  p <- helpNamePatterns(inv$surname, inv$initial)
  grepl(p$full, text, perl = TRUE) ||
    (!inv$ambiguous && !is.null(p$bare) && grepl(p$bare, text, perl = TRUE))
}

#' Wrap every mention of a registered investigator in a link
#'
#' @param html rendered HTML (a string)
#' @param context "prose" or "citation"; the citation-only investigators are
#'   linked only in the latter
#' @param exclude keys not to link (the person whose own pop-up this is)
#' @param registry the investigator registry
#' @param terms the external term links; NULL for none
#' @returns the HTML with the links added
#' @noRd
helpLinkInvestigators <- function(html, context = c("prose", "citation"), exclude = NULL,
                                  registry = helpInvestigatorsCached()$registry,
                                  terms = HELP_TERM_LINKS) {
  context <- match.arg(context)
  if (is.null(html) || length(html) != 1 || is.na(html) || !nzchar(html)) return(html)
  html <- enc2utf8(html)

  # Tags and text, alternating; text inside a link or code is left alone
  tokens <- regmatches(html, gregexpr("<[^>]*>|[^<]+", html, perl = TRUE))[[1]]
  isTag <- startsWith(tokens, "<")
  skipTags <- "(a|code|pre|script|style)"
  opens <- grepl(paste0("^<", skipTags, "(\\s|>)"), tokens, perl = TRUE, ignore.case = TRUE)
  closes <- grepl(paste0("^</", skipTags, "\\s*>"), tokens, perl = TRUE, ignore.case = TRUE)
  depth <- cumsum(opens) - cumsum(closes)
  idx <- which(!isTag & depth <= 0)
  if (length(idx) == 0) return(html)
  text <- tokens[idx]

  reg <- registry
  if (context == "prose") reg <- reg[!reg$citationOnly, ]
  reg <- reg[!reg$key %in% exclude, ]
  # Only the surnames that occur at all are worth a regular expression
  lastWord <- sub(".* ", "", reg$surname)
  present <- vapply(lastWord, function(w) any(grepl(w, text, fixed = TRUE)), logical(1))
  reg <- reg[present, ]
  patterns <- lapply(seq_len(nrow(reg)), function(i) helpNamePatterns(reg$surname[i], reg$initial[i]))

  # Placeholders keep a match from being matched again by a later pattern;
  # \x01 key \x02 text \x03 is an investigator, \x04 href \x05 text \x06 a term.
  mark <- function(key) paste0("\x01", key, "\x02\\1\x03")
  # First the full forms (given names or initials), so that "James" in
  # "James R. Jacobs" is Jacobs's given name before it can be W. P. T. James.
  for (i in seq_len(nrow(reg))) {
    text <- gsub(paste0("(", patterns[[i]]$full, ")"), mark(reg$key[i]), text, perl = TRUE)
  }
  for (i in seq_len(nrow(reg))) {
    if (reg$ambiguous[i] || is.null(patterns[[i]]$bare)) next
    text <- gsub(paste0("(", patterns[[i]]$bare, ")"), mark(reg$key[i]), text, perl = TRUE)
  }
  for (term in terms) {
    text <- gsub(paste0("((?<!\\p{L})", term$pattern, "(?!\\p{L})(?![^\\x01\\x03\\x04\\x06]*[\\x03\\x06]))"),
                 paste0("\x04", term$href, "\x05\\1\x06"), text, perl = TRUE)
  }
  text <- gsub("\x01([^\x02]*)\x02([^\x03]*)\x03",
               '<a href="#" class="investigator-link" data-investigator="\\1">\\2</a>',
               text, perl = TRUE)
  text <- gsub("\x04([^\x05]*)\x05([^\x06]*)\x06",
               '<a href="\\1" class="help-term-link" target="_blank" rel="noreferrer">\\2</a>',
               text, perl = TRUE)
  tokens[idx] <- text
  paste(tokens, collapse = "")
}

# --- The blocks of investigators.md ------------------------------------------

#' The paragraphs, list items and table rows of investigators.md
#'
#' @returns a data frame: section (the ## heading), kind ("paragraph", "item"
#'   or "row"), drug and investigators (table rows only) and body, all as
#'   Markdown; and plain, the body without bold marks and link targets, for
#'   matching
#' @noRd
helpInvestigatorBlocks <- function(text = helpReadMarkdown("investigators")) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  blocks <- list()
  section <- ""
  para <- character(0)
  tableHeader <- TRUE
  add <- function(kind, body, drug = "", investigators = "") {
    blocks[[length(blocks) + 1]] <<- data.frame(
      section = section, kind = kind, drug = drug, investigators = investigators,
      body = body, stringsAsFactors = FALSE)
  }
  flush <- function() {
    if (length(para) > 0) add("paragraph", paste(para, collapse = " "))
    para <<- character(0)
  }
  for (line in lines) {
    if (grepl("^## ", line)) {
      flush()
      section <- trimws(sub("^## ", "", line))
      tableHeader <- TRUE
    } else if (grepl("^\\|", line)) {
      flush()
      cells <- trimws(strsplit(sub("\\|\\s*$", "", sub("^\\|", "", line)), "|", fixed = TRUE)[[1]])
      if (grepl("^\\|\\s*-", line)) next
      if (tableHeader) { tableHeader <- FALSE; next }
      if (length(cells) >= 3) {
        add("row", cells[3], drug = cells[1], investigators = cells[2])
      }
    } else if (grepl("^- ", line)) {
      flush()
      add("item", trimws(sub("^- ", "", line)))
    } else if (!nzchar(trimws(line))) {
      flush()
    } else {
      para <- c(para, trimws(line))
    }
  }
  flush()
  if (length(blocks) == 0) {
    return(data.frame(section = character(0), kind = character(0), drug = character(0),
                      investigators = character(0), body = character(0), plain = character(0),
                      stringsAsFactors = FALSE))
  }
  blocks <- do.call(rbind, blocks)
  blocks$plain <- helpMarkdownPlain(paste(blocks$drug, blocks$investigators, blocks$body))
  rownames(blocks) <- NULL
  blocks
}

#' Markdown without bold marks and link targets, for matching names
#' @noRd
helpMarkdownPlain <- function(text) {
  text <- gsub("\\*\\*", "", text, fixed = FALSE)
  gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", text)
}

#' The paragraphs of the bibliography (inst/help/references.md)
#' @noRd
helpReferenceParagraphs <- function(text = helpReadMarkdown("references")) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines[nzchar(lines) & !grepl("^#", lines)]
}

#' Everything the help knows about the investigators, built once per session
#' @noRd
helpInvestigatorsData <- function() {
  list(
    registry = helpInvestigatorRegistry(),
    blocks = helpInvestigatorBlocks(),
    references = helpReferenceParagraphs()
  )
}

helpInvestigatorsCached <- function() {
  if (is.null(.sprglobals$helpInvestigators)) {
    .sprglobals$helpInvestigators <- helpInvestigatorsData()
  }
  .sprglobals$helpInvestigators
}

helpInvestigatorByKey <- function(key, registry = helpInvestigatorsCached()$registry) {
  if (!is.character(key) || length(key) != 1 || is.na(key)) return(NULL)
  row <- registry[registry$key == key, ]
  if (nrow(row) == 0) return(NULL)
  row[1, ]
}

# --- The pop-up ---------------------------------------------------------------

#' The HTML body of an investigator's pop-up
#'
#' Every block of investigators.md that names the person, grouped by section
#' and in the file's order, then their entries in the bibliography.  Other
#' investigators named in it are linked, so the pop-ups lead to one another.
#' @noRd
helpInvestigatorHTML <- function(inv, data = helpInvestigatorsCached()) {
  blocks <- data$blocks
  hit <- vapply(blocks$plain, function(p) helpMentions(p, inv), logical(1), USE.NAMES = FALSE)
  blocks <- blocks[hit, ]
  out <- character(0)
  for (section in unique(blocks$section)) {
    rows <- blocks[blocks$section == section, ]
    md <- vapply(seq_len(nrow(rows)), function(i) {
      if (rows$kind[i] == "row") {
        paste0("**", rows$drug[i], "** \u00b7 ", rows$investigators[i], "  \n", rows$body[i])
      } else {
        rows$body[i]
      }
    }, character(1))
    out <- c(out,
             sprintf("<h6>%s</h6>", htmltools::htmlEscape(section)),
             helpLinkInvestigators(helpMarkdownToHTML(paste(md, collapse = "\n\n")),
                                   "prose", exclude = inv$key, registry = data$registry))
  }
  refs <- data$references[vapply(data$references, function(r) helpMentions(r, inv),
                                 logical(1), USE.NAMES = FALSE)]
  if (length(refs) > 0) {
    out <- c(out,
             "<h6>In the bibliography</h6>",
             helpLinkInvestigators(helpMarkdownToHTML(paste(refs, collapse = "\n\n")),
                                   "citation", exclude = inv$key, registry = data$registry))
  }
  if (length(out) == 0) {
    out <- sprintf("<p>%s is named on the investigators page.</p>", htmltools::htmlEscape(inv$name))
  }
  paste(out, collapse = "\n")
}

#' The modal dialog for an investigator, or NULL for an unknown key
#'
#' The footer's link to the investigators page is an ordinary help link;
#' app.js closes the modal when a help link inside it is clicked.
#' @noRd
helpInvestigatorModal <- function(key, data = helpInvestigatorsCached()) {
  inv <- helpInvestigatorByKey(key, data$registry)
  if (is.null(inv)) return(NULL)
  modalDialog(
    title = inv$name,
    size = "l",
    easyClose = TRUE,
    footer = tagList(
      tags$a(href = "#", class = "btn btn-link", `data-help-page` = "investigators",
             "The investigators"),
      tags$a(href = "#", class = "btn btn-link", `data-help-page` = "references",
             "Bibliography"),
      modalButton("Close")
    ),
    tags$div(class = "help-investigator help-body", HTML(helpInvestigatorHTML(inv, data)))
  )
}
