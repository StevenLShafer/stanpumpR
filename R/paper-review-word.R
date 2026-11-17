# Single Paper Review: the review report, as a Word document.
#
# Shafer (2026-11-16) asked for the report as a Word document beside the HTML
# one. It is built from the same tags the app shows and the HTML download
# writes (paperReviewReportTags()), walked here into officer's paragraphs,
# tables and images, so the two cannot say different things: a section, a
# finding or a table added to the report reaches both. The report's tags are
# few and fixed (headings, paragraphs, the findings' and banners' divs, the
# tables and the plots' inline PNGs), and anything else is walked through for
# its text.
#
# Nothing is left on the server. officer unpacks its template into a folder
# under tempdir() and writes further scratch files there as it builds the
# document (the XML and text it assembles, a copy of each image), several of
# which hold the report's text, and does not remove them; they would stay
# until the R process ends. Every one of them, and the PNG this file writes
# for each plot, is named by tempfile() with its default pattern ("file" and
# hex digits). So the build notes what is in tempdir() before it starts and,
# when it ends, removes what is new AND so named, except the file it was
# asked to write. The second condition is what keeps the cleanup to the
# build's own files: R runs a download handler on the session's one thread,
# but a background process can still write into this tempdir() meanwhile,
# and a review's process writes its ID to a file named "paper-review-..."
# (paperReviewExtractAsync()), which the quick review needs to stop it;
# Shiny's uploads are folders of 24 hex digits (CodeRabbit, PR #736).
# (Claude Code, 2026-11-16, at the request of Steven L. Shafer.)

PAPER_REVIEW_DOCX_TYPE <- "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
# The colours of the report's severity badges (paperReviewCss()).
PAPER_REVIEW_SEVERITY_COLOUR <- c(error = "#b3261e", concern = "#c77700", info = "#1f4e9c", ok = "#2e7d32")
# A plot's width on the page, inches (a portrait page with 1-inch margins).
PAPER_REVIEW_DOCX_PLOT_WIDTH <- 6.5

#' Write the review report as a Word document
#'
#' @param review the output of \code{paperReviewRun()}
#' @param path the .docx file to write
#' @returns \code{path}, invisibly
#' @keywords internal
paperReviewReportDocx <- function(review, path)
  paperReviewDocxWrite(path, paperReviewReportTags(review, plots = TRUE))

#' A Word document that says why there is no report
#'
#' The Word download's counterpart of \code{paperReviewNoReportPage()}: a
#' link followed after Discard, or a report that could not be written.
#'
#' @param why one sentence
#' @param path the .docx file to write
#' @returns \code{path}, invisibly
#' @keywords internal
paperReviewNoReportDocx <- function(why, path)
  paperReviewDocxWrite(path, htmltools::tagList(htmltools::h3("Single Paper Review"), htmltools::p(why)))

# The names tempfile() gives by default, with or without an extension.
PAPER_REVIEW_DOCX_SCRATCH <- "^file[0-9a-f]+(\\.[A-Za-z0-9]+)?$"

# Write report tags to a .docx, leaving nothing else behind in tempdir().
paperReviewDocxWrite <- function(path, tags)
{
  # officer's scratch files, and ours, are removed whatever happens.
  before <- paperReviewTempEntries()
  on.exit(paperReviewDocxCleanup(before, path), add = TRUE)
  st <- new.env(parent = emptyenv())
  st$doc <- officer::read_docx()
  paperReviewDocxUnnumberHeadings(st$doc)
  paperReviewDocxBlock(tags, st, paperReviewDocxStyle())
  print(st$doc, target = path)
  invisible(path)
}

# officer's template numbers its headings (1., 1.1., 1.1.1.), which the
# report's are not; the numbering is taken out of the unpacked template's
# styles, which print() copies as they are. Should officer change where it
# keeps them, the headings are numbered and nothing else changes.
paperReviewDocxUnnumberHeadings <- function(doc)
{
  file <- file.path(doc$package_dir %or% "", "word", "styles.xml")
  if (!file.exists(file)) return(invisible(FALSE))
  xml <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  writeLines(gsub("<w:numPr>.*?</w:numPr>", "", xml, perl = TRUE), file, useBytes = TRUE)
  invisible(TRUE)
}

# Remove what the build left in tempdir(): entries new since `before` and
# named as tempfile() names them, never the file written. Both sides are
# compared fully resolved, once the file exists: before then a path through
# a symbolic link (macOS's /var) or with a "./" would not match its resolved
# self, and the file Shiny serves would be taken for scratch (CodeRabbit,
# PR #736).
paperReviewDocxCleanup <- function(before, path)
{
  new <- setdiff(paperReviewTempEntries(), before)
  new <- new[grepl(PAPER_REVIEW_DOCX_SCRATCH, basename(new))]
  if (length(new) == 0) return(invisible(character(0)))
  resolved <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  new <- new[resolved(new) != resolved(path)]
  unlink(new, recursive = TRUE, force = TRUE)
  invisible(new)
}

# Everything directly in tempdir(), as full paths.
paperReviewTempEntries <- function()
  list.files(tempdir(), all.files = TRUE, full.names = TRUE, no.. = TRUE)

# How a run of text, and the paragraph it is in, look: inherited down the tags.
paperReviewDocxStyle <- function(bold = FALSE, italic = FALSE, colour = NA_character_, size = NA_real_,
                                 par = officer::fp_par())
  list(bold = bold, italic = italic, colour = colour, size = size, par = par)

paperReviewDocxClasses <- function(node)
{
  cls <- node$attribs[names(node$attribs) == "class"]
  unlist(strsplit(paste(unlist(cls), collapse = " "), "\\s+"))
}

PAPER_REVIEW_DOCX_INLINE <- c("strong", "b", "em", "i", "span", "a", "code", "sub", "sup")
PAPER_REVIEW_DOCX_SKIP <- c("style", "script", "head", "meta", "title", "link")

paperReviewDocxIsInline <- function(node)
{
  if (is.null(node)) return(TRUE)
  if (is.character(node) || is.numeric(node) || is.logical(node)) return(TRUE)
  if (inherits(node, "shiny.tag")) return(node$name %in% PAPER_REVIEW_DOCX_INLINE &&
                                            all(vapply(node$children, paperReviewDocxIsInline, logical(1))))
  if (is.list(node)) return(all(vapply(node, paperReviewDocxIsInline, logical(1))))
  TRUE
}

# The style a tag gives the text inside it.
paperReviewDocxInner <- function(node, style)
{
  cls <- paperReviewDocxClasses(node)
  if (node$name %in% c("strong", "b")) style$bold <- TRUE
  if (node$name %in% c("em", "i")) style$italic <- TRUE
  if ("pr-badge" %in% cls) {
    sev <- intersect(sub("^pr-", "", cls), names(PAPER_REVIEW_SEVERITY_COLOUR))
    style$bold <- TRUE
    if (length(sev)) style$colour <- PAPER_REVIEW_SEVERITY_COLOUR[[sev[1]]]
  }
  if ("pr-source" %in% cls) { style$colour <- "#555555"; style$size <- 8 }
  if ("pr-where" %in% cls) style$size <- 9
  style
}

# A node's inline content as officer text runs.
paperReviewDocxRuns <- function(node, style)
{
  if (is.null(node) || (is.atomic(node) && length(node) == 0)) return(list())
  if (is.character(node) || is.numeric(node) || is.logical(node)) {
    text <- paste(as.character(node), collapse = "")
    if (!nzchar(text)) return(list())
    prop <- officer::fp_text_lite(bold = style$bold, italic = style$italic,
                                  color = style$colour, font.size = style$size)
    return(list(officer::ftext(text, prop)))
  }
  if (inherits(node, "shiny.tag")) {
    if (node$name %in% PAPER_REVIEW_DOCX_SKIP) return(list())
    inner <- paperReviewDocxInner(node, style)
    runs <- unlist(lapply(node$children, paperReviewDocxRuns, style = inner), recursive = FALSE)
    # A badge reads "Error" in the page's coloured box; here it is followed by
    # a space so the title does not run into it.
    if ("pr-badge" %in% paperReviewDocxClasses(node)) runs <- c(runs, paperReviewDocxRuns(" ", style))
    return(runs)
  }
  if (is.list(node)) return(unlist(lapply(node, paperReviewDocxRuns, style = style), recursive = FALSE))
  paperReviewDocxRuns(as.character(node), style)
}

# A node's text, flattened (a heading, a table cell).
paperReviewDocxText <- function(node)
{
  if (is.null(node)) return("")
  if (is.character(node) || is.numeric(node) || is.logical(node)) return(paste(as.character(node), collapse = ""))
  if (inherits(node, "shiny.tag"))
    return(if (node$name %in% PAPER_REVIEW_DOCX_SKIP) "" else paperReviewDocxText(node$children))
  if (is.list(node)) return(paste(vapply(node, paperReviewDocxText, character(1)), collapse = ""))
  as.character(node)
}

paperReviewDocxParagraph <- function(st, runs, style)
{
  if (length(runs) == 0) return(invisible())
  # With a style named, officer drops the paragraph's own format (the
  # findings' borders, the banners' shading), so none is named.
  st$doc <- officer::body_add_fpar(st$doc, do.call(officer::fpar, c(runs, list(fp_p = style$par))))
  invisible()
}

# A node as document blocks: headings, paragraphs, tables and images.
paperReviewDocxBlock <- function(node, st, style)
{
  if (is.null(node)) return(invisible())
  if (!inherits(node, "shiny.tag")) {
    if (is.list(node)) return(paperReviewDocxChildren(node, st, style))
    return(paperReviewDocxParagraph(st, paperReviewDocxRuns(node, style), style))
  }
  name <- node$name
  if (name %in% PAPER_REVIEW_DOCX_SKIP) return(invisible())
  if (name %in% c("h1", "h2", "h3", "h4", "h5")) {
    level <- c(h1 = 1, h2 = 1, h3 = 2, h4 = 3, h5 = 3)[[name]]
    st$doc <- officer::body_add_par(st$doc, trimws(paperReviewDocxText(node)),
                                    style = paste("heading", level))
    return(invisible())
  }
  if (name == "table") return(paperReviewDocxTable(node, st))
  if (name == "img") return(paperReviewDocxImage(node, st))
  cls <- paperReviewDocxClasses(node)
  inner <- paperReviewDocxInner(node, style)
  if ("pr-finding" %in% cls) {
    sev <- intersect(sub("^pr-", "", cls), names(PAPER_REVIEW_SEVERITY_COLOUR))
    colour <- if (length(sev)) PAPER_REVIEW_SEVERITY_COLOUR[[sev[1]]] else "#999999"
    inner$par <- officer::fp_par(padding.left = 6, padding.top = 1, padding.bottom = 1,
                                 border.left = officer::fp_border(color = colour, width = 3))
  } else if ("pr-banner" %in% cls) {
    inner$par <- officer::fp_par(padding = 4, shading.color = "#fff4e5",
                                 border = officer::fp_border(color = "#c77700", width = 1))
  }
  if (name == "p" || paperReviewDocxIsInline(node$children))
    return(paperReviewDocxParagraph(st, paperReviewDocxRuns(node$children, inner), inner))
  paperReviewDocxChildren(node$children, st, inner)
}

# A container's children, consecutive inline ones gathered into one paragraph.
paperReviewDocxChildren <- function(children, st, style)
{
  pending <- list()
  flush <- function() {
    paperReviewDocxParagraph(st, paperReviewDocxRuns(pending, style), style)
    pending <<- list()
  }
  for (child in children) {
    if (is.null(child)) next
    if (inherits(child, "shiny.tag") || !is.list(child)) {
      if (paperReviewDocxIsInline(child)) { pending[[length(pending) + 1]] <- child; next }
      flush()
      paperReviewDocxBlock(child, st, style)
    } else {
      flush()
      paperReviewDocxChildren(child, st, style)
    }
  }
  flush()
  invisible()
}

# A report table (paperReviewTable()): its header and cells, as text.
paperReviewDocxTable <- function(node, st)
{
  find <- function(n, name) {
    if (inherits(n, "shiny.tag")) {
      if (n$name == name) return(list(n))
      return(find(n$children, name))
    }
    if (is.list(n)) return(unlist(lapply(n, find, name = name), recursive = FALSE))
    list()
  }
  header <- vapply(find(node, "th"), paperReviewDocxText, character(1))
  rows <- lapply(find(find(node, "tbody"), "tr"), function(tr)
    vapply(find(tr, "td"), paperReviewDocxText, character(1)))
  if (length(header) == 0) return(invisible())
  rows <- Filter(function(r) length(r) == length(header), rows)
  df <- as.data.frame(matrix(unlist(rows), ncol = length(header), byrow = TRUE,
                             dimnames = list(NULL, header)),
                      stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0) df <- as.data.frame(matrix(character(0), ncol = length(header),
                                                dimnames = list(NULL, header)),
                                         stringsAsFactors = FALSE, check.names = FALSE)
  st$doc <- officer::body_add_table(st$doc, df, style = "table_template",
                                    alignment = rep("l", ncol(df)))
  st$doc <- officer::body_add_par(st$doc, "", style = "Normal")
  invisible()
}

# A plot (paperReviewPlotImg()): its inline PNG, at the page's width. The
# temporary file it is written to goes with officer's at the end of the build.
paperReviewDocxImage <- function(node, st)
{
  src <- node$attribs$src %or% ""
  prefix <- "data:image/png;base64,"
  if (!startsWith(src, prefix)) return(invisible())
  bytes <- jsonlite::base64_dec(substring(src, nchar(prefix) + 1))
  # The PNG's own size, from its header (width and height, bytes 17-24).
  wh <- if (length(bytes) >= 24) {
    v <- as.integer(bytes[17:24])
    c(sum(v[1:4] * 256^(3:0)), sum(v[5:8] * 256^(3:0)))
  } else c(7, 4.5)
  file <- tempfile(fileext = ".png")
  writeBin(bytes, file)
  width <- PAPER_REVIEW_DOCX_PLOT_WIDTH
  st$doc <- officer::body_add_img(st$doc, file, width = width, height = width * wh[2] / wh[1])
  invisible()
}
