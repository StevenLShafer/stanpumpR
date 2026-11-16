# Single Paper Review: the report as a Word document (R/paper-review-word.R;
# Shafer, 2026-11-16).  It is built from the tags the HTML report is, so the
# tests hold the two to the same headings, findings, tables and plots, and
# hold the build to leaving nothing in tempdir() but the file it writes.

docxParagraphs <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path))
  s$text[s$content_type == "paragraph"]
}
docxCells <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path))
  s$text[s$content_type == "table cell"]
}
docxMedia <- function(path) grep("^word/media/", utils::unzip(path, list = TRUE)$Name, value = TRUE)
docxPart <- function(path, part) {
  dir <- tempfile("docx-part-")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = part, exdir = dir)
  paste(readLines(file.path(dir, part), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
tagsText <- function(tags, name) {
  html <- xml2::read_html(as.character(htmltools::renderTags(tags)$html))
  trimws(xml2::xml_text(xml2::xml_find_all(html, paste0("//", name))))
}

exampleReview <- function() paperReviewQuietly(function() paperReviewRun(paperReviewExample()))

test_that("the Word report has the HTML report's headings, findings, tables and plots", {
  review <- exampleReview()
  tags <- paperReviewReportTags(review, plots = TRUE)
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  paperReviewReportDocx(review, path)
  paras <- docxParagraphs(path)
  # Every heading, as a heading.
  s <- officer::docx_summary(officer::read_docx(path))
  headings <- trimws(s$text[grepl("^heading", s$style_name %or% "") & s$content_type == "paragraph"])
  expect_setequal(headings, unique(unlist(lapply(c("h2", "h3", "h4"), function(h) tagsText(tags, h)))))
  # Every finding: its severity, then its title, in one paragraph.
  for (f in review$findings) {
    line <- paste0(PAPER_REVIEW_SEVERITY_LABEL[[f$severity]], " ", f$title)
    expect_true(any(startsWith(trimws(paras), line)), info = line)
  }
  # The extraction's tables, cell for cell.
  html <- xml2::read_html(as.character(htmltools::renderTags(tags)$html))
  cells <- trimws(xml2::xml_text(xml2::xml_find_all(html, "//table//td")))
  expect_equal(sort(trimws(docxCells(path))), sort(c(cells, trimws(xml2::xml_text(xml2::xml_find_all(html, "//table//th"))))))
  # One image per plot.
  expect_length(docxMedia(path), length(xml2::xml_find_all(html, "//img")))
  expect_gt(length(docxMedia(path)), 0)
  # The draft banner and the paper.
  expect_true(any(startsWith(paras, "Draft review, for the reader's judgment.")))
  expect_true(any(grepl("exampleol", paras, fixed = TRUE)))
})

test_that("the Word report keeps the findings' colours, the banner's shading and unnumbered headings", {
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  paperReviewReportDocx(exampleReview(), path)
  doc <- docxPart(path, "word/document.xml")
  # A finding's left border in its severity's colour; the banner shaded.
  expect_match(doc, '<w:left w:val="single"[^>]*w:color="B3261E"')
  expect_match(doc, 'w:fill="FFF4E5"')
  # officer's template numbers its headings; the report's are not numbered,
  # and they keep their outline levels.
  styles <- docxPart(path, "word/styles.xml")
  expect_false(grepl("<w:numPr>", styles, fixed = TRUE))
  expect_match(styles, "<w:outlineLvl w:val=\"0\"/>", fixed = TRUE)
})

test_that("a quick review and a document with no model say so in Word too", {
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  review <- exampleReview()
  review$quick <- TRUE
  paperReviewReportDocx(review, path)
  expect_true(any(startsWith(docxParagraphs(path), "A quick review.")))
  x <- paperReviewValidate(list(paper = list(title = "Sourdough"), drug = list(name = ""),
                                structure = list(compartments = 0), extraction_limitations = "Not a PK paper.",
                                reviewer_notes = list(list(topic = "Wrong document", finding = "A recipe.",
                                                           severity = "error", location = "p. 1"))))
  none <- paperReviewQuietly(function() paperReviewRun(x))
  none$quick <- TRUE
  paperReviewReportDocx(none, path)
  paras <- docxParagraphs(path)
  expect_true(any(grepl("found no pharmacokinetic model", paras, fixed = TRUE)))
  expect_true(any(startsWith(paras, "A quick review.")))
  expect_true(any(grepl("Sourdough", paras, fixed = TRUE)))
  paperReviewNoReportDocx("The report could not be written.", path)
  expect_equal(trimws(docxParagraphs(path)), c("Single Paper Review", "The report could not be written."))
})

test_that("writing the Word report leaves nothing in tempdir() but the file, and the paper nowhere else", {
  # officer unpacks its template and writes scratch files under tempdir(),
  # several holding the report's text, and leaves them there.
  ex <- paperReviewExample()
  ex$drug$name <- "SENTINELDRUG"
  review <- paperReviewQuietly(function() paperReviewRun(ex))
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)
  before <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  paperReviewReportDocx(review, path)
  after <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  expect_setequal(after, union(before, basename(path)))
  expect_true(any(grepl("SENTINELDRUG", docxParagraphs(path), fixed = TRUE)))
  # And when the build fails part way, as well.
  before <- list.files(tempdir(), recursive = TRUE, all.files = TRUE)
  testthat::local_mocked_bindings(paperReviewDocxTable = function(...) stop("a table could not be built"))
  expect_error(paperReviewReportDocx(review, tempfile(fileext = ".docx")), "could not be built")
  expect_setequal(list.files(tempdir(), recursive = TRUE, all.files = TRUE), before)
})

test_that("the app serves the Word report, and says when there is none", {
  word <- function(path) paste(docxParagraphs(path), collapse = "\n")
  shiny::testServer(paperReviewServer, {
    expect_match(word(output$downloadWord), "There is no review to download", fixed = TRUE)
    session$setInputs(example = 1)
    text <- word(output$downloadWord)
    expect_match(text, "Single Paper Review", fixed = TRUE)
    expect_match(text, "exampleol", fixed = TRUE)
    expect_false(grepl("There is no review to download", text, fixed = TRUE))
    session$setInputs(discard = 1)
    expect_match(word(output$downloadWord), "There is no review to download", fixed = TRUE)
  })
  # A report that cannot be written gets a document that says so.
  testthat::local_mocked_bindings(paperReviewReportDocx = function(...) stop("officer failed"))
  shiny::testServer(paperReviewServer, {
    session$setInputs(example = 1)
    expect_match(word(output$downloadWord), "The report could not be written.", fixed = TRUE)
  })
})
