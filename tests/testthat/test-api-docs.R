# The Simulation API documentation page (R/api-docs.R, inst/apidocs/api.md),
# published at https://stanpumpr.io/api.  Besides the page itself, these tests
# hold the document to the code: its limits, its endpoints and its example
# response are checked against what the API actually does, so the page sent
# to an integrator cannot quietly go out of date.

apiDocsMarkdown <- function() {
  paste(readLines(system.file("apidocs", "api.md", package = "stanpumpR"),
                  encoding = "UTF-8", warn = FALSE), collapse = "\n")
}


test_that("the page is a complete, self-contained HTML document", {
  html <- apiDocsHtml()
  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, "<title>stanpumpR Simulation API</title>", fixed = TRUE)
  expect_match(html, '<meta name="viewport"', fixed = TRUE)
  expect_match(html, "</html>\n$")
  # Nothing loaded from elsewhere: no scripts, no external style sheets.
  expect_no_match(html, "<script", fixed = TRUE)
  expect_no_match(html, "<link", fixed = TRUE)
  # The Markdown was rendered: tables and code blocks, no raw fences.
  expect_match(html, "<table>", fixed = TRUE)
  expect_match(html, "<pre><code", fixed = TRUE)
  expect_no_match(html, "```", fixed = TRUE)
  expect_no_match(html, "|---|", fixed = TRUE)
})


test_that("every section has an anchor, the contents list them, and every in-page link resolves", {
  html <- apiDocsHtml()
  ids <- regmatches(html, gregexpr('<h[23] id="([^"]+)"', html))[[1]]
  ids <- sub('<h[23] id="([^"]+)"', "\\1", ids)
  expect_gt(length(ids), 10)
  expect_false(any(duplicated(ids)))
  h2 <- regmatches(html, gregexpr("<h2 id=", html))[[1]]
  contents <- regmatches(html, gregexpr('<li><a href="#[^"]+">', html))[[1]]
  expect_length(contents, length(h2))
  links <- unique(sub('href="#', "", regmatches(html, gregexpr('href="#[^"]+', html))[[1]]))
  expect_true(all(links %in% ids), info = paste(setdiff(links, ids), collapse = ", "))
})


test_that("headings get the anchors GitHub would give them", {
  expect_equal(stanpumpR:::apiDocsSlug("The time grid"), "the-time-grid")
  expect_equal(stanpumpR:::apiDocsSlug("Using the API from R"), "using-the-api-from-r")
  expect_equal(stanpumpR:::apiDocsSlug("<code>GET</code> /drugs"), "get-drugs")
})


test_that("external links open in a new tab", {
  html <- apiDocsHtml()
  external <- regmatches(html, gregexpr('<a href="https?://[^"]*"[^>]*>', html))[[1]]
  expect_gt(length(external), 0)
  expect_true(all(grepl('target="_blank"', external)))
})


test_that("apiDocsDir() writes the page as index.html", {
  dir <- stanpumpR:::apiDocsDir(file.path(tempdir(), "api-docs-test"))
  expect_true(file.exists(file.path(dir, "index.html")))
  written <- readChar(file.path(dir, "index.html"), file.info(file.path(dir, "index.html"))$size,
                      useBytes = TRUE)
  Encoding(written) <- "UTF-8"
  expect_equal(written, apiDocsHtml())
})


test_that("the limits the document states are the ones the code enforces", {
  md <- apiDocsMarkdown()
  expect_match(md, sprintf("| Dose rows | %d |", MAX_DOSE_ROWS), fixed = TRUE)
  expect_match(md, sprintf("| Events | %d |", MAX_EVENT_ROWS), fixed = TRUE)
  expect_match(md, sprintf("up to %d days", max(MAX_TIME_VALUES) / MINS_PER_DAY), fixed = TRUE)
  expect_match(md, format(stanpumpR:::API_MAX_POINTS, big.mark = ","), fixed = TRUE)
  expect_match(md, sprintf("%s–%s kg", MIN_WEIGHT, MAX_WEIGHT), fixed = TRUE)
  expect_match(md, sprintf("%s–%s cm", MIN_HEIGHT, MAX_HEIGHT), fixed = TRUE)
  expect_match(md, sprintf("%s–%s years", MIN_AGE, MAX_AGE), fixed = TRUE)
  expect_match(md, sprintf("%s–%s mg/dL", MIN_CREATININE, MAX_CREATININE), fixed = TRUE)
  expect_match(md, sprintf("%s–%s mOsm/kg", MIN_OSMOLALITY, MAX_OSMOLALITY), fixed = TRUE)
  expect_match(md, sprintf("The default is %s.", OSMOLALITY_DEFAULT), fixed = TRUE)
  expect_match(md, sprintf("keeps each drug to %d rows", stanpumpR:::API_DEFAULT_POINTS), fixed = TRUE)
  expect_match(md, sprintf('`apiVersion` is `"%s"`', stanpumpR:::API_VERSION), fixed = TRUE)
  # "more than 100 drugs"
  expect_gt(nrow(apiDrugCatalog()), 100)
  for (v in c(CYP2D6_VALUES, CYP2C19_VALUES, CYP2C9_VALUES)) expect_match(md, v, fixed = TRUE)
})


test_that("the document lists every endpoint the service serves", {
  service <- readLines(system.file("plumber", "plumber.R", package = "stanpumpR"))
  routes <- sub("^#\\* @(get|post) ", "", grep("^#\\* @(get|post) ", service, value = TRUE))
  routes <- setdiff(routes, "/")  # the page itself
  md <- apiDocsMarkdown()
  for (r in routes) expect_match(md, paste0("`", r, "`"), fixed = TRUE, info = r)
})


test_that("the example response in the document is what the API returns", {
  md <- apiDocsMarkdown()
  json <- regmatches(md, regexpr("(?s)The response \\(JSON\\):\n\n```json\n(.*?)\n```", md, perl = TRUE))
  json <- sub("(?s)^.*```json\n(.*)\n```$", "\\1", json, perl = TRUE)
  documented <- jsonlite::fromJSON(json)

  actual <- apiSimulate(list(
    patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
    duration = 10,
    doses    = data.frame(Drug = "fentanyl", Time = 0, Dose = 100, Units = "mcg"),
    output   = list(interval = 5)))

  expect_named(documented, names(actual))
  expect_equal(documented$timeseries$Time, actual$timeseries$Time)
  expect_equal(documented$timeseries$Plasma, actual$timeseries$Plasma, tolerance = 1e-8)
  expect_equal(documented$timeseries$EffectSite, actual$timeseries$EffectSite, tolerance = 1e-8)
  expect_equal(documented$drugs$PeakEffectSite, actual$drugs$PeakEffectSite, tolerance = 1e-8)
  expect_equal(documented$drugs$TimePeakEffectSite, actual$drugs$TimePeakEffectSite, tolerance = 1e-8)
  expect_equal(documented$drugs$MEAC, actual$drugs$MEAC)
  expect_equal(names(documented$patient), names(actual$patient))
  expect_equal(names(documented$meta), names(actual$meta))
})
