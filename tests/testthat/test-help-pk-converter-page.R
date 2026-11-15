# The converter page's own guards (inst/www/pk-convert.js, the part that
# draws), in headless Chromium on a static page: the converter's markup
# (helpPKConverterHTML()), Shiny's jQuery and the script, with no app behind
# them, since the page computes in the browser and sends nothing.  The
# arithmetic is tested in Node in test-help-pk-converter.R; these are the
# checks the page makes before it computes, and what it says when it stops.
# From the code review of PR #682 (2026-11-15), which found them untested.

pkConverterPageSkip <- function() {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
  ok <- suppressWarnings(system2(Sys.which("node"), c("-e", shQuote("require('playwright')")),
                                 stdout = FALSE, stderr = FALSE))
  skip_if(!identical(ok, 0L), "playwright is not installed")
}

# Run `steps` (a JavaScript function body taking `page`, returning a value)
# on the converter page, after its example model is loaded; the value comes
# back through JSON
pkConverterPage <- function(steps, timeout = 60) {
  dir <- tempfile("pkconv")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  file.copy(system.file("www", "shared", "jquery.min.js", package = "shiny"), dir)
  file.copy(system.file("www", "pk-convert.js", package = "stanpumpR"), dir)
  writeLines(c("<!DOCTYPE html><html><head><meta charset=\"utf-8\">",
               "<script src=\"jquery.min.js\"></script></head><body>",
               helpPKConverterHTML(),
               "<script src=\"pk-convert.js\"></script></body></html>"),
             file.path(dir, "index.html"), useBytes = TRUE)
  script <- file.path(dir, "run.js")
  writeLines(c(
    "const { chromium } = require('playwright');",
    "(async () => {",
    "  const browser = await chromium.launch();",
    "  const page = await browser.newPage();",
    "  const errors = [];",
    "  page.on('pageerror', e => errors.push(String(e)));",
    sprintf("  await page.goto('file://%s');", normalizePath(file.path(dir, "index.html"), winslash = "/")),
    "  await page.click('[data-pk-action=\"example\"]');",
    "  const value = await (async (page) => {", steps, "})(page);",
    "  await browser.close();",
    "  process.stdout.write(JSON.stringify({value: value, errors: errors}));",
    "})().catch(e => { process.stdout.write(JSON.stringify({failed: String(e)})); process.exit(0); });"),
    script)
  out <- suppressWarnings(system2(Sys.which("node"), shQuote(script), stdout = TRUE, stderr = TRUE,
                                  timeout = timeout))
  if (identical(attr(out, "status"), 124L)) stop("the page did not answer within ", timeout, " s")
  res <- jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = TRUE)
  if (!is.null(res$failed)) stop(res$failed)
  expect_length(res$errors, 0)
  res$value
}

test_that("the decrement's longest infusion is refused below one update of the controller", {
  pkConverterPageSkip()
  note <- pkConverterPage(c(
    "await page.fill('input[data-pk-dec=\"until\"]', '0.001');",
    "return await page.textContent('.pkconv-dec-note');"))
  expect_match(note, "must be at least", fixed = TRUE)
  expect_match(note, "one update of the controller", fixed = TRUE)
})

test_that("an infusion stopped by the update limit is not blamed on the effect site's peak", {
  pkConverterPageSkip()
  # An ordinary ke0 and an infusion of 1e300 min: the update limit stops it,
  # and the note said the effect site "peaks 5.96 min after a bolus"
  note <- pkConverterPage(c(
    "await page.fill('input[data-pk-effect=\"ke0\"]', '0.5');",
    "await page.fill('input[data-pk-dec=\"until\"]', '1e300');",
    "return await page.textContent('.pkconv-dec-note');"))
  expect_match(note, "shorten the longest infusion", fixed = TRUE)
  expect_no_match(note, "peaks", fixed = TRUE)
})

test_that("an effect site too slow to search is blamed on its peak, and answered at once", {
  pkConverterPageSkip()
  res <- pkConverterPage(c(
    "await page.fill('input[data-pk-effect=\"ke0\"]', '1e-4');",
    "const t0 = Date.now();",
    "await page.fill('input[data-pk-dec=\"pct\"]', '60');",
    "return {ms: Date.now() - t0, note: await page.textContent('.pkconv-dec-note')};"))
  expect_match(res$note, "peaks", fixed = TRUE)
  expect_match(res$note, "A larger ke0 or a shorter infusion will draw", fixed = TRUE)
  # It spent the whole search, about a second, at every keystroke
  expect_lt(res$ms, 750)
})

test_that("extreme entries leave the page answering and draw no NaN", {
  pkConverterPageSkip()
  res <- pkConverterPage(c(
    "await page.fill('input[data-pk-effect=\"ke0\"]', '1e-300');",
    "await page.fill('input[data-pk-dec=\"until\"]', '1e-323');",
    "const dec = await page.textContent('.pkconv-dec-note');",
    "const out = [];",
    "for (const v of ['1e-323', '0', '-5', '1e309', 'abc', '1e300']) {",
    "  await page.fill('input[data-pk-plot=\"until\"]', v);",
    "  out.push(await page.innerHTML('.pkconv-plot'));",
    "}",
    "await page.fill('input[data-pk-effect=\"ke0\"]', '0.5');",
    "return {dec: dec, plots: out, answered: await page.textContent('.pkconv-effect-result')};"))
  expect_match(res$dec, "Not admissible", fixed = TRUE)
  expect_false(any(grepl("NaN", res$plots, fixed = TRUE)))
  # and the page still computes afterwards
  expect_match(res$answered, "[0-9]")
})
