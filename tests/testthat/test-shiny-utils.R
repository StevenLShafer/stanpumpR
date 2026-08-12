# Regression guard for the Handsontable security upgrade.
#
# rhandsontable 0.3.8 bundles Handsontable 6.2.2, which is inside the vulnerable
# ranges identified during the security review (XSS < 8.2.0; CVE-2021-23446).
# The app must actually serve 10.0.0 in the browser. Two earlier attempts failed
# only at render time and would have passed a naive test:
#   1. htmltools::attachDependencies() on the widget -- htmlwidgets ignores that
#      attribute, so 6.2.2 kept loading.
#   2. Overriding only the rendered widget -- the 6.2.2 actually reaching the
#      browser comes from the rHandsontableOutput() *placeholder* in the page,
#      not from the rendered widget payload.
# These tests reproduce both resolution paths, so a regression to either failure
# mode fails here.

# Versions of the "handsontable" dependency that survive htmltools resolution
# (i.e. that would actually reach the browser) for a given tag/widget/dep list.
resolvedHandsontableVersions <- function(x) {
  deps <- htmltools::resolveDependencies(htmltools::findDependencies(x))
  vapply(
    Filter(function(d) identical(d$name, "handsontable"), deps),
    function(d) d$version,
    character(1)
  )
}

test_that("the page supersedes the rHandsontableOutput placeholder's 6.2.2 with 10.0.0", {
  # This is the path that actually determines what the browser runs: the output
  # placeholder pulls in the bundled 6.2.2; the head dependency must win.
  page <- htmltools::tagList(
    handsontablePatchedDependency(),
    rhandsontable::rHandsontableOutput("doseTableHTML")
  )
  versions <- resolvedHandsontableVersions(page)

  expect_identical(versions, "10.0.0")
  expect_false("6.2.2" %in% versions)
})

test_that("secureRHandsontable render payload carries 10.0.0, not 6.2.2", {
  # shinyRenderWidget() sends resolveDependencies(widget$dependencies); assert
  # that payload prefers the patched version, and that it survives hot_col().
  hot <- secureRHandsontable(data.frame(Drug = "propofol", Dose = 1))
  versions <- resolvedHandsontableVersions(htmltools::as.tags(hot))
  expect_identical(versions, "10.0.0")
  expect_false("6.2.2" %in% versions)

  piped <- rhandsontable::hot_col(hot, col = "Dose", type = "numeric")
  pipedVersions <- resolvedHandsontableVersions(htmltools::as.tags(piped))
  expect_identical(pipedVersions, "10.0.0")
})

test_that("the patched Handsontable assets are shipped with the package", {
  base <- system.file("www", "handsontable-10.0.0", package = "stanpumpR")
  expect_true(nzchar(base))
  expect_true(file.exists(file.path(base, "handsontable.full.min.js")))
  expect_true(file.exists(file.path(base, "handsontable.full.min.css")))
})
