test_that("checkNumericCovariates correctly identifies out of bounds input", {
  expect_true(checkNumericCovariates(21, 70, 170))
  expect_false(checkNumericCovariates(0, 0, 0))
  expect_false(checkNumericCovariates(MIN_AGE - 1, 70, 170))
  expect_false(checkNumericCovariates(MAX_AGE + 1, 70, 170))
  expect_false(checkNumericCovariates(5, MIN_WEIGHT - 1, 170))
  expect_false(checkNumericCovariates(5, MAX_WEIGHT + 1, 170))
  expect_false(checkNumericCovariates(5, 70, MIN_HEIGHT - 1))
  expect_false(checkNumericCovariates(5, 70, MAX_HEIGHT + 1))
  expect_true(checkNumericCovariates(21, 70, 170, osmolality = 310))
  expect_false(checkNumericCovariates(21, 70, 170, osmolality = MIN_OSMOLALITY - 1))
  expect_false(checkNumericCovariates(21, 70, 170, osmolality = MAX_OSMOLALITY + 1))
  # An empty numeric field reports NA
  expect_false(checkNumericCovariates(21, 70, 170, osmolality = NA))
  # Creatinine is optional: blank (NA or NULL) passes, a value must be in range
  expect_true(checkNumericCovariates(21, 70, 170, creatinine = NA))
  expect_true(checkNumericCovariates(21, 70, 170, creatinine = NULL))
  expect_true(checkNumericCovariates(21, 70, 170, creatinine = 1.4))
  expect_false(checkNumericCovariates(21, 70, 170, creatinine = MIN_CREATININE / 2))
  expect_false(checkNumericCovariates(21, 70, 170, creatinine = MAX_CREATININE + 1))
  expect_false(checkNumericCovariates(21, 70, 170, creatinine = NaN))
})

test_that("the address bar keeps the debug level, in front of the bookmark", {
  bm <- "http://host/app/?_inputs_&age=50&_values_&DT=%7B%22Drug%22%3A%5B%22propofol%22%5D%7D"
  # Off, or at the configured level: the URL is left alone
  expect_identical(withDebugQuery(bm, DEBUG_LEVEL_OFF), bm)
  expect_identical(withDebugQuery(bm, 2, default = 2), bm)
  expect_identical(withDebugQuery(bm, 1, default = NULL), withDebugQuery(bm, 1))
  expect_identical(withDebugQuery(bm, NULL), bm)
  expect_identical(withDebugQuery(bm, "nonsense"), bm)

  # On: added before _inputs_, where parseQueryString() (which app_server()
  # reads the level from) finds it and Shiny's restore does not
  url <- withDebugQuery(bm, 1)
  expect_identical(url, sub("?", "?debug=1&", bm, fixed = TRUE))
  query <- sub("^[^?]*", "", url)
  expect_identical(shiny::parseQueryString(query)[["debug"]], "1")
  rc <- shiny:::RestoreContext$new(query)
  expect_identical(names(rc$values), "DT")
  expect_true(isBookmarkRestore(rc$values))

  # The debug menu sends a string; turning debugging off locally (config on)
  # is kept too
  expect_identical(withDebugQuery(bm, "2"), sub("?", "?debug=2&", bm, fixed = TRUE))
  expect_identical(withDebugQuery(bm, "0", default = 2), sub("?", "?debug=0&", bm, fixed = TRUE))
  # A URL with no query string at all
  expect_identical(withDebugQuery("http://host/app/", 1), "http://host/app/?debug=1")
})
