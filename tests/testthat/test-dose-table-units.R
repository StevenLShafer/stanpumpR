# The dose table's Units column: a long unit wraps after its route, so the
# column fits the 450 px Doses sidebar.  Drafted by Claude Code, 2026-10-27,
# at the request of Steven L. Shafer; moved to the dose grid (dose-grid.js)
# when it replaced rhandsontable, 2026-11-06.  The wrapping itself is browser
# code (unitWrapText() in inst/www/grid-funs.js, inst/www/grids.css), and the
# browser suite checks it (tests/playwright, "a long unit wraps only after its
# route"); these check that the grid is wired to it.

test_that("the Units column wraps through unitWrapText()", {
  js <- readLines(system.file("www", "grid-funs.js", package = "stanpumpR"))
  expect_true(any(grepl("^function unitWrapText\\(", js)))
  grid <- readLines(system.file("www", "dose-grid.js", package = "stanpumpR"))
  expect_true(any(grepl("unitWrapText(row[COLS.UNITS])", grid, fixed = TRUE)))
  css <- readLines(system.file("www", "grids.css", package = "stanpumpR"))
  expect_true(any(grepl("^\\.dose-grid-unit-text \\{", css)))
})

test_that("the columns fit the Doses sidebar", {
  # The four columns share the table's width by percentage (grids.css), and
  # the table is the sidebar's width, 450 px
  css <- paste(readLines(system.file("www", "grids.css", package = "stanpumpR")), collapse = "\n")
  widths <- as.numeric(regmatches(css, gregexpr("(?<=th\\.dose-grid-col[0-3] \\{ width: )[0-9]+(?=%)", css, perl = TRUE))[[1]])
  expect_length(widths, 4)
  expect_equal(sum(widths), 100)
})
