# The table grid, the dialogs' grids (R/table-grid.R, inst/www/table-grid.js;
# docs/grid-replacement.md): what is drawn, and what is read back from a
# browser that may send anything.  The behaviour in the browser is
# tests/playwright/table-grid.test.js's; the dialogs' handlers are
# test-ui-adversarial-review.R's and test-time-units-server.R's.  Drafted by
# Claude Code, 2026-11-06, when it replaced rhandsontable in every dialog.

specOf <- function(tag) {
  html <- as.character(tag)
  attr <- regmatches(html, regexpr('data-spec="[^"]*"', html))
  text <- xml2::xml_text(xml2::read_html(paste0("<p>", sub('^data-spec="(.*)"$', "\\1", attr), "</p>")))
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

test_that("the grid is drawn from its columns and rows, and carries no input of its own", {
  data <- data.frame(Delete = FALSE, Time = c("0", "10"), Dose = c("100", "1e3"), Units = c("mg", "mg/kg"))
  grid <- tableGridInput("g", editDosesGridColumns(c("mg", "mg/kg"), "Dose or Target"), data, tag = "propofol")
  html <- as.character(grid)
  expect_match(html, 'class="stanpumpr-table-grid"', fixed = TRUE)
  expect_false(grepl("<input|<select", html))
  spec <- specOf(grid)
  expect_equal(spec$tag, "propofol")
  # named to a screen reader (fourth review, F06)
  expect_equal(spec$label, "Table")
  expect_equal(specOf(tableGridInput("g", list(tableColumn("N")), data.frame(N = "a"), label = "Events"))$label, "Events")
  expect_equal(vapply(spec$columns, `[[`, "", "type"), c("checkbox", "time", "dose", "select"))
  expect_equal(spec$columns[[3]]$label, "Dose or Target")
  expect_equal(unlist(spec$columns[[4]]$options), c("mg", "mg/kg"))
  expect_identical(spec$rows[[1]][[1]], FALSE)
  expect_equal(unlist(spec$rows[[2]][2:4]), c("10", "1e3", "mg/kg"))
  # a one-option list stays a list, and the grid's scripts come with it
  one <- specOf(tableGridInput("g", list(tableColumn("U", type = "select", options = "mg")), data.frame(U = "mg")))
  expect_type(one$columns[[1]]$options, "list")
  deps <- htmltools::findDependencies(grid)
  expect_true(any(vapply(deps, function(d) d$name == "stanpumpr-grids", logical(1))))
})

test_that("markup in a cell is data, never markup", {
  grid <- tableGridInput("g", list(tableColumn("Name")), data.frame(Name = "<img src=x onerror=alert(1)>"))
  html <- as.character(grid)
  expect_false(grepl("<img", html, fixed = TRUE))
  expect_equal(specOf(grid)$rows[[1]][[1]], "<img src=x onerror=alert(1)>")
})

test_that("what the browser sends is read as text and ticks, whatever it holds", {
  cols <- editDosesGridColumns(c("mg"))
  v <- tableGridRead(tableGridInputValue(list(list(TRUE, "0", "100", "mg"), list(NULL, 5, list(1, 2), list(a = 1))),
                                         tag = "propofol"), cols)
  expect_equal(names(v), c("Delete", "Time", "Dose", "Units"))
  expect_identical(v$Delete, c(TRUE, FALSE))
  expect_identical(v$Time, c("0", "5"))
  expect_identical(v$Dose, c("100", ""))
  expect_identical(v$Units, c("mg", ""))
  expect_equal(tableGridTag(tableGridInputValue(list(), tag = "propofol")), "propofol")
  expect_equal(tableGridTag("x"), "")
  # a short row, an empty table, rows past the limit
  expect_equal(unlist(tableGridRead(tableGridInputValue(list(list(TRUE))), cols)[1, 2:4]),
               c(Time = "", Dose = "", Units = ""))
  expect_equal(nrow(tableGridRead(tableGridInputValue(list()), cols)), 0L)
  many <- rep(list(list(FALSE, "0", "1", "mg")), 30)
  cut <- tableGridRead(tableGridInputValue(many), cols, maxRows = 20)
  expect_equal(nrow(cut), 20L)
  # and says so, for the handler to refuse it (ChatGPT's fourth review)
  expect_true(tableGridTruncated(cut))
  expect_false(tableGridTruncated(tableGridRead(tableGridInputValue(many), cols, maxRows = 30)))
  expect_false(tableGridTruncated(NULL))
  # not a grid's value at all: NULL, never an error
  for (bad in list(NULL, "x", 1, list(), list(rows = NULL), list(rows = function() 1))) {
    expect_no_error(v <- tableGridRead(bad, cols))
  }
  expect_null(tableGridRead("x", cols))
})

test_that("the dialogs' columns are the ones their handlers read", {
  expect_equal(vapply(editEventsGridColumns("Induction"), `[[`, "", "name"), c("Delete", "Time", "Event"))
  expect_equal(vapply(targetGridColumns(), `[[`, "", "name"), c("Time", "Target"))
  expect_equal(vapply(thresholdGridColumns(), `[[`, "", "name"), c("Drug", "Threshold"))
  shown <- drugLibraryGridData(getDrugDefaultsGlobal())
  expect_equal(vapply(drugLibraryGridColumns(shown), `[[`, "", "name"), names(shown))
  # a drug not in mcg or ng keeps its concentration unit
  ro <- unlist(drugLibraryGridColumns(shown)[[2]]$readonlyRows) + 1L
  expect_true(all(!shown$Concentration.Units[ro] %in% c("mcg", "ng")))
  expect_true(all(shown$Concentration.Units[-ro] %in% c("mcg", "ng")))
  expect_equal(vapply(paperReviewDoseColumns("min", "mg"), `[[`, "", "label"), c("Time (min)", "Dose", "Units"))
})
