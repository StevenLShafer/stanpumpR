# The dose table's Units column (createHOT()): a long unit wraps after its
# route, in the cell and in its dropdown list, so the column fits the 450 px
# Doses sidebar.  Drafted by Claude Code, 2026-10-27, at the request of
# Steven L. Shafer.  The wrapping itself is browser code (inst/www/hot_funs.js,
# inst/www/app.css); these check that the grid is wired to it.

test_that("the Units column wraps through the hot_funs.js renderer and hook", {
  hot <- createHOT(doseTableInit, getDrugDefaultsGlobal())
  unitsCol <- which(names(doseTableInit) == "Units")
  expect_equal(as.character(hot$x$columns[[unitsCol]]$renderer), "renderUnitCell")
  render <- paste(unlist(hot$jsHooks$render), collapse = "\n")
  expect_true(grepl("hot.addHook('afterBeginEditing', hookWrapUnitList)", render, fixed = TRUE))
  expect_true(grepl("hot.addHook('afterBeginEditing', hookSelectAllDrugText)", render, fixed = TRUE))

  js <- readLines(system.file("www", "hot_funs.js", package = "stanpumpR"))
  for (f in c("unitWrapText", "wrapUnitText", "renderUnitCell", "hookWrapUnitList")) {
    expect_true(any(grepl(paste0("^function ", f, "\\("), js)), info = f)
  }
})

test_that("the columns fit the Doses sidebar with a vertical scrollbar", {
  hot <- createHOT(doseTableInit, getDrugDefaultsGlobal())
  # 450 px sidebar, less 16 px padding each side and a 15 px scrollbar:
  # measured in the app, 2026-10-27
  expect_lte(sum(unlist(hot$x$colWidths)), 450 - 2 * 16 - 15)
  expect_equal(unlist(hot$x$colWidths), c(152, 45, 90, 100))
})
