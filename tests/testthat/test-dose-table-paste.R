# Pasting rows into the dose table past its last row (keyboard review of the
# UI, 2026-11-02).  A paste of three rows starting on the third row of a
# four-row table lost the time and dose of the row it created: "fentanyl 15 50
# mcg" became "fentanyl 0 0 mcg", without a word.
#
# Handsontable (6.2.2, bundled by rhandsontable) applies a paste's changes from
# last to first, so the first one it applies creates the new row, and
# rhandsontable's afterCreateRow handler then writes each column's default
# (null) into that row with setDataAtCell().  Those writes change nothing, but
# each still reaches afterChange, and the Drug column's one is validated, which
# Handsontable does in a setTimeout: it arrived after the paste had filled the
# row, as a lone change on the Drug column, which hookDoseTableUpdate()
# (inst/www/hot_funs.js) read as a drug just chosen and so reset the row's time
# and dose to 0.
#
# The tests replay, against hookDoseTableUpdate() in Node, the sequence of
# afterChange calls logged in Chromium for that paste, in order.  A fake grid
# stands in for Handsontable: setDataAtCell() records the old values, applies
# the new ones and calls afterChange, as Handsontable does.

skipWithoutNode <- function() {
  skip_on_cran()
  skip_if(!nzchar(Sys.which("node")), "node is not installed")
}

# Run `steps` (JavaScript, with `hot`, `paste()`, `change()`, `late()` and `create()` in
# scope) against a fake grid holding `rows`, and return the grid's data
replayDoseTable <- function(rows, steps) {
  script <- tempfile(fileext = ".js")
  on.exit(unlink(script))
  js <- normalizePath(system.file("www", "hot_funs.js", package = "stanpumpR"), winslash = "/")
  header <- stanpumpR:::DOSE_COLUMN_HEADER
  writeLines(c(
    "const fs = require('fs'), vm = require('vm');",
    "global.$ = function() { return { val: function() { return undefined; } }; };",
    "global.drug_defaults = [",
    "  { Drug: 'fentanyl', Units: ['mcg', 'mcg/kg', 'mcg/kg/min'], 'Default.Units': 'mcg' },",
    "  { Drug: 'propofol', Units: ['mg', 'mg/kg', 'mcg/kg/min'], 'Default.Units': 'mg' }",
    "];",
    sprintf("vm.runInThisContext(fs.readFileSync(%s, 'utf8'));", jsonlite::toJSON(js, auto_unbox = TRUE)),
    sprintf("const hot = { data: %s,", jsonlite::toJSON(rows, auto_unbox = TRUE)),
    sprintf("  getSettings: function() { return { colHeaders: ['Drug', 'Time', %s, 'Units'] }; },",
            jsonlite::toJSON(header, auto_unbox = TRUE)),
    "  getDataAtRow: function(r) { return this.data[r].slice(); },",
    "  getDataAtCell: function(r, c) { return this.data[r] ? this.data[r][c] : null; },",
    "  setCellMeta: function() {},",
    "  setDataAtCell: function(changes, source) {",
    "    const self = this;",
    "    const full = changes.map(function(ch) { return [ch[0], ch[1], self.getDataAtCell(ch[0], ch[1]), ch[2]]; });",
    "    full.forEach(function(ch) { if (ch[2] !== null || ch[3] !== null) self.data[ch[0]][ch[1]] = ch[3]; });",
    "    hookDoseTableUpdate.call(self, full, source);",
    "  }",
    "};",
    # A row created by the grid holds nulls
    "function create() { hot.data.push([null, null, null, null]); }",
    # One cell written by setDataAtCell(row, col, value) with no source
    "function change(r, c, v) { hot.setDataAtCell([[r, c, v]], 'edit'); }",
    # The afterChange of a write of null into a cell that held null when it
    # was made (rhandsontable's afterCreateRow), delivered late because the
    # column is validated: Handsontable leaves the cell alone, whatever it
    # holds by then, and reports the change as it was made
    "function late(r, c) { hookDoseTableUpdate.call(hot, [[r, c, null, null]], 'edit'); }",
    # A paste: every cell set, then one afterChange for the lot
    "function paste(cells) { hot.setDataAtCell(cells, 'CopyPaste.paste'); }",
    steps,
    "process.stdout.write(JSON.stringify(hot.data));"
  ), script)
  out <- system2(Sys.which("node"), shQuote(script), stdout = TRUE, stderr = TRUE)
  jsonlite::fromJSON(paste(out, collapse = ""), simplifyVector = FALSE)
}

blankRow <- list("", "", "", "")
propofolTable <- list(list("propofol", "0", "2", "mg/kg"), blankRow, blankRow, blankRow)
row <- function(...) list(...)

test_that("a paste past the last row keeps the time and dose of the row it creates", {
  skipWithoutNode()
  # The afterChange calls Chromium logged for the paste of
  #   fentanyl 5 50 mg / fentanyl 10 50 mcg / fentanyl 15 50 mcg
  # into the Drug cell of the third row.  The Units are not among the pasted
  # changes: the dropdown refused them, its list being empty until a drug is
  # chosen.
  data <- replayDoseTable(propofolTable, "
    create();
    change(4, 1, null);
    change(4, 2, null);
    paste([[2, 0, 'fentanyl'], [2, 1, '5'], [2, 2, '50'],
           [3, 0, 'fentanyl'], [3, 1, '10'], [3, 2, '50'],
           [4, 0, 'fentanyl'], [4, 1, '15'], [4, 2, '50']]);
    late(4, 0);   // validated, so it lands after the paste
    late(4, 3);
  ")
  expect_equal(data[[3]], row("fentanyl", "5", "50", "mcg"))
  expect_equal(data[[4]], row("fentanyl", "10", "50", "mcg"))
  # was fentanyl 0 0 mcg
  expect_equal(data[[5]], row("fentanyl", "15", "50", "mcg"))
})

test_that("a paste that creates several rows keeps every one", {
  skipWithoutNode()
  data <- replayDoseTable(propofolTable, "
    create(); change(4, 1, null); change(4, 2, null);
    create(); change(5, 1, null); change(5, 2, null);
    paste([[2, 0, 'fentanyl'], [2, 1, '5'], [2, 2, '50'],
           [3, 0, 'fentanyl'], [3, 1, '10'], [3, 2, '50'],
           [4, 0, 'fentanyl'], [4, 1, '15'], [4, 2, '50'],
           [5, 0, 'fentanyl'], [5, 1, '20'], [5, 2, '25']]);
    late(5, 0); late(5, 3);
    late(4, 0); late(4, 3);
  ")
  expect_equal(data[[5]], row("fentanyl", "15", "50", "mcg"))
  expect_equal(data[[6]], row("fentanyl", "20", "25", "mcg"))
})

test_that("a row the paste creates is read as a row whose cells are blank", {
  skipWithoutNode()
  # Times and doses with no drug are cleared, in a created row (null drug) as
  # in an existing one ('' drug)
  data <- replayDoseTable(propofolTable, "
    create(); change(4, 1, null); change(4, 2, null);
    paste([[3, 1, '5'], [3, 2, '50'], [4, 1, '10'], [4, 2, '60']]);
    late(4, 0); late(4, 3);
  ")
  expect_equal(data[[4]], row("", "", "", ""))
  expect_equal(data[[5]], row(NULL, "", "", ""))
})

test_that("choosing a drug still starts its row at time 0, dose 0 and its default unit", {
  skipWithoutNode()
  data <- replayDoseTable(propofolTable, "change(1, 0, 'fentanyl');")
  expect_equal(data[[2]], row("fentanyl", "0", 0L, "mcg"))
  # and clearing it clears the row
  data <- replayDoseTable(
    list(list("propofol", "0", "2", "mg/kg"), list("fentanyl", "5", "50", "mcg"), blankRow),
    "change(1, 0, '');"
  )
  expect_equal(data[[2]], row("", "", "", ""))
})
