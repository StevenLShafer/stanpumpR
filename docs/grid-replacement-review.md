# Review brief: is the prototype dose grid a suitable replacement for rhandsontable?

> **Historical (2026-11-06).** This is the brief for the first reviews, kept
> as the record of what they were asked. Its commands no longer run: the
> `stanpumpR.doseGrid` switch, the Handsontable grid and its adapter were
> removed when every grid was replaced (Shafer's decision 1,
> `docs/grid-replacement.md`). To test the grids today, follow
> `tests/playwright/README.md`.

This is the brief for an external review of pull request #683
(`StevenLShafer/stanpumpR`, branch `claude/lucid-lovelace-fi2nqh`), written for
Codex (ChatGPT) and Antigravity (Gemini). Steven Shafer will compare the two
reports with each other and with the code. Read all of it before you start.

## What you are judging

stanpumpR is an R package and Shiny app for pharmacokinetic simulation. Its
main dose table, the grid in the Doses sidebar where a clinician enters drug,
time, dose and units, is drawn today by rhandsontable (Handsontable 6.2.2).
PR #683 adds a replacement, a plain HTML table of form controls with its own
Shiny input binding, behind a switch that is off by default.

The question is whether the replacement can stand in for rhandsontable **in
the main dose table**: every behaviour the current grid gives a user, nothing
worse, and nothing that could put a wrong dose, time or unit into the table
without the user knowing. A wrong unit can be a thousandfold dosing error, and
a wrong time silently shifts a simulation. Treat data entry correctness as the
first criterion, then robustness (the Shiny session must never end), then
keyboard and screen-reader use, then everything else.

Not in scope: the six other rhandsontable grids (the dialogs and the Single
Paper Review tab), which the PR leaves alone; and spreadsheet features Steven
has said he does not need (range selection, copying several cells, a paste
repeated over a selection, drag-fill, resizable columns). **Ctrl+Z / Ctrl+Y
undo and redo are in scope**: he uses them in every application.

## Files to read first

- `AGENTS.md`, the sections *Dose table lifecycle*, *Replacing rhandsontable*,
  *An error in an observer ends the session*, *A bookmark link is untrusted
  input* and *Time units*.
- `docs/grid-replacement.md`: the PR's comparison, design and claims.
- `inst/www/dose-grid.js`: the new grid (browser).
- `R/dose-grid.R` and the `doseGridMode() == "native"` block and
  `storeGridEdit()` in `R/app_server.R`: the new grid (server).
- `inst/www/hot_funs.js` and `R/createHOT.R`: the current grid, whose
  behaviour is the bar.
- `R/validate-input.R` (`validateTime()`, `validateDose()`) and
  `R/input-tables.R` (`validateDoseTableInput()`): the rules both grids must
  follow.
- `tests/playwright/` (start with its `README.md`): the PR's browser suite.

## Rules for your report

These are not optional. A report that breaks them cannot be used.

1. **Every finding names a file and line and quotes that line exactly**, for
   example `inst/www/dose-grid.js:123`, followed by line 123 exactly as it
   stands in the file.
   If a finding is about behaviour rather than one line, quote the line you
   believe causes it, and say that it is your belief.
2. **Every behavioural claim comes with its reproduction**: the grid (`native`
   or `handsontable`), the exact keys and clicks, and what the screen showed
   and what the server's draft held afterwards (how to read the draft is
   below). Run it on **both** grids. A defect both grids share is not a
   regression, but report it.
3. **Say which findings you reproduced and which you only suspect from
   reading.** Never present a suspicion as a result. Never report a test as
   run if you did not run it, and paste the command and its real output.
4. **Do not change product code.** You may add new test files under
   `tests/playwright/review/`, and you may write scratch scripts anywhere.
   Do not commit or push anything.
5. Do not trust the PR's own claims, including in `docs/grid-replacement.md`
   and in the test file's `KNOWN_GAP` list. Check them.
6. Severity: **critical** (a wrong dose, time, unit or drug can reach the table
   or the plot without the user being told, or the session ends), **major** (a
   behaviour of the current grid is lost, or an entry is lost with a message),
   **minor** (cosmetic, wording, a rough edge), **question** (needs Steven's
   decision).

## Setup

You need R (4.3 or later) with the packages in `renv.lock`, Node 18 or later,
and Playwright with Chromium. Firefox and WebKit are wanted too: see test M3.

```bash
git fetch origin claude/lucid-lovelace-fi2nqh && git checkout claude/lucid-lovelace-fi2nqh
```

```bash
cp config.yml.sample config.yml
```

```bash
Rscript -e 'renv::restore()'
```

```bash
npm install --no-save playwright && npx playwright install chromium firefox webkit
```

Run the R session in a UTF-8 locale (`export LANG=C.UTF-8` on Linux); some
tests compare em dashes and fail in the C locale.

Start each grid in its own terminal, in Shiny's test mode:

```bash
Rscript -e 'pkgload::load_all("."); options(shiny.testmode = TRUE, stanpumpR.doseGrid = "handsontable"); shiny::runApp(run_app(), port = 7311, launch.browser = FALSE)'
```

```bash
Rscript -e 'pkgload::load_all("."); options(shiny.testmode = TRUE, stanpumpR.doseGrid = "native"); shiny::runApp(run_app(), port = 7312, launch.browser = FALSE)'
```

Useful facts:

- **The startup menu** asks which drugs to start with; its checkboxes sit in
  collapsed sections. Ticking `propofol`, `fentanyl`, `remifentanil` and
  `rocuronium` and pressing **Start** (`#startup_ok`) gives the six-row table
  the suite starts from, followed by six blank rows. `openSession()` in
  `tests/playwright/harness.js` does this from script.
- **The grid's root** is `#doseGrid` (native) or `#doseTableHTML`
  (handsontable). `tests/playwright/adapters/*.js` show how to find a cell in
  each.
- **The server's draft**, in test mode, is read in the page with:
  ```js
  const u = Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl: true });
  JSON.parse(await (await fetch(u + '&input=0&output=0&export=1&format=json')).text()).export
  ```
  This gives `doseTableDraft` (what the grid holds and Apply will commit),
  `doseTable` (what was last applied), `doseTableFormat`, `canUndo` and
  `canRedo`. `serverState()` in `harness.js` does this for you.
- **The session has ended** if `Shiny.shinyapp.isConnected()` is false, or
  the page greys out with "Disconnected from the server". Check after every
  test.
- **The buttons** under the grid are Apply (`#dosetable_apply`), Undo
  (`#dosetable_undo`) and Redo (`#dosetable_redo`). The illicit-drug opt-in
  is `#showIllicitDrugs`; the time unit is `#timeUnits`, and elapsed or clock
  time is `#timeMode`.

## Part A: run what exists, and report the results

- **A1.** `node tests/playwright/run.js` (both grids, about 15 minutes). Report
  pass, fail and todo for each grid. Then remove one entry from `KNOWN_GAP` in
  a scratch copy and confirm that its case really fails on that grid for the
  stated reason.
- **A2.** `Rscript -e 'devtools::test(filter = "dose-grid|dose-table|ui-|time-units")'`
  in a UTF-8 locale. Report the counts.
- **A3.** Read `tests/playwright/dose-grid.test.js`. For each case, say whether
  it actually tests what its name claims. Name any case that could pass while
  the behaviour is broken: for example, one that asserts on the draft but
  never on the screen, or the reverse, or waits for a state that is already
  true before the action.

## Part B: tests to execute by hand or by script, on both grids

Report each as **pass**, **fail** (with the reproduction) or **not run** (with
the reason). Each expected result below is what the current grid does, or
what the rules require. Check that it is in fact what the current grid does;
where the two grids differ, say which is right.

### B1. Choosing a drug

1. In the first blank row, type `morphine` and press Enter. The row becomes
   `morphine 0 0 mg`, and a blank row is still below the last drug.
2. Type `MORPHINE`, then `Morphine`. Is either accepted? Compare the grids.
3. Type `morph` and press Enter; then type `morph`, press ArrowDown twice and
   press Enter. Which drug is taken, and is it the one highlighted?
4. Type `notadrug` and press Enter, then Escape. The cell must be empty, the
   draft unchanged, and the grid usable: type `fentanyl` into the same cell
   and press Enter.
5. Type `fentanyl`, then click elsewhere with the mouse instead of pressing
   Enter. Is the drug committed?
6. With the opt-in off, `cocaine` must not be offered or accepted. Tick
   **Show illicit drugs**: `cocaine` is accepted and drawn in red
   (`rgb(192, 0, 0)`). Untick it while `cocaine` is in the table: the row
   must keep it, still red. Press Enter twice on that cell: nothing changes.
7. Change an existing row's drug (fentanyl to morphine): its time and dose
   become 0 and its unit morphine's default. Press Enter twice on a drug cell
   whose row has a time and a dose: nothing must change.
8. Clear a drug (Delete, or select all then Backspace, then Enter): the whole
   row is cleared.

### B2. Units

1. For fentanyl, morphine (25 units), sevoflurane (one unit) and a drug in the
   `Illicit drugs` category: the list offers exactly the drug's `Units` from
   `inst/extdata/drugDefaults_global.csv`, in order.
2. Choose a unit with the mouse, then with the keyboard only (focus the cell,
   type the unit, or use the arrow keys, then Enter). Check that the draft
   holds the unit.
3. `mg/kg PO liquid q4h` is drawn as `mg/kg PO` over `liquid q4h`. The
   stored value must keep ordinary spaces.
4. Can a row ever hold a unit its drug does not offer? Try changing the drug
   after choosing a unit, pasting, undoing and redoing, a bookmark (B8.3), and
   the Edit doses dialog (double-click a drug's line on the plot).

### B3. Time and Dose entries

For each entry, type it into the Dose cell of `fentanyl 0 0 mcg` and press
Enter. Then type it into the Time cell. Then paste it into each. Record what
the cell shows, what the draft holds, and any message. The server's rule is
R's `validateDose()` and `validateTime()`: compute the expected value with
`Rscript -e 'devtools::load_all("."); validateDose("1,000")'`.

`5`, `0`, `0.5`, `.5`, `5.`, `007`, `1,000`, `1,5`, `1e3`, `1E-3`, `2e400`,
`-2`, `+3`, ` 4 ` (spaces), `"6"` (quotes), `1000000000`, `1000000001`, `abc`,
`1a0`, `１２` (full-width digits), `٣` (Arabic-Indic digit), `NaN`,
`Infinity`, an empty cell, 40 nines, and, for Time only, `1:30`, `0:80`,
`99:30`, `12345678901:00`.

Also check that:

- a blank Dose in a row that has a drug is refused with a message, and the
  cell keeps its value;
- a time or dose typed in a row with no drug is not kept.

Repeat the time entries in **clock** mode: set Time units to minutes and Time
Display to Actual time, so that `08:30` is typed with its colon.

### B4. Paste

Paste with the system clipboard (Ctrl+V, or Cmd+V on a Mac) from a real
spreadsheet if you can. Otherwise use `navigator.clipboard.writeText()` from
the page, then the key. Start the paste in the cell named.

1. Two whole rows into the first blank Drug cell:
   `fentanyl\t5\t50\tmcg/kg\nmorphine\t30\t10\tmg PO tablet`.
2. A unit the drug does not offer (`morphine\t0\t10\tmg PO`): the row takes
   morphine's default unit, with a message naming the refused unit.
3. Time and Dose only (`10\t50\n20\t5`), into a Time cell of a row that has a
   drug.
4. Rows past the last row of the table; then 600 rows (the table holds at
   most 499 doses plus a blank row). Record how long each grid takes.
5. A drug-only paste (`fentanyl`) over `propofol 0 2 mg/kg`; then
   `fentanyl\t0\t2` over the same row. Fentanyl must never end in `mg/kg`.
6. CRLF line ends, a trailing newline, a cell quoted as a spreadsheet writes it
   (`"x\ty"`), a unit with spaces around it (` mg PO tablet `), `Fentanyl` in
   capitals, and an unknown drug in the middle of three rows.
7. A single value pasted into a cell while you are typing in it, with the caret
   in the middle of the text.

### B5. Keyboard

1. Tab and Shift+Tab, Enter and Shift+Enter, ArrowUp and ArrowDown, Escape
   (while typing; while the drug list is open), Home and End.
2. A whole dose with no mouse: `morphine`, Tab, `15`, Tab, `4`, Tab, a unit,
   Enter. Type at normal speed, with no pauses. Repeat it fast with a script
   (`page.keyboard.type(text, { delay: 0 })`).
3. **Ctrl+Z and Ctrl+Y** (on a Mac, Cmd+Z, Cmd+Shift+Z and Cmd+Y):
   - undo and redo after each kind of edit: drug, time, dose, unit, paste, row
     inserted, row removed;
   - several undos in a row;
   - Ctrl+Z while typing in a cell, which should undo the typing only;
   - Ctrl+Z with the drug list open;
   - Ctrl+Z right after Enter;
   - Ctrl+Z after Apply, which stops at Apply as the Undo button does;
   - Ctrl+Z with focus outside the grid.

   After each, compare the screen, the draft and the enabled state of the Undo
   and Redo buttons.
4. Shift+F10 and the context-menu key on a row; the row menu's arrow keys,
   Escape and Tab.
5. Where focus lands after each row action, and after the server redraws the
   grid (B6).

### B6. The server and the network

Emulate a slow link from Playwright with the Chrome DevTools Protocol:

```js
const cdp = await context.newCDPSession(page);
await cdp.send('Network.enable');
await cdp.send('Network.emulateNetworkConditions', { offline: false, latency: 400, downloadThroughput: -1, uploadThroughput: -1 });
```

Then:

1. Run B5.2 and B5.3 at 400 ms and at 1500 ms of latency.
2. Make two edits in quick succession in different rows, then two in the same
   cell.
3. Enter `sevoflurane` in a blank row. The server adds a ventilation row, and
   the grid must draw it. Then enter `nitrousOxide`, which brings an oxygen row
   as well (`applyGasTableRules()`; oxygen comes only with nitrous oxide).
   Do each again while typing in another cell: is the text being typed kept?
4. Change Time units from minutes to hours while a cell is half typed, and
   while an edit is in flight on a slow link. A time of 90 minutes must become
   1.5 hours, never 90 hours.
5. Undo or Redo with the buttons while an edit is in flight.
6. Stop the app (Ctrl+C in its terminal) while the page is open, then edit a
   cell. What does the user see?

### B7. Everything is text

1. Type and paste `<img src=x onerror="window.__xss=1">`,
   `<b>mg</b>`, `"><script>window.__xss=2</script>` and
   `javascript:alert(1)` into every column, as single values and inside a
   pasted row. `window.__xss` must stay undefined, and no `img`, `b` or
   `script` element may appear inside the grid.
2. Search `inst/www/dose-grid.js` for every place a string reaches the DOM.
   Name any that is not `.value` or `.textContent`.

### B8. Untrusted values reaching the server

1. From the browser console, with the native grid, send malformed values and
   check that the session survives each:
   ```js
   Shiny.setInputValue('doseGrid', { rows: 'abc', seq: 9 }, { priority: 'event' })
   ```
   Try also:
   - `{ rows: [[1, 2, 3, 4, 5, 6]] }`;
   - `{ rows: [[null, {}, [], true]] }`;
   - `{ rows: [] }`;
   - 5000 rows;
   - a 1 MB string in one cell;
   - `{ rows: [["fentanyl", "0", "50", "mg/kg"]], timeFormat: "parsecs/x" }`;
   - `seq` of `-1`, `1e309`, `"x"` and `null`;
   - `Shiny.setInputValue('doseGrid_history', 'undo')`, and
     `{ action: ['undo', 'redo'] }`.

   For each, record what the grid shows afterwards.
2. Can any of them put into the **draft** a row that `validateDoseTableInput()`
   refuses, or a unit the drug does not offer? Can any make Apply produce a
   plot from a row the grid does not show?
3. The app rewrites the address bar into a bookmark of the session after
   every change: apply a table, then copy the address. Edit its `DT`
   parameter to hold a hostile cell, a unit the drug does not offer, and 600
   rows. Also check that no `doseGrid` or `doseGrid_history` value appears in
   any bookmark. Open it on both
   grids, and compare with what `AGENTS.md` (*A bookmark link is untrusted
   input*) says must happen.

### B9. The draft, Apply and the rest of the app

1. Edit, Undo, Redo and Apply with the buttons: Apply is enabled only while
   the draft differs from the applied table. After Apply, the plot shows the
   doses, and Undo is disabled.
2. While a cell is half typed:
   - click the plot to add a dose;
   - double-click a drug's line on the plot to open Edit doses, change a dose
     there and press OK;
   - load a teaching scenario from the Help tab;
   - run Suggest Dosing.

   Each writes the table directly. The grid must show the new table, the
   history must be cleared, and nothing half typed may leak into it.
3. Open the app with `?scenario=<id>`, using a scenario id from the Help tab's
   scenario list, and with a bookmark link. In both cases the grid must show
   the right table on first draw.

### B10. Accessibility

1. With a screen reader if you have one (NVDA, VoiceOver, Orca), or with
   Playwright's `locator.ariaSnapshot()`, or with axe
   (`npm install --no-save @axe-core/playwright`): report the name, role and
   state of each cell and of the drug list, and any axe violation inside the
   grid's root.
2. The whole of B5.2 with the keyboard only, and with the screen reader
   announcing each cell.
3. Focus is always visible. Colour is never the only signal: an illicit drug
   is red; is anything else said?

### B11. Layout and browsers

1. Viewports 1600×1000, 1280×800, 1024×768 and 390×844 (a phone; also
   Playwright's `devices['iPhone 13']`). Check that:
   - the columns fit the sidebar;
   - long units wrap;
   - the drug list is not cut off at the bottom of the sidebar;
   - the row menu is on screen;
   - there is no horizontal scroll.
2. 499 rows: time to draw, time from Enter to the draft holding the edit, and
   scrolling.
3. **M3, the browsers:** run `tests/playwright/dose-grid.test.js` (or B1, B2,
   B4 and B5 by hand) in Firefox and WebKit as well as Chromium. The native
   grid's Units cell is a real `<select>`, whose keyboard type-ahead and change
   events differ between browsers. Report every difference.

## Part C: the code

Read `inst/www/dose-grid.js`, `R/dose-grid.R` and the native block in
`R/app_server.R`, and report:

1. Every path by which the grid's rows and the server's draft can disagree for
   more than a moment (the `seq`/`ack` rule in `receive()`, and the
   server-side `doseGridAck`), and whether the user would see it.
2. Every rule of `hookDoseTableUpdate()`, `hookSanitize()` and
   `hookRememberPastedUnits()` in `hot_funs.js` that `dose-grid.js` does not
   reproduce, or reproduces differently. Quote both lines.
3. Every place an error could be raised in an observer of the native block,
   which would end the session.
4. Anything in the CSS or the markup that would break with bslib's dark mode
   or a browser zoom of 200%.

## Your report

1. A verdict, in one paragraph: is the native grid a suitable replacement for
   rhandsontable in the main dose table, as it is, with fixes (list them), or
   not (say why)?
2. A table of findings: severity, reproduced or suspected, grid(s), file and
   line, the quoted line, the reproduction, and the expected and actual
   results.
3. A table of every test in Parts A and B: its number, and its result on each
   grid.
4. What you could not test, and why.
