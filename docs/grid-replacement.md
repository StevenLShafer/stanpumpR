# Replacing rhandsontable

Status: **proposal and prototype, awaiting Steven Shafer's decision** (2026-11-04).
Nothing is switched over: the app still draws every grid with rhandsontable.
The prototype of the main dose grid is behind a switch that is off by default
(`options(stanpumpR.doseGrid = "native")`).

## Recommendation

Replace rhandsontable with **a small purpose-built grid** (option 1 below):
a plain HTML table of form controls with its own Shiny input binding. Do it
grid by grid in the order given at the end, the main dose table first. Then
drop rhandsontable from `DESCRIPTION`.

The reasons, each measured below:

1. **It fixes two data-entry defects the current grid has, which neither
   library option fixes by itself.** Both come from the server redrawing the
   grid after every edit:
   - the first key typed after a commit is lost;
   - over a link as slow as a hosted app's, a whole row typed after another
     is lost.
2. **Its licence is ours**, and it adds no dependency.
3. **It is about 9 KB gzipped**, against 100-525 KB for the libraries.
4. **It is held to the same regression suite**, which it passes: 55 of 56
   cases, the 56th being a page-level script error from the rhandsontable
   grid still on the Single Paper Review tab. rhandsontable has 16 known
   failures besides that one, and each is a real defect of the grid in use
   today. Several can put a wrong dose, drug or unit into the table without
   a word: a typed `1,5` stored as 15, a drug name or an emptied cell
   committed as another drug, a refused row's dose applied to the row's drug,
   and an undo back to a unit the drug does not offer.

## Why replace it

- **Frozen.** rhandsontable 0.3.8 (CRAN, 2021-05-27; `renv.lock`) bundles
  Handsontable 6.2.2 (released 2018-12-19), the last MIT-licensed release. Its
  README says: "To avoid license violations for existing users,
  `rhandsontable` will not update `handsontable` beyond v6.2.2." Handsontable
  is now at 18.1.2 (2026-10-07).
- **Advisories.** CVE-2021-23446 (GHSA-hf66-r44g-p7j9, CVSS 7.5), a ReDoS in
  `Handsontable.helper.isNumeric`, affects every version before 10.0.0. The
  brief also cites an XSS fix in 8.2.0, but I could not confirm it from this
  session: the vendor's site, osv.dev and GitHub's global advisory API are
  outside its network. The 2026-11-02 audit found no app-specific XSS (hostile
  cells rendered as text), and the suite's "everything is text" case passes on
  both grids.
- **A script error on every page load.** rhandsontable's bundled
  `numbro/languages.min.js` throws `exports is not defined`. It is harmless
  today, but it is noise in every browser console and in the suite, and it
  stays until the last rhandsontable grid is gone.
- **Fragility.** Most of the 2026-11 UI fixes were in or around it:
  - `hookDoseTableUpdate()`, `hookSanitize()`, `hookRememberPastedUnits()`
    and the paste and per-row unit handling in `inst/www/hot_funs.js`;
  - `blankMissingCells()` and `tickedRows()` for the nulls a context-menu
    insert sends;
  - `createHOT()`;
  - the `hot_to_r()` round trip on every edit.

  The causes are structural. Every edit is a server round trip that redraws
  the grid. The strict dropdown checks a pasted unit against the list the row
  had *before* the paste. And Handsontable reports the no-op writes of
  rhandsontable's own `afterCreateRow` handler, late, as edits.

## What the suite found

The suite is `tests/playwright/` (README there): 56 cases, driven by
keystrokes and the clipboard in headless Chromium, and checked on the screen
and in the server's draft (`exportTestValues()`, test mode only). The same
cases run against both grids, through a small adapter per grid.

| Case | rhandsontable | prototype |
|---|---|---|
| Drug autocomplete, strict, illicit opt-in, red | pass | pass |
| Units: only the drug's, chosen by keyboard, long units wrap after the route | pass | pass |
| Time/Dose validated as R's `validateTime()`/`validateDose()` | pass | pass |
| Paste: whole rows, partial columns, past the last row, refused units said | pass | pass |
| Paste of 600 rows stops at the row limit | pass, **269 s and 344 s** (two runs) | pass, **3.3 s and 3.5 s** |
| Paste of 510 rows stops at 499 doses, **and says so** | **fail: silent**, 331 s | pass, 0.3 s |
| A typed `1,5` or `1a0` is refused with a message, not changed | **fail: stored as 15 and 10, without a word** | pass |
| A name not in the library is refused | **fail, intermittently: `notadrug` stored as norepinephrine** | pass |
| An illicit drug is said, not only drawn red | **fail** | pass |
| Home and End go to the row's first and last cell | pass | pass |
| Ctrl+Z with the focus outside the grid undoes the last edit | **fail: Ctrl+Z does nothing** | pass |
| The grid's own code raises no script error (edits, drug list, row menu) | pass | pass |
| Clicking a cell again and typing replaces its value | pass | pass (failed before 2026-11-05: appended at the caret) |
| A drug chosen with the arrow keys is taken, even when the name typed is a drug | pass | pass (failed before: `digoxin` kept over `digoxinIV`) |
| A pasted row whose drug is refused changes nothing | **fail: its dose goes to the row's drug** | pass |
| A column of drug names pasted over rows starts each row afresh | **fail: the old doses kept for the new drugs** | pass |
| Emptying a drug cell and pressing Enter clears the row | **fail: stored as acenocoumarol** | pass |
| Undo never brings back a unit the drug does not offer | **fail: morphine in mg/kg** | pass |
| A unit pasted on a Units cell is used, or refused and said; ArrowDown there moves down | pass | pass (failed before: no paste; ArrowDown changed the unit) |
| A paste leaves a blank row below the last dose | pass | pass (failed before) |
| Rows inserted, removed, all removed | pass | pass |
| Time header tip; change of time unit; time-format stamp | pass | pass |
| Draft / Apply / undo / redo; a gas rule's rows drawn | pass | pass |
| Markup typed or pasted is never run or drawn | pass | pass |
| Typing `morphine` Tab `15` Tab `4` Tab at 80 ms a key | **fail: stored as time 5, dose 0** | pass |
| Two rows typed one after another over a 400 ms link | **fail: first row time 5 dose 0, second row lost** | pass |
| After a refused drug name, Escape gives the cell back | **fail: editor stuck open; next text appended** | pass |
| Every cell has a name a screen reader can read | **fail: one shared, unlabelled textarea** | pass |
| Ctrl+Z undoes the last edit, one at a time; Ctrl+Y / Ctrl+Shift+Z redo | **fail: Ctrl+Z does nothing** | pass |
| Ctrl+Z right after Enter over a 400 ms link undoes that edit | **fail: Ctrl+Z does nothing** | pass |
| Ctrl+Z while typing undoes the typing only; Ctrl+Z after Apply stops at Apply | pass | pass |
| No script errors on the page | fail (numbro) | fail (numbro, from Paper Review's grid) |

The two typing defects are the important findings. In the first, every
commit sends the edit to the server, the server's draft changes,
`renderRHandsontable()` redraws the grid, and the editor that the next key
opened is destroyed with that key. On localhost this happened at every typing
speed tried (0, 80, 150 and 300 ms a key). Over a 400 ms link, a whole
second row is lost as well. Nothing warns the user, and the table then holds
a time of 5 where 15 was typed.

The Handsontable-specific workarounds the suite needed are themselves
findings:

- **The 500 ms double-click detector.** Two clicks on one cell within 500 ms
  open the editor on the cell's text, so a typed value is appended to the old
  one.
- **Typing into an open Units editor** appends to the current unit, which the
  strict list then refuses.
- **ArrowDown in an open Units list** moves the grid's selection, not the
  list's.

## The options

| | 1. Purpose-built grid | 2a. Tabulator | 2b. AG Grid Community | 3. Current Handsontable |
|---|---|---|---|---|
| Licence | ours | MIT | MIT (community); Enterprise is commercial | proprietary; free licence for non-commercial use (below) |
| Maintenance | us (about 750 lines of JS) | active: 6.7.1, 2026-10-22; essentially one maintainer | active: 36.2.0, 2026-09-16; a company | active: 18.1.2, 2026-10-07; a company |
| Bundle (minified / gzipped) | 31 KB / 9 KB | 455 KB / 104 KB, plus 28 KB CSS | 2.06 MB / 525 KB | 1.88 MB / 452 KB (rhandsontable's 6.2.2 is 889 KB, plus 0.3 MB of jQuery, chroma, numbro and sparkline) |
| Security history | none; renders with `.value` and `.textContent` only, checked by a test | not checked here (see note); renders cells through `innerHTML` (66 uses in the bundle), so safe only with text formatters | not checked here | CVE-2021-23446 before 10.0.0 |
| Per-row unit list | native `<select>` per row | `editor: "list"` with `editorParams` as a function of the cell | `agSelectCellEditor` with `cellEditorParams` as a function; the searchable rich select is Enterprise | `cells()` returning a per-cell `source` |
| Strict drug autocomplete | ARIA combobox, about 100 lines | `list` editor with `autocomplete: true` | Enterprise (rich select), or custom | built in (the current one) |
| Paste of rows | custom TSV parser, about 30 lines; rules shared with the old grid | built in (`clipboardPasteAction`), but our unit rules need a custom parser and action | **Enterprise only** (`ClipboardModule` is in `ag-grid-enterprise`, licence "Commercial") | built in |
| Range selection, copy, drag-fill | not in the prototype (below) | built in | range selection is Enterprise | built in |
| Shiny wiring | its own input binding (done) | a custom binding or htmlwidget | a custom binding or htmlwidget | a custom binding (rhandsontable cannot be used) |
| Server round trip per edit | one value per commit; never redrawn under the user | depends on the binding: the same design is possible | the same | the same |
| Cost, main grid | **done (prototype)**; about 2-3 days to production | about 4-6 days | about 5-8 days, plus custom paste | about 4-6 days, plus a licence decision |
| Cost, each other grid | 0.5-1 day once the binding takes a column spec | about 1 day | about 1-2 days | about 1 day |

Note: I checked bundle sizes, licences and release dates from the npm registry
on 2026-11-04. I could not reach the advisory databases (osv.dev, GitHub's
global advisory API) from this session, so Tabulator's and AG Grid's security
history needs checking before either is chosen.

Some judgments behind the table:

- **The libraries do not remove the hard part.** Our rules are what took the
  2026-11 fixes: a unit kept only if the row's drug offers it, never swapped
  for another; a refused entry keeps the previous value and is said; a drug
  chosen alone resets its row; a time stamped with its format. Every library
  needs them written in its own hook API, as `hot_funs.js` writes them for
  Handsontable. The prototype writes them once, in the order they apply,
  without a hook sequence to reverse-engineer.
- **AG Grid Community lacks paste.** The thing the dose table most needs is in
  Enterprise.
- **Tabulator is the best library option.** It is MIT, small enough, does
  per-row lists and paste, and is maintained. If Shafer prefers not to own a
  grid, this is the one. Its HTML formatters need the same discipline the
  prototype has by construction.

### Option 3: does stanpumpR qualify for Handsontable's free licence?

**Probably not, or not cleanly. This question is Shafer's, and perhaps
counsel's.** The licence that ships in the npm package is
`handsontable-non-commercial-license.pdf`, version 4.0, last updated
2 March 2026.

- §2.2(a) permits "strictly non-commercial purposes, including education,
  research, study, personal use, testing, and demonstration".
- §2.2(b) limits use "in commercial or institutional environments (including
  business entities, professional practices, governmental and
  intergovernmental entities)" to a 45-day evaluation, outside production.
- §2.3(a) counts as commercial "using the Deliverables in the course of
  business or professional operations ... irrespective of whether commercial
  advantage is actually achieved".
- §2.3(b) deems use by "governmental and intergovernmental entities,
  including public offices" commercial.
- `LICENSE.txt` forbids using it "to develop software which may be considered
  competitive with this software".
- §3.1 ties the licence to a key "as from time to time amended".

stanpumpR is free, and its teaching and research use fits §2.2(a). But its
stated purpose is to "facilitate perioperative patient care" (AGENTS.md,
*Intended use*). It is used by clinicians in their professional work, and it
can be hosted by a hospital or a public university, which §2.2(b) and §2.3
put outside the free licence. Each such user, not only stanpumpR, would need a
commercial licence. Even if stanpumpR qualified, option 3 would still need a
custom binding and all of our rules ported to the new API.

## The prototype

The browser half is `inst/www/dose-grid.js`; the server half is
`R/dose-grid.R` plus a block in `app_server()`. The switch:
`options(stanpumpR.doseGrid = "native")`, or `STANPUMPR_DOSE_GRID=native`.
rhandsontable is drawn otherwise.

**The browser:**
- A `<table>` of `<input>`s (Drug, Time, Dose). Drug is an ARIA combobox with
  a listbox of the library's names, which honours the illicit opt-in and
  draws illicit names red.
- Units is a native `<select>` of the row's drug's units. It is transparent
  over a `<span>` that shows the unit wrapped after its route
  (`unitWrapText()`), so the browser's own list opens on click or key.
- Every cell is labelled ("Dose/Target, row 3").
- No cell has an id or a name, and each carries `data-shiny-no-bind-input`.
- Text goes in with `.value` and `.textContent` only;
  `tests/testthat/test-dose-grid.R` fails on any `innerHTML`.

**Checking:** each entry is checked as it is committed, by the same
`validateTime()`/`validateDose()` that `hot_funs.js` holds to R's. The rules
of a row are the old grid's, written once:
- a drug chosen alone starts at time 0, dose 0 and its default unit;
- a cleared drug clears its row;
- a time or dose beside no drug is not kept;
- a refused entry keeps its previous value, with a message;
- a blank dose in a drug's row is refused;
- a pasted unit is kept only if the drug offers it, and is never swapped for
  another;
- a paste stops at the row limit and says so.

**Committing:** Enter, Tab or leaving the cell commits it.

**To the server:** each committed edit sends the whole table once:
`{rows, timeFormat, seq}`. The server reads it with `doseGridValue()`, which
never raises an error and treats every cell as untrusted. It then stores it
through `storeGridEdit()`, the same function the rhandsontable observer now
calls: the time-format stamp, the trailing blank row, the gas rules, and
`doseTableHistory()$do()`.

**Back to the browser:** the server sends the draft back with `ack`, the seq
of the last edit it read. The grid ignores a draft older than its latest
edit, and does not redraw a draft equal to what it holds. So the grid is
redrawn only when the server changed something: a gas rule, undo or redo, a
dialog or scenario writing `doseTable()`, a change of time unit. Even then,
the text being typed in the focused cell is kept.

**Unchanged:** the draft, Apply and undo/redo lifecycle (`{undomanager}`), and
the rule that a direct write to `doseTable()` resets the draft.

**Ctrl+Z** (Cmd+Z on a Mac) undoes the last edit, one committed edit at a time.
**Ctrl+Y** or **Ctrl+Shift+Z** redoes it. Both go through the same history as
the Undo and Redo buttons. While a cell is being typed in, the keys belong to
the cell and undo the typing. The grid sends the request as an input of its
own (`input$doseGrid_history`) rather than clicking the button. The button is
enabled only once the server has the edit, so a Ctrl+Z pressed right after
Enter over a slow link would have been dropped; the request instead goes in
order, right behind the edit it undoes. As with the button, Ctrl+Z does not
reach back past Apply, which starts a fresh history.

On rhandsontable, Ctrl+Z does nothing, because the server's redraw after every
edit clears Handsontable's own undo history.

**Rows:** right-click a row, or press Shift+F10 or the context-menu key, for
Insert row above, Insert row below and Remove row.

**Not in the prototype**, and not needed (Shafer, 2026-11-04: never used with
the rhandsontable grid):
- rectangular range selection, and with it copy of several cells, a paste
  repeated over a selection, and Delete over a range;
- drag-fill;
- resizable columns.

**Tidying left for a production version:**
- move `validateTime()`/`validateDose()`, the notifications and
  `unitWrapText()` out of `hot_funs.js` into a file of their own;
- check the grid on a phone;
- port the Node replays in `test-dose-table-paste.R` and
  `test-ui-keyboard-crashes.R` from `hot_funs.js` to `dose-grid.js`'s rules;
- delete `createHOT()`, the HOT observer and their tests once the switch is
  removed.

## Migration order for the other grids

Once the main grid is accepted, the binding should take a column
specification from R (`text`, `time`, `dose`, `checkbox`, `select` with
per-row options, read-only). Then each dialog's grid is a few lines of R. In
order:

1. **Main dose table** (`doseTableHTML`; `R/createHOT.R`, `hot_funs.js`).
   Prototype done.
2. **Edit doses dialog** (`editPriorDosesTable`). Delete checkbox, Time, Dose,
   and Units for one drug: the main grid's rules without a Drug column. It is
   the second most used grid, and it shares `hookSanitize()` today.
3. **Single Paper Review doses** (`R/paper-review-app.R`, `ns("doses")`).
   Time, Dose and a Units dropdown, today as **numeric** Handsontable columns
   with none of the dose table's hooks. So it has the parsing `createHOT()`
   warns about (`"1,000"` read as 1, `"1,5"` as 1.5), and nothing is refused
   where it is typed. This is a finding outside this task, recorded here
   rather than fixed. It is also the grid whose bundled `numbro` keeps the
   page-level script error alive.
4. **Events editor** (`editEventsTableHTML`): Delete, Time, Event (a select).
5. **Suggest Dosing targets** (`targetTableHTML`): Time and Target, with
   inserted rows.
6. **Thresholds** (`editThresholdsTable`): a read-only name and one number. It
   could be a column of `numericInput()`s instead of a grid.
7. **Drug Library** (`editDrugsHTML`). Eleven columns, colours, per-cell
   read-only. It is the largest and least used, and better as a form for one
   drug at a time than as a grid.
8. **Remove rhandsontable**: `DESCRIPTION`, `renv.lock` (`renv::snapshot()`),
   `R/createHOT.R`, `R/utils-shiny-server.R`'s `addHotHooks()`, the
   rhandsontable parts of `hot_funs.js` and `app.css`, and the rhandsontable
   test inputs (`gridInput()` in the UI tests). Only then does the numbro
   error go, and with it Handsontable 6.2.2 and CVE-2021-23446: until this
   step the app still loads them, for the grids not yet moved, whatever the
   main grid uses.

## The external review of 2026-11-05 (Codex)

Codex reviewed the PR against `docs/grid-replacement-review.md`, on both
grids, at `4a4ff56`. Its verdict was "suitable with the listed fixes".
Each finding was checked here against the code, and reproduced in the
browser where it could be, before anything was changed (AGENTS.md,
*Adversarial review by another LLM*).

| # | Finding | Checked | What was done |
|---|---|---|---|
| F01 | Typed characters a number cannot hold are dropped by the key filter, so `1,5` becomes 15, `1e3` 13, `1a0` 10 and an elapsed `1:30` 130, without a word. **Both grids.** | Reproduced on both. | Prototype: no key filter. An entry is read whole when it is committed and refused with a message. The rhandsontable grid in production has the same defect (`hookFilterKeys()`); see decision 6. |
| F02 | A negative `seq` from the browser makes the grid ignore every draft the server sends, while Apply commits it. | Confirmed by reading. Needs a hand-made request. | `seq` is a count, so a negative one reads as 0, and the server's acknowledgement only rises. |
| F03 | The server accepts a unit the drug does not offer (fentanyl in mg/kg) sent from the browser's console. | Confirmed by reading: `validateDoseTableInput()` checks that a unit exists, not that the drug offers it. | Every table from the browser now passes through `restoredDoseTable()`, the rule bookmark links are read by: such a unit is blanked (never swapped for the default), with a message. |
| F04 | A draft over the dose limit is stored, and Apply commits it. | Confirmed by reading. | The same: the cell is blanked before it is stored. |
| F05 | Ctrl+Z with the focus outside the grid (on the Apply button) is the browser's own undo, which changes the last-edited cell on screen but not in the draft. | Reproduced. | Ctrl+Z / Ctrl+Y on a button or the page, not in a text field, are the dose table's undo and redo. |
| F06 | Home and End move the caret, where rhandsontable moved to the row's first and last cell. | Confirmed by reading. | Home and End go to the row's first and last cell unless the cell is being typed in; Ctrl+Home and Ctrl+End to the first and last row. |
| F07 | Hard-coded light colours under a dark theme; at 200% zoom the columns are crushed with no sideways scroll. | Confirmed by reading. The app has no dark mode today. | The colours are the theme's variables, and the table keeps a 300 px minimum width and scrolls sideways. |
| F08 | An illicit drug is shown only by its colour. **Both grids.** | Confirmed. | The prototype's Drug cell describes it ("illicit drug, a research model") to a screen reader and in a tooltip. |
| F09, F10 | The current grid loses keystrokes and is slow at 499 rows. | Already in the suite; confirmed again. | Nothing. These are what the prototype fixes. |
| F11 | A 1 MB string, or a row naming no drug, sent from the console is stored. | Confirmed by reading. | Blanked by `restoredDoseTable()` (F03). |
| F12 | Sevoflurane brings no oxygen row. | An error in the brief, not the code: oxygen comes only with nitrous oxide. | The brief is corrected. |
| F13 | The row menu's Bootstrap `dropdown-menu` class makes Bootstrap's own handler throw on Escape. | Reproduced. | The menu has its own class and style. |
| F14 | Several suite cases could pass with their behaviour broken. | Confirmed for each case named. | Strengthened (below). |

**Reproduced intermittently, on rhandsontable:** a typed drug name committed as another drug. The review reported `morphine` stored as `mannitol` and `cocaine` as `calcium`. A probe typing `morphine` over a drug at 0, 40 and 80 ms a key stored morphine every time. But a later full run of the suite stored the refused name `notadrug` as **norepinephrine**. When Enter is pressed, Handsontable's autocomplete can still hold the matches for what was typed a moment earlier ("no…", "m…"), and Enter takes its first entry. In the prototype the list is rebuilt on every keystroke, and Enter takes a match only when the text typed is no drug at all. The suite's "a name that is not in the library is refused" now records this as a Handsontable gap.

**Not reproduced:** rhandsontable drawing no rows on a phone. It draws them, but its Units column is cut off at 390 px wide, where the prototype fits all four columns.

**The suite now:**

- checks the screen as well as the draft wherever a case changes a row (`holds()`);
- requires a typed `1,5` or `1a0` to be refused with a message;
- compares the unit lists in order, including a drug with one unit;
- checks the cell's text after Ctrl+Z while typing;
- asserts the ventilation row a gas brings;
- requires a message when a paste reaches the row limit;
- covers Ctrl+Z with the focus outside the grid, Home and End, and the illicit description;
- separates script errors from the grid itself (none, for either grid) from the page-level rhandsontable error.

On the server side, `test-dose-grid.R` covers each value the review sent from the console.

## The external review of 2026-11-05 (Gemini)

Gemini (Antigravity) sent four Playwright scripts and one results file. The
scripts cover parts A1, B1-B2, B3 and B4-B5 of the brief; the results file is
B3, the Time and Dose entries. It sent no written findings. Its B3 data was
taken before `0d481c7`: a typed `1,5` is still stored as 15 in it, on the
prototype as well. So the scripts were run again here, unchanged except for
their paths, against `0d481c7` on both grids. Every result below is from those
runs, read against R's `validateTime()`/`validateDose()` and checked by hand in
the browser.

**Prototype defects found, each fixed and now a case of the suite:**

| What the scripts did | What happened | Fix |
|---|---|---|
| Clicked a cell that already had the focus, then typed | The text was added at the caret, not replaced: a reset to `0` over `1000` gave `01000`, over `1000000001` gave `10000000001`. The cause of every odd prototype value in Gemini's B3 data. | A cell is "being edited" only once something is typed in it, or after F2 or a double click. Until then any click selects its whole text, and what is typed replaces it, as in a spreadsheet. |
| Typed `digoxin`, ArrowDown to `digoxinIV`, Enter | **digoxin** was stored: because the text typed was itself a drug, Enter took the text over the drug chosen with the arrow keys. | Enter, Tab and leaving the cell take the option the user moved to. An exact name is listed first. |
| Pasted `notadrug 5 999 mg/kg` over a propofol row (**both grids**) | The drug was refused, but its time, dose and unit were applied to the propofol row: **propofol 5 999 mg/kg**. | A pasted row whose drug is refused is refused whole, and said so. |
| Pressed ArrowDown on a Units cell | The unit changed (`mg` to `mg/kg`): on a `<select>` the arrow keys change the value. A keyboard user going down the column changed every row's unit. | ArrowUp and ArrowDown move between rows in every column. The list opens with Alt+Down or Space, or the unit is typed. |
| Pressed Ctrl+V on a Units cell | Nothing, and nothing said: a browser sends no paste event to a `<select>`. | For that one key the focus moves to a hidden text field, which gets the paste, and comes back. The paste is applied to the cell, or refused and said. |
| Pasted 50 rows | No blank row was drawn below them, though the draft had one. | Drawn as soon as it is added. |
| Pasted `ketamine` alone over `propofol 0 2 mg/kg` | Time and dose were reset to 0, but the unit stayed `mg/kg`. | A drug pasted alone takes its default unit, as a drug chosen by typing does. |
| Home and End in the row menu | Did nothing. | First and last item. |

**Defects of the rhandsontable grid in use today, found by the same scripts:**

- Clearing a drug cell (select all, Backspace, Enter) stores **acenocoumarol**, the first drug in the list.
- Undo after a drug pasted over `propofol 0 2 mg/kg` restores **morphine 0 2 mg/kg**, a unit morphine does not offer. The history holds the paste half applied.
- A column of drug names pasted over rows keeps each row's time and dose for the new drug: `propofol 10 100 mg` becomes **fentanyl 10 100 mcg**.
- The refused-row case above, with no message at all.
- `notadrug` stored as **norepinephrine** (the stale list, above), reproduced again.

All five are known gaps of the suite on that grid. Each can put a wrong drug, dose or unit into the table, which is why decision 6 below now asks about more than the key filter.

**Not defects:**

- A time of `007` is stored as `7`. `hot_funs.js` strips leading zeros on purpose, and the result is one of R's fixed points.
- A paste while typing goes in at the caret (`45` becomes `41235`), as in any text field. Handsontable put it in the Time cell.
- The remaining B1-B5 results agree on both grids: list order and highlight, units in order, long units, Tab, Shift+Tab, Enter, Escape, Ctrl+Z and Ctrl+Y, rows inserted and removed, and every connection kept.

## The external review of 2026-11-06 (ChatGPT, second pass)

ChatGPT (Codex) reviewed `f175959` against `4a4ff56`, on both grids, with its
own scripts beside the suite. Its verdict: the prototype is "suitable with two
listed fixes", not yet to be made the default. It found every finding of its
first pass fixed on the prototype (F13 partly: see N03), and no script
injection, ended session, cross-session effect or hidden invalid dose. Each new
finding was checked against the code before anything changed; all four hold.

| | What it found | Checked | Fix |
|---|---|---|---|
| N01 | Typing `55` in a Dose cell, then Ctrl+Z, then Ctrl+Y, left `0`: the typing could not be redone. | `onKeyDown()` sent Ctrl+Z and Ctrl+Y to the table's history whenever the cell's text equalled the table's. Ctrl+Z puts the text back, so Ctrl+Y went to the table, which had nothing to redo. | The keys belong to the cell while it is being edited (from the first key typed, F2 or a double click, to Enter, Tab, Escape or leaving it), whatever its text. |
| N02 | `fentanyl BADTIME BADOSE mcg` pasted over `propofol 10 100 mg` gave **fentanyl 10 100 mcg** on both grids: the refused time and dose kept propofol's values under fentanyl, and Apply plotted them. The message said only that "the cell keeps its previous value". | Reproduced from the code on the prototype (`readEntry()` keeps the old value, and only a drug pasted alone reset the row) and by the suite on rhandsontable. | A pasted row is taken whole or not at all: one whose time or dose is refused leaves the row as it was, and the message says the row was not pasted. A row given a new drug starts afresh, at 0, 0 and the drug's default unit, before its pasted cells are read, so `fentanyl 5` over that row is fentanyl 5 0 mcg. A known gap on rhandsontable. |
| N03 | Tab on the row menu closed it and stayed on the cell; a second Tab moved on. | `onMenuKey()` treated Tab as Escape. | Tab closes the menu and moves to the next cell (Shift+Tab, the previous one) from the cell the menu was opened from. |
| N04 | A forged message of 5,000 rows was cut to 499 doses without a word. The grid itself never sends more than it holds. | `doseGridValue()` cut the rows before `doseGridAccepted()` could report a change. | `doseGridValue()` returns `truncated`, and the server says the rows past the limit were not added. |
| S01 | Handsontable 6.2.2, with CVE-2021-23446, is still loaded by the other grids (Single Paper Review and the dialogs). | True. | Recorded, not changed: this pull request replaces only the main grid, and the bundle and its advisory leave the app only at step 8 of the migration order. |

Each fix is a case of the suite. It did not run Firefox or WebKit either:
their downloads are outside its network, as they are outside this session's.

## Decisions for Steven Shafer

These are pending. Record each answer here.

1. **[decision] Which option.** Recommended: 1, the purpose-built grid. The
   alternative library is Tabulator. Licensed Handsontable is not recommended
   (§2.2(b) and §2.3 of its licence).
2. **Spreadsheet features. ANSWERED** (Shafer, 2026-11-04). Range selection,
   copying several cells, a paste repeated over a selection, drag-fill and
   column resizing are not needed: he has never used them with the
   rhandsontable grid. "Control-Z as an undo of the last operation is
   useful": it is implemented (above) and held by five cases of the suite.
   Ctrl+Z stops at Apply, as the Undo button does. If it should reach back
   past an Apply, that is a change to the lifecycle for both grids, and a
   decision of its own.
3. **[decision] A refused drug name is now said** ("not a drug in the
   library") and the cell is given back. rhandsontable refused it silently and
   left the editor stuck. Is a notification right?
4. **[decision] Trying it.** Keep the switch a server option, or let the PR
   preview open on the prototype (for example `STANPUMPR_DOSE_GRID=native` in
   the preview deploy) so it can be tried in a browser?
5. **[decision] The suite in CI.** A workflow with R and Chromium would run
   `tests/playwright/run.js` in about 15 minutes (most of it rhandsontable's
   510-row paste). Every PR, only PRs that touch the grid, or by hand?
6. **[decision] The production grid's data-entry defects** (2026-11-05).
   The rhandsontable grid in use today:
   - stores a typed `1,5` as 15 and `1a0` as 10 without a word, because
     `hookFilterKeys()` in `inst/www/hot_funs.js` drops the keys a number
     cannot hold (a paste of the same text is refused with a message);
   - can commit a drug name as another drug (`notadrug` as norepinephrine,
     a cleared cell as acenocoumarol);
   - applies a refused pasted row's dose to the drug the row held;
   - carries a dose over to a drug pasted in its place, also when the
     pasted time and dose are refused (`fentanyl BADTIME BADOSE mcg` over
     `propofol 10 100 mg` is fentanyl 10 100 mcg);
   - can be undone to a unit the drug does not offer.

   The prototype has none of these. Fix the first, the one-line cause, in its
   own small pull request now (letting every key through, so that
   `hookSanitize()` refuses the entry as it refuses a paste), and switch to
   the prototype for the rest? Or leave all of them until the switch?
