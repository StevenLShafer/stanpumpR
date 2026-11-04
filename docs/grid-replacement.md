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
4. **It is held to the same regression suite**, which it passes: 36 of 37
   cases, the 37th being a page-level script error from the rhandsontable
   grid still on the Single Paper Review tab. rhandsontable passes 32 of the
   37, and the four of its failures that concern the grid are real defects.

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

The suite is `tests/playwright/` (README there): 37 cases, driven by
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
| Rows inserted, removed, all removed | pass | pass |
| Time header tip; change of time unit; time-format stamp | pass | pass |
| Draft / Apply / undo / redo; a gas rule's rows drawn | pass | pass |
| Markup typed or pasted is never run or drawn | pass | pass |
| Typing `morphine` Tab `15` Tab `4` Tab at 80 ms a key | **fail: stored as time 5, dose 0** | pass |
| Two rows typed one after another over a 400 ms link | **fail: first row time 5 dose 0, second row lost** | pass |
| After a refused drug name, Escape gives the cell back | **fail: editor stuck open; next text appended** | pass |
| Every cell has a name a screen reader can read | **fail: one shared, unlabelled textarea** | pass |
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
the rule that a direct write to `doseTable()` resets the draft. Ctrl+Z and
Ctrl+Y outside a cell being typed in press Undo and Redo.

**Rows:** right-click a row, or press Shift+F10 or the context-menu key, for
Insert row above, Insert row below and Remove row.

**Not in the prototype, deliberately until decided:**
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
   error go.

## Decisions for Steven Shafer

These are pending. Record each answer here.

1. **[decision] Which option.** Recommended: 1, the purpose-built grid. The
   alternative library is Tabulator. Licensed Handsontable is not recommended
   (§2.2(b) and §2.3 of its licence).
2. **[decision] Spreadsheet features.** Do users select ranges, copy several
   cells out of the grid, paste one row over a selection of rows, or
   drag-fill? The prototype does none of these. Each can be added, but each is
   code to own: about 1-2 days for range selection with copy, clear and
   repeated paste.
3. **[decision] A refused drug name is now said** ("not a drug in the
   library") and the cell is given back. rhandsontable refused it silently and
   left the editor stuck. Is a notification right?
4. **[decision] Trying it.** Keep the switch a server option, or let the PR
   preview open on the prototype (for example `STANPUMPR_DOSE_GRID=native` in
   the preview deploy) so it can be tried in a browser?
5. **[decision] The suite in CI.** A workflow with R and Chromium would run
   `tests/playwright/run.js` in about 15 minutes (most of it rhandsontable's 600-row paste). Every PR, only PRs that
   touch the grid, or by hand?
