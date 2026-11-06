# The dose-grid regression suite

What the main dose table must do, driven in headless Chromium the way a user
drives it (clicks, keys, the clipboard), and checked both on the screen and in
the server's draft. It was written against the rhandsontable grid first, so
that a replacement is held to the same bar (`docs/grid-replacement.md`).

## Running it

Chromium and Playwright must be installed, `require('playwright')` must work,
and R must be able to `pkgload::load_all()` the package.

```bash
node tests/playwright/run.js                 # both grids, one app each
```

```bash
node tests/playwright/run.js handsontable    # the rhandsontable grid only
```

```bash
node tests/playwright/run.js native          # the prototype only
```

`run.js` starts the app for each grid in Shiny's test mode
(`options(shiny.testmode = TRUE, stanpumpR.doseGrid = "<grid>")`) on ports 7400
and up, runs `dose-grid.test.js` with `node --test`, and stops the app. A full
run takes about 4 minutes on the prototype and 10 on rhandsontable, whose
600-row paste alone takes 4 to 6.

To test an app that is already running, set `STANPUMPR_URL` (and
`STANPUMPR_GRID`, which grid it draws) and run the test file directly:

```bash
STANPUMPR_URL=http://127.0.0.1:7311/ STANPUMPR_GRID=native node --test --test-concurrency=1 tests/playwright/dose-grid.test.js
```

`RSCRIPT` names the R binary and `STANPUMPR_RSCRIPT_ARGS` adds arguments
(`--no-init-file` skips the project's `.Rprofile`, and with it renv).

## How it works

- `harness.js` starts the app, opens a session with the startup menu's
  default drugs ticked, and reads the server's state through
  `exportTestValues()` in `R/app_server.R` (the draft, the committed table,
  its time format, the undo state), which exists only in test mode.
- `adapters/<grid>.js` is each grid's DOM: how to find a cell, read the rows,
  open the row menu. The tests send the same keystrokes to both.
- After every case the session must still be connected: an error in an
  observer ends it.
- A case one grid is known to fail is listed in `KNOWN_GAP` at the top of the
  test file, with the reason. It is reported as a todo, so the run stays
  green while the suite still states the bar. On rhandsontable, eighteen
  cases are known to fail:
  - the first key typed after a commit is lost while the server redraws the
    grid;
  - over a 400 ms link, two doses typed one after another lose a row;
  - a typed `1,5` is stored as 15 and `1a0` as 10, without a word: the key
    filter drops the characters a number cannot hold;
  - intermittently, Enter commits the first match the drug list held a
    moment earlier (`notadrug` stored as norepinephrine, and `sevoflurane`
    as sertraline, which fails the gas-rule case as well);
  - a refused drug name leaves its editor stuck open;
  - a paste past the row limit drops the extra rows without a word;
  - the grid's controls have no names a screen reader can read, and an
    illicit drug is shown only by its colour;
  - Ctrl+Z does nothing (four cases): the redraw after every edit clears
    Handsontable's own undo history;
  - from Gemini's scripts (2026-11-05):
    - a cleared drug cell is stored as acenocoumarol;
    - a refused pasted row's dose is applied to the row's drug;
    - a column of drug names pasted over rows keeps the old doses for the
      new drugs;
    - undo can bring back a unit the drug does not offer.
  - from ChatGPT's second pass (2026-11-06): a pasted row whose time and
    dose are refused keeps the old drug's values under the new drug
    (`fentanyl BADTIME BADOSE mcg` over propofol 10 100 mg is fentanyl 10
    100 mcg).

  One case, a forged message of more rows than the grid holds, is the
  prototype's own input and is skipped on rhandsontable.

  The script error that rhandsontable's bundled `numbro` raises on every load
  is a known gap for both grids while any rhandsontable grid is still on the
  page (the Single Paper Review tab has one).
- Every case that changes a row checks it twice, on the screen and in the
  server's draft (`holds()` in the test file).
- A case with options is written `gt(name, { timeout }, fn)`: `gt()` takes
  the options before the function, as `test()` does.

The Node tests in `tests/testthat` (`test-dose-table-paste.R`,
`test-ui-keyboard-crashes.R`) replay these cases against `hot_funs.js` alone.
This suite runs them against the real grid and the real server.
