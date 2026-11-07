# The grids' browser suite

What the app's grids must do, driven in headless Chromium the way a user
drives them (clicks, keys, the clipboard), and checked on the screen and on
the server:

- `dose-grid.test.js`: the main dose table, the dose grid
  (`inst/www/dose-grid.js`, `R/dose-grid.R`), checked on the screen and in the
  server's draft;
- `table-grid.test.js`: the dialogs' grids, the table grid
  (`inst/www/table-grid.js`, `R/table-grid.R`), checked in what the grid sends
  and in what each dialog's Apply writes.

It was written against the rhandsontable grid first, so that its replacement
was held to the same bar; rhandsontable was removed on 2026-11-06
(`docs/grid-replacement.md` keeps what it failed).

## Running it

Chromium and Playwright must be installed, `require('playwright')` must work,
and R must be able to `pkgload::load_all()` the package.

```bash
node tests/playwright/run.js
```

```bash
node tests/playwright/run.js table-grid.test.js
```

`run.js` starts the app in Shiny's test mode (`options(shiny.testmode = TRUE)`)
on port 7400, runs the test files with `node --test`, and stops the app. A
full run takes about 7 minutes.

To test an app that is already running, set `STANPUMPR_URL` and run a test
file directly:

```bash
STANPUMPR_URL=http://127.0.0.1:7311/ node --test --test-concurrency=1 tests/playwright/dose-grid.test.js
```

`STANPUMPR_BROWSER` chooses the browser: `chromium` (the default), `firefox`
or `webkit`. Off Chromium the clipboard is opened by preference rather than
by permission, and the two slow-link cases are skipped (they emulate the
link through CDP). Firefox and WebKit have not been run here: their
downloads are outside this environment's network. Gemini ran the dose grid's
cases in Firefox 159 through a shim, on 2026-11-06 (`docs/grid-replacement.md`).

`RSCRIPT` names the R binary and `STANPUMPR_RSCRIPT_ARGS` adds arguments
(`--no-init-file` skips the project's `.Rprofile`, and with it renv).

## How it works

- `harness.js` starts the app, opens a session with the startup menu's
  default drugs ticked, and reads the server's state through
  `exportTestValues()` in `R/app_server.R` (the draft, the committed table,
  its time format, the undo state, the events, and each drug's threshold and
  colour), which exists only in test mode.
- `adapters/native.js` is the dose grid's DOM: how to find a cell, read the
  rows, open the row menu.
- After every case the session must still be connected: an error in an
  observer ends it. The dialogs' cases also require a page without script
  errors.
- Every case that changes a row of the dose table checks it twice, on the
  screen and in the server's draft (`holds()` in `dose-grid.test.js`).
- A case one grid is known to fail can be listed in `KNOWN_GAP` at the top of
  `dose-grid.test.js`, with the reason; it is then reported as a todo. None is
  listed today.
- A case with options is written `gt(name, { timeout }, fn)`: `gt()` takes
  the options before the function, as `test()` does.

The R tests (`test-dose-grid.R`, `test-table-grid.R`, and the dialogs'
handlers in `test-ui-adversarial-review.R` and `test-time-units-server.R`)
check the server's side with forged values; this suite runs the grids in a
real browser against the real server.
