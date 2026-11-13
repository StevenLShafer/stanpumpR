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
link through CDP). All three pass (2026-11-08; `docs/grid-replacement.md`,
"Other engines, a phone, and the review of the fixes").

`RSCRIPT` names the R binary and `STANPUMPR_RSCRIPT_ARGS` adds arguments
(`--no-init-file` skips the project's `.Rprofile`, and with it renv).

## On a phone

`phone.test.js` is a smaller suite for the dose grid on a phone: a tap
instead of a click, a value committed by tapping the next cell, the drug
list chosen by tap, a tap on Units that reaches its list, Apply, and the
bookmark. The Units list that opens is the system's own, which Playwright
cannot drive, so the case checks that the tap is not suppressed and then
chooses the unit as the list would (`selectOption()`); it does not open or
use the system's picker. On a phone the dose table's sidebar starts
collapsed, so each case opens it with its toggle, as a user does.

It runs as an iPhone 15 in Playwright's WebKit, so WebKit must be installed
(`npx playwright install webkit`) as well as Chromium, or with
`STANPUMPR_PHONE=android` as a Pixel 7 in Chromium:

```bash
node tests/playwright/run.js phone.test.js
```

```bash
STANPUMPR_PHONE=android node tests/playwright/run.js phone.test.js
```

It was first written to run on real phones through BrowserStack Automate
(2026-11-09 to 2026-11-10; `docs/grid-replacement.md`, "Real phones on
BrowserStack"), which was removed on 2026-11-13, and it keeps to the
stricter limits Playwright had there on a real iPhone: one browser context
and one page, so each case loads the app afresh; no `page.$$eval()`, no
`pageerror` event, no clipboard; the server's state read with a
synchronous request; text put into a cell with `locator.fill()` and
committed by tapping the next cell (Enter is the desktop suite's); and the
bookmark case reloading from inside the page (`location.reload()`).

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
