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

## On BrowserStack

`browserstack.js` runs the same suite in browsers on BrowserStack Automate:
real Chrome and Edge on Windows 11, Chrome on macOS, and Playwright's own
Firefox and WebKit builds. The app still runs here, in test mode, and
`run.js` opens a BrowserStack Local tunnel so the remote browser can reach
it. Set the account's credentials and a platform:

```bash
BROWSERSTACK_USERNAME=... BROWSERSTACK_ACCESS_KEY=... STANPUMPR_BROWSERSTACK=windows-chrome node tests/playwright/run.js
```

The desktop platforms are `windows-chrome`, `windows-edge`,
`macos-chrome`, `macos-webkit` and `windows-firefox`; the iPhones are below
(`PLATFORMS` in `browserstack.js`).
`require('browserstack-local')` must work as well as `require('playwright')`,
and the Playwright here must be one BrowserStack supports (1.63 is).

The usual way is the workflow `.github/workflows/browser-suite-browserstack.yaml`,
run by hand from the Actions tab with a platform and the files to run. It
needs the repository secrets `BROWSERSTACK_USERNAME` and
`BROWSERSTACK_ACCESS_KEY`. A cloud session of Claude Code cannot run it
itself: its network does not reach BrowserStack's endpoints, and the access
key has to travel inside the connection's address, where the session's
proxy cannot add it.

### On an iPhone

`iphone.test.js` is a smaller suite for the dose grid on a phone: a tap
instead of a click, a value committed by tapping the next cell, the drug
list chosen by tap, the system's own list for Units, Apply, and the
bookmark. It keeps to
the limits BrowserStack sets on Playwright on a real iPhone (its "Supported
Playwright commands for iOS"):

- one browser context and one page, so each case loads the app afresh;
- no `page.$$eval()`, no `pageerror` event, no clipboard;
- `page.evaluate()` returns real values only, so the server's state is read
  with a synchronous request;
- text goes to a cell (`locator.fill()`), and a cell is committed by tapping
  the next one: on a real iPhone neither the page's keyboard nor Enter
  reached the grid. Committing with Enter is the desktop suite's.

On a phone the dose table's sidebar starts collapsed, so each case opens it
with its toggle, as a user does.

Run here, it runs in Playwright's WebKit as an iPhone 15, so WebKit must be
installed (`npx playwright install webkit`) as well as Chromium:

```bash
node tests/playwright/run.js iphone.test.js
```

On BrowserStack it runs in Safari on a real iPhone (`iphone-16`: iPhone 16,
iOS 18; `iphone-17`: iPhone 17, iOS 26), which reaches the tunnelled app at
`bs-local.com`, since iOS Safari does not load `localhost` through the
tunnel:

```bash
BROWSERSTACK_USERNAME=... BROWSERSTACK_ACCESS_KEY=... STANPUMPR_BROWSERSTACK=iphone-16 node tests/playwright/run.js
```

An iPhone platform runs `iphone.test.js` by default, and the desktop files
refuse one.

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
