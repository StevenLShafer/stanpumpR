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
`macos-chrome`, `macos-webkit` and `windows-firefox`; the phones are below
(`PLATFORMS` in `browserstack.js`).

BrowserStack's Playwright service has no desktop Safari: its desktop
browsers are Chrome, Edge and Playwright's own Chromium, Firefox and WebKit,
and real Safari is offered only on iPhones (`phone.test.js`). `macos-webkit`,
Playwright's WebKit on macOS, does not work through the tunnel today: the
page loads, but its WebSocket to the app fails (`ws://bs-local.com:7400/`,
error then close 1006), so Shiny never connects and the startup menu never
comes (2026-11-10). HTTP crosses the tunnel and that WebSocket does not, as
it does from an iPhone's Safari, so it looks like WebKit's own networking
on macOS not sending WebSockets through BrowserStack's proxy. WebKit itself
runs here (`STANPUMPR_BROWSER=webkit`). On BrowserStack a session waits for
the load rather than for an idle network, polls for the startup menu (a
single long wait lets BrowserStack close the connection as idle after about
90 s), and, if the menu never comes, says what the page showed, the
WebSockets included (`openSession()` in `harness.js`).
`require('browserstack-local')` must work as well as `require('playwright')`,
and the Playwright here must be one BrowserStack supports (1.63 is).

The usual way is the workflow `.github/workflows/browser-suite-browserstack.yaml`,
run by hand from the Actions tab with a platform and the files to run. It
needs the repository secrets `BROWSERSTACK_USERNAME` and
`BROWSERSTACK_ACCESS_KEY`. A cloud session of Claude Code cannot run it
itself: its network does not reach BrowserStack's endpoints, and the access
key has to travel inside the connection's address, where the session's
proxy cannot add it.

### On a phone

`phone.test.js` is a smaller suite for the dose grid on a phone: a tap
instead of a click, a value committed by tapping the next cell, the drug
list chosen by tap, a tap on Units that reaches its list, Apply, and the
bookmark. The Units list that opens is the system's own, which Playwright
cannot drive, so the case checks that the tap is not suppressed and then
chooses the unit as the list would (`selectOption()`); it does not open or
use the system's picker. One file serves real iPhones (Safari) and real
Android phones (Chrome). It keeps to the limits BrowserStack sets on
Playwright on a real iPhone (its "Supported Playwright commands for iOS"),
the stricter of the two:

- one browser context and one page, so each case loads the app afresh;
- no `page.$$eval()`, no `pageerror` event, no clipboard;
- `page.evaluate()` returns real values only, so the server's state is read
  with a synchronous request;
- text goes to a cell (`locator.fill()`), and a cell is committed by tapping
  the next one: on a real iPhone neither the page's keyboard nor Enter
  reached the grid. Committing with Enter is the desktop suite's;
- the bookmark case reloads from inside the page (`location.reload()`): on
  the iPhone 16 Playwright's own navigation did not survive BrowserStack's
  translation.

On a real Android phone two things differ. The page's load event comes late
through the tunnel, so a case waits for the startup menu, and the reload for
a connected Shiny, not for the load. With the keyboard up, bringing a target
below it into view pans the visual viewport, and Playwright's check that
nothing covers a target does not allow for the pan: it found the rows above
the target and refused the tap. So on Android a tap scrolls its target into
view itself, asks the page whether the target is on top there, and is sent
without that check (`tapOn()`); each case checks what its tap did.

On a phone the dose table's sidebar starts collapsed, so each case opens it
with its toggle, as a user does.

Run here, it runs as an iPhone 15 in Playwright's WebKit, so WebKit must be
installed (`npx playwright install webkit`) as well as Chromium, or with
`STANPUMPR_PHONE=android` as a Pixel 7 in Chromium:

```bash
node tests/playwright/run.js phone.test.js
```

```bash
STANPUMPR_PHONE=android node tests/playwright/run.js phone.test.js
```

On BrowserStack it runs on a real phone, which reaches the tunnelled app at
`bs-local.com` (`localhost` would be the phone's own):

- `iphone-16` (iPhone 16, iOS 18) and `iphone-17` (iPhone 17, iOS 26), in
  Safari;
- `android-pixel-9` (Google Pixel 9, Android 16) and `android-galaxy-s25`
  (Samsung Galaxy S25 Ultra, Android 15), in Chrome. An Android phone is
  connected through Playwright's `_android` API, and only with Playwright
  1.62, the newest BrowserStack lists for Android (`ANDROID_PLAYWRIGHT` in
  `browserstack.js`); the workflow installs it for an Android platform, and
  a run with another version says so before it connects.

```bash
BROWSERSTACK_USERNAME=... BROWSERSTACK_ACCESS_KEY=... STANPUMPR_BROWSERSTACK=iphone-16 node tests/playwright/run.js
```

A phone platform runs `phone.test.js` by default, and the desktop files
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
