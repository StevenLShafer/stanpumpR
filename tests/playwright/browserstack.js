// Running the browser suite on BrowserStack Automate (README.md here).
//
// With BROWSERSTACK_USERNAME, BROWSERSTACK_ACCESS_KEY and
// STANPUMPR_BROWSERSTACK (a platform name from PLATFORMS) set, the test files
// connect to a browser on BrowserStack instead of launching one here, and
// run.js opens a BrowserStack Local tunnel so that the remote browser can
// reach the app on this machine.  Nothing else in the suite changes: the
// same cases, the same app in Shiny's test mode, the same checks of the
// server's state.
//
// The platforms are desktop browsers: real Chrome and Edge, and Playwright's
// own Firefox and WebKit builds.  BrowserStack also runs Playwright on real
// iPhones (Safari), but there a session allows one browser context and one
// page, and supports only part of Playwright's API (its "Supported Playwright
// commands for iOS"), where this suite opens a context for every case and
// uses page.$$eval(), the pageerror event and the clipboard.  iPhone Safari
// would need a smaller suite of its own (README.md).

'use strict';

// The browsers the suite can run on.  `engine` is the Playwright browser
// type that connects to it, which is also what the test files read as
// STANPUMPR_BROWSER (the clipboard permission and the CDP slow link are
// Chromium's, whichever Chromium it is).
const PLATFORMS = {
  'windows-chrome': { engine: 'chromium', caps: { os: 'Windows', os_version: '11', browser: 'chrome', browser_version: 'latest' } },
  'windows-edge': { engine: 'chromium', caps: { os: 'Windows', os_version: '11', browser: 'edge', browser_version: 'latest' } },
  'macos-chrome': { engine: 'chromium', caps: { os: 'OS X', os_version: 'Sequoia', browser: 'chrome', browser_version: 'latest' } },
  'macos-webkit': { engine: 'webkit', caps: { os: 'OS X', os_version: 'Sequoia', browser: 'playwright-webkit', browser_version: 'latest' } },
  'windows-firefox': { engine: 'firefox', caps: { os: 'Windows', os_version: '11', browser: 'playwright-firefox', browser_version: 'latest' } }
};

const ENDPOINT = 'wss://cdp.browserstack.com/playwright?caps=';

function platformName() {
  return process.env.STANPUMPR_BROWSERSTACK || '';
}

function enabled() {
  return platformName() !== '';
}

// The platform asked for, or an error that names the ones there are and the
// credentials that are missing: a run that cannot connect should say so at
// once, not after the app has started.
function platform() {
  const name = platformName();
  const p = PLATFORMS[name];
  if (!p) {
    throw new Error(`STANPUMPR_BROWSERSTACK=${name} is not a platform; choose one of ${Object.keys(PLATFORMS).join(', ')}`);
  }
  const missing = ['BROWSERSTACK_USERNAME', 'BROWSERSTACK_ACCESS_KEY'].filter(v => !process.env[v]);
  if (missing.length) throw new Error(`BrowserStack needs ${missing.join(' and ')} in the environment`);
  return Object.assign({ name }, p);
}

// One identifier per run, shared by the tunnel and every session, so that
// two runs at once do not use each other's tunnel.
function localIdentifier() {
  if (!process.env.STANPUMPR_BROWSERSTACK_LOCAL_ID) {
    process.env.STANPUMPR_BROWSERSTACK_LOCAL_ID = `stanpumpR-${process.pid}-${Date.now()}`;
  }
  return process.env.STANPUMPR_BROWSERSTACK_LOCAL_ID;
}

function playwrightVersion() {
  return require('playwright/package.json').version;
}

// The connection's address.  The credentials travel inside it, as
// BrowserStack requires, so it is never printed.
function wsEndpoint(sessionName) {
  const p = platform();
  const build = process.env.STANPUMPR_BROWSERSTACK_BUILD ||
    `stanpumpR grids ${process.env.GITHUB_SHA ? process.env.GITHUB_SHA.slice(0, 7) : 'local'} ${p.name}`;
  const caps = Object.assign({}, p.caps, {
    project: 'stanpumpR',
    build,
    name: sessionName,
    'browserstack.username': process.env.BROWSERSTACK_USERNAME,
    'browserstack.accessKey': process.env.BROWSERSTACK_ACCESS_KEY,
    'browserstack.local': 'true',
    'browserstack.localIdentifier': localIdentifier(),
    // the same Playwright on both ends (BrowserStack supports 1.63)
    'browserstack.playwrightVersion': playwrightVersion(),
    'client.playwrightVersion': playwrightVersion()
  });
  return ENDPOINT + encodeURIComponent(JSON.stringify(caps));
}

// The browser the test files use: launched here, or connected on BrowserStack.
async function launch(playwright, engine, options, sessionName) {
  if (!enabled()) return playwright[engine].launch(options);
  return playwright[platform().engine].connect(wsEndpoint(sessionName), { timeout: 120000 });
}

// The Playwright browser type the test files should treat the browser as.
function engine() {
  if (enabled()) return platform().engine;
  return process.env.STANPUMPR_BROWSER || 'chromium';
}

// The tunnel: started before the tests and stopped after them, by run.js.
async function startTunnel() {
  const { Local } = require('browserstack-local');
  const local = new Local();
  await new Promise((resolve, reject) => {
    local.start({
      key: process.env.BROWSERSTACK_ACCESS_KEY,
      localIdentifier: localIdentifier(),
      forceLocal: 'true',
      onlyAutomate: 'true'
    }, err => (err ? reject(new Error('BrowserStack Local did not start: ' + err.message)) : resolve()));
  });
  // Stopping never fails the run and never holds it: an error is said, and
  // a tunnel that does not answer within 30 s is left to the runner's end
  // (CodeRabbit, PR #693)
  return {
    stop: () => new Promise(resolve => {
      const timer = setTimeout(() => {
        process.stderr.write('BrowserStack Local did not stop within 30 s\n');
        resolve();
      }, 30000);
      try {
        local.stop(err => {
          clearTimeout(timer);
          if (err) process.stderr.write('BrowserStack Local did not stop cleanly: ' + err.message + '\n');
          resolve();
        });
      } catch (e) {
        clearTimeout(timer);
        process.stderr.write('BrowserStack Local did not stop cleanly: ' + e.message + '\n');
        resolve();
      }
    })
  };
}

module.exports = { PLATFORMS, enabled, platform, engine, launch, wsEndpoint, startTunnel, localIdentifier };
