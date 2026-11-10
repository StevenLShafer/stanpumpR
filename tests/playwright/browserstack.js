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
// Two kinds of platform.  The desktop browsers (real Chrome and Edge, and
// Playwright's own Firefox and WebKit builds) run the whole suite,
// dose-grid.test.js and table-grid.test.js.  The phones (real iPhones with
// Safari, real Android phones with Chrome) run phone.test.js only: on an
// iPhone a session allows one browser context and one page, and only part of
// Playwright's API (BrowserStack's "Supported Playwright commands for iOS"),
// where the desktop suite opens a context for every case and uses
// page.$$eval(), the pageerror event and the clipboard, and one smaller
// suite serves both kinds of phone.  A phone reaches the tunnelled app at
// bs-local.com, not localhost, which is the phone's own (appUrl()).
//
// An Android phone is connected through Playwright's _android API (the
// device, then Chrome on it, which gives one browser context), as
// BrowserStack's sample connects one, and only with Playwright 1.62:
// BrowserStack lists Android for 1.62 and not for 1.63, so the workflow
// installs 1.62 for an Android platform (ANDROID_PLAYWRIGHT).

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
  'windows-firefox': { engine: 'firefox', caps: { os: 'Windows', os_version: '11', browser: 'playwright-firefox', browser_version: 'latest' } },
  // as BrowserStack's own sample connects to one (webkit.connect, safari,
  // deviceName, osVersion, realMobile)
  'iphone-16': { engine: 'webkit', mobile: true, caps: { browser: 'safari', deviceName: 'iPhone 16', osVersion: '18', realMobile: 'true' } },
  'iphone-17': { engine: 'webkit', mobile: true, caps: { browser: 'safari', deviceName: 'iPhone 17', osVersion: '26', realMobile: 'true' } },
  // as BrowserStack's sample connects one (_android.connect, chrome,
  // deviceName, osVersion, realMobile); devices from its Android guide
  'android-pixel-9': { engine: 'chromium', mobile: true, android: true, caps: { browserName: 'chrome', browser: 'chrome', deviceName: 'Google Pixel 9', osVersion: '16.0', realMobile: 'true' } },
  'android-galaxy-s25': { engine: 'chromium', mobile: true, android: true, caps: { browserName: 'chrome', browser: 'chrome', deviceName: 'Samsung Galaxy S25 Ultra', osVersion: '15.0', realMobile: 'true' } }
};

// The Playwright an Android phone needs (the newest BrowserStack lists for
// Android); the workflow reads it from here
const ANDROID_PLAYWRIGHT = '1.62';

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

// Is the platform a phone (phone.test.js, one context and one page)?
function mobile() {
  return enabled() && !!platform().mobile;
}

// The phone's browser context: on BrowserStack, an iPhone's Safari (one
// context on a connected WebKit) or an Android phone's Chrome (the device
// connected, Chrome restarted on it, its one context); here, the browser
// `local` names launched as the phone its `device` descriptor describes.
// `close` ends whichever it was.
async function launchPhone(playwright, local, sessionName) {
  // What is opened is closed again if a later step fails, so that a failed
  // start leaves no browser or device connection behind (CodeRabbit, PR #697)
  const contextOf = async (browser, options) => {
    try {
      const context = await browser.newContext(options);
      return { context, close: () => browser.close() };
    } catch (e) {
      await browser.close().catch(() => {});
      throw e;
    }
  };
  if (!enabled()) {
    return contextOf(await playwright[local.engine].launch(), playwright.devices[local.device]);
  }
  const p = platform();
  if (p.android) {
    const version = playwrightVersion();
    if (!version.startsWith(ANDROID_PLAYWRIGHT + '.')) {
      throw new Error(`an Android phone on BrowserStack needs Playwright ${ANDROID_PLAYWRIGHT}, not ${version}`);
    }
    const device = await playwright._android.connect(wsEndpoint(sessionName), { timeout: 120000 });
    let context;
    try {
      await device.shell('am force-stop com.android.chrome');
      context = await device.launchBrowser();
    } catch (e) {
      await device.close().catch(() => {});
      throw e;
    }
    return { context, close: async () => { await context.close().catch(() => {}); await device.close(); } };
  }
  return contextOf(await playwright[p.engine].connect(wsEndpoint(sessionName), { timeout: 120000 }));
}

// The app's address as the browser must use it: a phone on BrowserStack
// reaches the tunnelled app at bs-local.com, which BrowserStack Local maps
// to this machine; localhost would be the phone's own, and iOS Safari does
// not load it through the tunnel
function appUrl(url) {
  if (!mobile()) return url;
  const u = new URL(url);
  u.hostname = 'bs-local.com';
  return u.toString();
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

module.exports = { PLATFORMS, ANDROID_PLAYWRIGHT, enabled, platform, mobile, appUrl, engine, launch, launchPhone, wsEndpoint, startTunnel, localIdentifier };
