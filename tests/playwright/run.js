#!/usr/bin/env node
// Run the browser suite: the dose grid (dose-grid.test.js) and the dialogs'
// grids (table-grid.test.js), against one app.
//
//   node tests/playwright/run.js                       # the desktop suite
//   node tests/playwright/run.js dose-grid.test.js     # one of its files
//   node tests/playwright/run.js iphone.test.js        # the iPhone suite
//
// The app runs in Shiny's test mode on port 7400.  Set STANPUMPR_URL to test
// an app that is already running instead.  With STANPUMPR_BROWSERSTACK set
// (browserstack.js), the browsers are on BrowserStack, reaching the app
// through a BrowserStack Local tunnel opened here.  See README.md.

'use strict';

const { spawn } = require('child_process');
const path = require('path');
const { startApp } = require('./harness');
const BS = require('./browserstack');

(async () => {
  // A platform that does not exist, or missing credentials, is said before
  // the app is started (BS.platform() throws)
  const remote = BS.enabled() ? BS.platform() : null;
  // By default, the files the platform runs: an iPhone runs iphone.test.js,
  // everything else the desktop suite (browserstack.js)
  const defaults = remote && remote.mobile ? ['iphone.test.js'] : ['dose-grid.test.js', 'table-grid.test.js'];
  const files = (process.argv.slice(2).length ? process.argv.slice(2) : defaults)
    .map(f => path.join(__dirname, f));
  let app = null;
  let tunnel = null;
  let url = process.env.STANPUMPR_URL;
  if (!url) {
    process.stdout.write('Starting the app...\n');
    app = await startApp(7400);
    url = app.url;
  }
  if (remote) {
    process.stdout.write(`Opening a BrowserStack Local tunnel for ${remote.name}...\n`);
    BS.localIdentifier();  // fixed here, so the tests' sessions use this tunnel
    try {
      tunnel = await BS.startTunnel();
    } catch (e) {
      if (app) app.stop();
      throw e;
    }
  }
  // spawned, not spawnSync: this process must stay free to run.  The tunnel
  // and the app are closed whatever happens, a test process that cannot
  // start included (CodeRabbit, PR #693)
  let status = 1;
  try {
    status = await new Promise((resolve, reject) => {
      const t = spawn(process.execPath, ['--test', '--test-concurrency=1', '--test-reporter=spec', ...files], {
        stdio: 'inherit',
        env: Object.assign({}, process.env, { STANPUMPR_URL: url, STANPUMPR_GRID: 'native' })
      });
      t.on('error', reject);
      t.on('exit', code => resolve(code));
    });
  } finally {
    try {
      if (tunnel) await tunnel.stop();
    } finally {
      if (app) {
        if (status !== 0) process.stdout.write(`The app's log: ${app.logFile}\n`);
        app.stop();
      }
    }
  }
  process.exit(status === 0 ? 0 : 1);
})().catch(e => { console.error(e); process.exit(1); });
