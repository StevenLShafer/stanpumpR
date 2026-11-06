#!/usr/bin/env node
// Run the browser suite: the dose grid (dose-grid.test.js) and the dialogs'
// grids (table-grid.test.js), against one app.
//
//   node tests/playwright/run.js                       # both files
//   node tests/playwright/run.js dose-grid.test.js     # one of them
//
// The app runs in Shiny's test mode on port 7400.  Set STANPUMPR_URL to test
// an app that is already running instead.  See README.md.

'use strict';

const { spawn } = require('child_process');
const path = require('path');
const { startApp } = require('./harness');

(async () => {
  const files = (process.argv.slice(2).length ? process.argv.slice(2) : ['dose-grid.test.js', 'table-grid.test.js'])
    .map(f => path.join(__dirname, f));
  let app = null;
  let url = process.env.STANPUMPR_URL;
  if (!url) {
    process.stdout.write('Starting the app...\n');
    app = await startApp(7400);
    url = app.url;
  }
  // spawned, not spawnSync: this process must stay free to run
  const status = await new Promise(resolve => {
    const t = spawn(process.execPath, ['--test', '--test-concurrency=1', '--test-reporter=spec', ...files], {
      stdio: 'inherit',
      env: Object.assign({}, process.env, { STANPUMPR_URL: url, STANPUMPR_GRID: 'native' })
    });
    t.on('exit', code => resolve(code));
  });
  if (app) {
    if (status !== 0) process.stdout.write(`The app's log: ${app.logFile}\n`);
    app.stop();
  }
  process.exit(status === 0 ? 0 : 1);
})().catch(e => { console.error(e); process.exit(1); });
