#!/usr/bin/env node
// Run the dose-grid suite against one grid or both:
//
//   node tests/playwright/run.js                 # both grids
//   node tests/playwright/run.js handsontable    # the rhandsontable grid
//   node tests/playwright/run.js native          # the prototype
//
// Each grid gets its own app (R, Shiny test mode) on its own port.  Set
// STANPUMPR_URL to test an app that is already running instead (with
// STANPUMPR_GRID naming its grid).  See README.md.

'use strict';

const { spawn } = require('child_process');
const path = require('path');
const { startApp } = require('./harness');

(async () => {
  const grids = process.argv.slice(2).length ? process.argv.slice(2) : ['handsontable', 'native'];
  const file = path.join(__dirname, 'dose-grid.test.js');
  let failed = false;
  for (let i = 0; i < grids.length; i++) {
    const grid = grids[i];
    let app = null;
    let url = process.env.STANPUMPR_URL;
    if (!url) {
      process.stdout.write(`Starting the app with the ${grid} grid...\n`);
      app = await startApp(grid, 7400 + i);
      url = app.url;
    }
    // spawned, not spawnSync: this process must stay free to run
    const status = await new Promise(resolve => {
      const t = spawn(process.execPath, ['--test', '--test-concurrency=1', '--test-reporter=spec', file], {
        stdio: 'inherit',
        env: Object.assign({}, process.env, { STANPUMPR_URL: url, STANPUMPR_GRID: grid })
      });
      t.on('exit', code => resolve(code));
    });
    if (app) {
      if (status !== 0) process.stdout.write(`The app's log: ${app.logFile}\n`);
      app.stop();
    }
    if (status !== 0) failed = true;
  }
  process.exit(failed ? 1 : 0);
})().catch(e => { console.error(e); process.exit(1); });
