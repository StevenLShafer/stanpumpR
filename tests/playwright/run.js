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

const { spawnSync } = require('child_process');
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
    const res = spawnSync(process.execPath, ['--test', '--test-concurrency=1', '--test-reporter=spec', file], {
      stdio: 'inherit',
      env: Object.assign({}, process.env, { STANPUMPR_URL: url, STANPUMPR_GRID: grid })
    });
    if (app) app.stop();
    if (res.status !== 0) failed = true;
  }
  process.exit(failed ? 1 : 0);
})().catch(e => { console.error(e); process.exit(1); });
