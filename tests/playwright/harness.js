// The browser harness for the dose-grid regression suite (README.md here).
//
// startApp() runs stanpumpR in a child R process in Shiny's test mode, with
// the dose grid the caller names; openSession() opens a fresh session with
// the startup menu's drugs ticked; serverState() reads what the server holds
// (the draft, the committed table, its format and the undo state) through
// exportTestValues() in R/app_server.R, which exists only in test mode.  The
// suite asserts on what the user sees AND on what the server stores, because
// the bugs this grid has had were disagreements between the two.

'use strict';

const { spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');
const http = require('http');

const REPO = path.resolve(__dirname, '..', '..');

function waitForHttp(url, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  return new Promise((resolve, reject) => {
    const attempt = () => {
      const req = http.get(url, res => {
        res.resume();
        if (res.statusCode === 200) resolve(); else retry();
      });
      req.on('error', retry);
      req.setTimeout(2000, () => req.destroy());
    };
    const retry = () => {
      if (Date.now() > deadline) reject(new Error('app did not start: ' + url));
      else setTimeout(attempt, 1000);
    };
    attempt();
  });
}

// grid: 'handsontable' or 'native' (R/dose-grid.R, doseGridMode()).  The
// app's output goes to a log file, not a pipe: a pipe nobody drains while the
// tests run fills, and R then blocks writing to it.
async function startApp(port) {
  const rscript = process.env.RSCRIPT || 'Rscript';
  const extra = (process.env.STANPUMPR_RSCRIPT_ARGS || '').split(' ').filter(Boolean);
  const code = [
    "suppressMessages(pkgload::load_all('.', quiet = TRUE))",
    'options(shiny.testmode = TRUE)',
    `shiny::runApp(run_app(), port = ${port}, launch.browser = FALSE)`
  ].join('; ');
  const logFile = path.join(os.tmpdir(), `stanpumpR-playwright-${port}.log`);
  const fd = fs.openSync(logFile, 'w');
  const child = spawn(rscript, [...extra, '-e', code], { cwd: REPO, stdio: ['ignore', fd, fd] });
  fs.closeSync(fd);
  const log = () => { try { return fs.readFileSync(logFile, 'utf8'); } catch (e) { return ''; } };
  const url = `http://127.0.0.1:${port}/`;
  try {
    await waitForHttp(url, 240000);
  } catch (e) {
    child.kill();
    throw new Error(e.message + '\n' + log().slice(-3000));
  }
  return { url, stop: () => child.kill(), log, logFile };
}

const DEFAULT_DRUGS = ['propofol', 'fentanyl', 'remifentanil', 'rocuronium'];

// A fresh session with `drugs` ticked in the startup menu (whose checkboxes
// sit in collapsed sections, so they are ticked from script, as a click on
// their summary would allow).  Waits until the grid shows the first drug.
async function openSession(context, url, adapter, drugs = DEFAULT_DRUGS) {
  const page = await context.newPage();
  page.pageErrors = [];
  page.on('pageerror', e => page.pageErrors.push(e.message));
  await page.goto(url, { waitUntil: 'networkidle' });
  await page.waitForSelector('#startup_ok');
  await page.evaluate(ds => {
    for (const d of ds) {
      const box = document.querySelector('.modal input[type=checkbox][value="' + d + '"]');
      box.checked = true;
      window.jQuery(box).trigger('change');
    }
  }, drugs);
  await page.click('#startup_ok');
  await page.waitForSelector('.modal', { state: 'detached' }).catch(() => {});
  if (drugs.length > 0) {
    await waitForRows(page, adapter, rows => rows.some(r => r[0] === drugs[0]));
  }
  await settle(page);
  return page;
}

// Until Shiny is idle and no request is in flight
async function settle(page, ms = 250) {
  await page.waitForFunction(() => {
    const busy = document.documentElement.classList.contains('shiny-busy');
    return !busy;
  }, null, { timeout: 180000 });
  await page.waitForTimeout(ms);
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 180000 });
}

async function serverState(page) {
  const base = await page.evaluate(() => window.Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl: true }));
  const text = await page.evaluate(async u => (await fetch(u + '&input=0&output=0&export=1&format=json')).text(), base);
  const json = JSON.parse(text).export;
  const rowsOf = t => (t && t.Drug) ? t.Drug.map((d, i) => [d, t.Time[i], t.Dose[i], t.Units[i]]) : [];
  return {
    draft: rowsOf(json.doseTableDraft),
    table: rowsOf(json.doseTable),
    format: json.doseTableFormat,
    canUndo: json.canUndo,
    canRedo: json.canRedo,
    // the events (Time in minutes), and each drug's threshold and colour
    events: json.eventTable || {},
    endCe: json.endCe || {},
    colors: json.colors || {}
  };
}

// The rows of the draft that hold a drug
const filled = rows => rows.filter(r => r[0] !== '' && r[0] !== null);

async function waitForRows(page, adapter, predicate, timeout = 15000) {
  const deadline = Date.now() + timeout;
  let rows;
  while (Date.now() < deadline) {
    rows = await adapter.readRows(page);
    if (predicate(rows)) return rows;
    await page.waitForTimeout(150);
  }
  throw new Error('grid never reached the expected state; it shows ' + JSON.stringify(rows));
}

async function waitForDraft(page, predicate, timeout = 15000) {
  const deadline = Date.now() + timeout;
  let state;
  while (Date.now() < deadline) {
    state = await serverState(page);
    if (predicate(state.draft, state)) return state;
    await page.waitForTimeout(150);
  }
  throw new Error('server draft never reached the expected state; it holds ' + JSON.stringify(state && state.draft));
}

async function connected(page) {
  return page.evaluate(() => !!(window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()));
}

async function notifications(page) {
  return page.$$eval('#shiny-notification-panel .shiny-notification-content-text', ns => ns.map(n => n.textContent));
}

async function clearNotifications(page) {
  await page.evaluate(() => document.querySelectorAll('#shiny-notification-panel .shiny-notification').forEach(n => n.remove()));
}

async function paste(page, text) {
  await page.evaluate(t => navigator.clipboard.writeText(t), text);
  await page.keyboard.press('ControlOrMeta+V');
}

async function drugInfo(page, drug) {
  return page.evaluate(d => {
    const x = window.drug_defaults.find(r => r.Drug === d);
    return x ? { units: x.Units, defaultUnit: x['Default.Units'] } : null;
  }, drug);
}

// A link as slow as a hosted app's (shinyapps.io answers in a few hundred
// ms): every request and websocket frame of the page delayed by `ms`
async function slowLink(context, page, ms) {
  const cdp = await context.newCDPSession(page);
  await cdp.send('Network.enable');
  await cdp.send('Network.emulateNetworkConditions',
    { offline: false, latency: ms, downloadThroughput: -1, uploadThroughput: -1 });
}

module.exports = {
  slowLink, REPO, DEFAULT_DRUGS, startApp, openSession, settle, serverState, filled, waitForRows,
  waitForDraft, connected, notifications, clearNotifications, paste, drugInfo
};
