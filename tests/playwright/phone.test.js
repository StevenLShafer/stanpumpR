// The dose grid on a phone: a smaller suite than dose-grid.test.js, for
// what a phone does differently (a tap instead of a click, a value
// committed by tapping the next cell, the system's own list for a
// <select>), run on real iPhones (Safari) and real Android phones (Chrome),
// and written to the limits BrowserStack sets on Playwright on a real iPhone
// (its "Supported Playwright commands for iOS"), the stricter of the two:
//
// - one browser context and one page for the whole file, so every case
//   starts by loading the app afresh in that page (a new Shiny session each
//   time), where the desktop suite opens a new context per case;
// - no page.$$eval() and no pageerror event: the page is read with
//   page.evaluate(), and script errors are collected by a script added
//   before the app loads (page.addInitScript());
// - page.evaluate() returns real values only, not an async function's, so
//   the server's state is read with a synchronous request;
// - text goes to a cell (locator.fill()), not to the page's keyboard, which
//   BrowserStack never typed on an iPhone, and a cell is committed by
//   tapping the next one, Enter not reaching the grid there either;
// - no clipboard: nothing here is pasted.
//
// Run here, it runs as an iPhone 15 in Playwright's WebKit, or with
// STANPUMPR_PHONE=android as a Pixel 7 in Chromium (touch, the phone's
// viewport and user agent), keeping to the same limits, so a case that
// needs more than BrowserStack allows fails here first.  On BrowserStack
// (STANPUMPR_BROWSERSTACK=iphone-16, iphone-17, android-pixel-9 or
// android-galaxy-s25, browserstack.js) it runs on that phone.
//
//   node tests/playwright/run.js phone.test.js
//   STANPUMPR_PHONE=android node tests/playwright/run.js phone.test.js

'use strict';

const { describe, test, before, after, beforeEach, afterEach } = require('node:test');
const assert = require('node:assert/strict');
const playwright = require('playwright');
const H = require('./harness');
const BS = require('./browserstack');

if (!process.env.STANPUMPR_URL) throw new Error('STANPUMPR_URL is not set: run the suite through run.js');
if (BS.enabled() && !BS.mobile()) {
  throw new Error('phone.test.js runs on a phone platform (' +
    Object.keys(BS.PLATFORMS).filter(n => BS.PLATFORMS[n].mobile).join(', ') + ') or here');
}
// The phone it stands in for here (no effect on BrowserStack)
const LOCAL_PHONES = {
  iphone: { engine: 'webkit', device: 'iPhone 15' },
  android: { engine: 'chromium', device: 'Pixel 7' }
};
const LOCAL_PHONE = LOCAL_PHONES[process.env.STANPUMPR_PHONE || 'iphone'];
if (!LOCAL_PHONE) throw new Error('STANPUMPR_PHONE is iphone or android');
const APP = BS.appUrl(process.env.STANPUMPR_URL);
// A real Android phone on BrowserStack (Chrome, through Playwright's _android)
const ANDROID = BS.enabled() && !!BS.platform().android;

// The startup table: propofol (bolus, infusion), fentanyl, remifentanil
// (bolus, infusion), rocuronium; then blank rows
const FENTANYL = 2;
const FIRST_BLANK = 6;

const ROOT = '#doseGrid';
const row = r => `${ROOT} tbody tr:nth-child(${r + 1})`;
const input = (r, c) => `${row(r)} td:nth-child(${c + 1}) input`;
const unitCell = r => `${row(r)} td:nth-child(4)`;
const unitSelect = r => `${row(r)} td:nth-child(4) select`;

let phone, context, page;

before(async () => {
  phone = await BS.launchPhone(playwright, LOCAL_PHONE, 'phone');
  context = phone.context;
  page = await context.newPage();
  await page.addInitScript(() => {
    window.__errors = [];
    window.addEventListener('error', e => window.__errors.push(String(e.message)));
    window.addEventListener('unhandledrejection', e => window.__errors.push(String(e.reason)));
  });
});

after(async () => {
  await phone.close();
});

// The app loaded afresh, the startup menu's four drugs ticked and Started
beforeEach(async () => {
  // On Android the page's load event came late or not at all through the
  // tunnel (two loads of eight timed out at 30 s, the first real-Pixel run
  // with touch, 2026-11-10); what the case needs is the startup menu, which
  // the next line waits for
  await page.goto(APP, ANDROID ? { waitUntil: 'domcontentloaded', timeout: 90000 } : { waitUntil: 'load' });
  await page.waitForSelector('#startup_ok', { timeout: 120000 });
  await page.evaluate(ds => {
    for (const d of ds) {
      const box = document.querySelector('.modal input[type=checkbox][value="' + d + '"]');
      box.checked = true;
      window.jQuery(box).trigger('change');
    }
  }, H.DEFAULT_DRUGS);
  await tapOn('#startup_ok');
  await page.waitForFunction(() => !document.querySelector('.modal.show'), null, { timeout: 60000 });
  await waitForRows(rows => rows.length > FIRST_BLANK && rows[0][0] === 'propofol');
  await H.settle(page);
  await openDoseTable();
});

// On a phone the dose table's sidebar starts collapsed: a user opens it with
// its toggle (the arrow at the top right), as this does
async function openDoseTable() {
  if (await page.isVisible(input(0, 0))) return;
  const id = await page.evaluate(() => document.querySelector('aside.dose-sidebar').id);
  await tapOn(`.collapse-toggle[aria-controls="${id}"]`);
  await page.waitForSelector(input(0, 0), { state: 'visible', timeout: 15000 });
  await H.settle(page, 300);
}

// Whatever a case did, the session must still be there and no script must
// have failed
afterEach(async () => {
  const state = await page.evaluate(() => ({
    connected: !!(window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()),
    errors: window.__errors || []
  }));
  assert.ok(state.connected, 'the Shiny session was closed');
  assert.deepEqual(state.errors, [], 'the page raised script errors');
});

// What the grid shows, row by row: Drug, Time, Dose, Units
// The page reloads itself, and this waits until the new page is up: a
// marker set before the reload is gone, and Shiny is connected.  A call made
// while the page unloads can fail, and is tried again
async function reloadInPage(timeout = 60000) {
  await page.evaluate(() => { window.__beforeReload = true; setTimeout(() => location.reload(), 50); });
  const deadline = Date.now() + timeout;
  while (Date.now() < deadline) {
    await page.waitForTimeout(500);
    const ready = await page.evaluate(() => !window.__beforeReload && document.readyState === 'complete' &&
      !!(window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected())).catch(() => false);
    if (ready) return;
  }
  throw new Error('the page did not reload within ' + timeout + ' ms');
}

async function readRows() {
  return page.evaluate(sel => Array.from(document.querySelectorAll(sel + ' tbody tr')).map(tr => {
    const tds = tr.querySelectorAll('td');
    return [tds[0].querySelector('input').value, tds[1].querySelector('input').value,
            tds[2].querySelector('input').value,
            tds[3].querySelector('.dose-grid-unit-text').textContent.replace(/ /g, ' ')];
  }), ROOT);
}

async function waitForRows(predicate, timeout = 30000) {
  const deadline = Date.now() + timeout;
  let rows;
  while (Date.now() < deadline) {
    rows = await readRows();
    if (predicate(rows)) return rows;
    await page.waitForTimeout(250);
  }
  throw new Error('grid never reached the expected state; it shows ' + JSON.stringify(rows));
}

async function notifications() {
  return page.evaluate(() => Array.from(document.querySelectorAll(
    '#shiny-notification-panel .shiny-notification-content-text')).map(n => n.textContent));
}

// Both the screen and the server's draft hold `expected` in row r
async function holds(r, expected) {
  const same = x => JSON.stringify(x) === JSON.stringify(expected);
  await waitForRows(rows => same(rows[r]));
  await waitForDraft(d => same(d[r]));
}

// A tap on a cell, the text, then a tap on the next cell: how a phone
// enters a value, the grid committing the cell as the focus leaves it
// (onFocusOut in dose-grid.js).  On a real iPhone BrowserStack turns each
// Playwright call into a WebDriver command, and neither way of pressing
// Enter worked: keys sent to the page (page.keyboard) were never typed (the
// first run, 2026-11-09), and Enter sent to the cell left the dose on the
// screen uncommitted (the second).  So the text goes to the cell itself
// (locator.fill()), and the cell is committed by tapping the next one.
// Enter is the desktop suite's (dose-grid.test.js)
async function enter(r, c, text) {
  await tapOn(input(r, c));
  await typeInto(input(r, c), text);
  await tapOn(input(r, c === 1 ? 2 : 1));
  await H.settle(page, 300);
}

// A tap.  Before it taps, Playwright checks that nothing covers the target
// by asking the page what lies at the point it will tap, and on a real
// Android phone that check looked in the wrong place: on the drug list and
// on Apply, both below the first rows, it found the rows and the navbar
// above the target "intercepting" the tap until it timed out, though nothing
// covers either (the first two real-Pixel runs with touch, 2026-11-10; the
// visual viewport was not panned).  So on Android the target is scrolled to
// the middle of the screen at once (no smooth scroll), the page itself is
// asked whether the target is on top at its centre, and the tap is sent
// without Playwright's check (force); each case then checks what its tap did.
// What the page saw is printed, so a tap that misses says where it went
async function tapOn(target) {
  const locator = typeof target === 'string' ? page.locator(target) : target;
  if (!ANDROID) return locator.tap();
  const seen = await locator.evaluate(el => {
    el.scrollIntoView({ block: 'center', inline: 'nearest', behavior: 'instant' });
    const r = el.getBoundingClientRect();
    const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2);
    const v = window.visualViewport;
    return {
      target: el.id || el.getAttribute('aria-label') || el.textContent.trim().slice(0, 30),
      rect: [r.left, r.top, r.width, r.height].map(Math.round),
      onTop: !!hit && (hit === el || el.contains(hit)),
      hit: hit ? (hit.id || hit.getAttribute('aria-label') || hit.tagName) : null,
      viewport: v ? { scale: v.scale, top: v.offsetTop, height: v.height, width: v.width } : null,
      inner: [window.innerWidth, window.innerHeight],
      scrollY: Math.round(window.scrollY),
      dpr: window.devicePixelRatio
    };
  });
  console.log('tap', JSON.stringify(seen));
  await page.waitForTimeout(100);
  await locator.tap({ force: true });
}

async function typeInto(selector, text) {
  await page.locator(selector).fill(text);
}

// The server's state (exportTestValues(), test mode), read with a
// synchronous request: on a real iPhone page.evaluate() returns real values
// only, and an async function's came back as "[object Object]" (the first
// run, 2026-11-09)
async function serverState() {
  const text = await page.evaluate(() => {
    const u = window.Shiny.shinyapp.getTestSnapshotBaseUrl({ fullUrl: true }) + '&input=0&output=0&export=1&format=json';
    const x = new XMLHttpRequest();
    x.open('GET', u, false);
    x.send();
    return x.responseText;
  });
  const json = JSON.parse(text).export;
  const rowsOf = t => (t && t.Drug) ? t.Drug.map((d, i) => [d, t.Time[i], t.Dose[i], t.Units[i]]) : [];
  return { draft: rowsOf(json.doseTableDraft), table: rowsOf(json.doseTable) };
}

async function waitForDraft(predicate, timeout = 30000) {
  const deadline = Date.now() + timeout;
  let state;
  while (Date.now() < deadline) {
    state = await serverState();
    if (predicate(state.draft)) return state;
    await page.waitForTimeout(250);
  }
  throw new Error('server draft never reached the expected state; it holds ' + JSON.stringify(state && state.draft));
}

describe('the dose grid on a phone', () => {
  test('the startup menu fills the dose table', async () => {
    const rows = await readRows();
    assert.deepEqual(rows.slice(0, FIRST_BLANK).map(r => r[0]),
                     ['propofol', 'propofol', 'fentanyl', 'remifentanil', 'remifentanil', 'rocuronium']);
    assert.deepEqual(rows[FENTANYL], ['fentanyl', '0', '0', 'mcg']);
  });

  // What is typed after a tap must replace the value, not add to it, and the
  // tap must leave the row menu closed.  The menu bug this guards against
  // (a tap that selected a cell's text sent a contextmenu event at once,
  // Gemini's final pass, 2026-11-06) was mobile Chromium's: WebKit sends no
  // contextmenu on a tap, so with the guard in dose-grid.js removed this case
  // still passes here (checked 2026-11-09).  dose-grid.test.js catches that
  // bug under Chromium's touch emulation; this checks only that an iPhone
  // tap behaves
  test('a tap selects the whole value without opening the row menu', async () => {
    await tapOn(input(0, 2));
    await H.settle(page, 500);
    const state = await page.evaluate(sel => {
      const el = document.querySelector(sel);
      const menu = document.querySelector('.dose-grid-menu');
      return {
        focused: document.activeElement === el,
        selected: [el.selectionStart, el.selectionEnd, el.value.length],
        menuOpen: !!menu && !menu.hidden
      };
    }, input(0, 2));
    assert.ok(state.focused, 'the tapped cell has the focus');
    assert.equal(state.selected[0], 0, 'the selection starts at the beginning');
    assert.equal(state.selected[1], state.selected[2], 'the selection runs to the end');
    assert.equal(state.menuOpen, false, 'a tap opened the row menu');
  });

  test('a dose typed after a tap replaces the old one, on the screen and on the server', async () => {
    await enter(FENTANYL, 2, '25');
    await holds(FENTANYL, ['fentanyl', '0', '25', 'mcg']);
  });

  test('an entry a number cannot hold is refused, said, and the dose kept', async () => {
    await enter(FENTANYL, 2, '25');
    await holds(FENTANYL, ['fentanyl', '0', '25', 'mcg']);
    await enter(FENTANYL, 2, '4,5');
    await page.waitForTimeout(500);
    await holds(FENTANYL, ['fentanyl', '0', '25', 'mcg']);
    const notes = await notifications();
    assert.ok(notes.some(n => n.includes('"4,5" was not accepted')), JSON.stringify(notes));
  });

  test('the drug list offers the drugs that match, and a tap on one chooses it', async () => {
    await tapOn(input(FIRST_BLANK, 0));
    await typeInto(input(FIRST_BLANK, 0), 'fent');
    const option = page.locator(`${ROOT} [role=listbox]:not([hidden]) [role=option]`).filter({ hasText: /^fentanyl$/ });
    await option.waitFor({ state: 'visible', timeout: 15000 });
    await tapOn(option);
    await H.settle(page, 300);
    await holds(FIRST_BLANK, ['fentanyl', '0', '0', 'mcg']);
  });

  // A tap on a Units cell anywhere but its arrow was suppressed, so its list
  // never opened (Gemini's final pass, 2026-11-06).  The list that opens is
  // the system's own, which a test cannot drive, so the unit is then chosen
  // as the list would choose it (selectOption)
  test('a tap on Units reaches its list, and a unit chosen there is stored', async () => {
    await page.evaluate(() => {
      window.__mousedown = [];
      window.addEventListener('mousedown', e => window.__mousedown.push(e.defaultPrevented));
    });
    // the cell's centre, clear of the arrow at its right
    await tapOn(unitCell(FENTANYL));
    await H.settle(page, 300);
    const prevented = await page.evaluate(() => window.__mousedown);
    assert.ok(prevented.length > 0, 'the tap sent no mousedown, so this checked nothing');
    assert.ok(!prevented.includes(true), 'the tap on Units was suppressed: ' + JSON.stringify(prevented));
    const units = await page.evaluate(sel => Array.from(document.querySelectorAll(sel + ' option'))
      .map(o => o.value).filter(u => u !== ''), unitSelect(FENTANYL));
    const other = units.find(u => u !== 'mcg');
    assert.ok(other, 'fentanyl offers more than one unit: ' + JSON.stringify(units));
    await page.locator(unitSelect(FENTANYL)).selectOption(other);
    await H.settle(page, 300);
    await waitForDraft(d => d[FENTANYL][3] === other);
  });

  test('Apply commits the dose and draws the plot again', async () => {
    const plotBefore = await page.evaluate(() => {
      const img = document.querySelector('#PlotSimulation img');
      return img ? img.getAttribute('src') : null;
    });
    assert.ok(plotBefore, 'the plot was drawn');
    await enter(FENTANYL, 2, '50');
    await holds(FENTANYL, ['fentanyl', '0', '50', 'mcg']);
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled, null, { timeout: 15000 });
    await tapOn('#dosetable_apply');
    await H.settle(page, 1000);
    const state = await serverState();
    assert.deepEqual(state.table[FENTANYL], ['fentanyl', '0', '50', 'mcg']);
    await page.waitForFunction(src => {
      const img = document.querySelector('#PlotSimulation img');
      return img && img.getAttribute('src') !== src;
    }, plotBefore, { timeout: 60000 });
  });

  // The link in the address bar after Apply, opened afresh in the same page
  // (one page is all an iPhone session has), restores the table applied.
  // The page reloads itself (location.reload()), which opens that same
  // address in a new session.  Playwright's own navigation failed on the
  // iPhone 16 (iOS 18), where BrowserStack translates it: page.goto() with
  // the bookmark address left the grid empty (the third run, 2026-11-09), and
  // page.reload() broke the connection ("Object with guid response@... was
  // not bound", the fifth), the Response each returns not surviving the
  // translation.  So the reload is the page's, and the wait is a poll of
  // page.evaluate(), which every run has kept
  test('the bookmark after Apply restores the table applied', async () => {
    await enter(FENTANYL, 2, '357');
    await holds(FENTANYL, ['fentanyl', '0', '357', 'mcg']);
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled, null, { timeout: 15000 });
    await tapOn('#dosetable_apply');
    await page.waitForFunction(() => decodeURIComponent(location.search).includes('357'), null, { timeout: 30000 });
    const link = await page.evaluate(() => location.href);
    await reloadInPage();
    try {
      await waitForRows(rows => rows.some(r => r[2] === '357'), 60000);
    } catch (e) {
      // what the page showed instead, so a failure on a device says why
      const seen = await page.evaluate(l => ({
        sameAddress: location.href === l,
        addressLength: location.href.length,
        readyState: document.readyState,
        shiny: !!(window.Shiny && window.Shiny.shinyapp && window.Shiny.shinyapp.isConnected()),
        startupMenu: !!document.getElementById('startup_ok'),
        grid: !!document.querySelector('#doseGrid'),
        body: document.body ? document.body.innerText.slice(0, 200) : null
      }), link).catch(err => ({ evaluateFailed: String(err) }));
      throw new Error(e.message + '\nthe page: ' + JSON.stringify(seen));
    }
    await openDoseTable();
    assert.deepEqual((await readRows())[FENTANYL], ['fentanyl', '0', '357', 'mcg']);
    // the grid shows the draft; the bookmark must restore the applied table
    // too (CodeRabbit, PR #697)
    assert.deepEqual((await serverState()).table[FENTANYL], ['fentanyl', '0', '357', 'mcg']);
  });
});
