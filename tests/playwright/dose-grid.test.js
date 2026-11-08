// The dose grid's regression suite: what the main dose table must do, driven
// as a user drives it (clicks, keys, the clipboard) in headless Chromium, and
// checked both on the screen and in the server's draft.  Written against the
// rhandsontable grid first, so that a replacement is held to the same bar
// (docs/grid-replacement.md).  Run with tests/playwright/run.js (README.md).
//
// Most cases come from the bugs the 2026-11 reviews found and fixed one by one
// (test-ui-keyboard-crashes.R and test-ui-adversarial-review.R test the
// server's side); here they run in a real browser against the real server.
// The dialogs' grids are table-grid.test.js's.
//
// A case marked KNOWN_GAP fails on a grid for a reason recorded beside it: it
// is reported as a todo rather than a failure, so that the suite states the
// bar without pretending the current grid meets it.

'use strict';

const { test, describe, before, after, beforeEach, afterEach } = require('node:test');
const assert = require('node:assert/strict');
const playwright = require('playwright');
const H = require('./harness');

const GRID = process.env.STANPUMPR_GRID || 'native';
const URL = process.env.STANPUMPR_URL;
if (!URL) throw new Error('STANPUMPR_URL is not set: run the suite through run.js');
const adapter = require('./adapters/' + GRID);

// The cases a grid is known not to meet, by grid
const KNOWN_GAP = {
  // rhandsontable's gaps went with it (2026-11-06; docs/grid-replacement.md
  // keeps the list of what it failed)
  native: {}
};
// test(), or test() as a todo when the grid has the gap: gt(name, fn) or
// gt(name, options, fn)
const gt = (name, options, fn) => {
  if (typeof options === 'function') { fn = options; options = {}; }
  const gap = (KNOWN_GAP[GRID] || {})[name];
  return test(name, gap ? Object.assign({}, options, { todo: gap }) : options, fn);
};

// The startup table (doseTableInit): six rows, then blank ones
const FIRST_BLANK = 6;

let browser, context, page;

// STANPUMPR_BROWSER: chromium (the default), firefox or webkit.  The
// clipboard permissions and the slow link (CDP) are Chromium's: elsewhere
// the clipboard is opened by preference and the slow-link cases are skipped
// (the external review of 2026-11-06, Gemini's final pass).
const BROWSER = process.env.STANPUMPR_BROWSER || 'chromium';
const CDP = BROWSER === 'chromium';
const SLOW_LINK = { skip: !CDP && 'a slow link is emulated through CDP, which only Chromium has' };

before(async () => {
  browser = await playwright[BROWSER].launch(BROWSER === 'firefox' ? {
    firefoxUserPrefs: { 'dom.events.asyncClipboard.readText': true, 'dom.events.testing.asyncClipboard': true }
  } : {});
});
after(async () => { await browser.close(); });

beforeEach(async () => {
  context = await browser.newContext({ viewport: { width: 1600, height: 1000 } });
  if (CDP) await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: URL });
  page = await H.openSession(context, URL, adapter);
});

afterEach(async () => {
  // Whatever a case did, the session must still be there: an error in an
  // observer ends it (AGENTS.md)
  const alive = await H.connected(page);
  await context.close();
  assert.ok(alive, 'the Shiny session was closed');
});

const rowsShown = () => adapter.readRows(page);
const draft = async () => (await H.serverState(page)).draft;
const drugRows = async () => H.filled(await draft());

// The server's draft and the screen both hold `row` at row r
async function holds(r, row, timeout) {
  const same = x => JSON.stringify(x) === JSON.stringify(row);
  await H.waitForDraft(page, d => same(d[r]), timeout);
  await H.waitForRows(page, adapter, rows => same(rows[r]), timeout);
}

async function enter(r, c, text, key = 'Enter') {
  await adapter.typeInto(page, r, c, text);
  await page.keyboard.press(key);
  await H.settle(page);
}

// ---------------------------------------------------------------------------
describe('the Drug column', () => {
  test('choosing a drug starts its row at time 0, dose 0 and its default unit', async () => {
    await enter(FIRST_BLANK, 0, 'morphine');
    const info = await H.drugInfo(page, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    assert.deepEqual((await draft())[FIRST_BLANK], ['morphine', '0', '0', info.defaultUnit]);
    assert.deepEqual((await rowsShown())[FIRST_BLANK], ['morphine', '0', '0', info.defaultUnit]);
    // and the grid keeps a blank row below the last drug
    const shown = await rowsShown();
    assert.equal(shown[shown.length - 1][0], '');
  });

  gt('a name that is not in the library is refused', async () => {
    const before = await draft();
    await adapter.typeInto(page, FIRST_BLANK, 0, 'notadrug');
    await page.keyboard.press('Enter');
    await page.keyboard.press('Escape');
    await H.settle(page);
    assert.equal((await rowsShown())[FIRST_BLANK][0], '');
    assert.deepEqual(await draft(), before);
  });

  test('the autocomplete offers the library drugs that match what is typed', async () => {
    await adapter.typeInto(page, FIRST_BLANK, 0, 'fent');
    const offered = await adapter.suggestions(page);
    assert.ok(offered.includes('fentanyl'), JSON.stringify(offered));
    assert.ok(!offered.includes('propofol'));
    await page.keyboard.press('Escape');
  });

  test('illicit drugs are not offered without the opt-in', async () => {
    await adapter.typeInto(page, FIRST_BLANK, 0, 'cocaine');
    assert.ok(!(await adapter.suggestions(page)).includes('cocaine'));
    await page.keyboard.press('Enter');
    await H.settle(page);
    assert.equal((await rowsShown())[FIRST_BLANK][0], '', 'cocaine was accepted without the opt-in');
    assert.ok(!(await draft()).some(r => r[0] === 'cocaine'));
  });

  test('with the opt-in an illicit drug is accepted and drawn red', async () => {
    await page.evaluate(() => { const b = document.getElementById('showIllicitDrugs'); b.checked = true; window.jQuery(b).trigger('change'); });
    await H.settle(page, 800);
    await enter(FIRST_BLANK, 0, 'cocaine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'cocaine');
    assert.equal(await adapter.drugColor(page, FIRST_BLANK), 'rgb(192, 0, 0)');
    assert.notEqual(await adapter.drugColor(page, 0), 'rgb(192, 0, 0)');
  });

  gt('after a refused name, Escape gives the cell back', async () => {
    await adapter.typeInto(page, FIRST_BLANK, 0, 'notadrug');
    await page.keyboard.press('Enter');
    await page.keyboard.press('Escape');
    await enter(FIRST_BLANK, 0, 'morphine');
    const info = await H.drugInfo(page, 'morphine');
    await holds(FIRST_BLANK, ['morphine', '0', '0', info.defaultUnit], 5000);
  });

  test('changing a row\'s drug resets its time, dose and unit', async () => {
    await enter(2, 1, '15');
    await enter(2, 2, '50');
    await H.waitForDraft(page, d => d[2][2] === '50');
    await enter(2, 0, 'morphine');
    const info = await H.drugInfo(page, 'morphine');
    await holds(2, ['morphine', '0', '0', info.defaultUnit]);
  });

  test('clearing a drug clears its row', async () => {
    await adapter.clearCell(page, 2, 0);
    await H.settle(page);
    await holds(2, ['', '', '', '']);
  });

  test('Enter twice on a drug cell does not reset its row', async () => {
    await enter(2, 1, '30');
    await enter(2, 2, '10');
    await H.waitForDraft(page, d => d[2][2] === '10');
    await adapter.focusCell(page, 2, 0);
    await page.keyboard.press('Enter');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await holds(2, ['fentanyl', '30', '10', 'mcg']);
  });

  gt('an illicit drug is said, not only drawn red', async () => {
    await page.evaluate(() => { const b = document.getElementById('showIllicitDrugs'); b.checked = true; window.jQuery(b).trigger('change'); });
    await H.settle(page, 800);
    await enter(FIRST_BLANK, 0, 'cocaine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'cocaine');
    assert.match(await adapter.drugDescription(page, FIRST_BLANK), /illicit/i);
    assert.doesNotMatch(await adapter.drugDescription(page, 0), /illicit/i);
  });
});

// ---------------------------------------------------------------------------
describe('the Units column', () => {
  test('offers exactly the units of the row\'s drug', async () => {
    // in the order the library lists them
    const info = await H.drugInfo(page, 'fentanyl');
    assert.deepEqual(await adapter.unitOptions(page, 2), info.units);
    await enter(FIRST_BLANK, 0, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    const morphine = await H.drugInfo(page, 'morphine');
    assert.deepEqual(await adapter.unitOptions(page, FIRST_BLANK), morphine.units);
    // and a drug with one unit
    await enter(FIRST_BLANK + 1, 0, 'sevoflurane');
    await H.waitForDraft(page, d => d.some(r => r[0] === 'sevoflurane'));
    const row = (await draft()).findIndex(r => r[0] === 'sevoflurane');
    assert.deepEqual(await adapter.unitOptions(page, row), (await H.drugInfo(page, 'sevoflurane')).units);
  });

  test('a unit chosen from the keyboard is stored', async () => {
    await adapter.chooseUnit(page, 2, 'mcg/kg');
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][3] === 'mcg/kg');
    assert.equal((await rowsShown())[2][3], 'mcg/kg');
  });

  // The browser's own search of a list takes the next option that starts
  // with what was typed: "mg" on oxycodone was "mg PO liquid", and on
  // fentanyl "mcg/kg", without a word (the external review of 2026-11-06,
  // Gemini's final pass)
  gt('a unit typed on a Units cell is the unit typed, and one the drug lacks is said', async () => {
    await enter(FIRST_BLANK, 0, 'oxycodone');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'oxycodone');
    await adapter.chooseUnit(page, FIRST_BLANK, 'mg');
    await H.settle(page);
    await holds(FIRST_BLANK, ['oxycodone', '0', '0', 'mg']);
    await H.clearNotifications(page);
    await adapter.chooseUnit(page, 2, 'mg');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /"mg" is not a unit offered for fentanyl/.test(n)), JSON.stringify(notes));
  });

  // "m" moved the list to "mcg"; "mg" then matched nothing and was said
  // "not changed", but the pause committed "mcg" over "mcg/kg" (Gemini's
  // production audit, 2026-11-08, finding 1)
  gt('letters that match no unit leave the unit as it was, even after a partial match', async () => {
    await adapter.chooseUnit(page, 2, 'mcg/kg');
    await H.waitForDraft(page, d => d[2][3] === 'mcg/kg');
    await H.settle(page);
    await H.clearNotifications(page);
    await adapter.chooseUnit(page, 2, 'mg');
    await H.settle(page, 1500);
    await holds(2, ['fentanyl', '0', '0', 'mcg/kg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /"mg" is not a unit offered for fentanyl/.test(n)), JSON.stringify(notes));
  });

  test('a long unit wraps only after its route', async () => {
    await enter(FIRST_BLANK, 0, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    await adapter.chooseUnit(page, FIRST_BLANK, 'mg/kg PO liquid q4h');
    await H.waitForDraft(page, d => d[FIRST_BLANK][3] === 'mg/kg PO liquid q4h');
    // "mg/kg PO" over "liquid q4h": only the space after the route may break
    assert.equal(await adapter.unitDisplay(page, FIRST_BLANK), 'mg/kg PO liquid q4h');
  });
});

// ---------------------------------------------------------------------------
describe('Time and Dose', () => {
  // A key filter that drops what a number cannot hold changes the number
  // without a word: "1,5" became 15 and "1a0" 10 (the external review of
  // 2026-11-05).  Typed or pasted, an entry is read whole.
  gt('a character a number cannot hold is refused with a message, not dropped', async () => {
    await enter(2, 2, '25');
    await holds(2, ['fentanyl', '0', '25', 'mcg']);
    for (const entry of ['1,5', '1a0']) {
      await H.clearNotifications(page);
      await enter(2, 2, entry);
      await page.waitForTimeout(300);
      await holds(2, ['fentanyl', '0', '25', 'mcg']);
      const notes = await H.notifications(page);
      assert.ok(notes.some(n => n.includes('"' + entry + '" was not accepted')), entry + ': ' + JSON.stringify(notes));
    }
    // and what a number can hold is read as it is pasted: 1e3 is 1000
    await enter(2, 2, '1e3');
    await holds(2, ['fentanyl', '0', '1000', 'mcg']);
  });

  test('a negative dose is refused, said, and the cell keeps its value', async () => {
    await enter(2, 2, '25');
    await H.waitForDraft(page, d => d[2][2] === '25');
    await H.clearNotifications(page);
    await enter(2, 2, '-2');
    await page.waitForTimeout(300);
    assert.equal((await draft())[2][2], '25');
    assert.equal((await rowsShown())[2][2], '25');
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /cannot be negative/.test(n)), JSON.stringify(notes));
  });

  test('emptying the dose of a row that keeps its drug is refused', async () => {
    await enter(2, 2, '25');
    await H.waitForDraft(page, d => d[2][2] === '25');
    await adapter.clearCell(page, 2, 2);
    await H.settle(page);
    assert.equal((await draft())[2][2], '25');
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /cannot be left blank/.test(n)), JSON.stringify(notes));
  });

  test('a dose above the limit is refused where it is typed', async () => {
    await enter(2, 2, '25');
    await H.waitForDraft(page, d => d[2][2] === '25');
    await enter(2, 2, '2000000000');
    await page.waitForTimeout(300);
    await holds(2, ['fentanyl', '0', '25', 'mcg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /cannot be more than 1,000,000,000/.test(n)), JSON.stringify(notes));
  });

  test('a time and a dose typed in a row with no drug are not kept', async () => {
    await enter(FIRST_BLANK, 1, '5');
    await enter(FIRST_BLANK, 2, '50');
    await H.settle(page);
    assert.deepEqual((await rowsShown())[FIRST_BLANK], ['', '', '', '']);
    assert.ok((await draft()).every(r => r[0] !== '' || (r[1] === '' && r[2] === '')));
  });

  test('a pasted "1,000" and "1e3" are both 1000; "1,5" is refused', async () => {
    await adapter.focusCell(page, 2, 2);
    await H.paste(page, '1,000');
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][2] === '1000');
    await adapter.focusCell(page, 3, 2);
    await H.paste(page, '1e3');
    await H.settle(page);
    await H.waitForDraft(page, d => d[3][2] === '1000');
    await adapter.focusCell(page, 0, 2);
    await H.paste(page, '1,5');
    await H.settle(page);
    await holds(0, ['propofol', '0', '0', 'mg']);
    await holds(2, ['fentanyl', '0', '1000', 'mcg']);
  });
});

// ---------------------------------------------------------------------------
describe('paste', () => {
  test('whole rows keep their times, doses and offered units', async () => {
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, 'fentanyl\t5\t50\tmcg/kg\nmorphine\t30\t10\tmg PO tablet');
    await H.settle(page);
    await holds(FIRST_BLANK, ['fentanyl', '5', '50', 'mcg/kg']);
    await holds(FIRST_BLANK + 1, ['morphine', '30', '10', 'mg PO tablet']);
  });

  test('a pasted unit the drug does not offer is never used, and is said', async () => {
    await H.clearNotifications(page);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, 'morphine\t0\t10\tmg PO');
    await H.settle(page);
    const info = await H.drugInfo(page, 'morphine');
    await holds(FIRST_BLANK, ['morphine', '0', '10', info.defaultUnit]);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /"mg PO" is not a unit offered for morphine/.test(n)), JSON.stringify(notes));
  });

  test('a paste past the last row creates rows and keeps their times and doses', async () => {
    const n = (await rowsShown()).length;
    const lines = [];
    for (let i = 0; i < 4; i++) lines.push(`fentanyl\t${5 * (i + 1)}\t${10 + i}\tmcg`);
    await adapter.focusCell(page, n - 2, 0);
    await H.paste(page, lines.join('\n'));
    await H.settle(page, 800);
    const want = [['fentanyl', '5', '10', 'mcg'], ['fentanyl', '10', '11', 'mcg'],
                  ['fentanyl', '15', '12', 'mcg'], ['fentanyl', '20', '13', 'mcg']];
    for (let i = 0; i < want.length; i++) await holds(n - 2 + i, want[i]);
  });

  test('times and doses alone paste into the row\'s own drug', async () => {
    await adapter.focusCell(page, 2, 1);
    await H.paste(page, '10\t50\n20\t5');
    await H.settle(page);
    await holds(2, ['fentanyl', '10', '50', 'mcg']);
    await holds(3, ['remifentanil', '20', '5', 'mcg']);
  });

  test('a paste that changes the drug and brings no unit takes the new drug\'s default', async () => {
    // fentanyl over propofol's mg/kg must not stay in mg/kg (a thousandfold)
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\t0\t2');
    await H.settle(page);
    await holds(0, ['fentanyl', '0', '2', 'mcg']);
  });

  // A pasted row is taken whole or not at all, and a row given a new drug
  // starts afresh: "fentanyl BADTIME BADOSE mcg" over "propofol 10 100 mg"
  // gave fentanyl 10 100 mcg (the external review of 2026-11-06, N02)
  gt('a pasted row with a time or dose that is refused changes nothing, and a new drug starts afresh', async () => {
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t10\t100\tmg');
    await H.settle(page);
    await holds(0, ['propofol', '10', '100', 'mg']);
    await H.clearNotifications(page);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\tBADTIME\tBADOSE\tmcg');
    await H.settle(page, 600);
    await holds(0, ['propofol', '10', '100', 'mg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => n.includes('"BADTIME" was not accepted')), JSON.stringify(notes));
    // and Apply commits what is shown
    await page.click('#dosetable_apply');
    await H.settle(page, 600);
    assert.deepEqual((await H.serverState(page)).table[0], ['propofol', '10', '100', 'mg']);
    // a new drug pasted with its time alone: its dose is 0, not propofol's
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\t5');
    await H.settle(page);
    await holds(0, ['fentanyl', '5', '0', 'mcg']);
  });

  // A pasted drug starts its row afresh, whether or not the row held it:
  // "propofol<Tab><Tab>80" over a propofol row was refused for its blank
  // time, where the same row of another drug was taken (the external review
  // of 2026-11-06, Gemini's final pass)
  gt('a pasted row of the drug the row already holds starts it afresh', async () => {
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t10\t100\tmg/kg');
    await H.settle(page);
    await holds(0, ['propofol', '10', '100', 'mg/kg']);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t\t80\tmg');
    await H.settle(page);
    await holds(0, ['propofol', '0', '80', 'mg']);
  });

  // 499 doses and the blank row below them were cut to 499 rows, and said
  // to be (the external review of 2026-11-06, Gemini's final pass)
  test('a table of exactly 499 doses is taken without a warning',
       { timeout: 120000, skip: GRID !== 'native' && 'rhandsontable takes minutes to paste 499 rows' }, async () => {
    await H.clearNotifications(page);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, Array.from({ length: 499 }, (_, i) => `propofol\t${i}\t10\tmg`).join('\n'));
    await H.waitForDraft(page, d => H.filled(d).length === 499, 60000);
    await H.settle(page, 1000);
    assert.deepEqual(await H.notifications(page), []);
  });

  // The blank row below the doses was counted against them, so a row could
  // not be inserted at 498 doses; and at 499 a blank row was still offered,
  // whose dose the server then cut (Gemini's production audit, 2026-11-08,
  // finding 7)
  test('a row can be inserted at 498 doses, and none is offered past 499',
       { timeout: 180000 }, async () => {
    await H.clearNotifications(page);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, Array.from({ length: 498 }, (_, i) => `propofol\t${i}\t10\tmg`).join('\n'));
    await H.waitForDraft(page, d => H.filled(d).length === 498, 60000);
    await H.settle(page, 1000);
    await adapter.insertRowAbove(page, 0);
    await H.settle(page, 500);
    const shown = await rowsShown();
    assert.equal(shown[0][0], '', 'no row was inserted');
    assert.ok(!(await H.notifications(page)).some(n => /at most/.test(n)));
    await enter(0, 0, 'fentanyl');
    await H.waitForDraft(page, d => H.filled(d).length === 499, 30000);
    await H.settle(page, 1000);
    const after = await rowsShown();
    assert.equal(H.filled(after).length, 499);
    assert.equal(after.length, 499, 'a blank row past the 499th dose was offered');
  });

  // The table holds 499 doses (MAX_DOSE_ROWS - 1) and a blank row.  510 rows
  // pasted under the six of the startup table are 17 too many.
  gt('a paste cannot grow the table past its row limit, and says so', { timeout: 900000 }, async () => {
    const lines = [];
    for (let i = 0; i < 510; i++) lines.push(`fentanyl\t${i + 1}\t1\tmcg`);
    await H.clearNotifications(page);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    const t0 = Date.now();
    await H.paste(page, lines.join('\n'));
    await H.waitForDraft(page, d => H.filled(d).length >= 499, 800000);
    process.stdout.write(`# ${GRID}: the 510-row paste reached the server in ${((Date.now() - t0) / 1000).toFixed(1)} s\n`);
    await H.settle(page, 3000);
    const d = await draft();
    assert.equal(H.filled(d).length, 499, `the draft holds ${H.filled(d).length} doses`);
    assert.ok(d.length <= 500, `the draft holds ${d.length} rows`);
    assert.equal(H.filled(await rowsShown()).length, 499);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /not added|at most 499/.test(n)), JSON.stringify(notes));
  });
});

// ---------------------------------------------------------------------------
describe('the keyboard', () => {
  test('Enter commits and moves down; Tab commits and moves right', async () => {
    await adapter.typeInto(page, 2, 1, '7');
    await page.keyboard.press('Enter');
    assert.deepEqual(await adapter.activeCell(page), [3, 1]);
    await adapter.typeInto(page, 2, 1, '8');
    await page.keyboard.press('Tab');
    assert.deepEqual(await adapter.activeCell(page), [2, 2]);
    await H.settle(page);
    await holds(2, ['fentanyl', '8', '0', 'mcg']);
  });

  test('Home and End go to the first and last cell of the row', async () => {
    await adapter.focusCell(page, 2, 1);
    await page.keyboard.press('Home');
    assert.deepEqual(await adapter.activeCell(page), [2, 0]);
    await adapter.focusCell(page, 2, 1);
    await page.keyboard.press('End');
    assert.deepEqual(await adapter.activeCell(page), [2, 3]);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
  });

  // At an ordinary typing speed: 80 ms a key, no pause after Tab
  gt('a dose can be entered without the mouse', async () => {
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await page.keyboard.type('morphine', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('15', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('4', { delay: 80 });
    await page.keyboard.press('Tab');
    await H.settle(page);
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine' && d[FIRST_BLANK][2] === '4', 5000);
    assert.deepEqual((await draft())[FIRST_BLANK].slice(0, 3), ['morphine', '15', '4']);
    await adapter.chooseUnit(page, FIRST_BLANK, 'mg PO tablet');
    await holds(FIRST_BLANK, ['morphine', '15', '4', 'mg PO tablet']);
  });

  // The server is a few hundred ms away on shinyapps.io
  gt('two doses typed one after the other over a slow link are both kept', SLOW_LINK, async () => {
    await H.slowLink(context, page, 400);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await page.keyboard.type('morphine', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('15', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('4', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('mg PO tablet');
    await page.keyboard.press('Enter');
    await page.keyboard.type('fentanyl', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('30', { delay: 80 });
    await page.keyboard.press('Tab');
    await page.keyboard.type('50', { delay: 80 });
    await page.keyboard.press('Enter');
    await page.waitForTimeout(3000);
    await H.settle(page, 1500);
    const want = [['morphine', '15', '4', 'mg PO tablet'], ['fentanyl', '30', '50', 'mcg']];
    assert.deepEqual((await draft()).slice(FIRST_BLANK, FIRST_BLANK + 2), want);
    assert.deepEqual((await rowsShown()).slice(FIRST_BLANK, FIRST_BLANK + 2), want);
  });

  gt('every cell the keyboard reaches has a name a screen reader can read', async () => {
    const headers = ['Drug', 'Time', 'Dose', 'Units'];
    for (let c = 0; c < 4; c++) {
      await adapter.focusCell(page, 2, c);
      const snap = await page.locator(':focus').ariaSnapshot();
      assert.match(snap, new RegExp(headers[c]), `column ${headers[c]}: ${snap}`);
      assert.match(snap, /row 3/, `column ${headers[c]}: ${snap}`);
    }
  });
});

// ---------------------------------------------------------------------------
describe('rows', () => {
  test('a row inserted above is blank and can be filled', async () => {
    await adapter.insertRowAbove(page, 2);
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][0] === '' && d[3][0] === 'fentanyl');
    const d = await draft();
    assert.ok(d.every(r => r.every(x => typeof x === 'string')), 'a cell is not a string: ' + JSON.stringify(d));
    await H.waitForRows(page, adapter, rows => rows[2][0] === '' && rows[3][0] === 'fentanyl');
    await enter(2, 0, 'morphine');
    const info = await H.drugInfo(page, 'morphine');
    await holds(2, ['morphine', '0', '0', info.defaultUnit]);
  });

  // Tab on the row menu closed it and stayed on the cell it was opened from
  // (the external review of 2026-11-06, N03)
  test('Tab closes the row menu and moves on to the next cell', async () => {
    await adapter.focusCell(page, 2, 0);
    await page.keyboard.press('Shift+F10');
    await H.settle(page, 300);
    await page.keyboard.press('Tab');
    await H.settle(page, 200);
    assert.deepEqual(await adapter.activeCell(page), [2, 1]);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
  });

  // A right click counted from the Drug cell, whichever cell was clicked
  // (T02); Shift+Tab from the first cell left the focus where it was (T03)
  // (the external review of 2026-11-06)
  gt('Tab on a row menu opened by a right click moves on from the cell clicked', async () => {
    await adapter.contextClick(page, 2, 2);
    await H.settle(page, 300);
    await page.keyboard.press('Tab');
    await H.settle(page, 200);
    assert.deepEqual(await adapter.activeCell(page), [2, 3]);
  });

  gt('Shift+Tab on the row menu of the first cell leaves the grid', async () => {
    await adapter.focusCell(page, 0, 0);
    await page.keyboard.press('Shift+F10');
    await H.settle(page, 300);
    await page.keyboard.press('Shift+Tab');
    await H.settle(page, 200);
    assert.equal(await adapter.activeCell(page), null, 'the focus stayed in the grid');
    await holds(0, ['propofol', '0', '0', 'mg']);
  });

  // A right click's mouseup took the focus back from the menu in Firefox,
  // and Escape on the cell did not close it (the external review of
  // 2026-11-06, Gemini's final pass)
  test('Escape closes the row menu opened by a right click', async () => {
    await adapter.contextClick(page, 2, 0);
    await H.settle(page, 300);
    assert.ok(await adapter.menuOpen(page), 'the row menu did not open');
    await page.keyboard.press('Escape');
    await H.settle(page, 200);
    assert.ok(!(await adapter.menuOpen(page)), 'the row menu stayed open');
    await adapter.focusCell(page, 2, 0);
    assert.deepEqual(await adapter.activeCell(page), [2, 0]);
  });

  // From the Dose cell of a row with no drug, whose Units list is disabled,
  // Tab went back to the row's Drug cell (the external review of 2026-11-06,
  // Gemini's final pass)
  gt('Tab on the row menu passes over the empty Units cell of a row with no drug', async () => {
    await adapter.contextClick(page, FIRST_BLANK, 2);
    await H.settle(page, 300);
    await page.keyboard.press('Tab');
    await H.settle(page, 200);
    assert.deepEqual(await adapter.activeCell(page), [FIRST_BLANK + 1, 0]);
  });

  test('a row can be removed', async () => {
    await adapter.removeRow(page, 2);
    await H.settle(page);
    await H.waitForDraft(page, d => !d.some(r => r[0] === 'fentanyl'));
    await H.waitForRows(page, adapter, rows => !rows.some(r => r[0] === 'fentanyl'));
  });

  test('removing every row leaves one blank row', async () => {
    for (let i = 0; i < 12; i++) {
      const shown = await rowsShown();
      if (shown.length <= 1) break;
      await adapter.removeRow(page, 0);
      await H.settle(page, 100);
    }
    await H.settle(page, 500);
    const d = await draft();
    assert.ok(d.length >= 1 && H.filled(d).length === 0, JSON.stringify(d));
    assert.equal(H.filled(await rowsShown()).length, 0);
  });
});

// ---------------------------------------------------------------------------
describe('time units', () => {
  test('the Time header says what the times are', async () => {
    const th = adapter.timeHeader(page);
    await th.hover();
    const tip = await th.getAttribute('title');
    assert.match(tip || '', /elapsed from the start, in minutes/);
  });

  test('a change of time unit converts the grid, and an edit after it is stored in the new unit', async () => {
    await enter(2, 1, '90');
    await H.waitForDraft(page, d => d[2][1] === '90');
    await page.evaluate(() => {
      const s = document.getElementById('timeUnits');
      if (s.selectize) s.selectize.setValue('hours'); else window.jQuery(s).val('hours').trigger('change');
    });
    await H.settle(page, 1500);
    await H.waitForDraft(page, (d, s) => s.format === 'hours/relative' && d[2][1] === '1.5');
    await H.waitForRows(page, adapter, rows => rows[2][1] === '1.5');
    const th = adapter.timeHeader(page);
    await th.hover();
    assert.match((await th.getAttribute('title')) || '', /in hours/);
    await enter(3, 1, '2');
    await H.waitForDraft(page, d => d[3][1] === '2');
    assert.equal((await H.serverState(page)).format, 'hours/relative');
  });
});

// ---------------------------------------------------------------------------
// Ctrl+Z and Ctrl+Y, as in most applications (Shafer, 2026-11-04: "control-Z
// as an undo of the last operation is useful").  Ctrl or Cmd, whichever the
// platform uses.
describe('Ctrl+Z and Ctrl+Y', () => {
  const undo = () => page.keyboard.press('ControlOrMeta+Z');
  const redo = () => page.keyboard.press('ControlOrMeta+Y');

  gt('Ctrl+Z undoes the last edit and Ctrl+Y redoes it, in the grid and on the server', async () => {
    await enter(2, 2, '100');
    await H.waitForDraft(page, d => d[2][2] === '100');
    await undo();
    await H.waitForDraft(page, d => d[2][2] === '0');
    await H.waitForRows(page, adapter, rows => rows[2][2] === '0');
    await redo();
    await H.waitForDraft(page, d => d[2][2] === '100');
    await H.waitForRows(page, adapter, rows => rows[2][2] === '100');
    // and Ctrl+Shift+Z is redo too
    await undo();
    await H.waitForDraft(page, d => d[2][2] === '0');
    await page.keyboard.press('ControlOrMeta+Shift+Z');
    await H.waitForDraft(page, d => d[2][2] === '100');
  });

  gt('Ctrl+Z steps back one edit at a time', async () => {
    await enter(FIRST_BLANK, 0, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    await enter(FIRST_BLANK, 1, '30');
    await H.waitForDraft(page, d => d[FIRST_BLANK][1] === '30');
    await enter(FIRST_BLANK, 2, '10');
    await H.waitForDraft(page, d => d[FIRST_BLANK][2] === '10');
    const info = await H.drugInfo(page, 'morphine');
    await undo();
    await H.waitForDraft(page, d => d[FIRST_BLANK][2] === '0' && d[FIRST_BLANK][1] === '30');
    await undo();
    await H.waitForDraft(page, d => d[FIRST_BLANK][1] === '0' && d[FIRST_BLANK][0] === 'morphine');
    assert.deepEqual((await draft())[FIRST_BLANK], ['morphine', '0', '0', info.defaultUnit]);
    await undo();
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === '');
    await H.waitForRows(page, adapter, rows => rows[FIRST_BLANK][0] === '');
    assert.ok(!(await H.serverState(page)).canUndo, 'there was nothing left to undo');
  });

  test('Ctrl+Z while typing in a cell undoes the typing, not the table', async () => {
    await enter(2, 2, '100');
    await H.waitForDraft(page, d => d[2][2] === '100');
    await adapter.typeInto(page, 3, 2, '55');
    await undo();
    // the cell's own typing was undone, whatever is left of it
    const text = await adapter.activeText(page);
    assert.notEqual(text, '55', 'Ctrl+Z left the typing as it was');
    await page.keyboard.press('Escape');
    await H.settle(page);
    const d = await draft();
    assert.equal(d[2][2], '100', 'Ctrl+Z while typing undid the previous edit');
    assert.equal(d[3][2], '0');
  });

  // Once Ctrl+Z had put the cell's text back, Ctrl+Y went to the table's
  // history, which had nothing to redo (the external review of 2026-11-06, N01)
  test('Ctrl+Y after Ctrl+Z while typing redoes the typing', async () => {
    await adapter.typeInto(page, 2, 2, '55');
    await undo();
    assert.notEqual(await adapter.activeText(page), '55', 'Ctrl+Z left the typing as it was');
    await redo();
    assert.equal(await adapter.activeText(page), '55', 'Ctrl+Y did not bring the typing back');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '55', 'mcg']);
  });

  gt('Ctrl+Z right after Enter, over a slow link, still undoes that edit', SLOW_LINK, async () => {
    await H.slowLink(context, page, 400);
    await adapter.typeInto(page, 2, 2, '100');
    await page.keyboard.press('Enter');
    await undo();
    await page.waitForTimeout(3000);
    await H.settle(page, 1500);
    assert.equal((await draft())[2][2], '0');
    assert.equal((await rowsShown())[2][2], '0');
  });

  // On a button or the page, not in a text field: the dose table's undo, as
  // the Undo button.  Left to the browser, Ctrl+Z undid the typing of the
  // last cell edited, and the grid showed 0 while the draft held 100 (the
  // external review of 2026-11-05).
  gt('Ctrl+Z with the focus outside the grid undoes the last edit', async () => {
    await enter(2, 2, '100');
    await holds(2, ['fentanyl', '0', '100', 'mcg']);
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled);
    await page.focus('#dosetable_apply');
    await undo();
    await H.settle(page, 800);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
    await page.focus('#dosetable_apply');
    await redo();
    await H.settle(page, 800);
    await holds(2, ['fentanyl', '0', '100', 'mcg']);
  });

  test('Ctrl+Z after Apply does not reach past it', async () => {
    await enter(2, 2, '100');
    await H.waitForDraft(page, d => d[2][2] === '100');
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled);
    await page.click('#dosetable_apply');
    await H.settle(page);
    await adapter.focusCell(page, 3, 2);
    await undo();
    await H.settle(page, 800);
    const s = await H.serverState(page);
    assert.equal(s.draft[2][2], '100');
    assert.equal(s.table[2][2], '100');
  });
});


// ---------------------------------------------------------------------------
// From Gemini's review scripts of 2026-11-05, run here against both grids
describe('the external review of 2026-11-05 (Gemini)', () => {
  // A click on a cell that already has the focus put the caret where it
  // landed, so what was typed was added to the value: 5 became 510 or 105
  test('clicking a cell again and typing replaces its value', async () => {
    await enter(2, 2, '5');
    await holds(2, ['fentanyl', '0', '5', 'mcg']);
    await adapter.focusCell(page, 2, 2);
    await adapter.focusCell(page, 2, 2);
    await page.keyboard.type('10');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '10', 'mcg']);
  });

  test('a drug chosen with the arrow keys is the one taken, even when the name typed is a drug', async () => {
    await adapter.typeInto(page, FIRST_BLANK, 0, 'digoxin');
    const offered = await adapter.suggestions(page);
    assert.deepEqual(offered.slice(0, 2), ['digoxin', 'digoxinIV']);
    await page.keyboard.press('ArrowDown');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] !== '');
    assert.equal((await draft())[FIRST_BLANK][0], 'digoxinIV');
    // and the name typed, without the arrow keys, is the name typed
    await adapter.typeInto(page, FIRST_BLANK + 1, 0, 'digoxin');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await H.waitForDraft(page, d => d[FIRST_BLANK + 1][0] !== '');
    assert.equal((await draft())[FIRST_BLANK + 1][0], 'digoxin');
  });

  gt('a pasted row whose drug is refused changes nothing', async () => {
    await H.clearNotifications(page);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\t0\t50\tmcg\nnotadrug\t5\t999\tmg/kg\nketamine\t10\t20\tmg');
    await H.settle(page, 600);
    await holds(0, ['fentanyl', '0', '50', 'mcg']);
    await holds(1, ['propofol', '0', '0', 'mcg/kg/min']);
    await holds(2, ['ketamine', '10', '20', 'mg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => n.includes('"notadrug" is not a drug')), JSON.stringify(notes));
  });

  // One drug name pasted alone starts its row as a drug chosen alone does;
  // so does a column of them
  test('a drug pasted alone over a row starts it at time 0, dose 0 and its default unit', async () => {
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t0\t2\tmg/kg');
    await H.settle(page);
    await holds(0, ['propofol', '0', '2', 'mg/kg']);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'ketamine');
    await H.settle(page);
    await holds(0, ['ketamine', '0', '0', 'mg']);
  });

  gt('a column of drug names pasted over rows starts each row afresh', async () => {
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t10\t100\tmg\npropofol\t20\t50\tmcg/kg/min\nfentanyl\t30\t25\tmcg');
    await H.settle(page);
    await holds(0, ['propofol', '10', '100', 'mg']);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\nremifentanil\nketamine');
    await H.settle(page, 600);
    const unit = async d => (await H.drugInfo(page, d)).defaultUnit;
    await holds(0, ['fentanyl', '0', '0', await unit('fentanyl')]);
    await holds(1, ['remifentanil', '0', '0', await unit('remifentanil')]);
    await holds(2, ['ketamine', '0', '0', await unit('ketamine')]);
  });

  gt('emptying a drug cell and pressing Enter clears the row', async () => {
    await adapter.editCell(page, 3, 0);
    await page.keyboard.press('ControlOrMeta+A');
    await page.keyboard.press('Backspace');
    await page.keyboard.press('Enter');
    await H.settle(page);
    await holds(3, ['', '', '', '']);
  });

  gt('undo never brings back a unit the row\'s drug does not offer', async () => {
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'propofol\t0\t2\tmg/kg');
    await H.settle(page);
    await holds(0, ['propofol', '0', '2', 'mg/kg']);
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'morphine');
    await H.settle(page);
    await H.waitForDraft(page, d => d[0][0] === 'morphine');
    await page.click('#dosetable_undo');
    await H.settle(page, 600);
    const d = await draft();
    for (const row of H.filled(d)) {
      const units = (await H.drugInfo(page, row[0])).units;
      assert.ok(units.includes(row[3]), `${row[0]} holds ${row[3]}, which it does not offer`);
    }
    await holds(0, ['propofol', '0', '2', 'mg/kg']);
  });

  // A browser sends no paste event to a <select>: Ctrl+V on a Units cell
  // did nothing.  The cell is reached from the keyboard here; a click opens
  // its list, as a click on any list does.
  test('a unit pasted on a Units cell is used, or refused and said', async () => {
    await adapter.focusCell(page, 2, 2);
    await page.keyboard.press('Tab');
    await H.clearNotifications(page);
    await H.paste(page, 'furlongs');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /"furlongs" is not a unit offered for fentanyl/.test(n)), JSON.stringify(notes));
    await adapter.focusCell(page, 2, 2);
    await page.keyboard.press('Tab');
    await H.paste(page, 'mcg/kg');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '0', 'mcg/kg']);
  });

  // A click on a Units cell opened the browser's list, which takes every
  // key, so a Ctrl+V on the cell just clicked was lost without a word (the
  // external review of 2026-11-06, T01).  Here the cell is reached by the
  // mouse, and clicked again once it has the focus.
  test('a unit pasted on a Units cell just clicked is used, the second time too', async () => {
    await adapter.focusCell(page, 2, 3);
    await H.paste(page, 'mcg/kg');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '0', 'mcg/kg']);
    await adapter.focusCell(page, 2, 3);
    await H.paste(page, 'mcg');
    await H.settle(page);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
  });

  // On a <select> the arrow keys change its value: going down the Units
  // column changed every row's unit
  test('ArrowDown on a Units cell moves down a row and changes no unit', async () => {
    await adapter.focusCell(page, 0, 2);
    await page.keyboard.press('Tab');
    await page.keyboard.press('ArrowDown');
    await page.keyboard.press('ArrowDown');
    await H.settle(page);
    assert.deepEqual(await adapter.activeCell(page), [2, 3]);
    await holds(0, ['propofol', '0', '0', 'mg']);
    await holds(1, ['propofol', '0', '0', 'mcg/kg/min']);
    await holds(2, ['fentanyl', '0', '0', 'mcg']);
  });

  test('a paste leaves a blank row below the last dose', async () => {
    const lines = [];
    for (let i = 0; i < 50; i++) lines.push(`fentanyl\t${i}\t1\tmcg`);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, lines.join('\n'));
    await H.settle(page, 800);
    await H.waitForDraft(page, d => H.filled(d).length === 56, 60000);
    const shown = await H.waitForRows(page, adapter, rows => H.filled(rows).length === 56, 60000);
    assert.deepEqual(shown[shown.length - 1], ['', '', '', '']);
  });
});

// ---------------------------------------------------------------------------
describe('the draft, Apply and undo', () => {
  // Apply was disabled until the server had the edit, so a dose typed and
  // then Apply clicked at once was not applied, without a word (the
  // external review of 2026-11-06, Gemini's final pass)
  gt('a dose typed and then Apply clicked at once is applied', async () => {
    await adapter.typeInto(page, 2, 2, '350');
    await page.click('#dosetable_apply', { timeout: 5000 });
    await H.settle(page, 800);
    const st = await H.serverState(page);
    assert.deepEqual(st.table[2], ['fentanyl', '0', '350', 'mcg']);
  });

  // The link in the address bar was made before Apply stored the draft, so
  // it kept the table from before Apply until another input changed
  // (Gemini's production audit, 2026-11-08, finding 3)
  test('the bookmark in the address bar holds the table just applied', async () => {
    await enter(2, 2, '357');
    await H.waitForDraft(page, d => d[2][2] === '357');
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled);
    await page.click('#dosetable_apply');
    await page.waitForFunction(() => decodeURIComponent(location.search).includes('357'), null, { timeout: 15000 });
  });

  test('an edit is a draft until Apply; undo and redo step through it', async () => {
    assert.ok(await page.isDisabled('#dosetable_apply'));
    await enter(2, 2, '100');
    await H.waitForDraft(page, d => d[2][2] === '100');
    let s = await H.serverState(page);
    assert.equal(s.table[2][2], '0', 'the edit reached the committed table without Apply');
    assert.ok(s.canUndo);
    await page.waitForFunction(() => !document.getElementById('dosetable_apply').disabled);

    await page.click('#dosetable_undo');
    await H.waitForDraft(page, d => d[2][2] === '0');
    await H.waitForRows(page, adapter, rows => rows[2][2] === '0');
    await page.click('#dosetable_redo');
    await H.waitForDraft(page, d => d[2][2] === '100');
    await H.waitForRows(page, adapter, rows => rows[2][2] === '100');

    await page.click('#dosetable_apply');
    await H.settle(page);
    s = await H.serverState(page);
    assert.equal(s.table[2][2], '100');
    assert.ok(!s.canUndo, 'Apply should start a fresh history');
    assert.ok(await page.isDisabled('#dosetable_apply'));
  });

  // applyGasTableRules(): a volatile agent brings a ventilation row (oxygen
  // comes only with nitrous oxide)
  test('a rule the server applies is drawn in the grid (a gas brings its ventilation)', async () => {
    await enter(FIRST_BLANK, 0, 'sevoflurane');
    await H.settle(page, 800);
    await H.waitForDraft(page, d => d.some(r => r[0] === 'sevoflurane') && d.some(r => r[0] === 'ventilation'));
    await H.waitForRows(page, adapter, rows => rows.some(r => r[0] === 'ventilation'));
    const d = await draft();
    const shown = await rowsShown();
    assert.deepEqual(H.filled(shown), H.filled(d));
  });
});

// ---------------------------------------------------------------------------
describe('everything is text', () => {
  // The grid never sends more rows than it holds; a forged or broken client
  // can, and its extra rows were dropped without a word (the external review
  // of 2026-11-06, N04)
  test('rows past the limit sent by a forged client are said, not dropped in silence',
       { skip: GRID !== 'native' && 'the prototype\'s own input' }, async () => {
    await H.clearNotifications(page);
    await page.evaluate(() => Shiny.setInputValue('doseGrid', {
      rows: Array.from({ length: 5000 }, (_, i) => ['fentanyl', String(i), '1', 'mcg'])
    }, { priority: 'event' }));
    await H.waitForDraft(page, d => H.filled(d).length === 499, 60000);
    await H.waitForRows(page, adapter, rows => H.filled(rows).length === 499, 60000);
    await H.settle(page, 500);
    const notes = await H.notifications(page);
    assert.ok(notes.some(n => /at most 499 doses/.test(n)), JSON.stringify(notes));
  });

  // On a phone, a tap that selected a cell's text made Chromium send a
  // contextmenu, which opened the row menu over the row; and a tap on a
  // Units cell anywhere but its arrow was suppressed, so its list never
  // opened (the external review of 2026-11-06, Gemini's final pass)
  test('a tap selects a cell without opening the row menu, and a tap on Units opens its list',
       { skip: GRID !== 'native' && 'the prototype\'s own touch handling' }, async () => {
    const ctx = await browser.newContext({ viewport: { width: 1600, height: 1000 }, hasTouch: true, isMobile: true });
    try {
      const p = await H.openSession(ctx, URL, adapter);
      await p.tap('#doseGrid tbody tr:nth-child(1) td:nth-child(3) input');
      await H.settle(p, 700);
      assert.ok(!(await adapter.menuOpen(p)), 'a tap opened the row menu');
      await p.evaluate(() => {
        window.__md = [];
        window.addEventListener('mousedown', e => window.__md.push(e.defaultPrevented));
      });
      await p.tap('#doseGrid tbody tr:nth-child(1) td:nth-child(4)', { position: { x: 10, y: 8 } });
      await H.settle(p, 300);
      assert.deepEqual(await p.evaluate(() => window.__md), [false], 'the tap on Units was suppressed');
      assert.ok(await H.connected(p));
    } finally {
      await ctx.close();
    }
  });

  test('markup typed or pasted into a cell is never run or drawn', async () => {
    const hostile = '<img src=x onerror="window.__xss=1">';
    await adapter.typeInto(page, FIRST_BLANK, 0, hostile);
    await page.keyboard.press('Enter');
    await page.keyboard.press('Escape');
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, `${hostile}\t0\t1\t<b>mg</b>`);
    await H.settle(page, 500);
    assert.equal(await page.evaluate(() => window.__xss), undefined);
    assert.equal(await page.locator(`${adapter.root} img, ${adapter.root} b`).count(), 0);
    assert.ok((await draft()).every(r => !/[<>]/.test(r.join(''))));
  });

  // The grid's own code: an edit, the drug list, the row menu opened and
  // closed by mouse and by key.  rhandsontable's bundled numbro throws on
  // every load ('exports is not defined'), from whichever rhandsontable grid
  // is on the page; that page-level error is the next case's.
  test('the grid raises no script errors', async () => {
    await enter(2, 2, '5');
    await adapter.typeInto(page, FIRST_BLANK, 0, 'morph');
    await page.keyboard.press('Escape');
    await page.keyboard.press('Escape');
    await page.click(`${adapter.root} td >> nth=0`, { button: 'right', force: true });
    await page.waitForTimeout(200);
    await page.keyboard.press('Escape');
    await adapter.focusCell(page, 2, 0);
    await page.keyboard.press('Shift+F10');
    await page.waitForTimeout(200);
    await page.keyboard.press('Escape');
    await page.waitForTimeout(300);
    const errors = page.pageErrors.filter(m => !/exports is not defined/.test(m));
    assert.deepEqual(errors, []);
  });

  gt('the page raises no script errors', async () => {
    await enter(2, 2, '5');
    assert.deepEqual(page.pageErrors, []);
  });
});
