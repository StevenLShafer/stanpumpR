// The dose grid's regression suite: what the main dose table must do, driven
// as a user drives it (clicks, keys, the clipboard) in headless Chromium, and
// checked both on the screen and in the server's draft.  Written against the
// rhandsontable grid first, so that a replacement is held to the same bar
// (docs/grid-replacement.md).  Run with tests/playwright/run.js (README.md).
//
// Most cases come from the bugs the 2026-11 reviews found and fixed one by one
// (test-ui-keyboard-crashes.R, test-ui-adversarial-review.R,
// test-dose-table-paste.R replay them in Node against hot_funs.js); here they
// run in a real browser against the real server.
//
// A case marked KNOWN_GAP fails on a grid for a reason recorded beside it: it
// is reported as a todo rather than a failure, so that the suite states the
// bar without pretending the current grid meets it.

'use strict';

const { test, describe, before, after, beforeEach, afterEach } = require('node:test');
const assert = require('node:assert/strict');
const { chromium } = require('playwright');
const H = require('./harness');

const GRID = process.env.STANPUMPR_GRID || 'handsontable';
const URL = process.env.STANPUMPR_URL;
if (!URL) throw new Error('STANPUMPR_URL is not set: run the suite through run.js');
const adapter = require('./adapters/' + GRID);

// The cases a grid is known not to meet, by grid
const KNOWN_GAP = {
  handsontable: {
    'every cell the keyboard reaches has a name a screen reader can read':
      'Handsontable 6 edits through one shared, unlabelled textarea',
    'the page raises no script errors':
      "rhandsontable's bundled numbro languages.min.js throws 'exports is not defined' on load",
    'a dose can be entered without the mouse':
      'the server redraws the grid after each commit and the first key typed meanwhile is lost: ' +
      'morphine, Tab, 15, Tab, 4 is stored as time 5, dose 0 (at 0-300 ms a key, on localhost)',
    'two doses typed one after the other over a slow link are both kept':
      'over a 400 ms link the first row is stored as time 5, dose 0 and the second row is lost',
    'after a refused name, Escape gives the cell back':
      'the editor stays open on the refused text and Escape does not close it; what is typed next is appended',
    'Ctrl+Z undoes the last edit and Ctrl+Y redoes it, in the grid and on the server':
      'Ctrl+Z does nothing: the redraw after every edit clears the grid\'s own undo history',
    'Ctrl+Z steps back one edit at a time':
      'Ctrl+Z does nothing: the redraw after every edit clears the grid\'s own undo history',
    'Ctrl+Z right after Enter, over a slow link, still undoes that edit':
      'Ctrl+Z does nothing: the redraw after every edit clears the grid\'s own undo history'
  },
  native: {
    'the page raises no script errors':
      "the Single Paper Review tab still draws an rhandsontable grid, whose bundled numbro throws on load"
  }
};
const gt = (name, fn) => {
  const gap = (KNOWN_GAP[GRID] || {})[name];
  return gap ? test(name, { todo: gap }, fn) : test(name, fn);
};

// The startup table (doseTableInit): six rows, then blank ones
const FIRST_BLANK = 6;

let browser, context, page;

before(async () => { browser = await chromium.launch(); });
after(async () => { await browser.close(); });

beforeEach(async () => {
  context = await browser.newContext({ viewport: { width: 1600, height: 1000 } });
  await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: URL });
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

  test('a name that is not in the library is refused', async () => {
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
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine', 5000);
  });

  test('changing a row\'s drug resets its time, dose and unit', async () => {
    await enter(2, 1, '15');
    await enter(2, 2, '50');
    await H.waitForDraft(page, d => d[2][2] === '50');
    await enter(2, 0, 'morphine');
    const info = await H.drugInfo(page, 'morphine');
    await H.waitForDraft(page, d => d[2][0] === 'morphine');
    assert.deepEqual((await draft())[2], ['morphine', '0', '0', info.defaultUnit]);
  });

  test('clearing a drug clears its row', async () => {
    await adapter.clearCell(page, 2, 0);
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][0] === '');
    assert.deepEqual((await draft())[2], ['', '', '', '']);
  });

  test('Enter twice on a drug cell does not reset its row', async () => {
    await enter(2, 1, '30');
    await enter(2, 2, '10');
    await H.waitForDraft(page, d => d[2][2] === '10');
    await adapter.focusCell(page, 2, 0);
    await page.keyboard.press('Enter');
    await page.keyboard.press('Enter');
    await H.settle(page);
    assert.deepEqual((await draft())[2], ['fentanyl', '30', '10', 'mcg']);
  });
});

// ---------------------------------------------------------------------------
describe('the Units column', () => {
  test('offers exactly the units of the row\'s drug', async () => {
    const info = await H.drugInfo(page, 'fentanyl');
    assert.deepEqual((await adapter.unitOptions(page, 2)).slice().sort(), info.units.slice().sort());
    await enter(FIRST_BLANK, 0, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    const morphine = await H.drugInfo(page, 'morphine');
    assert.deepEqual((await adapter.unitOptions(page, FIRST_BLANK)).slice().sort(), morphine.units.slice().sort());
  });

  test('a unit chosen from the keyboard is stored', async () => {
    await adapter.chooseUnit(page, 2, 'mcg/kg');
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][3] === 'mcg/kg');
    assert.equal((await rowsShown())[2][3], 'mcg/kg');
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
  test('letters cannot be typed into a dose', async () => {
    await enter(2, 2, '1a0x');
    await H.waitForDraft(page, d => d[2][2] === '10');
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
    assert.equal((await draft())[2][2], '25');
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
    assert.equal((await draft())[0][2], '0');
  });
});

// ---------------------------------------------------------------------------
describe('paste', () => {
  test('whole rows keep their times, doses and offered units', async () => {
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, 'fentanyl\t5\t50\tmcg/kg\nmorphine\t30\t10\tmg PO tablet');
    await H.settle(page);
    await H.waitForDraft(page, d => d[FIRST_BLANK + 1][0] === 'morphine');
    const d = await draft();
    assert.deepEqual(d[FIRST_BLANK], ['fentanyl', '5', '50', 'mcg/kg']);
    assert.deepEqual(d[FIRST_BLANK + 1], ['morphine', '30', '10', 'mg PO tablet']);
  });

  test('a pasted unit the drug does not offer is never used, and is said', async () => {
    await H.clearNotifications(page);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    await H.paste(page, 'morphine\t0\t10\tmg PO');
    await H.settle(page);
    const info = await H.drugInfo(page, 'morphine');
    await H.waitForDraft(page, d => d[FIRST_BLANK][0] === 'morphine');
    assert.deepEqual((await draft())[FIRST_BLANK], ['morphine', '0', '10', info.defaultUnit]);
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
    const rows = H.filled(await draft()).filter(r => r[0] === 'fentanyl' && r[1] !== '0');
    assert.deepEqual(rows, [['fentanyl', '5', '10', 'mcg'], ['fentanyl', '10', '11', 'mcg'],
                            ['fentanyl', '15', '12', 'mcg'], ['fentanyl', '20', '13', 'mcg']]);
  });

  test('times and doses alone paste into the row\'s own drug', async () => {
    await adapter.focusCell(page, 2, 1);
    await H.paste(page, '10\t50\n20\t5');
    await H.settle(page);
    await H.waitForDraft(page, d => d[2][1] === '10' && d[3][1] === '20');
    const d = await draft();
    assert.deepEqual(d[2], ['fentanyl', '10', '50', 'mcg']);
    assert.deepEqual(d[3], ['remifentanil', '20', '5', 'mcg']);
  });

  test('a paste that changes the drug and brings no unit takes the new drug\'s default', async () => {
    // fentanyl over propofol's mg/kg must not stay in mg/kg (a thousandfold)
    await adapter.focusCell(page, 0, 0);
    await H.paste(page, 'fentanyl\t0\t2');
    await H.settle(page);
    await H.waitForDraft(page, d => d[0][0] === 'fentanyl');
    assert.deepEqual((await draft())[0], ['fentanyl', '0', '2', 'mcg']);
  });

  test('a paste cannot grow the table past its row limit', { timeout: 400000 }, async () => {
    const lines = [];
    for (let i = 0; i < 600; i++) lines.push(`fentanyl\t${i}\t1\tmcg`);
    await adapter.focusCell(page, FIRST_BLANK, 0);
    const t0 = Date.now();
    await H.paste(page, lines.join('\n'));
    await H.waitForDraft(page, d => H.filled(d).length > 400, 300000);
    await H.settle(page, 3000);
    process.stdout.write(`# ${GRID}: the 600-row paste reached the server in ${((Date.now() - t0) / 1000).toFixed(1)} s\n`);
    const d = await draft();
    assert.ok(d.length <= 500, `the draft holds ${d.length} rows`);
    assert.ok((await rowsShown()).length <= 500);
    assert.ok(H.filled(d).length > 400, 'the paste was dropped altogether');
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
    await H.waitForDraft(page, d => d[2][1] === '8');
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
    await H.waitForDraft(page, d => d[FIRST_BLANK][3] === 'mg PO tablet');
    assert.deepEqual((await draft())[FIRST_BLANK], ['morphine', '15', '4', 'mg PO tablet']);
  });

  // The server is a few hundred ms away on shinyapps.io
  gt('two doses typed one after the other over a slow link are both kept', async () => {
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
    await enter(2, 0, 'morphine');
    await H.waitForDraft(page, d => d[2][0] === 'morphine');
  });

  test('a row can be removed', async () => {
    await adapter.removeRow(page, 2);
    await H.settle(page);
    await H.waitForDraft(page, d => !d.some(r => r[0] === 'fentanyl'));
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
    await page.keyboard.press('Escape');
    await H.settle(page);
    const d = await draft();
    assert.equal(d[2][2], '100', 'Ctrl+Z while typing undid the previous edit');
    assert.equal(d[3][2], '0');
  });

  gt('Ctrl+Z right after Enter, over a slow link, still undoes that edit', async () => {
    await H.slowLink(context, page, 400);
    await adapter.typeInto(page, 2, 2, '100');
    await page.keyboard.press('Enter');
    await undo();
    await page.waitForTimeout(3000);
    await H.settle(page, 1500);
    assert.equal((await draft())[2][2], '0');
    assert.equal((await rowsShown())[2][2], '0');
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
describe('the draft, Apply and undo', () => {
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

  test('a rule the server applies is drawn in the grid (a gas brings its ventilation)', async () => {
    await enter(FIRST_BLANK, 0, 'sevoflurane');
    await H.settle(page, 800);
    await H.waitForDraft(page, d => d.some(r => r[0] === 'sevoflurane') && d.length > 0);
    const d = await draft();
    const shown = await rowsShown();
    assert.deepEqual(H.filled(shown), H.filled(d));
  });
});

// ---------------------------------------------------------------------------
describe('everything is text', () => {
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

  gt('the page raises no script errors', async () => {
    await enter(2, 2, '5');
    assert.deepEqual(page.pageErrors, []);
  });
});
