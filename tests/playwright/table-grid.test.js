// The dialogs' grids (the table grid: inst/www/table-grid.js, R/table-grid.R),
// driven in headless Chromium as a user drives them, and checked in what the
// grid sends and in what the dialog's Apply writes on the server.  The dialog
// handlers themselves, with forged values, are test-ui-adversarial-review.R
// and test-time-units-server.R's.  Run with tests/playwright/run.js
// (README.md).  Drafted by Claude Code, 2026-11-06, when the table grid
// replaced rhandsontable in every dialog.

'use strict';

const { test, describe, before, after, beforeEach, afterEach } = require('node:test');
const assert = require('node:assert/strict');
const playwright = require('playwright');
const H = require('./harness');
const BS = require('./browserstack');
const adapter = require('./adapters/native');

const URL = process.env.STANPUMPR_URL;
if (!URL) throw new Error('STANPUMPR_URL is not set: run the suite through run.js');
// on BrowserStack, the engine of the platform chosen (browserstack.js)
const BROWSER = BS.engine();

let browser, context, page;

before(async () => {
  browser = await BS.launch(playwright, BROWSER, BROWSER === 'firefox' ? {
    firefoxUserPrefs: { 'dom.events.asyncClipboard.readText': true, 'dom.events.testing.asyncClipboard': true }
  } : {}, 'table grid');
});
after(async () => { await browser.close(); });

beforeEach(async () => {
  context = await browser.newContext({ viewport: { width: 1600, height: 1000 } });
  if (BROWSER === 'chromium') await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: URL });
  page = await H.openSession(context, URL, adapter);
});

afterEach(async () => {
  const alive = await H.connected(page);
  const errors = page.pageErrors.slice();
  await context.close();
  assert.ok(alive, 'the Shiny session was closed');
  assert.deepEqual(errors, [], 'the page raised script errors');
});

// The control of row r, column c (0-based) of the grid `id`
const cell = (id, r, c) => `#${id} tbody tr:nth-child(${r + 1}) td:nth-child(${c + 1}) :is(input, select)`;
const gridRows = id => page.$$eval(`#${id} tbody tr`, trs => trs.map(tr =>
  Array.from(tr.querySelectorAll('input, select')).map(x => x.type === 'checkbox' ? x.checked : x.value)));
// What the grid has sent (the value Shiny holds for it in the browser)
const sentRows = id => page.evaluate(i => {
  const v = window.Shiny.shinyapp.$inputValues[i] || window.Shiny.shinyapp.$inputValues[i + ':stanpumpr.tableGrid'];
  return v ? v.rows : null;
}, id);
const open = async (input, id) => {
  await page.evaluate(i => window.Shiny.setInputValue(i, Math.random(), { priority: 'event' }), input);
  // A cell, not a row: in WebKit the rows of a grid drawn inside a dialog's
  // uiOutput keep the visibility: hidden shinycssloaders set on the output
  // while it recalculated (the output is display: contents), though their
  // cells are visible and drawn (2026-11-08)
  await page.waitForSelector(`#${id} tbody td`, { timeout: 30000 });
  await H.settle(page, 300);
};
const rowOf = async (id, name) => (await gridRows(id)).findIndex(r => r[0] === name);

describe('Edit doses', () => {
  test('edits, deletes and applies a drug\'s doses; refuses what it cannot read', async () => {
    // propofol's rows: the startup table's bolus and infusion, and one more
    await adapter.typeInto(page, 0, 2, '100');
    await page.keyboard.press('Enter');
    await adapter.focusCell(page, 6, 0);
    await H.paste(page, 'propofol\t10\t50\tmg');
    await H.settle(page, 600);
    await page.click('#dosetable_apply');
    await H.settle(page, 1500);
    // a double click on propofol's panel
    await page.evaluate(() => window.Shiny.setInputValue('plot_dblclick',
      { x: 5, y: 1, panelvar1: 'propofol' }, { priority: 'event' }));
    await page.waitForSelector('#editPriorDosesTable tbody tr', { timeout: 30000 });
    await H.settle(page, 300);
    const rows = await gridRows('editPriorDosesTable');
    assert.deepEqual(rows, [[false, '0', '100', 'mg'], [false, '0', '0', 'mcg/kg/min'], [false, '10', '50', 'mg']]);
    // a dose that cannot be read is refused where it is typed, and said
    await H.clearNotifications(page);
    await page.click(cell('editPriorDosesTable', 0, 2));
    await page.keyboard.type('1,5');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    assert.ok((await H.notifications(page)).some(n => /"1,5" was not accepted/.test(n)));
    assert.equal((await gridRows('editPriorDosesTable'))[0][2], '100');
    // letters that match no unit leave the unit as it was, even after the
    // first moved the list (Gemini's production audit, 2026-11-08, finding 1)
    await H.clearNotifications(page);
    await page.focus(cell('editPriorDosesTable', 1, 3));
    await page.keyboard.type('mz');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    assert.equal((await gridRows('editPriorDosesTable'))[1][3], 'mcg/kg/min');
    assert.ok((await H.notifications(page)).some(n => /"mz" is not one of the choices/.test(n)));
    // a new dose, the last row deleted, then Apply
    await page.click(cell('editPriorDosesTable', 0, 2));
    await page.keyboard.type('120');
    await page.keyboard.press('Enter');
    await page.click(cell('editPriorDosesTable', 2, 0));
    await H.settle(page, 300);
    assert.deepEqual((await sentRows('editPriorDosesTable')).map(r => r.slice(0, 3)),
                     [[false, '0', '120'], [false, '0', '0'], [true, '10', '50']]);
    await page.click('#editDosesOK');
    await H.settle(page, 1000);
    const propofol = (await H.serverState(page)).table.filter(r => r[0] === 'propofol').map(r => r.join(' ')).sort();
    assert.deepEqual(propofol, ['propofol 0 0 mcg/kg/min', 'propofol 0 120 mg']);
    // The next drug's dialog shows that drug's rows from the moment it
    // opens, never the last dialog's under the new title (ChatGPT's fifth
    // review, N01)
    await page.evaluate(() => window.Shiny.setInputValue('plot_dblclick',
      { x: 5, y: 1, panelvar1: 'fentanyl' }, { priority: 'event' }));
    await page.waitForSelector('.modal-title:has-text("Edit fentanyl doses")', { timeout: 30000 });
    const first = await page.evaluate(() => {
      const g = document.getElementById('editPriorDosesTable');
      return g ? Array.from(g.querySelectorAll('tbody tr')).map(tr =>
        Array.from(tr.querySelectorAll('input, select')).map(x => x.type === 'checkbox' ? x.checked : x.value)) : null;
    });
    assert.deepEqual(first, [[false, '0', '0', 'mcg']]);
  });
});

describe('Suggest Dosing', () => {
  test('targets are typed, rows inserted from the row menu, and a bad pasted row refused whole', async () => {
    await open('setTarget', 'targetTableHTML');
    assert.equal((await gridRows('targetTableHTML')).length, 6);
    assert.match(await page.$eval('#targetTableHTML table', t =>
      document.getElementById(t.getAttribute('aria-describedby')).textContent), /Shift\+F10/);
    await page.click(cell('targetTableHTML', 0, 0));
    await page.keyboard.type('0');
    await page.keyboard.press('Tab');
    await page.keyboard.type('3');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    assert.deepEqual((await sentRows('targetTableHTML'))[0], ['0', '3']);
    // a row inserted above the first
    await page.click(cell('targetTableHTML', 0, 0), { button: 'right' });
    await page.click('#targetTableHTML [role=menu] [role=menuitem]:has-text("Insert row above")');
    await H.settle(page, 300);
    const rows = await sentRows('targetTableHTML');
    assert.equal(rows.length, 7);
    assert.deepEqual(rows.slice(0, 2), [['', ''], ['0', '3']]);
    // a pasted block: the row with a target that cannot be read is not pasted
    await H.clearNotifications(page);
    await page.click(cell('targetTableHTML', 2, 0));
    await H.paste(page, '10\t2\n20\tlots\n30\t4');
    await H.settle(page, 400);
    assert.deepEqual((await sentRows('targetTableHTML')).slice(2, 5), [['10', '2'], ['', ''], ['30', '4']]);
    assert.ok((await H.notifications(page)).some(n => /"lots" was not accepted/.test(n)));
  });
});

describe('Drug thresholds', () => {
  test('a drug\'s name cannot be changed, its threshold can, and Apply stores it', async () => {
    await open('editThresholds', 'editThresholdsTable');
    // named, and its keys told, to a screen reader
    const named = await page.$eval('#editThresholdsTable table', t => ({
      caption: t.caption && t.caption.textContent,
      help: (document.getElementById(t.getAttribute('aria-describedby')) || {}).textContent || ''
    }));
    assert.equal(named.caption, 'Drug thresholds');
    assert.match(named.help, /Escape puts it back/);
    assert.doesNotMatch(named.help, /Shift\+F10/);
    const r = await rowOf('editThresholdsTable', 'propofol');
    assert.ok(r >= 0);
    await page.click(cell('editThresholdsTable', r, 0));
    await page.keyboard.type('x');
    assert.equal((await gridRows('editThresholdsTable'))[r][0], 'propofol');
    // F2 on a read-only cell does not start an edit, so End still moves along
    // the row (Gemini's production audit, 2026-11-08, finding 5)
    await page.keyboard.press('F2');
    await page.keyboard.press('End');
    assert.ok(await page.$eval(cell('editThresholdsTable', r, 1), x => x === document.activeElement),
              'End did not move from the read-only cell after F2');
    await page.click(cell('editThresholdsTable', r, 1));
    await page.keyboard.type('2.5');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    // On the last row Enter keeps the focus: a refused entry still shows the
    // value kept, and an accepted one the value stored (CodeRabbit, PR #683)
    const last = (await gridRows('editThresholdsTable')).length - 1;
    const before = (await gridRows('editThresholdsTable'))[last][1];
    await page.click(cell('editThresholdsTable', last, 1));
    await page.keyboard.type('lots');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    assert.equal(await page.$eval(cell('editThresholdsTable', last, 1), x => x.value), before);
    await page.click(cell('editThresholdsTable', last, 1));
    await page.keyboard.type('1e1');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    assert.equal(await page.$eval(cell('editThresholdsTable', last, 1), x => x.value), '10');
    await page.click('#thresholdEditsOK');
    await H.settle(page, 1000);
    assert.equal((await H.serverState(page)).endCe.propofol, 2.5);
  });
});

describe('Drug Library', () => {
  test('a colour is edited and applied; a gas\'s concentration unit cannot be changed', async () => {
    await open('editDrugs', 'editDrugsHTML');
    const gas = await rowOf('editDrugsHTML', 'sevoflurane');
    assert.ok(gas >= 0);
    assert.ok(await page.$eval(cell('editDrugsHTML', gas, 1), x => x.disabled), 'the % unit could be changed');
    const r = await rowOf('editDrugsHTML', 'propofol');
    await page.click(cell('editDrugsHTML', r, 6));
    await page.keyboard.type('#123456');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
    await page.click('#drugEditsOK');
    await H.settle(page, 1500);
    assert.equal((await H.serverState(page)).colors.propofol, '#123456');
  });
});
