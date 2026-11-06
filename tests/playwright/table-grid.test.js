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
const adapter = require('./adapters/native');

const URL = process.env.STANPUMPR_URL;
if (!URL) throw new Error('STANPUMPR_URL is not set: run the suite through run.js');
const BROWSER = process.env.STANPUMPR_BROWSER || 'chromium';

let browser, context, page;

before(async () => {
  browser = await playwright[BROWSER].launch(BROWSER === 'firefox' ? {
    firefoxUserPrefs: { 'dom.events.asyncClipboard.readText': true, 'dom.events.testing.asyncClipboard': true }
  } : {});
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
  await page.waitForSelector(`#${id} tbody tr`, { timeout: 30000 });
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
  });
});

describe('Suggest Dosing', () => {
  test('targets are typed, rows inserted from the row menu, and a bad pasted row refused whole', async () => {
    await open('setTarget', 'targetTableHTML');
    assert.equal((await gridRows('targetTableHTML')).length, 6);
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
    const r = await rowOf('editThresholdsTable', 'propofol');
    assert.ok(r >= 0);
    await page.click(cell('editThresholdsTable', r, 0));
    await page.keyboard.type('x');
    assert.equal((await gridRows('editThresholdsTable'))[r][0], 'propofol');
    await page.click(cell('editThresholdsTable', r, 1));
    await page.keyboard.type('2.5');
    await page.keyboard.press('Enter');
    await H.settle(page, 300);
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
