// The suite's view of the rhandsontable grid (createHOT(), inst/www/hot_funs.js).
// Only the DOM lookups differ between grids; every keystroke the tests send is
// the same.  Rows and columns are 0-based; the columns are Drug, Time,
// Dose/Target, Units.

'use strict';

const ROOT = '#doseTableHTML';
const td = (r, c) => `${ROOT} .ht_master tbody tr:nth-child(${r + 1}) td:nth-child(${c + 1})`;
const clean = s => String(s).replace(/\u25BC/g, '').replace(/\u00a0/g, ' ').trim();

module.exports = {
  name: 'handsontable',
  root: ROOT,

  async readRows(page) {
    // not the rows of the autocomplete's list, a Handsontable of its own
    const rows = await page.$$eval(`${ROOT} .ht_master tbody tr`, trs =>
      trs.filter(tr => !tr.closest('.handsontableEditor'))
         .map(tr => [...tr.querySelectorAll('td')].map(td => td.textContent)));
    return rows.map(r => r.map(clean));
  },

  // The Units cell's text as drawn, non-breaking spaces and all
  async unitDisplay(page, r) {
    return page.$eval(td(r, 3), el => {
      const clone = el.cloneNode(true);
      clone.querySelectorAll('.htAutocompleteArrow').forEach(a => a.remove());
      return clone.textContent;
    });
  },

  // Put the cell under the keyboard: what typing replaces.  Handsontable
  // reads two clicks on one cell within 500 ms as a double click, which
  // opens the editor on the cell's text, so a typed entry is appended to it:
  // the pause keeps a test's clicks single, as a user's usually are.  The
  // click is forced because an open editor's textarea lies over the cell,
  // and that is what a user's click lands on too.
  async focusCell(page, r, c) {
    await page.waitForTimeout(550);
    await page.click(td(r, c), { force: true });
  },

  // Typing on a selected cell opens its editor with what is typed
  async typeInto(page, r, c, text) {
    await this.focusCell(page, r, c);
    await page.keyboard.type(text);
  },

  // Open the cell's editor on its text (Enter)
  async editCell(page, r, c) {
    await this.focusCell(page, r, c);
    await page.keyboard.press('Enter');
  },

  async clearCell(page, r, c) {
    await this.focusCell(page, r, c);
    await page.keyboard.press('Delete');
  },

  async unitOptions(page, r) {
    return page.evaluate(row => {
      const hot = window.HTMLWidgets.find('#doseTableHTML').hot;
      return (hot.getCellMeta(row, 3).source || []).filter(u => u !== '');
    }, r);
  },

  // Keyboard only: the unit typed on the selected cell, then Enter.  (Enter
  // first opens the list on the current unit, after which typing appends to
  // it and the strict list refuses the result; ArrowDown in the open list
  // moves the grid's selection, not the list's.)
  async chooseUnit(page, r, unit) {
    await this.focusCell(page, r, 3);
    await page.keyboard.type(unit);
    await page.waitForTimeout(200);
    await page.keyboard.press('Enter');
  },

  // The drug names the open autocomplete offers
  async suggestions(page) {
    await page.waitForTimeout(300);
    return page.$$eval('.handsontableEditor.autocompleteEditor .ht_master tbody td', tds =>
      tds.map(t => t.textContent.trim()).filter(Boolean));
  },

  async drugColor(page, r) {
    return page.$eval(td(r, 0), el => getComputedStyle(el).color);
  },

  // What a screen reader is told of a Drug cell beyond its name
  async drugDescription(page, r) {
    return page.$eval(td(r, 0), el =>
      [el.getAttribute('title'), el.getAttribute('aria-description'), el.getAttribute('aria-label')].filter(Boolean).join(' '));
  },

  // The text in the open editor, if a cell is being typed in
  async activeText(page) {
    return page.evaluate(() => {
      const e = window.HTMLWidgets.find('#doseTableHTML').hot.getActiveEditor();
      return e && e.isOpened() && e.TEXTAREA ? e.TEXTAREA.value : null;
    });
  },

  // A right click on a cell, which opens the row menu
  async contextClick(page, r, c) {
    await page.click(td(r, c), { button: 'right', force: true });
  },

  async rowMenu(page, r, item) {
    await page.click(td(r, 0), { button: 'right' });
    await page.click(`.htContextMenu td:has-text("${item}")`);
  },
  async insertRowAbove(page, r) { await this.rowMenu(page, r, 'Insert row above'); },
  async removeRow(page, r) { await this.rowMenu(page, r, 'Remove row'); },

  timeHeader(page) {
    return page.locator(`${ROOT} .ht_clone_top thead th`, { hasText: 'Time' }).first();
  },

  // Where the keyboard is: the [row, col] the grid has selected
  async activeCell(page) {
    return page.evaluate(() => {
      const hot = window.HTMLWidgets.find('#doseTableHTML').hot;
      const s = hot.getSelectedLast();
      return s ? [s[0], s[1]] : null;
    });
  }
};
