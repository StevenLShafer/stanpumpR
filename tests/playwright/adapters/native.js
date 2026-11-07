// The suite's view of the prototype grid (inst/www/dose-grid.js).  Rows and
// columns are 0-based; the columns are Drug, Time, Dose/Target, Units.

'use strict';

const ROOT = '#doseGrid';
const tr = r => `${ROOT} tbody tr:nth-child(${r + 1})`;
const ctl = (r, c) => `${tr(r)} td:nth-child(${c + 1}) ${c === 3 ? 'select' : 'input'}`;

module.exports = {
  name: 'native',
  root: ROOT,

  async readRows(page) {
    return page.$$eval(`${ROOT} tbody tr`, trs => trs.map(tr => {
      const tds = tr.querySelectorAll('td');
      return [tds[0].querySelector('input').value, tds[1].querySelector('input').value,
              tds[2].querySelector('input').value,
              tds[3].querySelector('.dose-grid-unit-text').textContent.replace(/ /g, ' ')];
    }));
  },

  async unitDisplay(page, r) {
    return page.$eval(`${tr(r)} .dose-grid-unit-text`, el => el.textContent);
  },

  // A click into a cell selects its text, so what is typed replaces it
  async focusCell(page, r, c) {
    await page.click(c === 3 ? `${tr(r)} td:nth-child(4)` : ctl(r, c));
  },

  async typeInto(page, r, c, text) {
    await this.focusCell(page, r, c);
    await page.keyboard.type(text);
  },

  // Edit the cell, the caret at the end of its text (F2)
  async editCell(page, r, c) {
    await this.focusCell(page, r, c);
    await page.keyboard.press('F2');
  },

  async clearCell(page, r, c) {
    await this.focusCell(page, r, c);
    await page.keyboard.press('Delete');
    await page.keyboard.press('Enter');
  },

  async unitOptions(page, r) {
    return page.$$eval(`${ctl(r, 3)} option`, os => os.map(o => o.value).filter(u => u !== ''));
  },

  // Keyboard only: the unit typed on the focused list (the browser's own
  // type-ahead), then Enter
  async chooseUnit(page, r, unit) {
    await page.focus(ctl(r, 3));
    await page.keyboard.type(unit);
    await page.waitForTimeout(200);
    await page.keyboard.press('Enter');
  },

  async suggestions(page) {
    await page.waitForTimeout(100);
    return page.$$eval(`${ROOT} [role=listbox]:not([hidden]) [role=option]`, os => os.map(o => o.textContent));
  },

  async drugColor(page, r) {
    return page.$eval(ctl(r, 0), el => getComputedStyle(el).color);
  },

  // What a screen reader is told of a Drug cell beyond its name
  async drugDescription(page, r) {
    return page.$eval(ctl(r, 0), el =>
      [el.getAttribute('title'), el.getAttribute('aria-description')].filter(Boolean).join(' '));
  },

  // The text in the focused cell
  async activeText(page) {
    return page.evaluate(() => {
      const a = document.activeElement;
      return a && a.tagName === 'INPUT' ? a.value : null;
    });
  },

  // Whether the row menu is showing
  async menuOpen(page) {
    return page.$eval(`${ROOT} [role=menu]`, m => !m.hidden);
  },

  // A right click on a cell, which opens the row menu
  async contextClick(page, r, c) {
    await page.click(c === 3 ? `${tr(r)} td:nth-child(4)` : ctl(r, c), { button: 'right' });
  },

  async rowMenu(page, r, item) {
    await page.click(ctl(r, 0), { button: 'right' });
    await page.click(`${ROOT} [role=menu] [role=menuitem]:has-text("${item}")`);
  },
  async insertRowAbove(page, r) { await this.rowMenu(page, r, 'Insert row above'); },
  async removeRow(page, r) { await this.rowMenu(page, r, 'Remove row'); },

  timeHeader(page) {
    return page.locator(`${ROOT} thead th`, { hasText: 'Time' }).first();
  },

  async activeCell(page) {
    return page.evaluate(root => {
      const a = document.activeElement;
      const t = a && a.closest('tr[data-row]');
      if (!t || !document.querySelector(root).contains(a)) return null;
      return [parseInt(t.getAttribute('data-row'), 10), parseInt(a.getAttribute('data-col'), 10)];
    }, ROOT);
  }
};
