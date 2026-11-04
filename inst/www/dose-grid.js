// The dose grid (PROTOTYPE, docs/grid-replacement.md): the main dose table
// as a plain HTML table of form controls with its own Shiny input binding,
// in place of rhandsontable.  Chosen with options(stanpumpR.doseGrid =
// "native"); R/dose-grid.R is its server half.
//
// The browser holds the grid and checks every entry as it is committed
// (validateTime() and validateDose() from hot_funs.js, which the tests hold to
// R's); the server receives the whole table, once per committed edit, and
// keeps the draft and its undo history as before.  The server sends the draft
// back only when it differs from what the grid holds (a gas rule, undo, a
// table written by a dialog, a change of time unit), so the grid is never
// redrawn under the user's fingers: rhandsontable redrew it after every edit,
// and the first key typed meanwhile was lost.
//
// Everything a cell shows is set with .value or .textContent, never as HTML.
// The cell controls have no id or name, and carry data-shiny-no-bind-input, so
// Shiny binds none of them as inputs of their own.

(function() {
  'use strict';

  var COLS = { DRUG: 0, TIME: 1, DOSE: 2, UNITS: 3 };
  var NCOL = 4;
  var gridCount = 0;

  function blankRow() { return ['', '', '', '']; }
  function isBlank(x) { return x === null || x === undefined || String(x).trim() === ''; }
  function sameRows(a, b) {
    if (a.length !== b.length) return false;
    for (var i = 0; i < a.length; i++) {
      for (var j = 0; j < NCOL; j++) if (String(a[i][j]) !== String(b[i][j])) return false;
    }
    return true;
  }
  function el(tag, attrs, text) {
    var e = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function(k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function notify(lines, id) {
    if (!lines.length || !window.Shiny || !Shiny.notifications) return;
    var html = lines.slice(0, 3).map(escapeHtml).join('<br>');
    if (lines.length > 3) html += '<br>(and ' + (lines.length - 3) + ' more)';
    Shiny.notifications.show({ html: html, type: 'warning', duration: 8000, id: id });
  }

  // A TSV clipboard (what Excel, Google Sheets and Handsontable copy), with
  // the quoted cells a spreadsheet writes for a value holding a tab, newline
  // or quote
  function parseTSV(text) {
    var rows = [], row = [], cell = '', i = 0, q = false;
    text = String(text).replace(/\r\n?/g, '\n');
    if (text.slice(-1) === '\n') text = text.slice(0, -1);
    while (i < text.length) {
      var ch = text[i];
      if (q) {
        if (ch === '"' && text[i + 1] === '"') { cell += '"'; i += 2; continue; }
        if (ch === '"') { q = false; i++; continue; }
        cell += ch; i++; continue;
      }
      if (ch === '"' && cell === '') { q = true; i++; continue; }
      if (ch === '\t') { row.push(cell); cell = ''; i++; continue; }
      if (ch === '\n') { row.push(cell); rows.push(row); row = []; cell = ''; i++; continue; }
      cell += ch; i++;
    }
    row.push(cell); rows.push(row);
    return rows;
  }

  function DoseGrid(root) {
    var self = this;
    this.root = root;
    this.id = ++gridCount;
    this.header = root.getAttribute('data-dose-header') || 'Dose';
    this.maxRows = parseInt(root.getAttribute('data-max-rows'), 10) || 499;
    this.rows = [blankRow()];
    this.format = null;
    this.seq = 0;             // edits sent
    this.value = null;        // what getValue() reports: null until the first edit
    this.callback = null;
    this.drugs = {};          // name -> { units: [], defaultUnit: '', illicit: bool }
    this.choices = [];        // the names the autocomplete offers
    this.choiceIndex = {};    // lower-case name -> name
    this.error = '';
    this.build();
    root.addEventListener('keydown', function(e) { self.onKeyDown(e); });
    root.addEventListener('focusin', function(e) { self.onFocusIn(e); });
    root.addEventListener('focusout', function(e) { self.onFocusOut(e); });
    root.addEventListener('mouseup', function(e) { self.onMouseUp(e); });
    root.addEventListener('input', function(e) { self.onInput(e); });
    root.addEventListener('change', function(e) { self.onChange(e); });
    root.addEventListener('paste', function(e) { self.onPaste(e); });
    root.addEventListener('contextmenu', function(e) { self.onContextMenu(e); });
    document.addEventListener('mousedown', function(e) {
      if (!self.menu.contains(e.target)) self.closeMenu();
      if (!self.listbox.contains(e.target) && e.target !== self.listboxFor) self.closeList();
    });
  }

  DoseGrid.prototype.build = function() {
    var self = this;
    var p = 'dosegrid' + this.id;
    this.root.textContent = '';
    this.status = el('div', { 'class': 'dose-grid-status', role: 'status', 'aria-live': 'polite' });
    this.scroller = el('div', { 'class': 'dose-grid-scroll' });
    this.table = el('table', { 'class': 'dose-grid', 'aria-describedby': p + '-help' });
    var caption = el('caption', { 'class': 'visually-hidden' }, 'Doses');
    var help = el('p', { id: p + '-help', 'class': 'visually-hidden' },
      'One dose a row: drug, time, dose and units. Enter moves down a row. ' +
      'For row actions (insert, remove), press Shift+F10 or right-click a row.');
    this.table.appendChild(caption);
    var thead = el('thead'), tr = el('tr');
    ['Drug', 'Time', this.header, 'Units'].forEach(function(h, j) {
      var th = el('th', { scope: 'col', id: p + '-col' + j, 'class': 'dose-grid-col' + j }, h);
      tr.appendChild(th);
    });
    thead.appendChild(tr);
    this.timeTh = tr.children[COLS.TIME];
    this.timeTh.addEventListener('mouseenter', function() { self.updateTimeTip(); });
    this.table.appendChild(thead);
    this.tbody = el('tbody');
    this.table.appendChild(this.tbody);
    this.scroller.appendChild(this.table);
    this.listbox = el('ul', { id: p + '-list', role: 'listbox', 'class': 'dose-grid-list', 'aria-label': 'Drugs', hidden: '' });
    this.listbox.addEventListener('mousedown', function(e) {
      var li = e.target.closest('li');
      if (!li) return;
      e.preventDefault();
      self.pickOption(li.textContent);
    });
    this.menu = el('ul', { role: 'menu', 'class': 'dose-grid-menu dropdown-menu', hidden: '' });
    [['above', 'Insert row above'], ['below', 'Insert row below'], ['remove', 'Remove row']].forEach(function(m) {
      var li = el('li', { role: 'none' });
      var b = el('button', { type: 'button', role: 'menuitem', 'class': 'dropdown-item', 'data-action': m[0], tabindex: '-1' }, m[1]);
      li.appendChild(b);
      self.menu.appendChild(li);
    });
    this.menu.addEventListener('click', function(e) {
      var b = e.target.closest('button[data-action]');
      if (b) self.rowAction(self.menuRow, b.getAttribute('data-action'));
    });
    this.menu.addEventListener('keydown', function(e) { self.onMenuKey(e); });
    this.root.appendChild(help);
    this.root.appendChild(this.status);
    this.root.appendChild(this.scroller);
    this.root.appendChild(this.listbox);
    this.root.appendChild(this.menu);
    this.renderAll();
  };

  // --- drawing ---------------------------------------------------------------

  DoseGrid.prototype.cellLabel = function(col, r) {
    return ['Drug', 'Time', this.header, 'Units'][col] + ', row ' + (r + 1);
  };

  DoseGrid.prototype.makeRow = function(r) {
    var tr = el('tr', { 'data-row': String(r) });
    for (var c = 0; c < NCOL; c++) {
      var td = el('td', { 'class': 'dose-grid-col' + c });
      var ctl;
      if (c === COLS.UNITS) {
        td.appendChild(el('span', { 'class': 'dose-grid-unit-text', 'aria-hidden': 'true' }));
        ctl = el('select');
      } else {
        ctl = el('input', { type: 'text', autocomplete: 'off', spellcheck: 'false' });
        if (c === COLS.DRUG) {
          ctl.setAttribute('role', 'combobox');
          ctl.setAttribute('aria-autocomplete', 'list');
          ctl.setAttribute('aria-expanded', 'false');
          ctl.setAttribute('aria-controls', this.listbox.id);
        } else {
          ctl.setAttribute('inputmode', 'decimal');
        }
      }
      ctl.setAttribute('data-shiny-no-bind-input', '');
      ctl.setAttribute('data-col', String(c));
      ctl.setAttribute('aria-label', this.cellLabel(c, r));
      td.appendChild(ctl);
      tr.appendChild(td);
    }
    this.fillRow(tr, r);
    return tr;
  };

  DoseGrid.prototype.fillRow = function(tr, r) {
    var row = this.rows[r];
    var cells = tr.children;
    var drug = cells[COLS.DRUG].firstChild;
    drug.value = row[COLS.DRUG];
    var info = this.drugs[row[COLS.DRUG]];
    drug.classList.toggle('dose-grid-illicit', !!(info && info.illicit));
    cells[COLS.TIME].firstChild.value = row[COLS.TIME];
    cells[COLS.DOSE].firstChild.value = row[COLS.DOSE];
    var sel = cells[COLS.UNITS].lastChild;
    var units = info ? info.units : [];
    // a stored unit the list lacks (a table restored with another drug list)
    // is still shown, not silently replaced
    var list = units.slice();
    if (row[COLS.UNITS] !== '' && list.indexOf(row[COLS.UNITS]) === -1) list.unshift(row[COLS.UNITS]);
    if (list.length === 0) list = [''];
    if (sel.getAttribute('data-units') !== list.join('\n')) {
      sel.textContent = '';
      list.forEach(function(u) { var o = document.createElement('option'); o.value = u; o.textContent = u; sel.appendChild(o); });
      sel.setAttribute('data-units', list.join('\n'));
    }
    sel.value = row[COLS.UNITS];
    sel.disabled = isBlank(row[COLS.DRUG]);
    cells[COLS.UNITS].firstChild.textContent = row[COLS.UNITS] ? unitWrapText(row[COLS.UNITS]) : '';
    for (var c = 0; c < NCOL; c++) cells[c].lastChild.setAttribute('aria-label', this.cellLabel(c, r));
    tr.setAttribute('data-row', String(r));
  };

  // Redraws every row.  The row elements are reused, so the keyboard stays
  // where it was; it is moved only if its row went.  Text being typed in the
  // focused cell is kept (`typing`, from receive()).
  DoseGrid.prototype.renderAll = function(typing) {
    var at = this.activeCell();
    var trs = this.tbody.children;
    while (trs.length > this.rows.length) this.tbody.removeChild(this.tbody.lastChild);
    for (var r = 0; r < this.rows.length; r++) {
      if (r < trs.length) this.fillRow(trs[r], r);
      else this.tbody.appendChild(this.makeRow(r));
    }
    if (at && !this.root.contains(document.activeElement)) {
      this.focusCell(Math.min(at[0], this.rows.length - 1), at[1]);
    } else if (at && typing !== undefined && typing !== null) {
      var ctl = this.control(at[0], at[1]);
      if (ctl && ctl.tagName === 'INPUT') ctl.value = typing;
    }
    this.updateTimeTip();
  };

  DoseGrid.prototype.renderRow = function(r) {
    var tr = this.tbody.children[r];
    if (tr) this.fillRow(tr, r); else this.renderAll();
  };

  DoseGrid.prototype.control = function(r, c) {
    var tr = this.tbody.children[r];
    return tr ? tr.children[c].lastChild : null;
  };

  DoseGrid.prototype.cellOf = function(node) {
    if (!node || !node.getAttribute || node.getAttribute('data-col') === null) return null;
    var tr = node.closest('tr[data-row]');
    if (!tr || !this.tbody.contains(tr)) return null;
    return [parseInt(tr.getAttribute('data-row'), 10), parseInt(node.getAttribute('data-col'), 10)];
  };

  DoseGrid.prototype.activeCell = function() {
    return this.root.contains(document.activeElement) ? this.cellOf(document.activeElement) : null;
  };

  DoseGrid.prototype.focusCell = function(r, c) {
    var ctl = this.control(r, c);
    if (!ctl) return;
    if (ctl.disabled) ctl = this.control(r, COLS.DRUG);
    ctl.focus();
    if (ctl.tagName === 'INPUT') ctl.select();
  };

  DoseGrid.prototype.updateTimeTip = function() {
    var tip = (typeof timeHeaderTipText === 'function') ? timeHeaderTipText() : '';
    if (tip) this.timeTh.setAttribute('title', tip);
  };

  DoseGrid.prototype.showError = function(msg) {
    this.error = msg || '';
    this.status.textContent = this.error;
    this.status.classList.toggle('text-danger', !!this.error);
  };

  // --- the rules of a row ------------------------------------------------------

  DoseGrid.prototype.unitsOf = function(drug) {
    var info = this.drugs[drug];
    return info ? info.units : [];
  };

  DoseGrid.prototype.defaultUnitOf = function(drug) {
    var info = this.drugs[drug];
    return info ? info.defaultUnit : '';
  };

  // A name typed or pasted, as the library spells it; null if it is not
  // offered.  The name the row already holds is kept even when the opt-in
  // would hide it (a bookmark's illicit drug, say).
  DoseGrid.prototype.canonicalDrug = function(text, current) {
    var t = trimEntry(text);
    if (t === '') return '';
    if (current && t.toLowerCase() === String(current).toLowerCase()) return current;
    var hit = this.choiceIndex[t.toLowerCase()];
    return hit === undefined ? null : hit;
  };

  // A row's unit once its drug is settled: the one asked for if the drug
  // offers it, else the one it has if the drug offers that, else the drug's
  // default.  A unit is never kept for a drug that does not offer it: a
  // wrong unit can be a thousandfold error.
  DoseGrid.prototype.settleUnit = function(row, asked) {
    var drug = row[COLS.DRUG];
    if (isBlank(drug)) { row[COLS.UNITS] = ''; return; }
    var units = this.unitsOf(drug);
    if (asked !== undefined && units.indexOf(asked) !== -1) { row[COLS.UNITS] = asked; return; }
    if (units.indexOf(row[COLS.UNITS]) !== -1) return;
    row[COLS.UNITS] = this.defaultUnitOf(drug);
  };

  // A Time or Dose entry, as hookSanitize() reads it: the value to store, or
  // a refusal.  `old` is what the cell held.
  DoseGrid.prototype.readEntry = function(col, text, old, hasDrug) {
    var blank = trimEntry(text) === '';
    if (blank) {
      if (!isBlank(old) && hasDrug) return { refused: '', keep: old };
      return { value: '' };
    }
    var v = (col === COLS.TIME) ? validateTime(text) : validateDose(text);
    if (v === '') return { refused: trimEntry(text), keep: isBlank(old) ? '' : old };
    return { value: String(v) };
  };

  // A blank row below the last drug, to type the next one in: the server's
  // rule too, so the table it stores is the one drawn
  DoseGrid.prototype.ensureTrailingBlank = function() {
    if (this.rows.length === 0 || !isBlank(this.rows[this.rows.length - 1][COLS.DRUG])) {
      this.rows.push(blankRow());
    }
  };

  // --- committing --------------------------------------------------------------

  DoseGrid.prototype.send = function() {
    this.ensureTrailingBlank();
    this.seq += 1;
    this.value = {
      rows: this.rows.map(function(r) { return r.slice(); }),
      timeFormat: this.format,
      seq: this.seq
    };
    if (this.callback) this.callback(false);
  };

  // The control's text committed to its cell; true if the model changed
  DoseGrid.prototype.commit = function(r, c) {
    var ctl = this.control(r, c);
    if (!ctl || !this.rows[r]) return false;
    var row = this.rows[r];
    var text = ctl.value;
    if (c === COLS.DRUG) {
      var drug = this.canonicalDrug(text, row[COLS.DRUG]);
      if (drug === null) {
        notify(['"' + trimEntry(text) + '" is not a drug in the library; the cell keeps what it had.'], 'doseGridDrugRefused');
        ctl.value = row[COLS.DRUG];
        return false;
      }
      if (drug === row[COLS.DRUG]) { ctl.value = drug; return false; }
      if (drug === '') {
        this.rows[r] = blankRow();
      } else {
        row[COLS.DRUG] = drug;
        row[COLS.TIME] = '0';
        row[COLS.DOSE] = '0';
        row[COLS.UNITS] = '';
        this.settleUnit(row);
      }
    } else if (c === COLS.UNITS) {
      if (text === row[COLS.UNITS] || this.unitsOf(row[COLS.DRUG]).indexOf(text) === -1) {
        ctl.value = row[COLS.UNITS];
        return false;
      }
      row[COLS.UNITS] = text;
    } else {
      var hasDrug = !isBlank(row[COLS.DRUG]);
      if (!hasDrug) {
        // a time or a dose beside no drug is not kept, as the old grid did
        ctl.value = '';
        return false;
      }
      var e = this.readEntry(c, text, row[c], hasDrug);
      if (e.refused !== undefined) {
        notifyRefusedEntries([{ col: c, entry: e.refused }], COLS.TIME);
        row[c] = e.keep;
        ctl.value = e.keep;
        return false;
      }
      if (e.value === row[c]) { ctl.value = e.value; return false; }
      row[c] = e.value;
    }
    this.renderRow(r);
    if (r === this.rows.length - 1) { this.ensureTrailingBlank(); this.renderAll(); }
    this.send();
    return true;
  };

  // --- paste ---------------------------------------------------------------------

  DoseGrid.prototype.onPaste = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var text = (e.clipboardData || window.clipboardData).getData('text');
    var block = parseTSV(text);
    var single = block.length === 1 && block[0].length === 1;
    var ctl = e.target;
    // One value pasted into a cell being typed in is ordinary text: it goes
    // in at the caret.  Anything else replaces cells, as in a spreadsheet.
    if (single && ctl.tagName === 'INPUT' && ctl.value !== this.rows[at[0]][at[1]]) return;
    e.preventDefault();
    this.closeList();
    this.pasteBlock(at[0], at[1], block);
  };

  DoseGrid.prototype.pasteBlock = function(r0, c0, block) {
    var refusedEntries = [], refusedUnits = [], refusedDrugs = [];
    var dropped = 0;
    for (var i = 0; i < block.length; i++) {
      var r = r0 + i;
      if (r >= this.maxRows) { dropped = block.length - i; break; }
      while (this.rows.length <= r) this.rows.push(blankRow());
      var line = block[i];
      var row = this.rows[r];
      var touched = {};
      for (var j = 0; j < line.length && c0 + j < NCOL; j++) touched[c0 + j] = line[j];
      if (touched.hasOwnProperty(COLS.DRUG)) {
        var d = this.canonicalDrug(touched[COLS.DRUG], row[COLS.DRUG]);
        if (d === null) refusedDrugs.push(trimEntry(touched[COLS.DRUG]));
        else if (d !== row[COLS.DRUG]) {
          var drugOnly = Object.keys(touched).length === 1;
          row[COLS.DRUG] = d;
          if (drugOnly && d !== '') { row[COLS.TIME] = '0'; row[COLS.DOSE] = '0'; }
        }
      }
      if (isBlank(row[COLS.DRUG])) { this.rows[r] = blankRow(); continue; }
      [COLS.TIME, COLS.DOSE].forEach(function(c) {
        // a cell the paste leaves blank in a row it gives a drug is 0, as a
        // drug chosen alone starts at time 0, dose 0
        if (!touched.hasOwnProperty(c)) {
          if (isBlank(row[c])) row[c] = '0';
          return;
        }
        var e = this.readEntry(c, touched[c], row[c], true);
        if (e.refused !== undefined) { refusedEntries.push({ col: c, entry: e.refused }); row[c] = e.keep; }
        else row[c] = (e.value === '') ? '0' : e.value;
      }, this);
      var asked;
      if (touched.hasOwnProperty(COLS.UNITS) && trimEntry(touched[COLS.UNITS]) !== '') {
        asked = trimEntry(touched[COLS.UNITS]);
        if (this.unitsOf(row[COLS.DRUG]).indexOf(asked) === -1) {
          refusedUnits.push({ drug: row[COLS.DRUG], unit: asked });
          asked = undefined;
        }
      }
      this.settleUnit(row, asked);
    }
    if (refusedDrugs.length) {
      notify(refusedDrugs.map(function(d) { return '"' + d + '" is not a drug in the library; it was not pasted.'; }), 'doseGridDrugRefused');
    }
    if (refusedEntries.length) notifyRefusedEntries(refusedEntries, COLS.TIME);
    notifyRefusedUnits(refusedUnits, "it was not used. Check the row's unit.");
    if (dropped > 0) {
      notify([dropped + ' pasted row' + (dropped === 1 ? ' was' : 's were') + ' not added: the table holds at most ' +
              this.maxRows + ' doses.'], 'doseGridRowLimit');
    }
    this.renderAll();
    this.send();
  };

  // --- rows ------------------------------------------------------------------------

  DoseGrid.prototype.rowAction = function(r, action) {
    this.closeMenu();
    if (r === null || r === undefined || !this.rows[r]) return;
    var col = 0;
    if (action === 'above' || action === 'below') {
      if (this.rows.length >= this.maxRows) {
        notify(['The table holds at most ' + this.maxRows + ' doses.'], 'doseGridRowLimit');
        return;
      }
      var at = action === 'above' ? r : r + 1;
      this.rows.splice(at, 0, blankRow());
      r = at;
    } else if (action === 'remove') {
      this.rows.splice(r, 1);
      if (this.rows.length === 0) this.rows.push(blankRow());
      r = Math.min(r, this.rows.length - 1);
    }
    this.renderAll();
    this.send();
    this.focusCell(r, col);
  };

  DoseGrid.prototype.openMenu = function(r, x, y) {
    this.menuRow = r;
    this.menu.hidden = false;
    var box = this.root.getBoundingClientRect();
    this.menu.style.left = Math.max(0, x - box.left) + 'px';
    this.menu.style.top = Math.max(0, y - box.top) + 'px';
    var first = this.menu.querySelector('button');
    if (first) first.focus();
  };

  DoseGrid.prototype.closeMenu = function() {
    if (this.menu.hidden) return;
    this.menu.hidden = true;
    if (this.menuRow !== null && this.menuRow !== undefined && this.menu.contains(document.activeElement)) {
      this.focusCell(this.menuRow, 0);
    }
  };

  DoseGrid.prototype.onContextMenu = function(e) {
    var tr = e.target.closest && e.target.closest('tr[data-row]');
    if (!tr || !this.tbody.contains(tr)) return;
    e.preventDefault();
    this.openMenu(parseInt(tr.getAttribute('data-row'), 10), e.clientX, e.clientY);
  };

  DoseGrid.prototype.onMenuKey = function(e) {
    var items = Array.prototype.slice.call(this.menu.querySelectorAll('button'));
    var i = items.indexOf(document.activeElement);
    if (e.key === 'ArrowDown') { items[(i + 1) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'ArrowUp') { items[(i - 1 + items.length) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'Escape' || e.key === 'Tab') { e.preventDefault(); this.closeMenu(); }
    e.stopPropagation();
  };

  // --- the drug list ---------------------------------------------------------------

  DoseGrid.prototype.matches = function(text) {
    var t = trimEntry(text).toLowerCase();
    if (t === '') return [];
    var starts = [], within = [];
    for (var i = 0; i < this.choices.length; i++) {
      var n = this.choices[i].toLowerCase();
      var k = n.indexOf(t);
      if (k === 0) starts.push(this.choices[i]); else if (k > 0) within.push(this.choices[i]);
    }
    return starts.concat(within).slice(0, 50);
  };

  DoseGrid.prototype.openList = function(input) {
    var names = this.matches(input.value);
    this.listbox.textContent = '';
    this.active = -1;
    if (names.length === 0) { this.closeList(); return; }
    var p = this.listbox.id;
    names.forEach(function(n, i) {
      this.listbox.appendChild(el('li', { role: 'option', id: p + '-opt' + i, 'aria-selected': 'false' }, n));
    }, this);
    var cell = input.getBoundingClientRect(), box = this.root.getBoundingClientRect();
    this.listbox.style.left = (cell.left - box.left) + 'px';
    this.listbox.style.top = (cell.bottom - box.top) + 'px';
    this.listbox.style.minWidth = cell.width + 'px';
    this.listbox.hidden = false;
    this.listboxFor = input;
    input.setAttribute('aria-expanded', 'true');
    // the first match is the one Enter takes, as in the old grid
    this.setActive(0);
  };

  DoseGrid.prototype.closeList = function() {
    if (this.listboxFor) {
      this.listboxFor.setAttribute('aria-expanded', 'false');
      this.listboxFor.removeAttribute('aria-activedescendant');
    }
    this.listbox.hidden = true;
    this.listboxFor = null;
    this.active = -1;
  };

  DoseGrid.prototype.setActive = function(i) {
    var opts = this.listbox.children;
    if (!opts.length) return;
    i = (i + opts.length) % opts.length;
    for (var k = 0; k < opts.length; k++) opts[k].setAttribute('aria-selected', k === i ? 'true' : 'false');
    this.active = i;
    opts[i].scrollIntoView({ block: 'nearest' });
    if (this.listboxFor) this.listboxFor.setAttribute('aria-activedescendant', opts[i].id);
  };

  DoseGrid.prototype.pickOption = function(name) {
    var input = this.listboxFor;
    if (!input) return;
    input.value = name;
    var at = this.cellOf(input);
    this.closeList();
    if (at) this.commit(at[0], at[1]);
  };

  // --- events ------------------------------------------------------------------------

  DoseGrid.prototype.onFocusIn = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    this.focusedAt = at;
    this.justFocused = true;
    if (e.target.tagName === 'INPUT') e.target.select();
  };

  // A click into a cell selects its text, as a tab into it does: what is
  // typed replaces it
  DoseGrid.prototype.onMouseUp = function(e) {
    if (this.justFocused && e.target.tagName === 'INPUT') e.preventDefault();
    this.justFocused = false;
  };

  DoseGrid.prototype.onFocusOut = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    this.justFocused = false;
    if (e.target.tagName === 'INPUT' && e.target.value !== this.rows[at[0]][at[1]]) {
      if (at[1] === COLS.DRUG && this.listboxFor === e.target && this.active >= 0 &&
          trimEntry(e.target.value) !== '' && this.canonicalDrug(e.target.value) === null) {
        e.target.value = this.listbox.children[this.active].textContent;
      }
      this.commit(at[0], at[1]);
    }
    if (this.listboxFor === e.target) this.closeList();
  };

  DoseGrid.prototype.onInput = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    this.justFocused = false;
    if (at[1] === COLS.DRUG) this.openList(e.target);
  };

  DoseGrid.prototype.onChange = function(e) {
    var at = this.cellOf(e.target);
    if (at && at[1] === COLS.UNITS) this.commit(at[0], at[1]);
  };

  DoseGrid.prototype.onKeyDown = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var r = at[0], c = at[1], t = e.target;
    var listOpen = this.listboxFor === t && !this.listbox.hidden;
    var edited = t.tagName === 'INPUT' && t.value !== this.rows[r][c];

    // Shift+F10 and the context-menu key: the row's actions
    if ((e.key === 'F10' && e.shiftKey) || e.key === 'ContextMenu') {
      e.preventDefault();
      var box = t.getBoundingClientRect();
      this.openMenu(r, box.left, box.bottom);
      return;
    }
    // Undo and Redo are the dose table's, unless a cell is being typed in
    if ((e.ctrlKey || e.metaKey) && !e.altKey && !edited &&
        (e.key === 'z' || e.key === 'Z' || e.key === 'y' || e.key === 'Y')) {
      var redo = e.key === 'y' || e.key === 'Y' || e.shiftKey;
      var b = document.getElementById(redo ? 'dosetable_redo' : 'dosetable_undo');
      if (b && !b.disabled) b.click();
      e.preventDefault();
      return;
    }
    if (listOpen && (e.key === 'ArrowDown' || e.key === 'ArrowUp')) {
      this.setActive(this.active + (e.key === 'ArrowDown' ? 1 : -1));
      e.preventDefault();
      return;
    }
    if (e.key === 'ArrowDown' && e.altKey && c === COLS.DRUG) { this.openList(t); e.preventDefault(); return; }
    if (e.key === 'Escape') {
      if (listOpen) { this.closeList(); e.preventDefault(); return; }
      if (t.tagName === 'INPUT') { t.value = this.rows[r][c]; t.select(); e.preventDefault(); }
      return;
    }
    if (e.key === 'Enter') {
      e.preventDefault();
      if (listOpen && this.active >= 0 && this.canonicalDrug(t.value, this.rows[r][c]) === null) {
        t.value = this.listbox.children[this.active].textContent;
      }
      this.closeList();
      if (t.tagName === 'INPUT') this.commit(r, c);
      var next = e.shiftKey ? r - 1 : r + 1;
      if (next >= 0 && next < this.rows.length) this.focusCell(next, c);
      return;
    }
    if (e.key === 'Tab' && listOpen && this.active >= 0 && this.canonicalDrug(t.value, this.rows[r][c]) === null) {
      t.value = this.listbox.children[this.active].textContent;
      return;   // committed as the focus leaves
    }
    if ((e.key === 'ArrowDown' || e.key === 'ArrowUp') && t.tagName === 'INPUT') {
      var to = r + (e.key === 'ArrowDown' ? 1 : -1);
      if (to >= 0 && to < this.rows.length) { e.preventDefault(); this.focusCell(to, c); }
      return;
    }
    // Time and Dose take only what a number or a time can hold; a minus sign
    // is let through so that "-2" is refused with a message, not typed as 2
    if ((c === COLS.TIME || c === COLS.DOSE) && e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      var ok = (c === COLS.DOSE || getTimeMode() === 'relative') ? /^[0-9.\-]$/.test(e.key) : /^[0-9.:\-]$/.test(e.key);
      if (!ok) e.preventDefault();
    }
  };

  // --- from the server --------------------------------------------------------------

  DoseGrid.prototype.receive = function(msg) {
    if (msg.drugs) {
      var drugs = {};
      msg.drugs.forEach(function(d) {
        drugs[d.Drug] = { units: [].concat(d.Units || []), defaultUnit: d.DefaultUnits || '', illicit: !!d.Illicit };
      });
      this.drugs = drugs;
    }
    if (msg.choices) {
      this.choices = [].concat(msg.choices);
      var index = {};
      this.choices.forEach(function(n) { index[n.toLowerCase()] = n; });
      this.choiceIndex = index;
    }
    if (msg.error !== undefined) this.showError(msg.error);
    if (msg.rows) {
      // A draft the server sent before it had our latest edit is stale: the
      // one it sends after that edit is the one to draw
      if ((msg.ack || 0) < this.seq) return;
      if (msg.timeFormat) this.format = msg.timeFormat;
      var rows = msg.rows.map(function(r) { return [0, 1, 2, 3].map(function(j) { return r[j] === null || r[j] === undefined ? '' : String(r[j]); }); });
      if (rows.length === 0) rows = [blankRow()];
      if (sameRows(rows, this.rows) && !msg.drugs) return;
      var at = this.activeCell(), typing = null;
      if (at) {
        var ctl = this.control(at[0], at[1]);
        if (ctl && ctl.tagName === 'INPUT' && ctl.value !== this.rows[at[0]][at[1]]) typing = ctl.value;
      }
      this.rows = rows;
      this.renderAll(typing);
    } else if (msg.drugs || msg.choices) {
      this.renderAll();
    }
  };

  // --- the Shiny binding ---------------------------------------------------------------

  // for test-dose-grid.R, which runs this file in Node
  if (typeof module !== 'undefined' && module.exports) module.exports = { parseTSV: parseTSV };
  if (typeof window === 'undefined' || !window.Shiny) return;
  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function(scope) { return $(scope).find('.stanpumpr-dose-grid'); },
    initialize: function(root) { root._doseGrid = new DoseGrid(root); },
    getValue: function(root) { return root._doseGrid ? root._doseGrid.value : null; },
    subscribe: function(root, callback) { root._doseGrid.callback = callback; },
    unsubscribe: function(root) { if (root._doseGrid) root._doseGrid.callback = null; },
    receiveMessage: function(root, msg) { root._doseGrid.receive(msg); },
    getRatePolicy: function() { return null; }
  });
  Shiny.inputBindings.register(binding, 'stanpumpr.doseGrid');
})();
