// The dose grid (docs/grid-replacement.md): the main dose table as a plain
// HTML table of form controls with its own Shiny input binding, which
// replaced rhandsontable (2026-11-06).  R/dose-grid.R is its server half;
// the dialogs' grids are its sibling, table-grid.js.
//
// The browser holds the grid and checks every entry as it is committed
// (validateTime() and validateDose() from grid-funs.js, which the tests hold to
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

  // The width of the arrow at the right of a Units cell (app.css), which
  // opens its list
  var UNIT_ARROW_WIDTH = 20;
  // A touch held this long (ms) is a long press, which opens the row menu
  var LONG_PRESS_MS = 450;
  // Letters typed on a Units cell within this many ms of each other are one
  // search (unitSearch())
  var UNIT_SEARCH_MS = 1000;
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
    // the id of the button that applies the table, enabled on the first key
    // typed (enableApply())
    this.applyId = root.getAttribute('data-apply');
    this.pickerFor = null;
    this.search = null;
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
    root.addEventListener('pointerdown', function(e) { self.lastPointer = { type: e.pointerType, time: Date.now() }; });
    root.addEventListener('mousedown', function(e) { self.onMouseDown(e); });
    root.addEventListener('mouseup', function(e) { self.onMouseUp(e); });
    root.addEventListener('dblclick', function(e) { self.onDoubleClick(e); });
    root.addEventListener('input', function(e) { self.onInput(e); });
    root.addEventListener('change', function(e) { self.onChange(e); });
    root.addEventListener('paste', function(e) { self.onPaste(e); });
    root.addEventListener('contextmenu', function(e) { self.onContextMenu(e); });
    document.addEventListener('keydown', function(e) { self.onDocumentKey(e); });
    // A browser sends no paste event to a <select>, so Ctrl+V on a Units
    // cell did nothing and said nothing (the external review of 2026-11-05,
    // Gemini's script, reproduced).  For that one key the focus moves to a
    // hidden text field, which the paste reaches, and comes back to the cell.
    this.pasteSink.addEventListener('paste', function(e) { self.onSinkPaste(e); });
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
      'One dose a row: drug, time, dose and units. Enter moves down a row; ' +
      'Ctrl+Z undoes the last edit and Ctrl+Y redoes it. ' +
      'In a Units cell, type the unit, or press Alt+Down or Space for the list. ' +
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
    // Not Bootstrap's .dropdown-menu: its global keydown handler, expecting a
    // dropdown toggle, threw on Escape (the external review of 2026-11-05)
    this.menu = el('ul', { role: 'menu', 'class': 'dose-grid-menu', hidden: '' });
    [['above', 'Insert row above'], ['below', 'Insert row below'], ['remove', 'Remove row']].forEach(function(m) {
      var li = el('li', { role: 'none' });
      var b = el('button', { type: 'button', role: 'menuitem', 'class': 'dose-grid-menu-item', 'data-action': m[0], tabindex: '-1' }, m[1]);
      li.appendChild(b);
      self.menu.appendChild(li);
    });
    this.menu.addEventListener('click', function(e) {
      var b = e.target.closest('button[data-action]');
      if (b) self.rowAction(self.menuRow, b.getAttribute('data-action'));
    });
    this.menu.addEventListener('keydown', function(e) { self.onMenuKey(e); });
    this.pasteSink = el('textarea', { 'class': 'dose-grid-paste-sink', 'aria-hidden': 'true', tabindex: '-1',
                                      'data-shiny-no-bind-input': '' });
    this.root.appendChild(this.pasteSink);
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
    var illicit = !!(info && info.illicit);
    drug.classList.toggle('dose-grid-illicit', illicit);
    // said, not only drawn red (colour is not the only signal)
    if (illicit) {
      drug.setAttribute('aria-description', 'illicit drug, a research model');
      drug.setAttribute('title', 'Illicit drug: a research model (Show illicit drugs)');
    } else {
      drug.removeAttribute('aria-description');
      drug.removeAttribute('title');
    }
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

  // A disabled cell (the Units list of a row with no drug) gives way to the
  // row's Drug cell, where the row is begun: Enter on the last dose's Units
  // goes there, ready for the next drug.  (Sending it to the Dose cell
  // instead, for the row menu's Tab, put the next drug typed in the Dose
  // cell; the menu's Tab passes over disabled cells by itself.)
  DoseGrid.prototype.focusCell = function(r, c) {
    var ctl = this.control(r, c);
    if (ctl && ctl.disabled) ctl = this.control(r, COLS.DRUG);
    if (!ctl) return;
    ctl.focus();
    if (ctl.tagName === 'INPUT') ctl.select();
  };

  // The control of cell k, counting cells row by row
  DoseGrid.prototype.controlAt = function(k) {
    return this.control(Math.floor(k / NCOL), k % NCOL);
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
  // None once the table holds its maxRows doses: a dose typed there was cut
  // by the server (doseGridValue()) after it had been accepted on screen
  // (Gemini's production audit, 2026-11-08, finding 7).
  DoseGrid.prototype.ensureTrailingBlank = function() {
    if (this.filledRows() >= this.maxRows) {
      while (this.rows.length > 1 && isBlank(this.rows[this.rows.length - 1][COLS.DRUG])) this.rows.pop();
      return;
    }
    if (this.rows.length === 0 || !isBlank(this.rows[this.rows.length - 1][COLS.DRUG])) {
      this.rows.push(blankRow());
    }
  };

  DoseGrid.prototype.filledRows = function() {
    var n = 0;
    for (var i = 0; i < this.rows.length; i++) if (!isBlank(this.rows[i][COLS.DRUG])) n++;
    return n;
  };

  // --- committing --------------------------------------------------------------

  DoseGrid.prototype.send = function() {
    // the blank row below the last drug, drawn as soon as it is added (a
    // paste of 50 rows left none on screen while the draft had one)
    var n = this.rows.length;
    this.ensureTrailingBlank();
    if (this.rows.length !== n) this.renderAll();
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
    this.editing = null;
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
      // a new dose in a blank row, once the table holds its maxRows doses
      if (drug !== '' && isBlank(row[COLS.DRUG]) && this.filledRows() >= this.maxRows) {
        notify(['The table holds at most ' + this.maxRows + ' doses.'], 'doseGridRowLimit');
        ctl.value = '';
        return false;
      }
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

  DoseGrid.prototype.sendHistory = function(action) {
    this.historyCount = (this.historyCount || 0) + 1;
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(this.root.id + '_history', { action: action, n: this.historyCount }, { priority: 'event' });
    }
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

  // Ctrl+V on a Units cell: the paste arrives at the hidden field
  DoseGrid.prototype.catchPaste = function(at) {
    var self = this;
    this.sinkFor = at;
    this.pasteSink.value = '';
    this.pasteSink.focus();
    // if no paste comes (an empty clipboard), the focus goes back all the same
    setTimeout(function() { self.releasePaste(); }, 100);
  };

  DoseGrid.prototype.releasePaste = function() {
    var at = this.sinkFor;
    this.sinkFor = null;
    if (at && document.activeElement === this.pasteSink) {
      var ctl = this.control(at[0], at[1]);
      if (ctl) ctl.focus();
    }
  };

  DoseGrid.prototype.onSinkPaste = function(e) {
    var at = this.sinkFor;
    e.preventDefault();
    if (!at) return;
    var text = (e.clipboardData || window.clipboardData).getData('text');
    this.releasePaste();
    this.closeList();
    this.pasteBlock(at[0], at[1], parseTSV(text));
  };

  // A pasted row is taken whole or not at all.  Its drug, time and dose
  // belong together: a row whose drug is refused, or whose time or dose
  // cannot be read, leaves the table's row as it was, and is said.  A row
  // whose drug changes starts afresh, at time 0, dose 0 and the new drug's
  // default unit, before the cells pasted with it are read: until 2026-11-06
  // "fentanyl BADTIME BADOSE mcg" over "propofol 10 100 mg" gave fentanyl
  // 10 100 mcg, propofol's time and dose carried over to fentanyl (the
  // external review of that day, N02, reproduced on both grids).
  DoseGrid.prototype.pasteBlock = function(r0, c0, block) {
    var refusedEntries = [], refusedUnits = [], refusedDrugs = [];
    var dropped = 0;
    for (var i = 0; i < block.length; i++) {
      var r = r0 + i;
      if (r >= this.maxRows) { dropped = block.length - i; break; }
      while (this.rows.length <= r) this.rows.push(blankRow());
      var line = block[i];
      var row = this.rows[r].slice();
      var touched = {};
      for (var j = 0; j < line.length && c0 + j < NCOL; j++) touched[c0 + j] = line[j];
      var fresh = false;
      if (touched.hasOwnProperty(COLS.DRUG)) {
        var d = this.canonicalDrug(touched[COLS.DRUG], row[COLS.DRUG]);
        // a drug that is refused refuses its row ("notadrug 5 999 mg/kg"
        // over a propofol row made it propofol 999 mg/kg; the external
        // review of 2026-11-05, Gemini's script)
        if (d === null) { refusedDrugs.push(trimEntry(touched[COLS.DRUG])); continue; }
        // A pasted drug starts its row afresh, the same drug as before or
        // not: the row pasted is the row wanted.  Only a new drug did, so
        // "propofol<Tab><Tab>80" over a propofol row was refused for its
        // blank time where "fentanyl<Tab><Tab>80" was taken with time 0 (the
        // external review of 2026-11-06, Gemini's final pass).
        fresh = d !== '';
        row = fresh ? [d, '0', '0', ''] : blankRow();
      }
      if (isBlank(row[COLS.DRUG])) { this.rows[r] = blankRow(); continue; }
      var refusedHere = [];
      [COLS.TIME, COLS.DOSE].forEach(function(c) {
        // a cell the paste leaves blank in a row it gives a drug is 0, as a
        // drug chosen alone starts at time 0, dose 0
        if (!touched.hasOwnProperty(c)) {
          if (isBlank(row[c])) row[c] = '0';
          return;
        }
        var e = this.readEntry(c, touched[c], fresh ? '' : row[c], true);
        if (e.refused !== undefined) refusedHere.push({ col: c, entry: e.refused });
        else row[c] = (e.value === '') ? '0' : e.value;
      }, this);
      if (refusedHere.length) {
        Array.prototype.push.apply(refusedEntries, refusedHere);
        continue;
      }
      var asked;
      if (touched.hasOwnProperty(COLS.UNITS) && trimEntry(touched[COLS.UNITS]) !== '') {
        asked = trimEntry(touched[COLS.UNITS]);
        if (this.unitsOf(row[COLS.DRUG]).indexOf(asked) === -1) {
          refusedUnits.push({ drug: row[COLS.DRUG], unit: asked });
          asked = undefined;
        }
      }
      this.settleUnit(row, asked);
      this.rows[r] = row;
    }
    if (refusedDrugs.length) {
      notify(refusedDrugs.map(function(d) { return '"' + d + '" is not a drug in the library; its row was not pasted.'; }), 'doseGridDrugRefused');
    }
    if (refusedEntries.length) {
      notifyRefusedEntries(refusedEntries, COLS.TIME,
        'A pasted row with a time or dose that was not accepted was not pasted: its row is as it was.');
    }
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
      // maxRows doses and the blank row below them (doseGridValue()'s
      // MAX_DOSE_ROWS): counting that blank row against the doses refused an
      // insert at 498 (Gemini's production audit, 2026-11-08, finding 7)
      if (this.filledRows() >= this.maxRows || this.rows.length > this.maxRows) {
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

  DoseGrid.prototype.openMenu = function(r, x, y, c) {
    this.menuRow = r;
    this.menuCol = (c === undefined) ? COLS.DRUG : c;
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
      this.focusCell(this.menuRow, this.menuCol || 0);
    }
  };

  // A touch opens the row menu only by a long press.  A tap that selected a
  // cell's text made mobile Chromium send a contextmenu at once, which opened
  // the menu over the row (the external review of 2026-11-06, Gemini's final
  // pass).
  DoseGrid.prototype.onContextMenu = function(e) {
    var tr = e.target.closest && e.target.closest('tr[data-row]');
    if (!tr || !this.tbody.contains(tr)) return;
    var lp = this.lastPointer;
    if (lp && lp.type === 'touch' && Date.now() - lp.time < LONG_PRESS_MS) { e.preventDefault(); return; }
    e.preventDefault();
    // from the cell clicked, so Tab moves on from it (the external review of
    // 2026-11-06, T02: a right click always counted from the Drug cell)
    var td = e.target.closest('td');
    var c = td && tr.contains(td) ? Math.min(Math.max(td.cellIndex, 0), NCOL - 1) : COLS.DRUG;
    this.openMenu(parseInt(tr.getAttribute('data-row'), 10), e.clientX, e.clientY, c);
  };

  DoseGrid.prototype.onMenuKey = function(e) {
    var items = Array.prototype.slice.call(this.menu.querySelectorAll('button'));
    var i = items.indexOf(document.activeElement);
    if (e.key === 'ArrowDown') { items[(i + 1) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'ArrowUp') { items[(i - 1 + items.length) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'Home') { items[0].focus(); e.preventDefault(); }
    else if (e.key === 'End') { items[items.length - 1].focus(); e.preventDefault(); }
    else if (e.key === 'Escape') { e.preventDefault(); this.closeMenu(); }
    // Tab closes the menu and moves on from the cell it was opened from, as
    // Tab on that cell would: it took a second Tab (the external review of
    // 2026-11-06, N03).  From the first or last cell, Tab leaves the grid
    // from that cell, as it would without the menu (T03: it stayed put)
    // A disabled cell is passed over, as Tab passes it over without the menu
    // (the external review of 2026-11-06, Gemini's final pass: from the Dose
    // cell of a row with no drug, Tab went back to its Drug cell)
    else if (e.key === 'Tab') {
      var r = this.menuRow, c = this.menuCol || 0;
      var total = this.rows.length * NCOL, step = e.shiftKey ? -1 : 1;
      var k = r * NCOL + c + step;
      while (k >= 0 && k < total && this.controlAt(k) && this.controlAt(k).disabled) k += step;
      this.closeMenu();
      if (k >= 0 && k < total) {
        e.preventDefault();
        this.focusCell(Math.floor(k / NCOL), k % NCOL);
      } else {
        // the browser's Tab leaves the grid from its first or last cell
        var edge = step > 0 ? total - 1 : 0;
        while (this.controlAt(edge) && this.controlAt(edge).disabled) edge -= step;
        if (this.controlAt(edge)) this.controlAt(edge).focus();
      }
    }
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
    // the name typed, if it is one, first
    var exact = starts.filter(function(n) { return n.toLowerCase() === t; });
    starts = exact.concat(starts.filter(function(n) { return n.toLowerCase() !== t; }));
    return starts.concat(within).slice(0, 50);
  };

  // The option Enter, Tab or leaving the cell takes: the one the user moved
  // to with the arrow keys, or else the first match when what is typed is no
  // drug.  Typing "digoxin", ArrowDown to "digoxinIV" and Enter stored
  // digoxin, the text, not the drug chosen (the external review of
  // 2026-11-05, Gemini's script, reproduced).
  DoseGrid.prototype.chosenOption = function(t, current) {
    if (this.listboxFor !== t || this.listbox.hidden || this.active < 0) return null;
    if (!this.navigated && this.canonicalDrug(t.value, current) !== null) return null;
    if (!this.navigated && trimEntry(t.value) === '') return null;
    return this.listbox.children[this.active].textContent;
  };

  DoseGrid.prototype.openList = function(input) {
    var names = this.matches(input.value);
    this.listbox.textContent = '';
    this.active = -1;
    this.navigated = false;
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
    this.navigated = false;
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
    this.editing = null;
    if (e.target.tagName === 'INPUT') e.target.select();
  };

  // A cell is being edited (`editing`, its "row:col") once something is typed
  // in it, or after F2 or a double click.  Until then a click selects its
  // whole text, as a tab into it does, and what is typed replaces it, as in a
  // spreadsheet.  A click on a cell that already had the focus used to put
  // the caret where it landed, so a dose of 5 clicked again and typed as 10
  // became 510 or 105 (the external review of 2026-11-05, Gemini's script,
  // reproduced).  In a cell being edited a click places the caret.
  DoseGrid.prototype.cellKey = function(at) { return at[0] + ':' + at[1]; };

  // A click on a Units cell selects the cell, as a click on any cell does.
  // Its list opens from the arrow at its right, a double click, Alt+Down or
  // Space, as the old grid's does.  A click used to open the browser's own
  // list at once, which takes every key, so Ctrl+V on the cell just clicked
  // was lost without a word and the unit pasted was never used (the external
  // review of 2026-11-06, T01, reproduced in both Chromium builds).
  // A touch opens the list at once, as a phone's own list is the way to
  // choose there and a phone has no Ctrl+V to lose (the external review of
  // 2026-11-06, Gemini's final pass: a tap anywhere but on the arrow did
  // nothing).
  DoseGrid.prototype.onMouseDown = function(e) {
    var t = e.target;
    if (e.button !== 0 || t.tagName !== 'SELECT' || t.disabled || !this.cellOf(t)) return;
    var box = t.getBoundingClientRect();
    var lp = this.lastPointer;
    var touch = lp && lp.type !== 'mouse' && Date.now() - lp.time < 1000;
    if (touch || e.detail >= 2 || e.clientX >= box.right - UNIT_ARROW_WIDTH) {
      this.pickerFor = t;
      return;
    }
    e.preventDefault();
    t.focus();
  };

  // The left button only: a right click's mouseup selected the cell's text,
  // which in Firefox took the focus back from the row menu it had just
  // opened, so Escape no longer closed it (the external review of
  // 2026-11-06, Gemini's final pass)
  DoseGrid.prototype.onMouseUp = function(e) {
    if (e.button !== 0) return;
    var at = this.cellOf(e.target);
    if (!at || e.target.tagName !== 'INPUT' || this.editing === this.cellKey(at)) return;
    e.preventDefault();
    e.target.select();
  };

  DoseGrid.prototype.startEditing = function(t, at) {
    this.editing = this.cellKey(at);
    var n = t.value.length;
    t.setSelectionRange(n, n);
  };

  DoseGrid.prototype.onDoubleClick = function(e) {
    var at = this.cellOf(e.target);
    if (at && e.target.tagName === 'INPUT') this.startEditing(e.target, at);
  };

  DoseGrid.prototype.onFocusOut = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    if (e.target.tagName === 'INPUT') {
      var picked = at[1] === COLS.DRUG ? this.chosenOption(e.target, this.rows[at[0]][at[1]]) : null;
      if (picked !== null) e.target.value = picked;
      if (e.target.value !== this.rows[at[0]][at[1]]) this.commit(at[0], at[1]);
    }
    if (e.target.tagName === 'SELECT') {
      this.pickerFor = null;
      this.flushUnitSearch();
    }
    this.editing = null;
    if (this.listboxFor === e.target) this.closeList();
  };

  DoseGrid.prototype.onInput = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    this.editing = this.cellKey(at);
    this.enableApply();
    if (at[1] === COLS.DRUG) this.openList(e.target);
  };

  DoseGrid.prototype.onChange = function(e) {
    var at = this.cellOf(e.target);
    if (at && at[1] === COLS.UNITS) {
      this.pickerFor = null;
      this.commit(at[0], at[1]);
    }
  };

  // The Apply button (data-apply) is enabled as soon as a cell is typed in.
  // The server enables it only once it has the edit, which is when the cell
  // is committed: a dose typed and then Apply clicked at once committed the
  // dose as the focus left, but the click had found the button still
  // disabled, so nothing was applied and nothing said (the external review
  // of 2026-11-06, Gemini's final pass).  The server reads the edit before
  // the click (its Apply observer runs after the grid's).
  DoseGrid.prototype.enableApply = function() {
    var b = this.applyId ? document.getElementById(this.applyId) : null;
    if (b && b.disabled) { b.disabled = false; b.classList.remove('disabled'); }
  };

  // A unit typed on a Units cell.  The browser's own search of a list takes
  // the next option that starts with what was typed, so "mg" on oxycodone
  // was "mg PO liquid" and on fentanyl "mcg/kg", without a word (the
  // external review of 2026-11-06, Gemini's final pass, reproduced).  Here
  // an option equal to what was typed comes first, then the first that starts
  // with it, in the list's order; letters that match no unit are said, and
  // the unit is left as it was.  The choice is committed when the typing
  // pauses, or at once on Enter, Tab or leaving the cell.
  DoseGrid.prototype.unitSearch = function(t, at, key) {
    var now = Date.now();
    if (!this.search || this.search.ctl !== t || now - this.search.time > UNIT_SEARCH_MS) {
      this.flushUnitSearch();
      this.search = { ctl: t, at: at, text: '', time: now, said: false };
    }
    var s = this.search;
    s.text += key;
    s.time = now;
    var want = s.text.toLowerCase(), opts = Array.prototype.map.call(t.options, function(o) { return o.value; });
    var pick = null;
    for (var i = 0; i < opts.length && pick === null; i++) if (opts[i].toLowerCase() === want) pick = opts[i];
    for (var j = 0; j < opts.length && pick === null; j++) if (opts[j] !== '' && opts[j].toLowerCase().indexOf(want) === 0) pick = opts[j];
    if (pick === null) {
      if (!s.said) {
        s.said = true;
        notifyRefusedUnits([{ drug: this.rows[at[0]][COLS.DRUG], unit: s.text }], 'the unit was not changed.');
      }
      // and it is not: the letters before may have moved the list ("m" to
      // "mcg" on a row in "mcg/kg"), which the pause then committed under
      // the message that nothing changed (Gemini's production audit,
      // 2026-11-08, finding 1)
      var kept = this.rows[at[0]] ? this.rows[at[0]][at[1]] : '';
      t.value = kept;
      t.parentNode.firstChild.textContent = unitWrapText(kept);
    } else {
      t.value = pick;
      t.parentNode.firstChild.textContent = unitWrapText(pick);
    }
    var self = this;
    clearTimeout(s.timer);
    s.timer = setTimeout(function() { self.flushUnitSearch(); }, UNIT_SEARCH_MS);
  };

  DoseGrid.prototype.flushUnitSearch = function() {
    var s = this.search;
    if (!s) return;
    clearTimeout(s.timer);
    this.search = null;
    var at = this.cellOf(s.ctl) || s.at;
    if (at && this.rows[at[0]] && s.ctl.value !== this.rows[at[0]][at[1]]) this.commit(at[0], at[1]);
  };

  DoseGrid.prototype.onKeyDown = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var r = at[0], c = at[1], t = e.target;
    var listOpen = this.listboxFor === t && !this.listbox.hidden;
    var edited = t.tagName === 'INPUT' && t.value !== this.rows[r][c];
    var editing = t.tagName === 'INPUT' && (edited || this.editing === this.cellKey(at));

    // Ctrl+V (Cmd+V) on a Units cell: see catchPaste()
    if ((e.ctrlKey || e.metaKey) && !e.altKey && (e.key === 'v' || e.key === 'V') && t.tagName === 'SELECT') {
      this.catchPaste(at);
      return;
    }
    // F2: edit the cell, the caret at the end of its text
    if (e.key === 'F2' && t.tagName === 'INPUT') {
      e.preventDefault();
      this.startEditing(t, at);
      return;
    }
    // Shift+F10 and the context-menu key: the row's actions
    if ((e.key === 'F10' && e.shiftKey) || e.key === 'ContextMenu') {
      e.preventDefault();
      var box = t.getBoundingClientRect();
      this.openMenu(r, box.left, box.bottom, c);
      return;
    }
    // Ctrl+Z (Cmd+Z) undoes the last edit, Ctrl+Y or Ctrl+Shift+Z redoes
    // it, through the dose table's own history (doseTableHistory()), as the
    // Undo and Redo buttons do.  While a cell is being typed in, the keys
    // are the cell's own: they undo the typing, and redo it.  That is the
    // cell's editing, not whether its text differs from the table: once
    // Ctrl+Z had put the text back, Ctrl+Y went to the table, which had
    // nothing to redo, and the typing was lost (the external review of
    // 2026-11-06, N01).  The request is an input of
    // its own, not a click on the button: the button is enabled only once
    // the server has the edit, so a Ctrl+Z pressed right after Enter on a
    // slow link would have been dropped, where the request goes in order
    // right behind the edit it undoes.
    if ((e.ctrlKey || e.metaKey) && !e.altKey && !editing &&
        (e.key === 'z' || e.key === 'Z' || e.key === 'y' || e.key === 'Y')) {
      var redo = e.key === 'y' || e.key === 'Y' || e.shiftKey;
      this.sendHistory(redo ? 'redo' : 'undo');
      e.preventDefault();
      return;
    }
    if (listOpen && (e.key === 'ArrowDown' || e.key === 'ArrowUp')) {
      this.setActive(this.active + (e.key === 'ArrowDown' ? 1 : -1));
      this.navigated = true;
      e.preventDefault();
      return;
    }
    if (e.key === 'ArrowDown' && e.altKey && c === COLS.DRUG) { this.openList(t); e.preventDefault(); return; }
    if (t.tagName === 'SELECT') {
      // a space while a unit is being typed is part of it ("mg PO liquid")
      var searching = this.search && this.search.ctl === t && Date.now() - this.search.time <= UNIT_SEARCH_MS;
      // the browser's own list, opened: its keys are its own
      if ((e.key === 'ArrowDown' && e.altKey) || (e.key === ' ' && !searching) || e.key === 'F4') { this.pickerFor = t; return; }
      if (this.pickerFor === t) {
        if (e.key === 'Escape' || e.key === 'Enter' || e.key === 'Tab') this.pickerFor = null;
        else return;
      } else if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        e.preventDefault();
        this.unitSearch(t, at, e.key);
        return;
      }
    }
    if (e.key === 'Escape') {
      // the row menu, if open, closes first (it may not have the focus)
      if (!this.menu.hidden) { this.closeMenu(); e.preventDefault(); return; }
      if (listOpen) { this.closeList(); e.preventDefault(); return; }
      if (t.tagName === 'INPUT') { t.value = this.rows[r][c]; t.select(); e.preventDefault(); }
      this.editing = null;
      return;
    }
    if (e.key === 'Enter') {
      e.preventDefault();
      var chosen = this.chosenOption(t, this.rows[r][c]);
      if (chosen !== null) t.value = chosen;
      this.closeList();
      if (t.tagName === 'INPUT') this.commit(r, c);
      else this.flushUnitSearch();
      var next = e.shiftKey ? r - 1 : r + 1;
      if (next >= 0 && next < this.rows.length) this.focusCell(next, c);
      return;
    }
    if (e.key === 'Tab' && listOpen) {
      var picked = this.chosenOption(t, this.rows[r][c]);
      if (picked !== null) t.value = picked;
      this.closeList();
      return;   // committed as the focus leaves
    }
    // ArrowUp and ArrowDown move between rows in every column.  On a Units
    // list (a <select>) they would change the unit instead, so a keyboard
    // user going down the column changed every row's unit (the external
    // review of 2026-11-05, Gemini's script); the list opens with Alt+Down
    // or Space, or a unit is typed.
    if ((e.key === 'ArrowDown' || e.key === 'ArrowUp') && !e.altKey &&
        (t.tagName === 'INPUT' || t.tagName === 'SELECT')) {
      var to = r + (e.key === 'ArrowDown' ? 1 : -1);
      if (to >= 0 && to < this.rows.length) { e.preventDefault(); this.focusCell(to, c); }
      return;
    }
    // Home and End go to the row's first and last cell, as in the old grid,
    // unless the cell is being typed in, when they move the caret; with Ctrl
    // (Cmd), to the first and last row
    if ((e.key === 'Home' || e.key === 'End') && !e.shiftKey && !e.altKey && !editing) {
      e.preventDefault();
      var end = e.key === 'End';
      if (e.ctrlKey || e.metaKey) this.focusCell(end ? this.rows.length - 1 : 0, c);
      else this.focusCell(r, end ? COLS.UNITS : COLS.DRUG);
      return;
    }
    // No key is filtered out of a Time or Dose cell.  The rhandsontable grid
    // dropped every key a number cannot hold (hookFilterKeys() in
    // grid-funs.js), which changed the number without a word: "1,5" became
    // 15, "1e3" 13, "1a0" 10 and an elapsed "1:30" 130 (the external review
    // of 2026-11-05, reproduced).  Everything typed is read whole when the
    // cell is committed, as a paste is, and an entry that cannot be read is
    // refused with a message (readEntry(), commit()).
  };

  // Ctrl+Z / Ctrl+Y with the focus outside the grid, on no text field (a
  // button, the plot, the page): the dose table's undo and redo, not the
  // browser's.  Left to the browser, Ctrl+Z on the Apply button undid the
  // typing of the last cell edited, so the grid showed one value and the
  // draft held another (the external review of 2026-11-05, reproduced).
  // A text field elsewhere keeps its own undo; a hidden grid does nothing.
  DoseGrid.prototype.onDocumentKey = function(e) {
    if (e.defaultPrevented || !(e.ctrlKey || e.metaKey) || e.altKey) return;
    if (!(e.key === 'z' || e.key === 'Z' || e.key === 'y' || e.key === 'Y')) return;
    if (this.root.offsetParent === null) return;
    var t = e.target;
    if (t && t.closest && t.closest('.stanpumpr-dose-grid')) return;
    if (t && (t.isContentEditable || /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName))) return;
    e.preventDefault();
    this.sendHistory(e.key === 'y' || e.key === 'Y' || e.shiftKey ? 'redo' : 'undo');
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

  // for test-dose-grid.R, which runs this file in Node, and for table-grid.js
  if (typeof module !== 'undefined' && module.exports) module.exports = { parseTSV: parseTSV };
  if (typeof window !== 'undefined') window.stanpumprGridKit = { parseTSV: parseTSV };
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
