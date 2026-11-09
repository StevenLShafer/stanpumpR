// The table grid: the dialogs' grids (Edit doses, Edit events, Suggest
// Dosing's targets, Drug thresholds, the Drug Library, Single Paper Review's
// doses) as a plain HTML table of form controls with its own Shiny input
// binding, in place of rhandsontable (docs/grid-replacement.md).  R/table-grid.R
// is its server half: tableGridInput() draws it from a column specification,
// tableGridRead() reads what it sends.
//
// It is the main dose grid's younger sibling (dose-grid.js), with the same
// keys and the same rules, and without what only the dose table needs (the
// drug autocomplete, the server's draft and its undo history).  A dialog's
// grid is read when its Apply or OK is pressed; Cancel is its undo.
//
// Column types: text, time, dose, number, checkbox, select (a list of
// choices, the same for every row or one list per row) and readonly.  A time
// is read as validateTime() reads it, a dose or a number as validateDose()
// does (grid-funs.js, held to R's by the tests); an entry that cannot be read
// is refused where it is typed, with a message, and the cell keeps its value.
// A pasted row is taken whole or not at all.
//
// Everything a cell shows is set with .value or .textContent, never as HTML.
// The cell controls have no id or name, and carry data-shiny-no-bind-input, so
// Shiny binds none of them as inputs of their own.

(function() {
  'use strict';

  var ARROW_WIDTH = 20;      // the arrow at the right of a list cell (app.css)
  var LONG_PRESS_MS = 450;   // a touch held this long opens the row menu
  var SEARCH_MS = 1000;      // letters typed on a list cell this close are one search
  var gridCount = 0;

  function isBlank(x) { return x === null || x === undefined || String(x).trim() === ''; }
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
  function parseTSV(text) {
    return window.stanpumprGridKit.parseTSV(text);
  }
  // A checkbox from a pasted or sent value
  function readTick(x) {
    if (x === true || x === false) return { value: x };
    var t = String(x === null || x === undefined ? '' : x).trim().toLowerCase();
    if (t === '' || t === 'false' || t === '0' || t === 'no') return { value: false };
    if (t === 'true' || t === '1' || t === 'yes' || t === 'x') return { value: true };
    return { refused: String(x).trim() };
  }

  function TableGrid(root) {
    var self = this;
    this.root = root;
    this.id = String(++gridCount);
    var spec = {};
    try { spec = JSON.parse(root.getAttribute('data-spec') || '{}'); } catch (e) { spec = {}; }
    this.cols = [].concat(spec.columns || []);
    this.ncol = this.cols.length;
    this.tag = spec.tag === undefined ? null : spec.tag;
    this.rowMenu = !!spec.rowMenu;
    this.label = typeof spec.label === 'string' && spec.label ? spec.label : 'Table';
    this.maxRows = spec.maxRows || 500;
    this.rows = [].concat(spec.rows || []).map(function(r) { return self.normalRow(r); });
    if (this.rows.length === 0 && this.rowMenu) this.rows.push(this.blankRow());
    this.editing = null;
    this.pickerFor = null;
    this.search = null;
    this.callback = null;
    this.seq = 0;
    this.build();
    this.renderAll();
    this.value = this.snapshot();

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
    // Ctrl+V on a list or a checkbox: no paste event reaches either, so the
    // focus visits a hidden text field, which gets the paste, and comes back
    this.pasteSink = el('textarea', { 'class': 'dose-grid-paste-sink', 'aria-hidden': 'true', tabindex: '-1', 'data-shiny-no-bind-input': '' });
    root.appendChild(this.pasteSink);
    this.pasteSink.addEventListener('paste', function(e) { self.onSinkPaste(e); });
    this.onDocMouseDown = function(e) { if (self.menu && !self.menu.contains(e.target)) self.closeMenu(); };
    document.addEventListener('mousedown', this.onDocMouseDown);
  }

  TableGrid.prototype.blankRow = function() {
    return this.cols.map(function(c) { return c.type === 'checkbox' ? false : ''; });
  };

  TableGrid.prototype.normalRow = function(r) {
    r = [].concat(r || []);
    return this.cols.map(function(col, j) {
      var v = r[j];
      if (col.type === 'checkbox') return readTick(v).value === true;
      return v === null || v === undefined ? '' : String(v);
    });
  };

  TableGrid.prototype.build = function() {
    var self = this;
    var p = 'tablegrid' + this.id;
    this.root.textContent = '';
    var scroll = el('div', { 'class': 'dose-grid-scroll table-grid-scroll' });
    // Named by its caption, and told the keys, as the dose grid is
    // (ChatGPT's fourth review, F06)
    var table = el('table', { 'class': 'dose-grid table-grid', role: 'grid', 'aria-describedby': p + '-help' });
    table.appendChild(el('caption', { 'class': 'visually-hidden' }, this.label));
    var help = el('p', { id: p + '-help', 'class': 'visually-hidden' },
      'Enter or Tab keeps an entry and moves on; Escape puts it back; F2 edits at the caret. ' +
      'In a list cell, type to choose, or press Alt+Down or Space for the list.' +
      (this.rowMenu ? ' For row actions (insert, remove), press Shift+F10 or right-click a row.' : ''));
    var colgroup = el('colgroup');
    var tr = el('tr');
    this.cols.forEach(function(col, j) {
      var c = el('col');
      if (col.width) c.setAttribute('style', 'width:' + col.width + 'px');
      colgroup.appendChild(c);
      var th = el('th', { scope: 'col', id: p + '-h' + j }, col.label || col.name);
      if (col.type === 'time' && typeof timeHeaderTipText === 'function') th.setAttribute('title', timeHeaderTipText());
      if (col.tip) th.setAttribute('title', col.tip);
      tr.appendChild(th);
    });
    table.appendChild(colgroup);
    var thead = el('thead');
    thead.appendChild(tr);
    table.appendChild(thead);
    this.tbody = el('tbody');
    table.appendChild(this.tbody);
    scroll.appendChild(table);
    this.root.appendChild(help);
    this.root.appendChild(scroll);
    if (this.rowMenu) {
      this.menu = el('ul', { role: 'menu', 'class': 'dose-grid-menu', hidden: '' });
      [['above', 'Insert row above'], ['below', 'Insert row below'], ['remove', 'Remove row']].forEach(function(a) {
        var li = el('li', { role: 'none' });
        var b = el('button', { type: 'button', role: 'menuitem', 'class': 'dose-grid-menu-item', 'data-action': a[0], tabindex: '-1' }, a[1]);
        li.appendChild(b);
        self.menu.appendChild(li);
      });
      this.menu.addEventListener('click', function(e) {
        var b = e.target.closest('button');
        if (b) self.rowAction(self.menuRow, b.getAttribute('data-action'));
      });
      this.menu.addEventListener('keydown', function(e) { self.onMenuKey(e); });
      this.root.appendChild(this.menu);
    }
    this.status = el('div', { 'class': 'dose-grid-status', role: 'status', 'aria-live': 'polite' });
    this.root.appendChild(this.status);
  };

  // --- rendering ----------------------------------------------------------------------

  TableGrid.prototype.optionsFor = function(r, c) {
    var col = this.cols[c];
    if (col.rowOptions) return [].concat(col.rowOptions[r] || []);
    return [].concat(col.options || []);
  };

  TableGrid.prototype.readOnly = function(r, c) {
    var col = this.cols[c];
    return col.type === 'readonly' || (col.readonlyRows || []).indexOf(r) !== -1;
  };

  TableGrid.prototype.cellLabel = function(c, r) {
    return (this.cols[c].label || this.cols[c].name) + ', row ' + (r + 1);
  };

  TableGrid.prototype.makeRow = function() {
    var tr = el('tr');
    for (var c = 0; c < this.ncol; c++) {
      var col = this.cols[c];
      var td = el('td', { 'class': 'table-grid-' + col.type });
      var ctl;
      if (col.type === 'checkbox') {
        ctl = el('input', { type: 'checkbox', 'data-col': String(c), 'data-shiny-no-bind-input': '' });
      } else if (col.type === 'select') {
        td.className += ' dose-grid-col3';
        td.appendChild(el('span', { 'class': 'dose-grid-unit-text', 'aria-hidden': 'true' }));
        ctl = el('select', { 'data-col': String(c), 'data-shiny-no-bind-input': '' });
      } else {
        ctl = el('input', { type: 'text', autocomplete: 'off', spellcheck: 'false', 'data-col': String(c), 'data-shiny-no-bind-input': '' });
        if (col.type === 'time' || col.type === 'dose' || col.type === 'number') ctl.setAttribute('inputmode', 'decimal');
      }
      td.appendChild(ctl);
      tr.appendChild(td);
    }
    return tr;
  };

  TableGrid.prototype.renderRow = function(r) {
    var tr = this.tbody.children[r];
    var row = this.rows[r];
    for (var c = 0; c < this.ncol; c++) {
      var col = this.cols[c], td = tr.children[c], ctl = td.lastChild, ro = this.readOnly(r, c);
      if (col.type === 'checkbox') {
        ctl.checked = row[c] === true;
        ctl.disabled = ro;
      } else if (col.type === 'select') {
        var list = this.optionsFor(r, c);
        // a value the list lacks is still shown, never silently replaced
        if (row[c] !== '' && list.indexOf(row[c]) === -1) list.unshift(row[c]);
        if (list.indexOf('') === -1 && row[c] === '') list.unshift('');
        if (ctl.getAttribute('data-options') !== list.join('\n')) {
          ctl.textContent = '';
          list.forEach(function(u) { var o = document.createElement('option'); o.value = u; o.textContent = u; ctl.appendChild(o); });
          ctl.setAttribute('data-options', list.join('\n'));
        }
        ctl.value = row[c];
        ctl.disabled = ro;
        td.firstChild.textContent = col.wrapUnits && typeof unitWrapText === 'function' ? unitWrapText(row[c]) : row[c];
      } else {
        if (document.activeElement !== ctl || ctl.value === this.shown(r, c)) ctl.value = row[c];
        ctl.readOnly = ro;
      }
      ctl.setAttribute('aria-label', this.cellLabel(c, r));
    }
    tr.setAttribute('data-row', String(r));
  };

  TableGrid.prototype.shown = function(r, c) { return this.rows[r] ? String(this.rows[r][c]) : ''; };

  TableGrid.prototype.renderAll = function() {
    while (this.tbody.children.length < this.rows.length) this.tbody.appendChild(this.makeRow());
    while (this.tbody.children.length > this.rows.length) this.tbody.removeChild(this.tbody.lastChild);
    for (var r = 0; r < this.rows.length; r++) this.renderRow(r);
  };

  TableGrid.prototype.control = function(r, c) {
    var tr = this.tbody.children[r];
    return tr && tr.children[c] ? tr.children[c].lastChild : null;
  };

  TableGrid.prototype.cellOf = function(node) {
    if (!node || !node.getAttribute || node.getAttribute('data-col') === null) return null;
    var tr = node.closest('tr[data-row]');
    if (!tr || !this.tbody.contains(tr)) return null;
    return [parseInt(tr.getAttribute('data-row'), 10), parseInt(node.getAttribute('data-col'), 10)];
  };

  TableGrid.prototype.cellKey = function(at) { return at[0] + ':' + at[1]; };

  // A disabled cell gives way to the nearest cell on its left that is not
  TableGrid.prototype.focusCell = function(r, c) {
    var ctl = this.control(r, c);
    while (ctl && ctl.disabled && c > 0) ctl = this.control(r, --c);
    if (!ctl) return;
    ctl.focus();
    if (ctl.tagName === 'INPUT' && ctl.type === 'text') ctl.select();
  };

  // --- reading and committing ------------------------------------------------------------

  // A typed or pasted entry for column c: { value } or { refused, reason }
  TableGrid.prototype.readEntry = function(r, c, text) {
    var col = this.cols[c];
    var t = trimEntry(text);
    if (col.type === 'checkbox') {
      var tick = readTick(text);
      return tick.refused !== undefined ? { refused: tick.refused, reason: 'tick or untick it' } : tick;
    }
    if (col.type === 'select') {
      if (t === '') return { value: '' };
      var list = this.optionsFor(r, c);
      for (var i = 0; i < list.length; i++) if (list[i] === t) return { value: list[i] };
      for (var k = 0; k < list.length; k++) if (list[k].toLowerCase() === t.toLowerCase()) return { value: list[k] };
      return { refused: t, reason: 'it is not one of the choices' };
    }
    if (t === '') return { value: '' };
    if (col.type === 'time') {
      var v = validateTime(text);
      if (v === '') {
        return { refused: t, reason: (typeof getTimeMode === 'function' && getTimeMode() === 'clock') ?
                 'enter a clock time as hh:mm' : 'enter a time as a number' };
      }
      return { value: String(v) };
    }
    if (col.type === 'dose' || col.type === 'number') {
      var d = validateDose(text);
      if (d === '') return { refused: t, reason: 'enter a number from 0 to 1,000,000,000' };
      return { value: String(d) };
    }
    return { value: String(text) };
  };

  TableGrid.prototype.refusalLine = function(c, e) {
    return '"' + e.refused + '" was not accepted in ' + (this.cols[c].label || this.cols[c].name) + ': ' + e.reason + '.';
  };

  // The control's text committed to its cell; true if the cell changed
  TableGrid.prototype.commit = function(r, c) {
    this.editing = null;
    var ctl = this.control(r, c);
    if (!ctl || !this.rows[r] || this.readOnly(r, c)) return false;
    var col = this.cols[c];
    var text = col.type === 'checkbox' ? ctl.checked : ctl.value;
    var e = this.readEntry(r, c, text);
    var changed = e.refused === undefined && this.rows[r][c] !== e.value;
    if (e.refused !== undefined) {
      notify([this.refusalLine(c, e) + ' The cell keeps its previous value.'], 'tableGridRefused');
    } else if (changed) {
      this.rows[r][c] = e.value;
    }
    // The entry is done, so the control shows what the cell holds: the value
    // kept, or the value read ("1e3" as 1000).  renderRow() leaves a focused
    // control's text alone, and on the last row Enter keeps the focus, so a
    // refused entry stayed on screen over the value Apply would use
    // (CodeRabbit, PR #683; dose-grid.js does the same).
    if (col.type !== 'checkbox') ctl.value = this.shown(r, c);
    this.renderRow(r);
    if (changed) this.send();
    return changed;
  };

  TableGrid.prototype.snapshot = function() {
    return { rows: this.rows.map(function(r) { return r.slice(); }), tag: this.tag, seq: this.seq };
  };

  TableGrid.prototype.send = function() {
    this.seq += 1;
    this.value = this.snapshot();
    if (this.callback) this.callback(false);
  };

  // --- paste --------------------------------------------------------------------------

  TableGrid.prototype.onPaste = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var text = (e.clipboardData || window.clipboardData).getData('text');
    var block = parseTSV(text);
    var single = block.length === 1 && block[0].length === 1;
    var ctl = e.target;
    // one value pasted into a cell being typed in goes in at the caret
    if (single && ctl.tagName === 'INPUT' && ctl.type === 'text' && ctl.value !== this.shown(at[0], at[1])) return;
    e.preventDefault();
    this.pasteBlock(at[0], at[1], block);
  };

  TableGrid.prototype.catchPaste = function(at) {
    var self = this;
    this.sinkFor = at;
    this.pasteSink.value = '';
    this.pasteSink.focus();
    setTimeout(function() { self.releasePaste(); }, 100);
  };

  TableGrid.prototype.releasePaste = function() {
    var at = this.sinkFor;
    this.sinkFor = null;
    if (at && document.activeElement === this.pasteSink) {
      var ctl = this.control(at[0], at[1]);
      if (ctl) ctl.focus();
    }
  };

  TableGrid.prototype.onSinkPaste = function(e) {
    var at = this.sinkFor;
    e.preventDefault();
    if (!at) return;
    var text = (e.clipboardData || window.clipboardData).getData('text');
    this.releasePaste();
    this.pasteBlock(at[0], at[1], parseTSV(text));
  };

  // A pasted row is taken whole or not at all: one with a cell that cannot be
  // read leaves the table's row as it was, and is said.  A read-only cell
  // keeps its value.  Rows past the last are added where the grid can add
  // rows (Suggest Dosing's targets), and otherwise refused and said.
  TableGrid.prototype.pasteBlock = function(r0, c0, block) {
    var refused = [], dropped = 0;
    for (var i = 0; i < block.length; i++) {
      var r = r0 + i;
      if (r >= this.rows.length) {
        if (!this.rowMenu || r >= this.maxRows) { dropped = block.length - i; break; }
        while (this.rows.length <= r) this.rows.push(this.blankRow());
      }
      var row = this.rows[r].slice(), bad = [];
      for (var j = 0; j < block[i].length && c0 + j < this.ncol; j++) {
        var c = c0 + j;
        if (this.readOnly(r, c)) continue;
        var e = this.readEntry(r, c, block[i][j]);
        if (e.refused !== undefined) bad.push(this.refusalLine(c, e));
        else row[c] = e.value;
      }
      if (bad.length) { refused = refused.concat(bad); continue; }
      this.rows[r] = row;
    }
    var lines = refused.slice();
    if (refused.length) lines.push('A pasted row with an entry that was not accepted was not pasted: its row is as it was.');
    if (dropped > 0) {
      lines.push(dropped + ' pasted row' + (dropped === 1 ? ' was' : 's were') + ' not added: the table has no more rows.');
    }
    notify(lines, 'tableGridPaste');
    this.renderAll();
    this.send();
  };

  // --- rows ---------------------------------------------------------------------------

  TableGrid.prototype.rowAction = function(r, action) {
    this.closeMenu();
    if (r === null || r === undefined || !this.rows[r]) return;
    if (action === 'above' || action === 'below') {
      if (this.rows.length >= this.maxRows) {
        notify(['The table holds at most ' + this.maxRows + ' rows.'], 'tableGridRowLimit');
        return;
      }
      var at = action === 'above' ? r : r + 1;
      this.rows.splice(at, 0, this.blankRow());
      r = at;
    } else if (action === 'remove') {
      this.rows.splice(r, 1);
      if (this.rows.length === 0) this.rows.push(this.blankRow());
      r = Math.min(r, this.rows.length - 1);
    }
    this.renderAll();
    this.send();
    this.focusCell(r, 0);
  };

  TableGrid.prototype.openMenu = function(r, x, y, c) {
    if (!this.menu) return;
    this.menuRow = r;
    this.menuCol = c || 0;
    this.menu.hidden = false;
    var box = this.root.getBoundingClientRect();
    this.menu.style.left = Math.max(0, x - box.left) + 'px';
    this.menu.style.top = Math.max(0, y - box.top) + 'px';
    var first = this.menu.querySelector('button');
    if (first) first.focus();
  };

  TableGrid.prototype.closeMenu = function() {
    if (!this.menu || this.menu.hidden) return;
    this.menu.hidden = true;
    if (this.menu.contains(document.activeElement)) this.focusCell(this.menuRow, this.menuCol || 0);
  };

  TableGrid.prototype.onContextMenu = function(e) {
    if (!this.menu) return;
    var tr = e.target.closest && e.target.closest('tr[data-row]');
    if (!tr || !this.tbody.contains(tr)) return;
    var lp = this.lastPointer;
    if (lp && lp.type === 'touch' && Date.now() - lp.time < LONG_PRESS_MS) { e.preventDefault(); return; }
    e.preventDefault();
    var td = e.target.closest('td');
    var c = td && tr.contains(td) ? Math.min(Math.max(td.cellIndex, 0), this.ncol - 1) : 0;
    this.openMenu(parseInt(tr.getAttribute('data-row'), 10), e.clientX, e.clientY, c);
  };

  TableGrid.prototype.onMenuKey = function(e) {
    var items = Array.prototype.slice.call(this.menu.querySelectorAll('button'));
    var i = items.indexOf(document.activeElement);
    if (e.key === 'ArrowDown') { items[(i + 1) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'ArrowUp') { items[(i - 1 + items.length) % items.length].focus(); e.preventDefault(); }
    else if (e.key === 'Home') { items[0].focus(); e.preventDefault(); }
    else if (e.key === 'End') { items[items.length - 1].focus(); e.preventDefault(); }
    else if (e.key === 'Escape') { e.preventDefault(); this.closeMenu(); }
    else if (e.key === 'Tab') {
      var total = this.rows.length * this.ncol, step = e.shiftKey ? -1 : 1;
      var k = this.menuRow * this.ncol + (this.menuCol || 0) + step;
      var ctlAt = function(self, k) { return self.control(Math.floor(k / self.ncol), k % self.ncol); };
      while (k >= 0 && k < total && ctlAt(this, k) && ctlAt(this, k).disabled) k += step;
      this.closeMenu();
      if (k >= 0 && k < total) { e.preventDefault(); this.focusCell(Math.floor(k / this.ncol), k % this.ncol); }
    }
    e.stopPropagation();
  };

  // --- events -------------------------------------------------------------------------

  TableGrid.prototype.onFocusIn = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    this.editing = null;
    if (e.target.tagName === 'INPUT' && e.target.type === 'text') e.target.select();
  };

  // A list cell: a mouse click selects it, as any cell; its list opens from
  // its arrow, a double click, a touch, Alt+Down or Space (dose-grid.js says
  // why: an open list takes every key, Ctrl+V included)
  TableGrid.prototype.onMouseDown = function(e) {
    var t = e.target;
    if (e.button !== 0 || t.tagName !== 'SELECT' || t.disabled || !this.cellOf(t)) return;
    var box = t.getBoundingClientRect();
    var lp = this.lastPointer;
    var touch = lp && lp.type !== 'mouse' && Date.now() - lp.time < 1000;
    if (touch || e.detail >= 2 || e.clientX >= box.right - ARROW_WIDTH) { this.pickerFor = t; return; }
    e.preventDefault();
    t.focus();
  };

  // A click selects a cell's whole text, unless it is being edited
  TableGrid.prototype.onMouseUp = function(e) {
    if (e.button !== 0) return;
    var at = this.cellOf(e.target);
    if (!at || e.target.tagName !== 'INPUT' || e.target.type !== 'text' || this.editing === this.cellKey(at)) return;
    e.preventDefault();
    e.target.select();
  };

  TableGrid.prototype.startEditing = function(t, at) {
    this.editing = this.cellKey(at);
    var n = t.value.length;
    t.setSelectionRange(n, n);
  };

  TableGrid.prototype.onDoubleClick = function(e) {
    var at = this.cellOf(e.target);
    if (at && e.target.tagName === 'INPUT' && e.target.type === 'text' && !e.target.readOnly) this.startEditing(e.target, at);
  };

  TableGrid.prototype.onFocusOut = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var t = e.target;
    if (t.tagName === 'INPUT' && t.type === 'text' && t.value !== this.shown(at[0], at[1])) this.commit(at[0], at[1]);
    if (t.tagName === 'SELECT') { this.pickerFor = null; this.flushSearch(); }
    this.editing = null;
  };

  TableGrid.prototype.onInput = function(e) {
    var at = this.cellOf(e.target);
    if (at && e.target.type === 'text') this.editing = this.cellKey(at);
  };

  TableGrid.prototype.onChange = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    if (e.target.tagName === 'SELECT') { this.pickerFor = null; this.commit(at[0], at[1]); }
    else if (e.target.type === 'checkbox') this.commit(at[0], at[1]);
  };

  // A choice typed on a list cell: an option equal to what was typed first,
  // then the first that starts with it; letters that match none are said,
  // and the choice is kept.  Committed when the typing pauses, or on Enter,
  // Tab or leaving the cell.
  TableGrid.prototype.listSearch = function(t, at, key) {
    var now = Date.now();
    if (!this.search || this.search.ctl !== t || now - this.search.time > SEARCH_MS) {
      this.flushSearch();
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
        var col = this.cols[at[1]];
        notify(['"' + s.text + '" is not one of the choices in ' + (col.label || col.name) + '; it was not changed.'], 'tableGridRefused');
      }
      // and it is not: the letters before may have moved the list, which the
      // pause then committed (Gemini's production audit, 2026-11-08, finding 1)
      var kept = this.shown(at[0], at[1]), col3 = this.cols[at[1]];
      t.value = kept;
      t.parentNode.firstChild.textContent = col3.wrapUnits && typeof unitWrapText === 'function' ? unitWrapText(kept) : kept;
    } else {
      t.value = pick;
      var col2 = this.cols[at[1]];
      t.parentNode.firstChild.textContent = col2.wrapUnits && typeof unitWrapText === 'function' ? unitWrapText(pick) : pick;
    }
    var self = this;
    clearTimeout(s.timer);
    s.timer = setTimeout(function() { self.flushSearch(); }, SEARCH_MS);
  };

  TableGrid.prototype.flushSearch = function() {
    var s = this.search;
    if (!s) return;
    clearTimeout(s.timer);
    this.search = null;
    var at = this.cellOf(s.ctl) || s.at;
    if (at && this.rows[at[0]] && s.ctl.value !== this.shown(at[0], at[1])) this.commit(at[0], at[1]);
  };

  TableGrid.prototype.onKeyDown = function(e) {
    var at = this.cellOf(e.target);
    if (!at) return;
    var r = at[0], c = at[1], t = e.target;
    var text = t.tagName === 'INPUT' && t.type === 'text';
    var editing = text && (t.value !== this.shown(r, c) || this.editing === this.cellKey(at));

    // Ctrl+V on a list or a checkbox: see catchPaste()
    if ((e.ctrlKey || e.metaKey) && !e.altKey && (e.key === 'v' || e.key === 'V') && !text) {
      this.catchPaste(at);
      return;
    }
    // not a read-only cell, which a double click does not edit either (F2 on
    // one held the row's Home/End and swallowed Escape: Gemini's production
    // audit, 2026-11-08, finding 5)
    if (e.key === 'F2' && text && !t.readOnly) { e.preventDefault(); this.startEditing(t, at); return; }
    if (this.menu && ((e.key === 'F10' && e.shiftKey) || e.key === 'ContextMenu')) {
      e.preventDefault();
      var box = t.getBoundingClientRect();
      this.openMenu(r, box.left, box.bottom, c);
      return;
    }
    if (t.tagName === 'SELECT') {
      // a space while a choice is being typed is part of it
      var searching = this.search && this.search.ctl === t && Date.now() - this.search.time <= SEARCH_MS;
      if ((e.key === 'ArrowDown' && e.altKey) || (e.key === ' ' && !searching) || e.key === 'F4') { this.pickerFor = t; return; }
      if (this.pickerFor === t) {
        if (e.key === 'Escape' || e.key === 'Enter' || e.key === 'Tab') this.pickerFor = null;
        else return;
      } else if (e.key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        e.preventDefault();
        this.listSearch(t, at, e.key);
        return;
      }
    }
    if (e.key === 'Escape') {
      if (this.menu && !this.menu.hidden) { this.closeMenu(); e.preventDefault(); e.stopPropagation(); return; }
      // a cell being typed in gets its value back, and the dialog stays open;
      // otherwise Escape is the dialog's
      if (editing) {
        t.value = this.shown(r, c);
        t.select();
        e.preventDefault();
        e.stopPropagation();
      }
      this.editing = null;
      return;
    }
    if (e.key === 'Enter') {
      e.preventDefault();
      // Enter never submits the dialog from a cell: it commits and moves down
      e.stopPropagation();
      if (text) this.commit(r, c);
      else if (t.tagName === 'SELECT') this.flushSearch();
      var next = e.shiftKey ? r - 1 : r + 1;
      if (next >= 0 && next < this.rows.length) this.focusCell(next, c);
      return;
    }
    if ((e.key === 'ArrowDown' || e.key === 'ArrowUp') && !e.altKey) {
      var to = r + (e.key === 'ArrowDown' ? 1 : -1);
      if (to >= 0 && to < this.rows.length) { e.preventDefault(); this.focusCell(to, c); }
      return;
    }
    if ((e.key === 'Home' || e.key === 'End') && !e.shiftKey && !e.altKey && !editing) {
      e.preventDefault();
      var end = e.key === 'End';
      if (e.ctrlKey || e.metaKey) this.focusCell(end ? this.rows.length - 1 : 0, c);
      else this.focusCell(r, end ? this.ncol - 1 : 0);
      return;
    }
  };

  TableGrid.prototype.destroy = function() {
    document.removeEventListener('mousedown', this.onDocMouseDown);
  };

  // --- the Shiny binding ---------------------------------------------------------------

  if (typeof window === 'undefined' || !window.Shiny) return;
  var binding = new Shiny.InputBinding();
  $.extend(binding, {
    find: function(scope) { return $(scope).find('.stanpumpr-table-grid'); },
    initialize: function(root) { root._tableGrid = new TableGrid(root); },
    getValue: function(root) { return root._tableGrid ? root._tableGrid.value : null; },
    subscribe: function(root, callback) { root._tableGrid.callback = callback; },
    unsubscribe: function(root) { if (root._tableGrid) { root._tableGrid.callback = null; root._tableGrid.destroy(); } },
    getRatePolicy: function() { return null; }
  });
  Shiny.inputBindings.register(binding, 'stanpumpr.tableGrid');
})();
