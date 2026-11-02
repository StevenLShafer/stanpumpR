// handsontable validation and utility functions

// make a dirty unique function using a hash
//  to avoid an additional dependency
//  https://stackoverflow.com/questions/9229645/remove-duplicate-values-from-js-array
//  so that we can calculate each row once
function uniq(a) {
  var seen = {};
  return a.filter(function(item) {
    return seen.hasOwnProperty(item) ? false : (seen[item] = true);
  });
}

// return the index of a column in a HOT based on its name.  A header may
// carry a qualifier after the name ("Dose/Target" in the dose table, "Dose or
// Target" in the Edit doses dialog of a drug with TCI units): the name is
// what comes before a "/" or an "or".
function findColByHeader(hot, headerName) {
  let headers = hot.getSettings().colHeaders;

  if (Array.isArray(headers)) {
    for (let i = 0; i < headers.length; i++) {
      let name = String(headers[i]).trim().split(/\s*(?:\/|\bor\b)\s*/)[0].trim();
      if (name === headerName) return i;
    }
    return null;
  }

  return null;
}

function getTimeMode() {
  var timeMode = $("#timeMode").val();
  return timeMode || 'clock'; // default to clock if not found
}

function isNullish(value) {
  return value === null || value === undefined;
}

function isUserEditSource(source) {
  return source === 'edit' || source === 'CopyPaste.paste' || source === 'Autofill.fill';
}

// A hover tip on the Time column header (afterGetColHeader), so a reader knows
// what the Time column means -- which unit, and whether it is elapsed or clock
// time -- and where to change it (Dr. Fan's review, 2026-10-28).  The header
// text must stay "Time" (the hooks find columns by it), so the explanation is a
// title attribute rather than a longer header.
function hookTimeHeaderTip(col, TH) {
  try {
    var hot = this;
    if (col !== findColByHeader(hot, "Time")) return;
    var unit = $("#timeUnits").val() || "minutes";
    var tip;
    if (getTimeMode() === "clock") {
      tip = "Clock time (hh:mm) of each dose. Switch to elapsed time, or change " +
            "the unit, with the Time Display and Time units controls under the plot.";
    } else {
      tip = "Time of each dose, elapsed from the start, in " + unit + ". Change " +
            "the unit (minutes, hours, days, weeks) or switch to clock time with " +
            "the Time units and Time Display controls under the plot.";
    }
    var span = TH.querySelector(".colHeader");
    if (span) span.setAttribute("title", tip);
    TH.setAttribute("title", tip);
  } catch (e) {}
}

// Don't allow user to type illegal characters into the Time or Dose columns
function hookFilterKeys(event) {
  let hot = this;
  let timeCol = findColByHeader(hot, "Time");
  let doseCol = findColByHeader(hot, "Dose");
  if (timeCol === null && doseCol === null) return;

  let selected = hot.getSelected();

  if (!selected || selected.length === 0) return;

  let row = selected[0][0];
  let col = selected[0][1];

  let key = event.key;

  // Allow special/control keys
  if (key.length > 1 || event.ctrlKey || event.metaKey || event.altKey) {
    return;
  }

  let ok = true;

  // A minus sign is let through so that hookSanitize() sees "-2" and refuses
  // it with a message.  Blocked here, "-2" was typed as "2", and a sign error
  // became a positive dose without a word.
  if (col === timeCol) {
    // Elapsed times are numbers in the time unit.  In clock mode a time is
    // HH:MM, or a number of units after the procedure start, which may have a
    // decimal point ("1.5" hours).  A pasted elapsed H:MM is still accepted.
    let timeMode = getTimeMode();
    ok = (timeMode === 'relative') ? /^[0-9.\-]$/.test(key) : /^[0-9.:\-]$/.test(key);
  } else if (col === doseCol) {
    ok = /^[0-9.\-]$/.test(key);
  }

  if (!ok) {
    event.stopImmediatePropagation();
    event.preventDefault();
    return false;
  }
}

// When a Time or Dose cell is changed, sanitize the input.  An entry that
// cannot be read (validateTime() and validateDose() below return '', e.g. ".",
// "-2" or "abc") is refused with a message, and so is emptying a filled cell
// of a row that keeps its drug (a blank cell was once quietly read as 0).  A
// refused entry puts back what the cell held before, so the table never shows
// one thing while the simulation uses another.  A cell that held nothing is
// left blank, and remembered in hot.rejectedCells until hookDoseTableUpdate()
// has run for this change, so that it stays blank rather than being filled
// with 0 as a cell that was never filled in is; the row is then ignored until
// it is corrected.
function hookSanitize(changes, source) {
  if (!changes || !isUserEditSource(source)) return;

  let hot = this;
  hot.rejectedCells = {};
  let timeCol = findColByHeader(hot, "Time");
  let doseCol = findColByHeader(hot, "Dose");
  let drugCol = findColByHeader(hot, "Drug");
  if (timeCol === null && doseCol === null) return;

  // Rows whose drug this same change clears: emptying the rest of such a row
  // is clearing it, not a refused entry.
  let clearedRows = {};
  if (drugCol !== null) {
    changes.forEach(function(change) {
      if (change && change[1] === drugCol && (change[3] === null || change[3] === '')) {
        clearedRows[change[0]] = true;
      }
    });
  }

  let refused = [];
  changes.forEach(function(change) {
    if (!change) return;
    let row = change[0];
    let col = change[1];
    let oldVal = change[2];
    let newVal = change[3];
    if (col !== timeCol && col !== doseCol) return;

    let hadValue = trimEntry(oldVal) !== '';
    let blank = trimEntry(newVal) === '';
    if (blank) {
      // Emptying a filled cell is refused only in the dose table, and only
      // in a row that keeps its drug: a cell next to no drug is meant to be
      // empty.  The dialogs' tables (no drug column) handle a blank
      // themselves.
      let keepsDrug = drugCol !== null && !clearedRows[row] &&
        trimEntry(hot.getDataAtCell(row, drugCol)) !== '';
      if (hadValue && keepsDrug) {
        change[3] = oldVal;
        refused.push({ col: col, entry: '' });
      }
      return;
    }

    change[3] = (col === timeCol) ? validateTime(newVal) : validateDose(newVal);
    if (change[3] !== '') return;

    refused.push({ col: col, entry: trimEntry(newVal) });
    if (hadValue) {
      change[3] = oldVal;
    } else {
      hot.rejectedCells[row + ':' + col] = true;
    }
  });

  if (refused.length > 0) notifyRefusedEntries(refused, timeCol);

  setTimeout(function() {

    hot.validateCells();
  }, 10);
}

// Say why an entry in the Time or Dose column was not accepted
function notifyRefusedEntries(refused, timeCol) {
  let lines = refused.slice(0, 3).map(function(r) {
    let what = (r.col === timeCol) ? 'time' : 'dose';
    if (r.entry === '') {
      return (what === 'dose') ? 'A dose cannot be left blank; enter 0 for no dose.'
                               : 'A time cannot be left blank.';
    }
    if (/^\s*-/.test(r.entry)) {
      return '"' + r.entry + '" was not accepted: a ' + what + ' cannot be negative.';
    }
    if (readEntryNumber(r.entry) !== null) {
      return '"' + r.entry + '" was not accepted: ' + (what === 'dose'
        ? 'a dose cannot be more than 1,000,000,000.'
        : 'a time cannot be more than ' + MAX_TIME_STRING_LENGTH + ' characters long.');
    }
    return '"' + r.entry + '" was not accepted: a ' + what + ' must be a number.';
  });
  if (refused.length > 3) lines.push('(and ' + (refused.length - 3) + ' more)');
  let html = lines.map(escapeHtml).join('<br>') +
    '<br>The cell keeps its previous value, if it had one.';
  if (window.Shiny && Shiny.notifications && typeof Shiny.notifications.show === 'function') {
    Shiny.notifications.show({ html: html, type: 'warning', duration: 8000, id: 'doseEntryRefused' });
  }
}

function escapeHtml(text) {
  return String(text).replace(/[&<>"']/g, function(c) {
    return { '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c];
  });
}


// validateTime() and validateDose() mirror the functions of the same names in
// R/validate-input.R, whose header gives the rules, and must agree with them:
// the server checks that every stored time is one its validateTime() leaves
// unchanged.  In short, a blank entry is '0'; one non-negative number, in
// plain decimal or scientific notation, with commas only between groups of
// three digits, is that number; a time may instead be H:MM; anything else is
// '', an unfinished cell, and the row is ignored until it is corrected.
// Nothing is guessed at.  These once deleted every character that was not a
// digit, a decimal point or a colon, so a pasted "-5" became 5, "1e3" 13 and
// "8;30" 830 minutes.  The rules are the same whatever the time display: what
// a time means depends on the format the table is in (R/utils-time.R).

// The entry as a string, without the white space and quotation marks around it
function trimEntry(value) {
  if (value === null || value === undefined) return '';
  return String(value).replace(/^[\s'"`]+|[\s'"`]+$/g, '');
}

// A number written out in plain decimal, as R's format(x, scientific = FALSE,
// digits = 15) writes it: 1e3 is "1000", 2.5e-7 "0.00000025"
function plainDecimal(n) {
  return n.toLocaleString('en-US', { useGrouping: false, maximumSignificantDigits: 15 });
}

// A trimmed entry that is one non-negative number in an accepted form, as the
// number to store; null for anything else
function readEntryNumber(x) {
  x = x.replace(/^\+/, '');
  var ok = /^(([0-9]+|[0-9]{1,3}(,[0-9]{3})+)(\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?$/.test(x);
  if (!ok) return null;
  x = x.replace(/,/g, '');
  if (!/[eE]/.test(x)) return x;
  var n = Number(x);
  if (!isFinite(n)) return null;
  return plainDecimal(n);
}

// "007" -> "7", "00.5" -> "0.5", "000" -> "0".  R's validateTime() keeps
// leading zeros, but the result here is still one of its fixed points, and
// means the same number.
function stripLeadingZeros(x) {
  return x.replace(/^0+(?=[0-9])/, '');
}

function padTwo(n) {
  return n < 10 ? '0' + n : String(n);
}

// The limits validateDoseTableInput() (R/input-tables.R) enforces on the
// stored table, MAX_DOSE_VALUE and MAX_TIME_STRING_LENGTH in R/constants.R:
// an entry beyond them is refused here, as R's validateDose() and
// validateTime() refuse it, rather than stored and then refused by the
// server, which replaced the whole grid with an error (adversarial UI review,
// 2026-11, finding 7).  test-validate-input.R holds them to the R constants.
var MAX_DOSE_VALUE = 1e9;
var MAX_TIME_STRING_LENGTH = 32;

// A blank dose is 0, the number, as the grid has always stored it
function validateDose(dose) {
  var x = trimEntry(dose);
  if (x === '') return 0;
  var n = readEntryNumber(x);
  return (n === null || Number(n) > MAX_DOSE_VALUE) ? '' : n;
}

function validateTime(time) {
  var x = trimEntry(time);
  if (x === '') return '0';

  if (x.indexOf(':') !== -1) {
    var m = /^\+?([0-9]*):([0-9]*)$/.exec(x);
    if (m === null) return '';
    var HH = m[1] === '' ? 0 : parseInt(m[1], 10);
    var MM = m[2] === '' ? 0 : parseInt(m[2], 10);
    // convert minutes greater than 60 to hours and minutes
    HH = HH + Math.floor(MM / 60);
    MM = MM % 60;
    // past 2^53 the hour count is not exact and String() writes "1e+21":
    // refused, as R's validateTime() refuses it
    if (!Number.isSafeInteger(HH)) return '';
    // at least two digits each; an hour count of 100 or more is kept whole
    // (an elapsed "100:30"), not cut to its last two digits
    var hm = padTwo(HH) + ':' + padTwo(MM);
    return hm.length > MAX_TIME_STRING_LENGTH ? '' : hm;
  }

  var n = readEntryNumber(x);
  if (n === null) return '';
  n = stripLeadingZeros(n);
  return n.length > MAX_TIME_STRING_LENGTH ? '' : n;
}

// Hook to take care of advanced logic every time a cell is updatd in the main
// dose table
function hookDoseTableUpdate(changes, source) {
  if (!changes || !isUserEditSource(source)) return;

  let hot = this;
  let timeCol = findColByHeader(hot, "Time");
  let doseCol = findColByHeader(hot, "Dose");
  let drugCol = findColByHeader(hot, "Drug");
  let unitsCol = findColByHeader(hot, "Units");
  if (drugCol === null || timeCol === null) return;

  // A change from nothing to nothing is no change, and Handsontable does not
  // apply it; it is dropped here too, so that it can never be read as a drug
  // being chosen.  When a paste runs past the last row, the row it creates
  // gets one such change per column from rhandsontable's afterCreateRow
  // handler, which writes each column's default (null).  The Drug column's
  // is validated, and Handsontable validates in a setTimeout, so it arrived
  // after the paste had filled the row, alone and on the Drug column, and
  // the created row's pasted time and dose were reset to 0 and 0
  // (keyboard review, 2026-11-02; test-dose-table-paste.R).
  //
  // So is a cell re-entered as it was: pressing Enter twice on a Drug cell
  // re-commits the same drug, which was read as choosing it and reset the
  // row's time and dose to 0 (found with #675, fixed here).
  changes = !Array.isArray(changes) ? [] : changes.filter(function(change) {
    return change && !(isNullish(change[2]) && isNullish(change[3])) &&
      trimEntry(change[2]) !== trimEntry(change[3]);
  });
  if (changes.length === 0) {
    // hot.pastedUnits is kept: a paste that creates rows reports these no-op
    // edits around its own change, and beforePaste starts it afresh
    hot.rejectedCells = {};
    return;
  }
  let rows = uniq(
    changes.map(function(change) { return change[0]; })
  );

  // check to see if only change is drug
  // If yes, then mark so we know to reset time, dose, and unit
  let drugchange = false;
  if (changes.length === 1 && changes[0][1] === drugCol) {
    drugchange = true;
  }

  let allChanges = [];
  let refusedUnits = [];

  rows.forEach(function(row) {
    let rowdata = hot.getDataAtRow(row);
    // if everything in row is empty then exit
    if (rowdata.every(function(d) { return d === null; })) { return; }
    // The Time and Dose cells are rewritten only when they, or the drug, were
    // edited: a stored value is already clean, and cleaning a time again
    // because the dose next to it changed once corrupted it (when the grid
    // cleaned by time display, "1.5" became "15").  Rewriting fills a blank
    // cell with 0, except one hookSanitize() has just cleared because its
    // entry could not be read: that stays blank, so the row is ignored until
    // it is corrected, rather than the entry becoming 0.
    let rowChanges = changes.filter(function(change) { return change[0] === row; });
    let timeEdited = rowChanges.some(function(change) { return change[1] === timeCol; });
    let doseEdited = rowChanges.some(function(change) { return change[1] === doseCol; });
    let drugEdited = rowChanges.some(function(change) { return change[1] === drugCol; });
    let rejected = hot.rejectedCells || {};
    var drug = rowdata[drugCol];
    var time = rejected[row + ':' + timeCol] ? '' : validateTime(rowdata[timeCol]);
    var dose = rejected[row + ':' + doseCol] ? '' : validateDose(rowdata[doseCol]);
    var unit = rowdata[unitsCol];
    var writeTime = timeEdited || drugEdited;
    var writeDose = doseEdited || drugEdited;

    // if drug change then reset time and dose and update unit
    if (drugchange) {
      time = '0';
      dose = 0;
      unit = '';
      writeTime = true;
      writeDose = true;
    }

    // if drug is empty, set everything to empty (null in a row the grid
    // created, as when a paste of times and doses runs past the last row)
    if (trimEntry(drug) === '') {
      time = '';
      dose = '';
      unit = '';
      writeTime = true;
      writeDose = true;
    }

    // dynamically change dropdown for units based on drug
    var units = getDrugUnits(drug);
    setUnitDropdown(hot, row, unitsCol, units);
    hot.setCellMeta(row, unitsCol, 'readOnly', false);

    // A row the grid created holds null, not '', where nothing was written
    unit = trimEntry(unit) !== '' ? unit : validateUnit(unit, drug);
    // The unit a paste carried for this row (hookRememberPastedUnits())
    let pastedUnit = (source === 'CopyPaste.paste' && hot.pastedUnits) ? hot.pastedUnits[row] : undefined;
    if (pastedUnit !== undefined && trimEntry(drug) !== '') {
      if (Array.isArray(units) && units.indexOf(pastedUnit) !== -1) {
        unit = pastedUnit;
      } else {
        refusedUnits.push({ drug: drug, unit: pastedUnit });
      }
    }
    if (writeTime) {
      allChanges.push([row, timeCol, time]);
    }
    if (writeDose && doseCol !== null) {
      allChanges.push([row, doseCol, dose]);
    }
    allChanges.push([row, unitsCol, unit]);
  });
  hot.rejectedCells = {};
  hot.pastedUnits = {};

  if (refusedUnits.length > 0 && typeof window !== 'undefined' && window.Shiny &&
      Shiny.notifications && typeof Shiny.notifications.show === 'function') {
    let lines = refusedUnits.slice(0, 3).map(function(r) {
      return '"' + r.unit + '" is not a unit offered for ' + r.drug + '; the row has its default unit.';
    });
    if (refusedUnits.length > 3) lines.push('(and ' + (refusedUnits.length - 3) + ' more)');
    Shiny.notifications.show({ html: lines.map(escapeHtml).join('<br>'), type: 'warning',
                               duration: 8000, id: 'unitEntryRefused' });
  }

  if (allChanges.length > 0) {
    hot.setDataAtCell(allChanges, 'calculate');
  }
}

function getDrugUnits(drug) {
  if (drug === '') { return ''; }
  var drug_default = drug_defaults.filter(function(d) {
    return d.Drug === drug;
  });

  if (drug_default.length === 0) { return ''; }

  return drug_default[0].Units;
}

// Remember the units a paste carries, by row.  A pasted Units cell is
// validated against the dropdown list its row had before the paste -- for a
// blank row, [""] -- so Handsontable drops it, and the row then took its
// drug's default unit: a pasted "morphine 0 10 mg PO" became 10 mg
// intravenous without a word (keyboard review of 2026-11-02).
// hookDoseTableUpdate() applies the remembered unit once the drug is in
// place, or says why not.
function hookRememberPastedUnits(data, coords) {
  let hot = this;
  hot.pastedUnits = {};
  let unitsCol = findColByHeader(hot, "Units");
  if (unitsCol === null || !Array.isArray(data) || !coords || !coords[0]) return;
  let startRow = coords[0].startRow;
  let startCol = coords[0].startCol;
  let j = unitsCol - startCol;
  if (j < 0) return;
  data.forEach(function(line, i) {
    if (Array.isArray(line) && j < line.length) {
      let unit = trimEntry(line[j]);
      if (unit !== '') hot.pastedUnits[startRow + i] = unit;
    }
  });
}

function setUnitDropdown(hot, row, column, source) {
  hot.setCellMeta(
    row,
    column,
    'source',
    source
  );
}

function validateUnit(unit, drug) {
  if( drug === '') { return ''; }

  var default_unit = drug_defaults.filter(function(d) {
    return d.Drug === drug;
  });

  if (default_unit.length === 0) { return ''; }

  return default_unit[0]['Default.Units'];
}

// A unit as the dose table shows it.  The Units column is narrow, so a long
// unit wraps (app.css), and it may wrap only after its route: the spaces in
// "mg/kg PO" and in "liquid q4h" become non-breaking, so "mg/kg PO liquid q4h"
// wraps as "mg/kg PO" over "liquid q4h", in the cell and in its dropdown list
// alike.  A unit with no route, or nothing after it, wraps where it must.
// Display only: the cell's value keeps its ordinary spaces.
var UNIT_ROUTES = ['PO', 'SL', 'IM', 'IN', 'RA', 'SC'];

function unitWrapText(unit) {
  var words = String(unit).split(' ');
  var route = -1;
  for (var i = 0; i < words.length; i++) {
    if (UNIT_ROUTES.indexOf(words[i]) >= 0) { route = i; break; }
  }
  if (route < 0 || route === words.length - 1) return String(unit);
  return words.slice(0, route + 1).join(' ') + ' ' +
    words.slice(route + 1).join(' ');
}

// Rewrites the unit's text in a rendered cell, leaving its markup alone (the
// dropdown arrow; the list's bold match while typing).
function wrapUnitText(td, value) {
  if (typeof value !== 'string' || value.indexOf(' ') < 0) return;
  var text = unitWrapText(value);
  if (text === value) return;
  var walker = document.createTreeWalker(td, NodeFilter.SHOW_TEXT);
  var offset = 0;
  var node;
  while ((node = walker.nextNode())) {
    var parent = node.parentNode;
    if (parent.classList && parent.classList.contains('htAutocompleteArrow')) continue;
    var n = node.nodeValue.length;
    if (value.substr(offset, n) !== node.nodeValue) return;
    node.nodeValue = text.substr(offset, n);
    offset += n;
  }
}

// The renderer of the dose table's Units cells (createHOT()).
function renderUnitCell(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.DropdownRenderer.apply(this, arguments);
  wrapUnitText(td, value);
  return td;
}

function wrapUnitListItem(td, row, col, prop, value) {
  wrapUnitText(td, value);
}

// The Units dropdown list is a Handsontable of its own, which the editor
// fills in an afterRenderer hook when it opens, before this hook runs; adding
// wrapUnitListItem() after it lets it rewrite what that hook wrote.
function hookWrapUnitList(row, col) {
  var hot = this;
  if (col !== findColByHeader(hot, "Units")) return;
  var list = hot.getActiveEditor().htEditor;
  if (!list) return;
  list.removeHook('afterRenderer', wrapUnitListItem);
  list.addHook('afterRenderer', wrapUnitListItem);
  list.render();
}

// When clicking on a drug cell, select all the text so that it's easier to replace
function hookSelectAllDrugText(row, col) {
  let hot = this;
  let drugCol = findColByHeader(hot, "Drug");
  if (drugCol === null || drugCol !== col) return;

  let textarea = hot.getActiveEditor().TEXTAREA;
  if (textarea && typeof textarea.select === "function") {
    textarea.select();
  }
}
