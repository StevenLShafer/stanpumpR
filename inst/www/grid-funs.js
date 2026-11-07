// What the grids (dose-grid.js, table-grid.js) share with R: the time and
// dose readers held to R's validateTime() and validateDose() by the tests,
// and the messages that say why an entry was refused.

function getTimeMode() {
  var timeMode = $("#timeMode").val();
  return timeMode || 'clock'; // default to clock if not found
}

// The tip's text, from the Time units and Time Display controls; shared with
// the prototype grid (dose-grid.js)
function timeHeaderTipText() {
  var unit = $("#timeUnits").val() || "minutes";
  if (getTimeMode() === "clock") {
    return "Clock time (hh:mm) of each dose. Switch to elapsed time, or change " +
           "the unit, with the Time Display and Time units controls under the plot.";
  }
  return "Time of each dose, elapsed from the start, in " + unit + ". Change " +
         "the unit (minutes, hours, days, weeks) or switch to clock time with " +
         "the Time units and Time Display controls under the plot.";
}

// Say why an entry in the Time or Dose column was not accepted.  `outcome`,
// the closing line, says what became of it; by default the cell kept its
// value.
function notifyRefusedEntries(refused, timeCol, outcome) {
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
  let html = lines.map(escapeHtml).join('<br>') + '<br>' +
    escapeHtml(outcome || 'The cell keeps its previous value, if it had one.');
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

// Say which pasted units were not taken, and what the rows hold instead
function notifyRefusedUnits(refused, outcome) {
  if (refused.length === 0 || typeof window === 'undefined' || !window.Shiny ||
      !Shiny.notifications || typeof Shiny.notifications.show !== 'function') return;
  let lines = refused.slice(0, 3).map(function(r) {
    return '"' + r.unit + '" is not a unit offered for ' + r.drug + '; ' + outcome;
  });
  if (refused.length > 3) lines.push('(and ' + (refused.length - 3) + ' more)');
  Shiny.notifications.show({ html: lines.map(escapeHtml).join('<br>'), type: 'warning',
                             duration: 8000, id: 'unitEntryRefused' });
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
