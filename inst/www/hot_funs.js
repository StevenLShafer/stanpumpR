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

// return the index of a column in a HOT based on its name
function findColByHeader(hot, headerName) {
  let headers = hot.getSettings().colHeaders;

  if (Array.isArray(headers)) {
    for (let i = 0; i < headers.length; i++) {
      if (headers[i].trim() === headerName) return i;
    }
    return null;
  }

  return null;
}

function getTimeMode() {
  var timeMode = $("#timeMode").val();
  return timeMode || 'clock'; // default to clock if not found
}

function isUserEditSource(source) {
  return source === 'edit' || source === 'CopyPaste.paste' || source === 'Autofill.fill';
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

  if (col === timeCol) {
    // Elapsed times are numbers in the time unit.  In clock mode a time is
    // HH:MM, or a number of units after the procedure start, which may have a
    // decimal point ("1.5" hours).  A pasted elapsed H:MM is still accepted.
    let timeMode = getTimeMode();
    ok = (timeMode === 'relative') ? /^[0-9.]$/.test(key) : /^[0-9.:]$/.test(key);
  } else if (col === doseCol) {
    ok = /^[0-9.]$/.test(key);
  }

  if (!ok) {
    event.stopImmediatePropagation();
    event.preventDefault();
    return false;
  }
}

// When a Time or Dose cell is changed, sanitize the input
function hookSanitize(changes, source) {
  if (!changes || !isUserEditSource(source)) return;

  let hot = this;
  let timeCol = findColByHeader(hot, "Time");
  let doseCol = findColByHeader(hot, "Dose");
  if (timeCol === null && doseCol === null) return;

  changes.forEach(function(change) {
    let col = change[1];
    let newVal = change[3];

    if (newVal === null || newVal === '') return;

    if (col === timeCol) {
      change[3] = validateTime(newVal);
    } else if (col === doseCol) {
      change[3] = validateDose(newVal);
    }
  });

  setTimeout(function() {

    hot.validateCells();
  }, 10);
}


// Clean a time: keep only digits, decimal points and colons, and only the
// first of each.  The same whatever the time display: what a time means
// depends on the format the table is in (R/utils-time.R), and the server's
// validateTime() (R/validate-input.R) checks every Time string against its own
// cleaning, so the two must agree character for character.  Cleaning by mode,
// as this once did, rewrote valid times whenever ANOTHER cell in the row was
// edited: in clock mode "1.5" became "15", "0.25" "025" and "10080" (four
// digits at most) "1008"; in elapsed mode "01:30" became "130" and "36:00"
// "3600".
function cleanTime(value) {
  if (value === null || value === undefined) return '';
  let str = String(value).replace(/[^0-9.:]/g, '');
  str = removeExtraDecimal(str);
  str = removeExtraColon(str);
  return str;
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

// https://stackoverflow.com/questions/8140612/remove-all-dots-except-the-first-one-from-a-string
function removeExtraDecimal(x) {
  return x.replace( /^([^.]*\.)(.*)$/, function ( a, b, c ) {
    return b + c.replace( /\./g, '' );
  });
}

function removeExtraColon(x) {
  return x.replace( /^([^:]*:)(.*)$/, function ( a, b, c ) {
    return b + c.replace( /:/g, '' );
  });
}

function cleanNumeric(x) {
  return x.replace(/[^\d.]/g, '');
}

function validateDose(dose) {
  dose = String(dose);
  // remove anything but numbers and decimal points
  var clean = cleanNumeric(dose);
  var clean = removeExtraDecimal(clean);
  if (clean === '') {
    clean = 0;
  }
  return clean;
}

// Mirrors validateTime() in R/validate-input.R; see cleanTime() above.
function validateTime(time) {
  // remove anything but numbers, decimal points, and colons, and all but the
  // first decimal point and the first colon
  var clean = cleanTime(time);

  // nothing left, or only a decimal point and a colon
  if (clean === '' || clean === '.' || clean === '.:' || clean === ':.') {
    return '0';
  }

  // if there is decimal then remove colon
  if (/\./.test(clean)) {
    return stripLeadingZeros(clean.replace(/:/g, ''));
  }

  var colon_pos = clean.indexOf(':');

  if (colon_pos !== -1) {
    var HH = colon_pos === 0 ? 0 : parseInt(clean.substring(0, colon_pos), 10);
    var MM = colon_pos === clean.length - 1 ? 0 : parseInt(clean.substring(colon_pos + 1), 10);
    // convert minutes greater than 60 to hours and minutes
    HH = HH + Math.floor(MM / 60);
    MM = MM % 60;
    // at least two digits each; an hour count of 100 or more is kept whole
    // (an elapsed "100:30"), not cut to its last two digits
    return padTwo(HH) + ':' + padTwo(MM);
  }

  return stripLeadingZeros(clean);
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

  changes = !Array.isArray(changes) ? [] : changes;
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

  rows.forEach(function(row) {
    let rowdata = hot.getDataAtRow(row);
    // if everything in row is empty then exit
    if (rowdata.every(function(d) { return d === null; })) { return; }
    // The Time cell is rewritten only when it, or the drug, was edited: a
    // stored time is already clean, and cleaning it again because the dose
    // next to it changed once corrupted it (see cleanTime()).
    let rowChanges = changes.filter(function(change) { return change[0] === row; });
    let timeEdited = rowChanges.some(function(change) { return change[1] === timeCol; });
    let drugEdited = rowChanges.some(function(change) { return change[1] === drugCol; });
    var drug = rowdata[drugCol];
    var time = validateTime(rowdata[timeCol]);
    var dose = validateDose(rowdata[doseCol]);
    var unit = rowdata[unitsCol];
    var writeTime = timeEdited || drugEdited;

    // if drug change then reset time and dose and update unit
    if (drugchange) {
      time = '0';
      dose = 0;
      unit = '';
      writeTime = true;
    }

    // if drug is empty, set everything to empty
    if (drug === '') {
      time = '';
      dose = '';
      unit = '';
      writeTime = true;
    }

    // dynamically change dropdown for units based on drug
    var units = getDrugUnits(drug);
    setUnitDropdown(hot, row, unitsCol, units);
    hot.setCellMeta(row, unitsCol, 'readOnly', false);

    unit = unit !== '' ? unit : validateUnit(unit, drug);
    if (writeTime) {
      allChanges.push([row, timeCol, time]);
    }
    allChanges.push([row, doseCol, dose]);
    allChanges.push([row, unitsCol, unit]);
  });

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
