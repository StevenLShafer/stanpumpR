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
  var timeMode = $('input[name="timeMode"]:checked').val();
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
    let timeMode = getTimeMode();
    ok = (timeMode === 'relative') ? /^[0-9.]$/.test(key) : /^[0-9:]$/.test(key);
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


// clean the time input (remove extra dots, colons, etc) based the time mode
function cleanTime(value) {
  if (!value) return value;

  let timeMode = getTimeMode();
  let str = String(value);

  if (timeMode === 'relative') {
    // Allow only digits and decimal point
    str = str.replace(/[^0-9.]/g, '');

    // Keep only first decimal point
    let firstDot = str.indexOf('.');
    if (firstDot !== -1) {
      let beforeDot = str.substring(0, firstDot);
      let afterDot = str.substring(firstDot + 1).replace(/\./g, '');
      str = beforeDot + '.' + afterDot;
    }

    // Remove leading zeros (except for decimals like 0.5)
    if (str.length > 1 && str[0] === '0' && str[1] !== '.') {
      str = str.replace(/^0+/, '') || '0';
    }

  } else {
    // Clock mode: allow only digits and colon
    str = str.replace(/[^0-9:]/g, '');

    // Keep only first colon
    let firstColon = str.indexOf(':');
    if (firstColon !== -1) {
      let beforeColon = str.substring(0, firstColon);
      let afterColon = str.substring(firstColon + 1).replace(/:/g, '');
      str = beforeColon + ':' + afterColon;
    }

    // Limit HH to 2 digits, MM to 2 digits
    if (firstColon !== -1) {
      let parts = str.split(':');
      if (parts[0].length > 2) {
        parts[0] = parts[0].substring(0, 2);
      }
      if (parts[1] && parts[1].length > 2) {
        parts[1] = parts[1].substring(0, 2);
      }
      str = parts.join(':');
    } else {
      // No colon yet, limit to reasonable length
      if (str.length > 4) {
        str = str.substring(0, 4);
      }
    }
  }

  return str;
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

function validateTime(time) {
  time = String(time);

  // remove anything but numbers, decimal points, and colons
  var clean = cleanTime(time);

  // remove all but first decimal
  clean = removeExtraDecimal(clean);

  // remove all but first colon
  clean = removeExtraColon(clean);

  // if only decimal points and colon return 0
  if(clean.replace(/[\.:]/g,'').length === 0) {
    return 0;
  }

  // if there is decimal then remove colon
  if(/\./.test(clean)) {
    return clean.replace(/:/g,'');
  }

  var colon_pos = clean.match(/:/);

  // if no colon and number 4 digits or greater then parse into hours and minutes
  //  if(colon_pos === null && clean.length >= 4) {
    //    clean = clean.substring(0,2) + ':' + clean.substring(2,clean.length);
    //  }

  colon_pos = clean.match(/:/);

  if(colon_pos !== null) {
    colon_pos = colon_pos.index;
    var HH = colon_pos === 0 ? 0 : parseInt(clean.substring(0,colon_pos));
    var MM = colon_pos === clean.length - 1 ? 0 : parseInt(clean.substring(colon_pos + 1, clean.length));
    // convert minutes greater than 60 to hours and minutes
    HH = HH + Math.floor(MM/60);
    MM = MM % 60;
    // combine into two digit hours and minutes
    clean = ('0' + HH).slice(-2) + ':' + ('0' + MM).slice(-2);
  }

  return clean;
}

// Hook to take care of advanced logic every time a cell is updatd in the main
// dose table
function hookDoseTableUpdate(changes, source) {
  if (!changes || !isUserEditSource(source)) return;

  var hot = this;
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
  rows.forEach(function(row) {
    let rowdata = hot.getDataAtRow(row);
    // if everything in row is empty then exit
    if (rowdata.every(function(d) { return d === null; })) { return; }
    var drug = rowdata[drugCol];
    var time = validateTime(rowdata[timeCol]);
    var dose = validateDose(rowdata[doseCol]);
    var unit = rowdata[unitsCol];

    // if drug change then reset time and dose and update unit
    if (drugchange) {
      time = '0';
      dose = 0;
      unit = '';
    }

    // if drug is empty, set everything to empty
    if (drug === '') {
      time = '';
      dose = '';
      unit = '';
    }

    // dynamically change dropdown for units based on drug
    var units = getDrugUnits(drug);
    setUnitDropdown(hot, row, unitsCol, units);
    hot.setCellMeta(row, unitsCol, 'readOnly', false);

    setTimeout(function() {
      // if user provides a unit then use this instead of drug default
      //   should we warn user that not default?
        unit = unit !== '' ? unit : validateUnit(unit, drug);
        hot.setDataAtCell(
          [
            [row, timeCol, time],
            [row, doseCol, dose],
            [row, unitsCol, unit]
          ],
          null,
          null,
          'calculate'  // avoid infinite loop by using custom source
        );
        addEmptyRowIfNeeded(hot);
    }, 0);
  });
}

// add new row for user input
function addEmptyRowIfNeeded(hot) {
  var rows = hot.countRows();
  // only add if non-empty drug name on last row
  var rowdata = hot.getDataAtRow(rows - 1);
  let drugCol = findColByHeader(hot, "Drug");
  if (rowdata[drugCol] === null || rowdata[drugCol] === '') { return; }
  hot.alter(
    'insert_row',
    rows,      // last row
    1,         // one row
    'manual'   // define our own source to avoid infinite loop
  );
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
