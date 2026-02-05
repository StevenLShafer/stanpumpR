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

// add new row for user input
function addEmptyRowIfNeeded(hot) {
  var rows = hot.countRows();
  // only add if non-empty drug name on last row
  var rowdata = hot.getDataAtRow(rows - 1);
  if (rowdata[0] === null || rowdata[0] === '') { return; }
  hot.alter(
    'insert_row',
    rows,      // last row
    1,         // one row
    'manual'   // define our own source to avoid infinite loop
  );
}

// This function gets called every time the table is modified
function changeHot(changes, source) {
  var hot = this;
  if(source === 'edit' || source === 'CopyPaste.paste' ||
    source === 'Autofill.fill'
  ) {
    changes = !Array.isArray(changes) ? [] : changes;
    var rows = [];

    rows = uniq(
      changes.map(function(change) { return change[0]; })
    );

    // check to see if only change is drug
    // If yes, then mark so we know to reset time, dose, and unit
    var drugchange = false;
    if (changes.length === 1 &&  changes[0][1] === 0) {
      drugchange = true;
    }
    rows.forEach(function(row) {
      var rowdata = hot.getDataAtRow(row);
      // if everything in row is empty then exit
      if (rowdata.every(function(d) { return d === null; })) { return; }
      var drug = rowdata[0];
      var time = validateTime(rowdata[1]);
      var dose = validateDose(rowdata[2]);
      var unit = rowdata[3];

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
      setUnitDropdown(hot, row, 3, units);
      hot.setCellMeta(row, 3, 'readOnly', false);

      setTimeout(function() {
        // if user provides a unit then use this instead of drug default
        //   should we warn user that not default?
        unit = unit !== '' ? unit : validateUnit(unit, drug);
        hot.setDataAtCell(
          [
            [row, 1, time],
            [row, 2, dose],
            [row, 3, unit]
          ],
          null,
          null,
          'calculate'  // avoid infinite loop by using custom source
        );
        addEmptyRowIfNeeded(hot);
      }, 0);
    });
  }
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

function cleanTime(x) {
  return x.replace(/[^\d.:]/g, '');
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

function getDrugUnits(drug) {
  if (drug === '') { return ''; }
  var drug_default = drug_defaults.filter(function(d) {
    return d.Drug === drug;
  });

  if (drug_default.length === 0) { return ''; }

  return drug_default[0].Units.split(',');
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
