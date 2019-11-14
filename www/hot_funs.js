// handsontable validation and utility functions

// This variable is created using jsonlite::toJSON(drugDefaults)
var drug_defaults = [{"Drug":"propofol","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mcg/kg/min","Default.Units":"mg","Units":"mg,mg/kg,mcg/kg/min,mg/kg/hr","Color":"#FFCC00","Lower":2.5,"Upper":4,"Typical":3,"MEAC":0,"Emerge":1},{"Drug":"remifentanil","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/kg/min","Default.Units":"mcg/kg/min","Units":"mcg,mcg/kg,mcg/kg/min","Color":"#0000C0","Lower":0.8,"Upper":2,"Typical":1.2,"MEAC":1,"Emerge":1},{"Drug":"fentanyl","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/kg/hr","Default.Units":"mcg","Units":"mcg,mcg/kg,mcg/kg/hr","Color":"#0491E2","Lower":0.48,"Upper":1.2,"Typical":0.72,"MEAC":0.6,"Emerge":0.6},{"Drug":"alfentanil","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/kg/hr","Default.Units":"mcg","Units":"mcg,mcg/kg,mcg/kg/hr","Color":"#0491E2","Lower":31.2,"Upper":78,"Typical":46.8,"MEAC":39,"Emerge":39},{"Drug":"sufentanil","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/kg/hr","Default.Units":"mcg","Units":"mcg,mcg/kg,mcg/kg/hr","Color":"#0491E2","Lower":0.0448,"Upper":0.112,"Typical":0.0672,"MEAC":0.056,"Emerge":0.056},{"Drug":"morphine","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/hr","Color":"#032FED","Lower":0.0064,"Upper":0.016,"Typical":0.0096,"MEAC":0.008,"Emerge":0.008},{"Drug":"pethidine","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/hr","Color":"#5155FF","Lower":0.2,"Upper":0.5,"Typical":0.3,"MEAC":0.25,"Emerge":0.25},{"Drug":"hydromorphone","Concentration.Units":"ng","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/kg,mg/hr,mg/kg/hr,mg PO,mg IM,mg IN","Color":"#032FED","Lower":1.2,"Upper":3,"Typical":1.8,"MEAC":1.5,"Emerge":1.5},{"Drug":"methadone","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/hr","Color":"#71C5E8","Lower":0.048,"Upper":0.12,"Typical":0.072,"MEAC":0.06,"Emerge":0.06},{"Drug":"ketamine","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/hr","Color":"#FFCC00","Lower":0.1,"Upper":0.16,"Typical":0.12,"MEAC":0,"Emerge":0.1},{"Drug":"dexmedetomidine","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/kg/hr","Default.Units":"mcg/kg/hr","Units":"mcg,mcg/kg,mcg/hr,mcg/kg/hr","Color":"#791AEE","Lower":0.4,"Upper":0.8,"Typical":10,"MEAC":0,"Emerge":0.4},{"Drug":"midazolam","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/kg,mg/hr","Color":"#E36C0A","Lower":0.04,"Upper":0.12,"Typical":0.1,"MEAC":0,"Emerge":0.04},{"Drug":"etomidate","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/kg/min","Default.Units":"mg","Units":"mg,mg/kg/min","Color":"#FFCC00","Lower":0.4,"Upper":0.8,"Typical":0.5,"MEAC":0,"Emerge":0.4},{"Drug":"lidocaine","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/hr","Default.Units":"mg","Units":"mg,mg/hr","Color":"#B7AE7F","Lower":0.5,"Upper":1.5,"Typical":1,"MEAC":0,"Emerge":0.5},{"Drug":"rocuronium","Concentration.Units":"mcg","Bolus.Units":"mg","Infusion.Units":"mg/kg/hr","Default.Units":"mg","Units":"mg,mg/kg/hr","Color":"#F9423A","Lower":1,"Upper":2.2,"Typical":1.5,"MEAC":0,"Emerge":1},{"Drug":"naloxone","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/min","Default.Units":"mcg","Units":"mcg,mcg/min","Color":"#404040","Lower":0,"Upper":0,"Typical":0,"MEAC":0,"Emerge":1},{"Drug":"oxytocin","Concentration.Units":"ng","Bolus.Units":"mcg","Infusion.Units":"mcg/min","Default.Units":"mcg","Units":"mcg,mcg/min","Color":"#008F7D","Lower":0.05,"Upper":0.2,"Typical":0.1,"MEAC":0,"Emerge":0.05},{"Drug":"oxycodone","Concentration.Units":"ng","Default.Units":"mg PO","Units":"mg PO","Color":"#032FED","Lower":10,"Upper":20,"Typical":14,"MEAC":12,"Emerge":10}];

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
  if(colon_pos === null && clean.length >= 4) {
    clean = clean.substring(0,2) + ':' + clean.substring(2,clean.length);
  }

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
