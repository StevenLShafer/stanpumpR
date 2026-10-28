// Not 'shiny:connected': since Shiny 1.14 that fires before the browser sends
// its "init" message, so the inputs set here went to the server first, without
// the page's URL, and the server never restored a bookmarked link's dose
// table, events or thresholds (only the inputs drawn into the page were).
$(document).on('shiny:sessioninitialized', function(event) {
  // Send shiny the current local time
  var timeNow = new Date().toLocaleTimeString();
  Shiny.setInputValue("client_time", timeNow);

  // determine if to show the intro modal based on last time it was shown
  if (!getCookie('intro_shown')) {
    setCookie('intro_shown', 'true', 7);
    Shiny.setInputValue('show_intro_modal', true, {priority: 'event'});
  }

  // send shiny the width of the plot section
  const plotBox = document.getElementById('plotContainer');
  let plotBoxTimeout = null;
  const ro = new ResizeObserver(entries => {
    for (let entry of entries) {
      const newWidth = entry.contentRect.width;
      clearTimeout(plotBoxTimeout);
      plotBoxTimeout = setTimeout(function() {
        Shiny.setInputValue('plotWidth', newWidth);
      }, 100);
    }
  });
  ro.observe(plotBox);
});

// The Max time and Min time sliders (R/app_ui.R) hold minutes, as the
// server, bookmarks and scenarios read them; they are shown in the Time
// units.  Shiny gives each update from the server (a new range and step
// when the unit changes) its own number formatter, so the slider's onUpdate,
// which Shiny's updates leave in place, puts this one back.  Without it the
// slider still works, in minutes.
var TIME_UNIT_MINUTES = {minutes: 1, hours: 60, days: 1440, weeks: 10080};
var TIME_UNIT_SHORT = {minutes: 'min', hours: 'h', days: 'd', weeks: 'wk'};
function formatSliderMinutes(minutes) {
  var unit = $('#timeUnits').val();
  if (!TIME_UNIT_MINUTES[unit]) unit = 'minutes';
  var x = Math.round(minutes / TIME_UNIT_MINUTES[unit] * 100) / 100;
  return x + ' ' + TIME_UNIT_SHORT[unit];
}
function formatTimeSlider(el) {
  var slider = $(el).data('ionRangeSlider');
  if (!slider || slider.options.prettify === formatSliderMinutes) return;
  slider.update({
    prettify_enabled: true,
    prettify: formatSliderMinutes,
    onUpdate: function() { formatTimeSlider(el); }
  });
}
$(document).on('shiny:bound', '#maximum, #minimum', function() { formatTimeSlider(this); });
$(document).on('shiny:sessioninitialized', function() {
  formatTimeSlider('#maximum');
  formatTimeSlider('#minimum');
});

// The startup drug menu (R/startup-drugs.R): each category's summary says how
// many of its drugs are ticked, so a tick is not lost when it is collapsed.
function startupDrugTickedCount(group) {
  var n = $(group).find('input[type=checkbox]:checked').length;
  $(group).find('.startup-drug-ticked').text(n > 0 ? n + ' ticked' : '');
}
$(document).on('change', '.startup-drug-group input[type=checkbox]', function() {
  startupDrugTickedCount($(this).closest('.startup-drug-group'));
});

// Track mouse position so hover tooltip can be positioned correctly when scrolled
let lastMouseX = 0, lastMouseY = 0;
// Where the hover tip was last placed, and how far the pointer may move
// before it is taken down.  The tip is drawn by the server from a debounced
// hover (hoverOpts() in R/app_ui.R), so a new one arrives only once the
// pointer has rested; without this the old tip stayed on screen, at the old
// place, for as long as the pointer kept moving.  Hiding it here is
// immediate; the next tip the server sends is shown afresh.
let hoverTipX = null, hoverTipY = null;
const HOVER_TIP_MOVE_PX = 6;
$(document).on('mousemove', function(e) {
  lastMouseX = e.clientX;
  lastMouseY = e.clientY;
  if (hoverTipX !== null &&
      Math.hypot(e.clientX - hoverTipX, e.clientY - hoverTipY) > HOVER_TIP_MOVE_PX) {
    hideHoverTip();
  }
});

function hideHoverTip() {
  hoverTipX = null;
  hoverTipY = null;
  let tooltip = document.querySelector('#hover_info_box');
  if (tooltip) tooltip.style.display = 'none';
}

// Leaving the plot takes the tip down too: the hover input is not nulled
// outside the plot (nullOutside = FALSE), so the server would not.  A tip
// still on its way from the server when the pointer left (the debounce had
// already fired) is dropped on arrival rather than drawn beside the pointer
// wherever it is now.
let pointerInPlot = false;
$(document).on('mouseenter', '#PlotSimulation', function() { pointerInPlot = true; });
$(document).on('mouseleave', '#PlotSimulation', function() {
  pointerInPlot = false;
  hideHoverTip();
});

// auto-scroll to the bottom of the logs, and fix hover tooltip scroll position
$(document).on('shiny:value', function(event) {
  if (event.target.id === 'logContent' || event.target.id === 'profiling') {
    setTimeout(function() {
      var $logger = $('#' + event.target.id);
      $logger.scrollTop($logger[0].scrollHeight);
    }, 0);
  }
  if (event.target.id === 'hover_info') {
    setTimeout(function() {
      let tooltip = document.querySelector('#hover_info_box');
      if (tooltip && !pointerInPlot) {
        hideHoverTip();
      } else if (tooltip) {
        tooltip.style.left = (lastMouseX + 10) + 'px';
        tooltip.style.top = (lastMouseY + 10) + 'px';
        tooltip.style.display = '';
        hoverTipX = lastMouseX;
        hoverTipY = lastMouseY;
      } else {
        hoverTipX = null;
        hoverTipY = null;
      }
    }, 0);
  }
});

// Help tab (R/help-server.R).  Every link to a help page is an <a> with a
// data-help-page attribute, and every "load this scenario" button has
// data-help-scenario; one delegated handler each forwards them to Shiny, so
// the help needs no per-link inputs.
$(document).on('click', '[data-help-page]', function(e) {
  e.preventDefault();
  // A help link inside a modal (the investigator pop-up) closes it first
  if ($(this).closest('#shiny-modal').length) $('#shiny-modal').modal('hide');
  Shiny.setInputValue('help_goto', $(this).attr('data-help-page'), {priority: 'event'});
});
// An investigator's name (R/help-investigators.R) opens a pop-up about them
$(document).on('click', '[data-investigator]', function(e) {
  e.preventDefault();
  Shiny.setInputValue('investigator_show', $(this).attr('data-investigator'), {priority: 'event'});
});
$(document).on('click', '[data-help-scenario]', function(e) {
  e.preventDefault();
  Shiny.setInputValue('help_scenario_load', $(this).attr('data-help-scenario'), {priority: 'event'});
});
// A new help page starts at the top, whatever the scroll position of the last
$(document).on('shiny:value', function(event) {
  if (event.target.id === 'help_content') {
    window.scrollTo(0, 0);
    var main = document.querySelector('.help-main');
    if (main) main.scrollTop = 0;
  }
});

// When a modal opens, auto-focus on the first element that wants focus.  The
// Add a dose dialog opened empty (the Add new drug button) focuses its drug
// box instead, so the user can start typing a name at once.
$(document).on('shown.bs.modal', function() {
  var drug = document.getElementById('addDoseDrug');
  if (drug && drug.selectize && !drug.selectize.getValue()) {
    drug.selectize.focus();
    return;
  }
  $('.modal-body .modal-focusme:eq(0)').focus();
});

// Submit a modal when Enter is pressed while an input is focused (to enable,
// add an attribute `data-submit-btn` containing the ID of the submit button
// to the `modalDialog()` function)
$(document).on('keyup', '.modal-body input, .modal-body textarea', function(e) {
  if (e.key === 'Enter') {
    // ignore if Enter is pressed in a selectize or rhandsontable input
    if ($(this).closest('.selectize-input, .rhandsontable').length > 0) return;

    e.preventDefault();
    let modal = $(this).closest('.modal-body');
    let submitBtn = modal.attr('data-submit-btn');
    if (submitBtn) $('#' + submitBtn).click();
  }
});

function getCookie(name) {
  let match = document.cookie.match(new RegExp('(^| )' + name + '=([^;]+)'));
  return match ? match[2] : null;
}

function setCookie(name, value, days) {
  let date = new Date();
  date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
  document.cookie = name + '=' + value + '; expires=' + date.toUTCString() + '; path=/';
}

