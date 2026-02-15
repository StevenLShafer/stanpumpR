$(document).on('shiny:connected', function(event) {
  var timeNow = new Date().toLocaleTimeString();
  Shiny.setInputValue("client_time", timeNow);
});


$(document).on('shiny:value', function(event) {
  if (event.target.id === 'logContent' || event.target.id === 'profiling') {
    setTimeout(function() {
      var $logger = $('#' + event.target.id);
      $logger.scrollTop($logger[0].scrollHeight);
    }, 0);
  }
});

// When a modal opens, auto-focus on the first element that wants focus
$(document).on('shown.bs.modal', function() {
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
