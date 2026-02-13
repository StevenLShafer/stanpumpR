$(document).on('shiny:connected', function(event) {
  var timeNow = new Date().toLocaleTimeString();
  Shiny.setInputValue("client_time", timeNow);
});


$(document).on('shiny:value', function(event) {
  if (event.target.id === 'logContent') {
    setTimeout(function() {
      var $logger = $('#logContent');
      $logger.scrollTop($logger[0].scrollHeight);
    }, 0);
  }
});

// When a modal opens, auto-focus on the first element that wants focus
$(document).on('shown.bs.modal', function() {
  $('.modal-body .modal-focusme:eq(0)').focus();
});
