$(document).on('shiny:connected', function(event) {
  var timeNow = new Date().toLocaleTimeString();
  Shiny.setInputValue("client_time", timeNow);
});


$(document).on('shiny:value', function(event) {
  if (event.target.id === 'logContent') {
    setTimeout(function() {
      var $logger = $('#logSection');
      $logger.scrollTop($logger[0].scrollHeight);
    }, 0);
  }
});

$(document).on('shown.bs.modal', function() {
  $('.modal-body .modal-focusme:eq(0)').focus();
});
