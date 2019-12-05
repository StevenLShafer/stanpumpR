$(document).on('shiny:sessioninitialized', function(event) {
  var isChrome = /Chrome/.test(navigator.userAgent);
  var isFirefox = /Firefox/.test(navigator.userAgent);
  var message = {data : [isChrome, isFirefox] };
  Shiny.onInputChange("check", message);
});