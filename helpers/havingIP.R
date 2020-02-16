# Check if the system has a valid IP number
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    cmd <- "ipconfig"
  } else {
    cmd <- "ifconfig"
  }
  ipmessage <- system(cmd, intern = TRUE)
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}

ping <- function(url) {
  tryCatch({
    result <- httr::GET(url)
    httr::status_code(result) == 200
  }, error = function(e) {
    FALSE
  })
}

# Check whether or not the current machine has a functioning internet connection
checkConnection <- function() {
  havingIP() && ping("https://www.google.com")
}
