# Check if the system has a valid IP number
havingIP <- function() {
  if (.Platform$OS.type == "windows") {
    ipmessage <- system("ipconfig", intern = TRUE)
  } else {
    ipmessage <- system("ifconfig", intern = TRUE)
  }
  validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  any(grep(validIP, ipmessage))
}
ping <- function(x, stderr = FALSE, stdout = FALSE, ...){
  pingvec <- system2("ping", x,
                     stderr = FALSE,
                     stdout = FALSE,...)
  if (pingvec == 0) TRUE else FALSE
}