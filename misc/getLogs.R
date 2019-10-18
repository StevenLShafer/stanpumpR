# Get logs from the shiny server
getLogs <- function()
{
  cat("Fetching log for stanpumpr\n")
  logdata <- capture.output(showLogs(appName = "stanpumpr", entries = 50000))
  fileConn<-file("log.txt")
  writeLines(logdata, fileConn)
  close(fileConn)
  cat("log written to log.txt\n")
}
