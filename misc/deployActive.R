# Deploy stanpumpR to the shiny server
deployActive <- function(appFiles)
{
  dir <- "c:/google drive/projects/stanpumpR"
  setwd(dir)
  rsconnect::deployApp(
    appDir = dir,
    appFiles = appFiles,
    forceUpdate = TRUE,
    account = "steveshafer",
    appName = "stanpumpR"
  )
}
