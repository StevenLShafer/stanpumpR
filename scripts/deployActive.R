# Deploy stanpumpR to the shiny server
deployActive <- function(appFiles1 = appFiles)
{
  dir <- "g:/projects/stanpumpR"
  setwd(dir)
  rsconnect::deployApp(
    appDir = dir,
    appFiles = appFiles1,
    forceUpdate = TRUE,
    account = "steveshafer",
    appName = "stanpumpR"
  )
}
