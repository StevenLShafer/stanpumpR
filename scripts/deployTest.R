# deploy stanpumpR to the test site (stanpumpR_test)
deployTest <- function(appFiles1 = appFiles)
{
  dir <- "g:/projects/stanpumpR"
  setwd(dir)
  rsconnect::deployApp(
    appDir = dir,
    appFiles = appFiles1,
    forceUpdate = TRUE,
    account = "steveshafer",
    appName = "stanpumpR_test"
  )
}
