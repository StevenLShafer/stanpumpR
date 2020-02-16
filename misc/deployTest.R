# deploy stanpumpR to the test site (stanpumpR_test)
deployTest <- function(appFiles)
{
  dir <- "c:/google drive/projects/stanpumpR"
  setwd(dir)
  rsconnect::deployApp(
    appDir = dir,
    appFiles = appFiles,
    forceUpdate = TRUE,
    account = "steveshafer",
    appName = "stanpumpR_test"
  )
}
