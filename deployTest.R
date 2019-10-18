# deploy stanpumpR to the test site (stanpumpR_test)
deployTest <- function()
{
  setwd("c:/google drive/projects/stanpumpR")
  library(rsconnect)
  deployApp(
    appDir = "c:/google drive/projects/stanpumpR",
    appFiles = appFiles,
    forceUpdate = TRUE, 
    account="steveshafer", 
    appName = "stanpumpR_test"
  )
}
