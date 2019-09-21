# Deploy stanpumpR to the shiny server 
deployActive <- function()
{
  setwd("c:/google drive/projects/stanpumpR")
  library(rsconnect)
  deployApp(
    forceUpdate = TRUE, 
    appDir = "c:/google drive/projects/stanpumpR",
    appFiles = appFiles,
    account="steveshafer", 
    appName = "stanpumpR"
    )
}
