# deploy stanpumpR to the test site (stanpumpR_test)
library(here)
library(yaml)

getVariable <- function(variableName) {
  envVariable <- Sys.getenv(variableName)
  if (is.na(envVariable) || envVariable == "") {
    stop(paste(variableName, "is empty"))
  }
  envVariable
}

writeShinyConfig <- function(title, emailUsername, emailPassword) {
  shinyConfig <- list(
    default = list(
      title = title,
      email_username = emailUsername,
      email_password = emailPassword
    )
  )

  write_yaml(shinyConfig, "config.yml", fileEncoding = "UTF-8")
}

deployShinyApp <- function(account, appName) {
  appDir <- here()

  appFiles <- dir()
  appFiles <- appFiles[grepl("\\.", appFiles)]
  appFiles <- c(appFiles, "R","data","www", "misc")

  print(paste("Deploying app:", appName))
  print(paste("Directory:", appDir))
  print(paste("AppFiles:", appFiles))

  rsconnect::deployApp(
    appDir = appDir,
    appFiles = appFiles,
    forceUpdate = TRUE,
    account = account,
    appName = appName,
    launch.browser = FALSE,
  )
}

loginAndDeploy <- function()
{
  account <- getVariable("SHINY_ACCOUNT")
  token <- getVariable("SHINY_TOKEN")
  secret <- getVariable("SHINY_SECRET")
  appName <- getVariable("SHINY_APP_NAME")
  emailUsername <- getVariable("SHINY_CONFIG_EMAIL_USERNAME")
  emailPassword <- getVariable("SHINY_CONFIG_EMAIL_PASSWORD")

  writeShinyConfig("stanpumpR", emailUsername, emailPassword)
  rsconnect::setAccountInfo(name=account, token=token, secret=secret)
  deployShinyApp(account, appName)
}

loginAndDeploy()
