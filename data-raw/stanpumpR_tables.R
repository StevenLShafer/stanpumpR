library(devtools)
drugDefaults_global <- read.csv("data-raw/drugDefaults_global.csv", stringsAsFactors = FALSE, na.strings = "")
eventDefaults <- read.csv("data-raw/eventDefaults.csv", stringsAsFactors = FALSE)
use_data(drugDefaults_global, eventDefaults, internal = TRUE, overwrite = TRUE)
