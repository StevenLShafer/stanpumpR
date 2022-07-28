library(devtools)
drugDefaults_global <- read.csv("data-raw/Drug Defaults.csv", stringsAsFactors = FALSE, na.strings = "")
use_data(drugDefaults_global)
