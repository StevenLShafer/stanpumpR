library(devtools)
library(openxlsx)
drugDefaults_global <- read.csv("data-raw/drugDefaults_global.csv", stringsAsFactors = FALSE, na.strings = "")
eventDefaults <- read.csv("data-raw/eventDefaults.csv", stringsAsFactors = FALSE)
use_data(drugDefaults_global, eventDefaults, internal = TRUE, overwrite = TRUE)


#drugDefaults_global <- read.xlsx("data/temp.xlsx")
#write.xlsx(drugDefaults_global, file = "data/temp.xlsx")
#write.csv(drugDefaults_global, file = "data/temp.csv")

