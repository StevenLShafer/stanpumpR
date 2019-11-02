drugDefaults_global <- read.csv("data/Drug Defaults.csv", stringsAsFactors = FALSE, na.strings = "")
outputString <-""
con <- textConnection("outputString","w",local=TRUE)
capture.output(print(drugDefaults_global[,1:6], digits = 3), file = con, type="output", split = FALSE)
close(con)
outputString[1]
length(outputString)
outputString
methods(print.data.frame)


test <- "https://steveshafer.shinyapps.io/stanpumpr/?_inputs_&addedPlots=null&age=50&ageUnit=%221%22&caption=%22%22&client_time=%228%3A29%3A11%20AM%22&cyp2d6=%22typical%22&effectsiteLinetype=%22solid%22&height=66&heightUnit=%222.56%22&logY=false&maximum=%2260%22&normalization=%22none%22&plasmaLinetype=%22blank%22&pregnant=%22FALSE%22&referenceTime=%2207%3A15%22&Refresh=0&renal=%22normal%22&sex=%22female%22&shinyjs-delay-37cf8b81f5b4e48bcb9314dd6f57a20f=0&shinyjs-delay-6fc4999e65fc46adf300b7a8dbcf6843=0&shinyjs-delay-9cb58e84073749f54fb68a0032d704ac=0&shinyjs-delay-a9e776a00fda54ca3c1d98c97438588e=0&shinyjs-delay-b1ad173232c41199eea61b91d4092b3a=0&shinyjs-delay-cb21d905b667b9307d8eafb0b7dbc980=0&title=%22Simulation%20on%202019-10-23%2012%3A21%3A54%22&typical=%22Range%22&weight=60&weightUnit=%221%22&_values_&DT=%7B%22Drug%22%3A%5B%22propofol%22%2C%22fentanyl%22%2C%22remifentanil%22%2C%22rocuronium%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%5D%2C%22Time%22%3A%5B%220%22%2C%220%22%2C%220%22%2C%220%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%5D%2C%22Dose%22%3A%5B%220%22%2C%220%22%2C%220%22%2C%220%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%5D%2C%22Units%22%3A%5B%22mg%22%2C%22mcg%22%2C%22mcg%2Fkg%2Fmin%22%2C%22mg%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%5D%7D&ET=%7B%22Time%22%3A%5B%5D%2C%22Event%22%3A%5B%5D%2C%22Fill%22%3A%5B%5D%7D"
test <- strsplit(test,"&")
test <- test[grep("shinyjs",test)]


gsub("&shinyjs-delay-.{34}", "",test)
nchar("b1ad173232c41199eea61b91d4092b3a")
