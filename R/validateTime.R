# Validate time. Time can be entered either as a scaler, or as HHSS, or as HH:SS.
# Routine designed to accept pretty much anything and not return an error.
validateTime <- function(x)
{
  if (length(x) > 1) {
    stop("validateTime can only accept single items, not vectors.")
  }
  if (is.null(x) || is.na(x) || is.nan(x)) {
    x <- ""
  }
  if (is.factor(x) || is.numeric(x)) {
    x <- as.character(x)
  }
  # Remove everything except digits, colons, and decimal points
  x <- gsub("[^0-9:.]","",x)
  if (x == "") {
    return("0")
  }
  # Remove all decimal points and colons except the first ones
  parts <- strsplit(x, "\\.")[[1]]
  if (length(parts) > 1) {
    x <- paste(parts[1], paste(parts[-1], collapse = ""), sep = ".")
  }
  parts <- strsplit(x, ":")[[1]]
  if (length(parts) > 1) {
    x <- paste(parts[1], paste(parts[-1], collapse = ""), sep = ":")
  }
  if (x == ".:" || x == ":.") {
    return("0")
  }

  # If there is a period, remove colon and return
  if (as.numeric(regexpr("[.]", x) > -1)) return (gsub("[^[:digit:].]","",x))
  colonPosition <- as.numeric(regexpr("[:]", x))
  if (colonPosition == -1 & nchar(x) >= 4) # No colon, 4 or more characters means the colon was omitted
    x <- paste0(substr(x,1,2),":",substr(x,3,4))
  # If there is a colon, ensure HH:MM format
  colonPosition <- as.numeric(regexpr("[:]", x))
  if (colonPosition > -1)
  {
    HH <- as.numeric(substr(x,1,colonPosition-1))
    if (is.na(HH)) HH <- 0
    MM <- as.numeric(substr(x,colonPosition+1, 100))
    if (is.na(MM)) MM <- 0
    # force 80 minutes into 1 hour and 20 minutes
    HH <- HH + floor(MM/60)
    MM <- MM %% 60
    x <- sprintf("%02d:%02d", HH, MM)
  }
  return(x)

}

getReferenceTime <- function(time) {
  time <- gsub("[^[:digit:]:. APM]","",time) # Get rid of strange formatting characters
  x <- unlist(strsplit(time, " "))
  if (length(x) == 1) x <- c(x, "AM") # European time doesn't use PM
  y <- unlist(strsplit(x[1],":"))
  time <- (as.numeric(y[1]) + 12 * (x[2] == "PM")) * 60 + as.numeric(y[2]) - 60
  if (time < 0) time <- time + 1440
  time <- floor(time / 15) * 15
  HH   <- floor(time / 60)
  MM   <- time %% 60
  start <- sprintf("%02d:%02d",HH,MM)
  start
}
