# Validate time. Time can be entered either as a scaler, or as HHSS, or as HH:SS. 
# Routine designed to accept pretty much anything and not return an error.
validateTime <- function(x)
{
  # cat("x = ",x,"\n")
  # cat("is.null(x)"     , is.null(x),"\n")
  # cat("is.na(x)"       , is.na(x),"\n")
  # cat("is.numeric(x)"   , is.numeric(x),"\n")
  # cat("is.character(x)", is.character(x),"\n")
  if (is.null(x) | is.na(x) | is.nan(x)) x <- ""
  if (is.factor(x) | is.numeric(x)) x <- as.character(x)
  # Remove everything except digits, colon, and decimal point
  x <- gsub("[^[:digit:]:.]","",x)
  if (x == "") return("")
  # Remove second instance of a decimal point
  x <- sapply(strsplit(x, "[.]"), function(y) 
    if(length(y) >1) paste(y[1], paste(y[-1], collapse=""), sep=".") else y)
  # Remove second instance of a colon
  x <- sapply(strsplit(x, "[:]"), function(y) 
    if(length(y) >1) paste(y[1], paste(y[-1], collapse=""), sep=":") else y)
  # Return 0 if nothing is left
  if (gsub("[:.]","",x) == "") return("0")
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
