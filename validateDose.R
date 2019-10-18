# Validate dose. Dose is returned as a character string
# Routine designed to accept pretty much anything and not return an error.
validateDose <- function(x)
{
  if (is.null(x) | is.na(x) | is.nan(x)) x <- ""
  if (is.factor(x) | is.numeric(x)) x <- as.character(x)
  # cat("dose to validate",x,"\n")
  # cat(str(x))
  # cat("\n")
  # Remove everything except digits, and decimal point
  x <- gsub("[^[:digit:].]","",x)
  if (x == "") return("")
  # Remove second instance of a decimal point
  x <- sapply(strsplit(x, "[.]"), function(y) 
    if(length(y) >1) paste(y[1], paste(y[-1], collapse=""), sep=".") else y)
  # Return 0 if nothing is left
  if (gsub("[.]","",x) == "") return("0")
  return(x)
}
