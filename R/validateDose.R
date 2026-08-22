# Validate dose. Dose is returned as a character string
# Routine designed to accept pretty much anything and not return an error.
validateDose <- function(x)
{
  if (length(x) > 1 || is.list(x)) {
    stop("validateDose can only accept single items.")
  }
  if (is.null(x) || is.na(x) || is.nan(x)) {
    x <- ""
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.numeric(x)) {
    x <- format(x, scientific = FALSE)
  }

  # Remove everything except digits and first decimal point
  x <- gsub("[^0-9.]", "", x)
  parts <- strsplit(x, "\\.")[[1]]
  if (length(parts) > 1) {
    x <- paste(parts[1], paste(parts[-1], collapse = ""), sep = ".")
  }
  if (x == ".") {
    return("0")
  }
  if (x == "") {
    return("0")
  }
  return(x)
}
