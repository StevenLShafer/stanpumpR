# Convert clock times (x) to difference from the reference time
clockTimeToDelta <- function(reference, x) {
  if (reference == "none") {
    FIX <- grepl(":",x)
    x[FIX] <- as.numeric(unlist(lapply(x[FIX],FUN = hourMinute)))
    x <- as.numeric(x)
    return(x)
  }
  start <- hourMinute(reference)
  if (is.na(start)) return(NA)
  FIX <- grepl(":",x)
  x[FIX] <- as.numeric(unlist(lapply(x[FIX],FUN = hourMinute))) - start
  x <- as.numeric(x)
  x[x < 0] <- x[x < 0] + 1440 # Wrap around midnight
  x
}
