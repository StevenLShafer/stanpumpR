# Convert clock times (x) to difference from the reference time
clockTimeToDelta <- function(reference, x)
{
  start <-unlist(strsplit(reference, ":")) # Remove the colon
  startHours <- as.numeric(start[1])
  startMinute <- as.numeric(start[2])
  start <- startHours * 60 + startMinute
  FIX <- grepl(":",x)
  x[FIX] <- as.numeric(unlist(lapply(x[FIX],FUN = hourMinute))) - start
  x <- as.numeric(x)
  x[x < 0] <- x[x < 0] + 1440 # Wrap around midnight
  return(x)
}
# test with
# clockTimeToDelta("08:00", c("7", "09:00","10:15", "06:00"))
