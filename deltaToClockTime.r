# Convert delta time (x) from the reference time to an actual clock time
deltaToClockTime <- function(reference, x)
{
  start <-unlist(strsplit(reference, ":")) # Remove the colon
  startHours <- as.numeric(start[1])
  startMinute <- as.numeric(start[2])
  start <- startHours * 60 + startMinute
  x <- x + start
  xHours <- floor(x/60)
  xMinutes <- round(x-xHours * 60, 0)
  xHours <- xHours %% 24
  return(sprintf("%02d:%02d",xHours,xMinutes))
}
