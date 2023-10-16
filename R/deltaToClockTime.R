# Convert delta time (x) from the reference time to an actual clock time
deltaToClockTime <- function(reference, x)
{
  if (reference == "none") {
      return(as.numeric(x))
  }
  start <- hourMinute(reference)
  x <- x + start
  xHours <- floor(x/60)
  xMinutes <- round(x-xHours * 60, 0)
  xHours <- xHours %% 24
  return(sprintf("%02d:%02d",xHours,xMinutes))
}
