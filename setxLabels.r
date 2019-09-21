# Set xLabels for the plot to something that looks nice
setxLabels <- function (min,max,step)
{
  if (max <= min) return(0)
  max <- ceiling(max/30)*30
  result <- 0:ceiling(max/step) * step
  if (length(result) > 3 & length(result) < 11) return(result)
  result <- 0:ceiling(max/15) * 15
  if (length(result) > 3 & length(result) < 8) return(result)
  result <- 0:ceiling(max/30) * 30
  if (length(result) > 3 & length(result) < 8) return(result)
  result <- 0:ceiling(max/60) * 60
  if (length(result) > 3 & length(result) < 8) return(result)
  result <- 0:ceiling(max/120) * 120
  if (length(result) > 3 & length(result) < 8) return(result)
  result <- 0:ceiling(max/120) * 240
  return(result)
}
