sufentanil <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  default <- list(
    v1 = 14.3,
    v2 = 63.38694,
    v3 = 251.9,
    cl1 = 0.92235,
    cl2 = 1.55298,
    cl3 = 0.32747
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 5.8		# from Shafer/Varvel, t_peaks.xls
  MEAC <- 0.056
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Anesthesiology 1995 83:1194-1204"
  
  return(
    list(
      PK = PK, 
      tPeak = tPeak, 
      MEAC = MEAC, 
      typical = typical, 
      upperTypical = upperTypical, 
      lowerTypical = lowerTypical, 
      reference = reference
    )
  )
}
