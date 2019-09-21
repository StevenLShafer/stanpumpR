fentanyl <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  default <- list(
  v1 = 12.7,
  v2 = 49.34479,
  v3 = 296.8831,
  cl1 = 0.7112,
  cl2 = 4.7371,
  cl3 = 2.286
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 3.694		# from Shafer/Varvel, t_peaks.xls
  MEAC <- 0.6
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <-  "JPET 1987240:159-166"
  
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
