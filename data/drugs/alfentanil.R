alfentanil <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  default <- list(
    v1 = 2.1853,
    v2 = 6.698864,
    v3 = 14.52582,
    cl1 = 0.1988623,
    cl2 = 1.433557,
    cl3 = 0.2469389
  )  
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 1.4		# see t_peaks.xls
  MEAC <- 39
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
