naloxone <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1 <- 0.408 * weight
  v2 <- 0.636 * weight
  v3 <- 1.637 * weight
  cl1 <- 0.049 * weight
  cl2 <- 0.046 * weight
  cl3 <- 0.026 * weight
  
  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak = 1 # peaks quickly, just based on clinical observations
  MEAC <- 0
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Papathanasiou"
  
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
