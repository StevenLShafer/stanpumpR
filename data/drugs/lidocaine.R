lidocaine <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1 <- 0.088 * weight
  k10 <- 0.227273
  k12 <- 0.636364
  k13 <- 0.0
  k21 <- 0.14
  k31 <- 0.0
  
  typical <- 1
  upperTypical <- 1.5
  lowerTypical <- 0.5
  tPeak <- 5
  MEAC <- 0
  reference <- "Schnider?"
  
  v2 <- v1 * k12 / k21
  v3 <- 1
  cl1 <- v1 * k10
  cl2 <- v1 * k12
  cl3 <- 0
  
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
