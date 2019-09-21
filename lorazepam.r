lorazepam <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- 41
  k10 <- 0.00276
  k12 <- 0.04366
  k13 <- 0.0
  k21 <- 0.01738
  k31 <- 0.0
  k41 <- .693 / 1.2
  
  typical <- 200 # Barr/Zomorodi 2001, 2x midazolam
  upperTypical <- 80
  lowerTypical <- 240
  tPeak <- 10
  MEAC <- 0
  reference <- "Zomorodi, 1997"
  
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
