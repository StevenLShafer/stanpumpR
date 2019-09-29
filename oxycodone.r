oxycodone <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- 0.16 * weight
  k10 <- 0.116
  k12 <- 0.3
  k13 <- 0.08
  k21 <- 0.03
  k31 <- 0.00095
  
  tPeak <- 19.6
  MEAC <- 1.5/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Drover PK"
  ka = 0.1
  bioavailability <- 0.5
  
  v2 <- v1 * k12 / k21
  v3 <- v1 * k13 / k31
  cl1 <- v1 * k10
  cl2 <- v1 * k12
  cl3 <- v1 * k13
  
  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka = ka,
    bioavailability = bioavailability
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
