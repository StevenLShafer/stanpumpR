pethidine <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- 18.1 / 70 * weight
  k10 <- 0.042131499
  k12 <- 0.300022713
  k13 <- 0.098461009
  k21 <- 0.089209873
  k31 <- 0.010774228
  
  tPeak <- 10
  MEAC <- 250/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Fit of Sven Bjorkman data"
  
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
