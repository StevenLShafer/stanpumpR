oxytocin <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  tPeak <- 1 # guess
  MEAC <- 0
  typical <-  0.1
  upperTypical <- 0.05
  lowerTypical <- 0.2
  reference <- "190912-135448" # Initial Eisenach Data
  v1	= 10.1 # l
  v2	= 7.03 # l
  v3 <- 1    # l
  cl1	= 0.974 # l/min
  cl2 = 0.204 # l/min
  cl3 <- 0 # l.min
  
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
