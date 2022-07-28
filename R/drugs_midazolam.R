midazolam <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  default <- list(
  v1 = 3.3,
  v2 = 17.56348,
  v3 = 96.75715,
  cl1 = 0.5351973,
  cl2 = 2.014531,
  cl3 = 0.8321115
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  reference <- "Clin Pharmacol Ther. 1995 Jul;58(1):35-43, and Barr/Zomorodi 2001"
  typical <- .100 
  upperTypical <- .040
  lowerTypical <- .120
  MEAC <- 0
  tPeak <- 4
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
