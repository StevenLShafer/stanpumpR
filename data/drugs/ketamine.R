ketamine <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- .063 * weight
  k10 <- 0.438
  k12 <- 0.592
  k13 <- 0.590
  k21 <- 0.247
  k31 <- 0.0146
  
  typical <- 0.12  # Moen 2013 review
  upperTypical <- 0.1 # mcg/ml - Moen 2013 review
  lowerTypical <- 0.16 # mcg/ml - Moen 2013 review
  tPeak <- 3 # Just a guess
  MEAC <- 0
  reference <- "Clin Pharmacol Ther 199136:645-653"
  
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
