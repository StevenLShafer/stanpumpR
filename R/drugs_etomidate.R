etomidate <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1  <- .090 * weight
  k10 <- 0.205
  k12 <- 0.284
  k13 <- 0.209
  k21 <- 0.131
  k31 <- 0.00479
  k41 <- .693/1.6
  
  typical <- 0.5  # Journal of Clinical Pharmacology, 2011;51:482-491
  upperTypical <- 0.4 # Journal of Clinical Pharmacology, 2011;51:482-491
  lowerTypical <- 0.8 # Journal of Clinical Pharmacology, 2011;51:482-491
  tPeak <- 1.6
  MEAC <- 0
  reference <- "Anesthesiology 1986 65:19-27"
  
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
