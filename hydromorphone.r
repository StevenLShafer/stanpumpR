hydromorphone <- function(weight, height, age, sex)
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
  
  v2 <- v1 * k12 / k21
  v3 <- v1 * k13 / k31
  cl1 <- v1 * k10
  cl2 <- v1 * k12
  cl3 <- v1 * k13
  
  ka_PO <- 0.01 # Selected to get the right Cmax with the Lamminsalo PK to match the mandema curves
  bioavailability_PO <- 0.6 # Seems to be the most consistent number (0.6 is most consistent, but levels seem high)

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = 0,
    ka_IM = ka_PO,
    bioavailability_IM = bioavailability_PO,
    tlag_IM = 90,
    ka_IN = ka_PO,
    bioavailability_IN = bioavailability_PO,
    tlag_IN = 180
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
