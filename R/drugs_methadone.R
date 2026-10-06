methadone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1Ref <- 0.18 * 70   # 0.18 L/kg at the 70 kg reference
  k10 <- 0.00806
  k12 <- 0.408
  k13 <- 0.112
  k21 <- 0.06
  k31 <- 0.00114
  
  tPeak <- 11.3
  MEAC <- 60/1000
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Inturrisi CE et al., Clin Pharmacol Ther 1987;41(4):392-401. https://pubmed.ncbi.nlm.nih.gov/3829576/"
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: V1 proportional to weight with fixed rate
  # constants, so volumes and clearances both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1Ref * size$volume
  v2 <- v1Ref * k12 / k21 * size$volume
  v3 <- v1Ref * k13 / k31 * size$volume
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
  cl3 <- v1Ref * k13 * size$clearance
  
  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )
  
  events <- c(PK_EVENT_DEFAULT)
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
