naloxone <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: volumes and clearances all x weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- 0.408 * 70 * size$volume
  v2 <- 0.636 * 70 * size$volume
  v3 <- 1.637 * 70 * size$volume
  cl1 <- 0.049 * 70 * size$clearance
  cl2 <- 0.046 * 70 * size$clearance
  cl3 <- 0.026 * 70 * size$clearance
  
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
  
  tPeak = 1 # peaks quickly, just based on clinical observations
  MEAC <- 0
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Papathanasiou T et al., Br J Anaesth 2019;123(2):e204-e214. https://pubmed.ncbi.nlm.nih.gov/30915992/"
  
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
