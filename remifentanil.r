remifentanil <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  lbm <- lbmJames(weight, height, sex)
  v1 <- 5.1-0.0201*(age-40)+0.072*(lbm-55)
  v2 <- 9.82-0.0811*(age-40)+0.108*(lbm-55)
  v3 <- 5.42
  cl1 <- 2.6-0.0162*(age-40)+0.0191*(lbm-55)
  cl2 <- 2.05-0.0301*(age-40)
  cl3 <- 0.076-0.00113*(age-40)
  
  default = list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  infusate_concentration <- 50
  awake_concentration <- 1	# Desired Cp on emergence
  weightAdjust <- TRUE
  tPeak = 1.6 # Opioid simulation spreadsheet
  MEAC <- 1
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Minto/Schnider"
  
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
