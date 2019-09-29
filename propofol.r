propofol <- function(weight = 70, height = 171, age = 50, sex = "man")
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  default <- list(
    v1 = 4.27,
    v2 = 18.9-0.391*(age-53),
    v3 = 238,
    cl1 = 1.89+0.0456*(weight-77)-0.0681*(lbmJames(weight, height, sex)-59)+0.0264*(height-177),
    cl2 = 1.29-0.024*(age-53),
    cl3 = 0.836
  )
  
  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 1.600 # Anesthesiology 90:1502-1516, 1999
  # typical <- 3
  # upperTypical <- 4.0
  # lowerTypical <- 2.5
  # MEAC <- 0
  reference <- "Anesthesiology 1998"
  
  return(
    list(
      PK = PK,
      tPeak = tPeak,
      reference = reference
    )
  )
}

