oliceridine <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Hours
  # Volume: Liters

  v1 <- 28
  v2 <- 29.1
  v3 <- 1 #NA
  cl1 <- 31.7 / 60
  cl2 <- 37.5 / 60
  cl3 <- 0 #NA

# these are returned but not used except tPeak
# those from the drugDefaults table are

  typical <- NA
  upperTypical <- NA
  lowerTypical <- NA
  MEAC <- NA
  reference <- "Dahan 2020"

  tPeak <- 0.25*60

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
