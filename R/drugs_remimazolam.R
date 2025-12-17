remimazolam <- function(weight, height, age, sex)
{
  # Eleveld British Journal of Anaesthesia, 135 (1): 206e217 (2025)
  # Units **************
  # Time: Hours
  # Volume: Liters

  Fsize <- weight / 70
  Kv3_age <-  7.31
  Kcl1_sex <- 16.3
  Kv3_sex <- 28.7

  # Ignored for now
  Kcl1_opiates <- 13.9
  Kv3_Pugh  <- 82.4

  Fv3_age <- exp(Kv3_age/1000 * (age-35))
  if (sex == "female")
  {
    Fcl1_sex <- exp(Kcl1_sex/100)
    Fv3_sex <- exp(Kv3_sex / 100)
  } else {
    Fcl1_sex <- 1
    Fv3_sex <- 1
  }

  v1 <- 4.31 * Fsize
  v2 <- 12.3 * Fsize
  v3 <- 18.6 * Fsize * Fv3_age * Fv3_sex
  cl1 <- 1.12 * Fsize ** 0.75
  cl2 <- 1.45 * (v2 / 12.3) ** 0.75
  cl3 <- 0.298 * (v3 /18.6) ** 0.75

# these are returned but not used except tPeak
# those from the drugDefaults table are

  typical <- NA
  upperTypical <- NA
  lowerTypical <- NA
  MEAC <- NA
  reference <- "Eleveld 2025"

  tPeak <- 2.5

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
