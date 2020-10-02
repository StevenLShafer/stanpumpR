oxycodone <- function(weight, height, age, sex)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # IV PK from Lamminsalo, 2019

  # From Table 2:
  THETA1 <- 37.4 # (litre/h) Clearance
  THETA2 <- 90.2 # (litre) Central volume of distribution
  THETA3 <- 68.9 # (litre) Peripheral volume of distribution
  THETA4 <- 0.035 # (litre) CSF Volume of distribution
  THETA5 <- 206 # (litre/h) Intercompartmental clearance
  THETA6 <- 0.0537 # (litre/h) Clearance - V1 to CSF volume
  THETA7 <- 0.749 # (1/h) Transfer rate epidural to central compartment
  THETA8 <- 0.0886 # (1/h) Transfer rate epidural to CSF compartment
  THETA9 <- 0.00516 # (litre/h)  Intercompartmental rate between CSF and CSF peripheral
  THETA10 <- 0.0385 # (litre)  CSF peripheral volume of distribution

  v1  <- THETA2 # liters
  v2  <- THETA3 # Liters
  v3  <- 1 # no third compartment
  cl1 <- THETA1 # l/h
  cl2 <- THETA5 # l/h
  cl3 <- 0

  cl1 <- cl1 / 60 # l/min
  cl2 <- cl2 / 60 # l/min

  tPeak <- 60 # Peak CSF concentration following IV administration in Lamminsalo is at 1 hour
  MEAC <- 12   # A compromise between the lower values suggested by Mandema, figure 5, and the value of 45-50 suggested by kokki 2012
  typical <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference <- "Lamminsalo 2019"

  ka_PO = 0.22 # 1/h # From Mandema
  ka_PO = ka_PO / 60 # 1/min
  # Mandema shows a peak at 45 minutes
  # Olesen 2013 shows a peak at 30
  # ka_ chosen to be close to Olesen

  ka_PO <- 0.06 # Selected to get the right Cmax with the Lamminsalo PK to match the mandema curves
  bioavailability_PO <- 0.5 # Seems to be the most consistent number (0.6 is most consistent, but levels seem high)
  tlag_PO <- 0


  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
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
