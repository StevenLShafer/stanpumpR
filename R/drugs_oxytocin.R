oxytocin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  # adjustToFFM is accepted for a uniform signature but deliberately not used.
  # The human model was fitted in parturients, so the 70 kg reference male that
  # anchors the fat-free-mass scaling does not describe the population these
  # parameters belong to, and the Al-Sallami formula has not been validated in
  # pregnancy.  The rat model has no meaningful height.

  if (weight > 1)
  {
    # Human

    tPeak <- 5 # guess
    MEAC <- 0
    typical <-  0.1
    lowerTypical <- 0.05
    upperTypical <- 0.2
    reference <- "Eisenach (unpublished data)" # Initial Eisenach Data
    v1	<- 10.1 # l
    v2	<- 7.03 # l
    v3  <- 1    # l
    cl1	<- 0.974 # l/min
    cl2 <- 0.204 # l/min
    cl3 <- 0     # l.min
  } else {
    # Rat
    tPeak <- 5 # guess
    MEAC <- 0
    typical <-  0.1
    lowerTypical <- 0.05
    upperTypical <- 0.2
    # V1 = 0.10 L, V2 = 0.02 L, CL1 = 0.017 L/min, and CL2 = 0.000524
    reference <- paste0( # Paper using LC/MS Assay
      "Tanaka A et al. Delivery of oxytocin to the brain for the treatment of ",
      "autism spectrum disorder by nasal application. Mol Pharm ",
      "2018;15(3):1105-1111. PMID 29338251."
    )
    # V1 = 0.10 L, V2 = 0.02 L, CL1 = 0.017 L/min, and CL2 = 0.000524
    v1	<- 0.10 # l
    v2	<- 0.02 # l
    v3  <- 1    # l
    cl1	<- 0.017    # l/min
    cl2 <- 0.000524 # l/min
    cl3 <- 0        # l.min
  }

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
