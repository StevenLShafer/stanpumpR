oliceridine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Hours
  # Volume: Liters

  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: no size scaling, the parameters were used as published.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1 <- 28   * size$volume
  v2 <- 29.1 * size$volume
  v3 <- 1 #NA
  cl1 <- 31.7 / 60 * size$clearance
  cl2 <- 37.5 / 60 * size$clearance
  cl3 <- 0 #NA

# these are returned but not used except tPeak
# those from the drugDefaults table are

  typical <- NA
  upperTypical <- NA
  lowerTypical <- NA
  MEAC <- NA
  reference <- "Dahan A et al., Anesthesiology 2020;133(3):559-568. https://pubmed.ncbi.nlm.nih.gov/32788558/"

  # Effect site.  Dahan 2020's library MEAC (27.9 ng/mL, drugDefaults) is the
  # concentration that doubled cold-pressor tolerance, an analgesic end point,
  # and for that end point the paper found oliceridine's equilibration too fast
  # to estimate.  Its 0.25 h is the t1/2 ke0 of the ventilatory response, not
  # a time to peak; read as one until 2026-10-12 it gave tPeak 15 min (as a
  # t1/2 ke0 on this disposition it would give 26 min).  The tPeak now carries
  # "too fast to measure" as a t1/2 ke0 of 1 min, which peaks a bolus at
  # 4.4 min on this disposition: a stated assumption, not a measurement.
  # Set by decision of Steven L. Shafer, 2026-10-12, in the opioid MEAC and
  # tPeak audit.
  tPeak <- 4.4

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
