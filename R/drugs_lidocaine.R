lidocaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1Ref <- 0.088 * 70   # 0.088 L/kg at the 70 kg reference
  k10 <- 0.227273
  k12 <- 0.636364
  k13 <- 0.0
  k21 <- 0.14
  k31 <- 0.0
  
  typical <- 1
  upperTypical <- 1.5
  lowerTypical <- 0.5
  tPeak <- 5
  MEAC <- 0
  reference <- "Schnider TW et al., Anesthesiology 1996;84(5):1043-1050. https://pubmed.ncbi.nlm.nih.gov/8623997/"
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: V1 proportional to weight with fixed rate
  # constants, so volumes and clearances both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1Ref * size$volume
  v2 <- v1Ref * k12 / k21 * size$volume
  v3 <- 1
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
  cl3 <- 0
  
  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    # Regional anesthesia (RA): a dose injected into tissue, absorbed
    # first-order into the central compartment.  ka_RA is the single rate
    # that best reproduces, with this disposition and F = 1, the arterial
    # plasma curve after 600 mg of 1.5% lidocaine with epinephrine 5 mcg/mL
    # for axillary block (Simon et al. 2002: mean peak 2.87 mcg/mL at 25.8
    # min), as a two-rate input fitted to the published means (24% at
    # 0.0903/min, 76% at 0.0088/min, on the Burm 1987 intravenous tracer
    # disposition) describes it from 5 to 180 min: weighted least squares,
    # each residual divided by the concentration + 0.3 mcg/mL.  0.0111/min is
    # an absorption half-time of 62 min, and predicts a peak of 2.76 mcg/mL
    # at 42 min.  Simon's own one-rate half-time, 8.4 min, does not transfer
    # to an independent intravenous model: it predicts a peak of 6.9
    # mcg/mL.  Axillary block with epinephrine only; other sites and plain
    # solutions absorb differently, and F = 1 is an assumption (the label
    # describes complete absorption after parenteral injection; no
    # site-specific absolute bioavailability has been measured).  (Claude
    # Code, 2026-10-10, at the request of Steven L. Shafer.)
    ka_RA = 0.0111,
    bioavailability_RA = 1,
    tlag_RA = 0
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
