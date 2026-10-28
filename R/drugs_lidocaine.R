lidocaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Schnider 1996, Table 1, parameter set C: the final estimates from all 13
  # patients (the abstract's), per kg of total body weight, taken at the 70 kg
  # reference.  Until 2026-10-28 the library used set B (V1 0.088, V2 0.4,
  # Cl1 0.02, Cl2 0.056), the interim set fitted to the first 8 patients and
  # tested prospectively on the last 5.
  v1Ref  <- 0.101  * 70   # L
  v2Ref  <- 0.452  * 70   # L
  cl1Ref <- 0.0215 * 70   # L/min
  cl2Ref <- 0.0589 * 70   # L/min
  
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
  v2 <- v2Ref * size$volume
  v3 <- 1
  cl1 <- cl1Ref * size$clearance
  cl2 <- cl2Ref * size$clearance
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
    # plasma means after 600 mg of 1.5% lidocaine with epinephrine 5 mcg/mL
    # for axillary block (Simon et al. 2002, Table 2: 17 means from 5 to 180
    # min, largest 2.94 mcg/mL at 40 min; Table 3's mean of individual peaks
    # is 2.87 at 25.8 min): weighted least squares, each residual divided by
    # the concentration + 0.3 mcg/mL.  0.0124/min is an absorption half-time
    # of 56 min, and predicts a peak of 2.73 mcg/mL at 41 min.  (Refitted
    # 2026-10-28 for set C.  The set B rate, 0.0111/min, came from a two-rate
    # input fitted on the Burm 1987 intravenous tracer disposition; fitting
    # the means directly on set B gives 0.0113, so the methods agree.  On set
    # C, 0.0111 would predict a peak of 2.53.)
    # Simon's own one-rate half-time, 8.4 min, does not transfer
    # to an independent intravenous model: it predicts a peak of 6.9
    # mcg/mL.  Axillary block with epinephrine only; other sites and plain
    # solutions absorb differently, and F = 1 is an assumption (the label
    # describes complete absorption after parenteral injection; no
    # site-specific absolute bioavailability has been measured).  A slow
    # second depot (ka_RA_slow, as mepivacaine has) does not help here: fitted
    # to the same curve on this disposition, the fast share falls to 3% and
    # the fit barely improves, because this model's clearance (1.4 L/min) and
    # Vss (34 L) cannot give the curve's long tail whatever the input.  (That
    # test was on set B; set C's 1.5 L/min and 39 L are no different in kind.)
    # (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
    ka_RA = 0.0124,
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
