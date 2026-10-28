lidocaine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Schnider 1996, Table 1, parameter set C: the paper's final estimates, from
  # all 13 patients (V1 0.101, V2 0.452 L/kg; Cl1 0.0215, Cl2 0.0589
  # L/kg/min).  Changed by decision of Steven L. Shafer, 2026-10-28.  Until
  # then the library used set B (V1 0.088, V2 0.4 L/kg; Cl1 0.02, Cl2 0.056
  # L/kg/min), the interim estimates from the first 8 patients that dosed the
  # last 5 prospectively (median prediction error -6%, absolute 15%).  At 70
  # kg set C is V1 7.07 and V2 31.6 L, Cl1 1.505 and Cl2 4.12 L/min; an
  # infusion runs about 8% lower than with set B.
  v1PerKg  <- 0.101
  v2PerKg  <- 0.452
  cl1PerKg <- 0.0215
  cl2PerKg <- 0.0589
  
  typical <- 1
  upperTypical <- 1.5
  lowerTypical <- 0.5
  tPeak <- 5
  MEAC <- 0
  reference <- "Schnider TW et al., Anesthesiology 1996;84(5):1043-1050. https://pubmed.ncbi.nlm.nih.gov/8623997/"
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces Schnider's model
  # exactly: every volume and clearance per kilogram of total body weight.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1PerKg * 70 * size$volume
  v2 <- v2PerKg * 70 * size$volume
  v3 <- 1
  cl1 <- cl1PerKg * 70 * size$clearance
  cl2 <- cl2PerKg * 70 * size$clearance
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
    # that best reproduces, with this disposition (set C, 70 kg) and F = 1,
    # the 17 mean arterial plasma concentrations from 5 to 180 min after 600
    # mg of 1.5% lidocaine with epinephrine 5 mcg/mL for axillary block
    # (Simon et al. 2002, Table 2; mean of individual peaks 2.87 mcg/mL at
    # 25.8 min): weighted least squares, each residual divided by the
    # concentration + 0.3 mcg/mL.  0.0125/min is an absorption half-time of
    # 55 min and predicts a peak of 2.74 mcg/mL at 41 min.  Refitted
    # 2026-10-28 with the change to set C; on set B the same fit to the
    # means gives 0.0113, against the 0.0111 then in use (fitted to a
    # two-rate description of the same means).  Simon's own one-rate
    # half-time, 8.4 min, does not transfer to an independent intravenous
    # model: it predicts a peak of 6.8 mcg/mL.  Axillary block with epinephrine only; other sites and plain
    # solutions absorb differently, and F = 1 is an assumption (the label
    # describes complete absorption after parenteral injection; no
    # site-specific absolute bioavailability has been measured).  A slow
    # second depot (ka_RA_slow, as mepivacaine has) does not help here: fitted
    # to the same means on this disposition, the two rates collapse to one
    # (both 0.0125/min) and the fit does not improve, because this model's
    # clearance (1.5 L/min) and Vss (39 L) cannot give the curve's long tail
    # whatever the input: at 180 min it predicts 0.78 mcg/mL against 1.44
    # observed.  (Claude Code, 2026-10-10 and 2026-10-28, at the request of
    # Steven L. Shafer.)
    ka_RA = 0.0125,
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
