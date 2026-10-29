sufentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: no size scaling, the parameters were used as published.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Gepts 1995, Table 3 and abstract: the two-stage log means, as printed.
  # Until 2026-10-17 V3 was 251.9 L, the volume implied by k31 rounded to
  # 0.0013 /min (0.32747 / 0.0013), outside the paper's 95% CI of
  # 260.2-262.9 L; changed to 261.6 L by decision of Steven L. Shafer, from
  # an external review.  Until 2026-10-28 V2 and the three clearances were
  # likewise back-calculated from the rounded rate constants (V2 63.38694 L;
  # CL1 0.92235, CL2 1.55298, CL3 0.32747 L/min); changed to the printed
  # 63.1 L and 0.92, 1.55 and 0.33 L/min by decision of Steven L. Shafer,
  # 2026-10-12 (applied 2026-10-28).  That second change moves concentrations
  # by under 0.5%.  Table 3 is internally consistent (Shafer did the
  # pharmacokinetic analysis): any six of its parameters, for instance these
  # three volumes and three clearances, give the others to the precision
  # printed (k10 0.0643, k21 0.0246, k31 0.00126 /min; half-lives 3.3, 66
  # and 763 min against 3.3, 67 and 769).  The discrepancy was the library's,
  # from deriving volumes from rate constants printed to two figures.
  default <- list(
    v1 = 14.3  * size$volume,
    v2 = 63.1  * size$volume,
    v3 = 261.6 * size$volume,
    cl1 = 0.92 * size$clearance,
    cl2 = 1.55 * size$clearance,
    cl3 = 0.33 * size$clearance
  )
  
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 5.6		# Shafer & Varvel 1991, Fig 2 (EEG); 5.8 until 2026-10-12, changed by decision of Steven L. Shafer
  MEAC <- 0.056
  typical <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0
  reference <- "Gepts E et al., Anesthesiology 1995;83(6):1194-1204. https://pubmed.ncbi.nlm.nih.gov/8533912/"
  
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
