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

  default <- list(
    v1 = 14.3      * size$volume,
    v2 = 63.38694  * size$volume,
    # Gepts 1995's slowly equilibrating volume, 261.6 L (95% CI 260.2-262.9;
    # Vss 339 L).  Until 2026-10-17 this was 251.9 L, the volume implied by
    # k31 rounded to 0.0013 /min (0.32747 / 0.0013).  Changed by decision of
    # Steven L. Shafer, from an external review.
    v3 = 261.6     * size$volume,
    cl1 = 0.92235 * size$clearance,
    cl2 = 1.55298 * size$clearance,
    cl3 = 0.32747 * size$clearance
  )
  
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  
  tPeak <- 5.8		# from Shafer/Varvel, t_peaks.xls
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
