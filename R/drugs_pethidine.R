pethidine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  v1Ref <- 18.1   # liters at the 70 kg reference
  k10 <- 0.042131499
  k12 <- 0.300022713
  k13 <- 0.098461009
  k21 <- 0.089209873
  k31 <- 0.010774228
  
  tPeak <- 10
  # MEAC 0.46 mcg/mL: Austin, Stapleton and Mather 1980 (Anesthesiology
  # 53:460-466), the mean blood meperidine concentration at the minimum
  # effective analgesic concentration in nine postoperative patients given
  # intramuscular doses (0.46, SD 0.18, n = 19; the maximum concentration still
  # associated with severe pain was 0.41).  Until 2026-10-27 the value was
  # 0.25, which the help attributed to Austin but which is not in the paper;
  # its origin was not recorded.  Changed by decision of Steven L. Shafer on
  # the 2026-10-27 opioid potency audit (docs/opioid-review-2026-10-27.md).
  # Austin measured WHOLE BLOOD and the library treats the value as an
  # effect-site divisor: no blood-to-effect-site bridge is applied.
  MEAC <- 460/1000
  typical <- MEAC * 1.2
  lowerTypical <- MEAC * 0.8
  upperTypical <- MEAC * 2.0
  reference <- paste0(
    "Bjorkman S, J Pharmacokinet Pharmacodyn 2003;30(4):285-307. ",
    "https://pubmed.ncbi.nlm.nih.gov/14650375/ (kinetics); ",
    "Austin KL, Stapleton JV, Mather LE, Anesthesiology 1980;53:460-466. ",
    "https://pubmed.ncbi.nlm.nih.gov/7457961/ (MEAC)"
  )
  
  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: V1 proportional to weight with fixed rate
  # constants, so volumes and clearances both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  v1 <- v1Ref * size$volume
  v2 <- v1Ref * k12 / k21 * size$volume
  v3 <- v1Ref * k13 / k31 * size$volume
  cl1 <- v1Ref * k10 * size$clearance
  cl2 <- v1Ref * k12 * size$clearance
  cl3 <- v1Ref * k13 * size$clearance
  
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
