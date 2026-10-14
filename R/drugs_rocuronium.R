rocuronium <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  v1Ref <- 0.056 * 70   # 0.056 L/kg at the 70 kg reference
  k10 <- 0.1746
  k12 <- 0.100381
  k13 <- 0
  k21 <- 0.0245
  k31 <- 0
  k41 <- 0.168

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
    cl3 = cl3
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Time to peak effect at the adductor pollicis: the population median time
  # to maximum block after 0.15 mg/kg in Cortinez LI et al., Br J Anaesth
  # 2007;99(5):679-685 (https://pubmed.ncbi.nlm.nih.gov/17681967/), 4.5 min
  # (range 2.0-7.7), whose ke0 by every method was 0.19-0.20 /min.  With
  # Plaud's kinetics the solved ke0 is about 0.19 /min, and Plaud's own
  # adductor pollicis ke0 (k41 above, 0.168 /min, unused) agrees.  Until
  # 2026-10-14 this was 2.2 min, the time of one illustrated subject, which
  # implied a ke0 near 0.7 /min; test-drugs-rocuronium.R pins the ke0.
  tPeak = 4.5
  typical <- 1.5
  upperTypical <- 2.2
  lowerTypical <- 1
  MEAC <- 0
  reference <- "Plaud B et al., Clin Pharmacol Ther 1995;58(2):185-191. https://pubmed.ncbi.nlm.nih.gov/7648768/"


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
