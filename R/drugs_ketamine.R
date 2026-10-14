ketamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Domino 1984, eight healthy men, 2.2 mg/kg over 1 min, placebo arm.  The
  # paper reports MEANS of individually fitted quantities: a central volume
  # of 0.063 L/kg, a clearance of 0.848 L/kg/h, a deep volume of about 1.51
  # L/kg and the three half-lives 24.1 s, 4.68 min and 2.17 h.  Means of
  # volumes and rate constants do not multiply into the mean clearance: the
  # rate constants this file carried until 2026-10-14 (k10 0.438, k12 0.592,
  # k13 0.590, k21 0.247, k31 0.0146 /min) reproduced the three half-lives
  # but gave a clearance of 1.66 L/kg/h, twice the paper's, and a deep volume
  # of 2.55 L/kg.  The constants below are the one set that reproduces, at
  # once, the reported central volume, clearance, deep volume and all three
  # half-lives (five constraints for five constants, solved exactly; the
  # script is in the commit that introduced them).  They imply V2 0.124 L/kg
  # and Vss 1.70 L/kg.  Diazepam premedication raised the alpha half-life to
  # 6.37 min and is not represented.
  v1Ref <- .063 * 70   # .063 L/kg at the 70 kg reference
  k10 <- 0.2243386    # CL / V1: 0.848 L/kg/h / 0.063 L/kg
  k12 <- 0.8702122
  k13 <- 0.3291103
  k21 <- 0.4417174
  k31 <- 0.01373109   # V3 / V1 = 1.51 / 0.063
  
  typical <- 0.12  # Moen 2013 review
  upperTypical <- 0.1 # mcg/ml - Moen 2013 review
  lowerTypical <- 0.16 # mcg/ml - Moen 2013 review
  tPeak <- 3 # Assumed, not from Domino (who measured no effect): see the help
  MEAC <- 0
  reference <- "Domino EF et al., Clin Pharmacol Ther 1984;36(5):645-653. https://pubmed.ncbi.nlm.nih.gov/6488686/"
  
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
