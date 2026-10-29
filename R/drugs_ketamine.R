ketamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  
  # Domino 1984, eight healthy men, 2.2 mg/kg over 1 min, placebo arm.
  # Refitted to Domino's measured concentrations; changed by decision of
  # Steven L. Shafer, 2026-10-12 (applied 2026-10-28).
  #
  # The paper's Table IV reports MEANS of individually fitted quantities (a
  # two-stage analysis): central volume 0.063 L/kg, clearance 0.848 L/kg/h,
  # deep volume 1.51 L/kg, half-lives 24.1 s, 4.68 min and 2.17 h.  Means of
  # individual parameters need not describe the data at all.  Two earlier
  # versions of this file show it:
  #   - until 2026-10-14, the mean rate constants (k10 0.438, k12 0.592,
  #     k13 0.590, k21 0.247, k31 0.0146 /min): clearance 1.66 L/kg/h, twice
  #     the paper's, and every Table I mean underpredicted, by 27-99%;
  #   - 2026-10-14 to 2026-10-28, the five constants solved exactly to the
  #     mean V1, clearance, V3 and three half-lives (k10 0.2243386,
  #     k12 0.8702122, k13 0.3291103, k21 0.4417174, k31 0.01373109): right
  #     from 20 min to 8 h, but 44-92% high at 3-10 min and 63% and 97% low
  #     at 12 and 24 h.
  # Shafer's preferred remedy is to simulate every subject, take the median
  # at exponentially spaced times and fit a polyexponential to the medians.
  # Domino does not print the individual parameters, so the next best is
  # used: a three-exponential fit to all 16 mean plasma concentrations in
  # Table I (n = 8, 1 min to 24 h), on the log scale, which is close to
  # Domino's own 1/C^2 weighting.  Three exponentials fit far better than two
  # (summed squared log error 0.67 against 3.59).  The fit has half-lives of
  # 0.61, 25 and 288 min, a clearance of 0.79 L/kg/h (Domino 0.848) and Vss
  # 3.1 L/kg; its root-mean-square log error against Table I is 0.21 (0.96
  # for the solved constants above), and it is within about 30% of every mean
  # except at 8 h (+49%).  The terminal half-life is longer than Domino's
  # 2.25 h because the 12 and 24 h means (30.4 and 8.14 ng/mL) require it.
  # Diazepam premedication is not represented.  (Claude Code, at the request
  # of Steven L. Shafer.)
  v1Ref <- 0.0583 * 70   # 0.0583 L/kg at the 70 kg reference
  k10 <- 0.226
  k12 <- 0.657
  k13 <- 0.206
  k21 <- 0.0676
  k31 <- 0.00489
  
  # Band, mcg/mL: "The ketamine analgesic effect persists for steady-state
  # plasma concentrations superior to 100-160 ng/ml" (Mion G, Villevieille T,
  # CNS Neurosci Ther 2013;19:370-380, citing Clements JA et al., J Pharm Sci
  # 1982;71:539-542), an analgesic threshold, not a window.
  typical <- 0.12
  lowerTypical <- 0.1
  upperTypical <- 0.16
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
