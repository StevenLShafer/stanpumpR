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

# The concentrations below are returned but not used: those in the drug
# library (drugDefaults_global.csv) are.  There, MEAC 27.9 ng/mL is Dahan's
# ANALGESIC potency (the effect-site concentration for a 50% rise in cold
# pressor hand-withdrawal latency), and the band's typical value 27.4 ng/mL
# is the RESPIRATORY potency (a 25% fall in the hypercapnic ventilatory
# response); the two happen to coincide.

  typical <- NA
  upperTypical <- NA
  lowerTypical <- NA
  MEAC <- NA
  reference <- "Dahan A et al., Anesthesiology 2020;133(3):559-568. https://pubmed.ncbi.nlm.nih.gov/32788558/"

  # Effect site.  Dahan 2020 estimated the plasma-effect-site equilibration
  # of the VENTILATORY effect (hypercapnic ventilatory response) with a
  # half-life of 0.25 h, and did not quantify one for analgesia.  That is a
  # half-life, so ke0 = ln 2 / 15 min = 0.0462 /min is supplied directly and
  # getDrugPK() solves the time to peak effect after a bolus from it, about
  # 26 min with these kinetics.  Until 2026-10-14 the 15 min was read as the
  # time to peak effect itself, which implied an equilibration half-life of
  # about 6 min; test-drugs-oliceridine.R pins the solved peak.
  ke0 <- log(2) / 15

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
      ke0 = ke0,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
