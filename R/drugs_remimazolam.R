remimazolam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Eleveld British Journal of Anaesthesia, 135 (1): 206e217 (2025)
  # Units **************
  # Time: Hours
  # Volume: Liters

  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: volumes x weight/70, clearances x (weight/70)^0.75
  # (the published Fsize).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)
  Fsize <- size$volume
  Kv3_age <-  7.31
  Kcl1_sex <- 16.3
  Kv3_sex <- 28.7

  # Ignored for now.  Kv3_Pugh is not a clearance term: in the published model
  # Pugh-Child > 8 increases V3 by exp(0.824) (and so Q3, through the V3
  # ratio ^ 0.75).  Normal hepatic function is assumed.
  Kcl1_opiates <- 13.9
  Kv3_Pugh  <- 82.4

  Fv3_age <- exp(Kv3_age/1000 * (age-35))
  if (sex == SEX_FEMALE)
  {
    Fcl1_sex <- exp(Kcl1_sex/100)
    Fv3_sex <- exp(Kv3_sex / 100)
  } else {
    Fcl1_sex <- 1
    Fv3_sex <- 1
  }

  v1 <- 4.31 * Fsize
  v2 <- 12.3 * Fsize
  v3 <- 18.6 * Fsize * Fv3_age * Fv3_sex
  # Published CL includes FCLsex (Table 1, KCLsex = 16.3 %): a woman's
  # elimination clearance is exp(0.163) = 1.18 times a man's.
  cl1 <- 1.12 * size$clearance * Fcl1_sex
  cl2 <- 1.45 * (v2 / 12.3) ** 0.75
  cl3 <- 0.298 * (v3 /18.6) ** 0.75

# these are returned but not used except tPeak
# those from the drugDefaults table are

  typical <- NA
  upperTypical <- NA
  lowerTypical <- NA
  MEAC <- NA
  reference <- "Eleveld DJ et al., Br J Anaesth 2025;135(1):206-217. https://pubmed.ncbi.nlm.nih.gov/40312166/"

  tPeak <- 2.5

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
