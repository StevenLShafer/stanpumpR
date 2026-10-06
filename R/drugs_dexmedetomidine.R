dexmedetomidine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  if (age > 1)
  {
    v1Ref <- 8.0574   # liters, 70 kg adult
    k10 <- 0.0552
    k12 <- 0.258
    k13 <- 0.247
    k21 <- 0.163
    k31 <- 0.0112

    tPeak <- 10 # Just a guess
    typical <- 0.6 # midpoint of the 0.4-0.8 ng/mL sedation range (the app reads drugDefaults_global.csv)
    upperTypical <- 0.4
    lowerTypical <- 0.8
    MEAC <- 0
    reference <- "Dyck JB et al., Anesthesiology 1993;78(5):821-828. https://pubmed.ncbi.nlm.nih.gov/8098191/"

    # Size scaling (see docs/weight-adjustment.md): Dyck's parameters describe
    # a 70 kg adult and formerly did not scale at all.  Volumes scale with
    # fat-free mass relative to the 70 kg, 170 cm reference male, clearances
    # with that ratio ^ 0.75 (Al-Sallami 2015).  adjustToFFM = FALSE
    # reproduces the former unscaled parameters exactly.
    size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
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
  } else {
    v3 <- 1
    cl3 <- 0
    # Size scaling (see docs/weight-adjustment.md): Zuppa scaled volumes with
    # weight/70 and clearances with (weight/70)^0.75.  Fv and Fcl replace those
    # factors with fat-free mass relative to the 70 kg, 170 cm reference male
    # (Al-Sallami 2015, an extrapolation below 3 years); adjustToFFM = FALSE
    # reproduces Zuppa's total-body-weight factors exactly.
    size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                          legacyClearance = (weight/70)^0.75)
    Fv  <- size$volume
    Fcl <- size$clearance
    # Before cardiopulmonary bypass:
    v1 <- 132   * Fv # liters
    v2 <- 78.9  * Fv # liters
    cl1 <- 1240 * Fcl / 1000 # (L / min)
    cl2 <- 2300 * Fcl / 1000 # (L / min)
    default <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # During cardiopulmonary bypass ************************************************
    # 37 degrees
    v1 <- 115 * Fv * (37 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPBStart <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 36 degrees
    v1 <- 115 * Fv * (36 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB36 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 35 degrees
    v1 <- 115 * Fv * (35 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB35 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 34 degrees
    v1 <- 115 * Fv * (34 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB34 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 33 degrees
    v1 <- 115 * Fv * (33 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB33 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )
    # 32 degrees
    v1 <- 115 * Fv * (32 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB32 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # 31 degrees
    v1 <- 115 * Fv * (31 / 37) ^ (-1.6)  # liters
    v2 <- 144 * Fv # liters
    cl1 <- 74.1 * Fcl / 1000 # (L / min)
    cl2 <- 2980 * Fcl / 1000 # (L / min)
    CPB31 <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )

    # After cardiopulmonary bypass:
    v1 <-  155 * Fv # liters
    v2 <-  105 * Fv # liters
    cl1 <- 623 * Fcl * (age * 365) / (1.77 + age * 365) / 1000 # (L / min)
    cl2 <- 209 * Fcl / 1000 # (L / min)
    CPBEnd <- list(
      v1 = v1,
      v2 = v2,
      v3 = v3,
      cl1 = cl1,
      cl2 = cl2,
      cl3 = cl3
    )
    events <- c(PK_EVENT_DEFAULT, "CPBStart","CPB36", "CPB35", "CPB34", "CPB33", "CPB32", "CPB31", "CPBEnd")

    tPeak <- 2 # Just a guess
    typical <- 0.6 # midpoint of the 0.4-0.8 ng/mL sedation range (the app reads drugDefaults_global.csv)
    upperTypical <- 0.4
    lowerTypical <- 0.8
    MEAC <- 0
    reference <- "Zuppa BJA 2019"
  }

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
