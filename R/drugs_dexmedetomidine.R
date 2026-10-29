dexmedetomidine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters

  if (age > 1)
  {
    # Fixed parameters, close to the pooled estimates of Dyck 1993 (V1 8.05,
    # V2 12.4, V3 175 L; Cl2 2.05, Cl3 2.0 L/min).  Dyck's elimination
    # clearance depends on height (pooled: 0.0101 x height - 1.33 L/min;
    # final: 0.00791 x height - 0.927); that term is not applied, and the
    # 0.445 L/min here is the pooled equation at about 176 cm.  Kept as is,
    # by Steven L. Shafer's decision, 2026-10-09.
    v1Ref <- 8.0574   # liters, 70 kg adult
    k10 <- 0.0552
    k12 <- 0.258
    k13 <- 0.247
    k21 <- 0.163
    k31 <- 0.0112

    # Time to peak effect, sedation: Colin PJ et al., Br J Anaesth
    # 2017;119:200-210 (https://pubmed.ncbi.nlm.nih.gov/28854538/), MOAA/S
    # ke0 0.0428 /min, estimated on the Hannivoort 2015 kinetics (V1 1.78,
    # V2 30.3, V3 52.0 L; CL 0.686, Q2 2.98, Q3 0.602 L/min at 70 kg).  The PD
    # and the PK come from different papers, so the ke0 is carried as a tPeak
    # (Minto 2003): on Hannivoort's kinetics a bolus peaks at the effect site
    # at 14.9 min, and getDrugPK() solves ke0 again on Dyck's (about 0.033
    # /min).  MOAA/S, not BIS (ke0 0.120 /min, tPeak 1.6 min): the band is a
    # sedation band, matching Colin's MOAA/S C50 of 0.43 ng/mL, and the BIS
    # ke0 rests on Colin's arousal sub-model (0.99 /min without it).  Colin's
    # text gives the MOAA/S equilibration half-time as 14 min where the table's
    # ke0 implies 16.2; 14 min would give a tPeak of 11.3.  Was 10 min, a
    # guess, until 2026-10-28; changed by decision of Steven L. Shafer.
    tPeak <- 14.9
    typical <- 0.6 # midpoint of the 0.4-0.8 ng/mL sedation range (the app reads drugDefaults_global.csv)
    lowerTypical <- 0.4
    upperTypical <- 0.8
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

    # After cardiopulmonary bypass.  Clearance matures with postnatal age in
    # days, PNA / (1.77 + PNA), which is zero at birth: a newborn entered as
    # age 0 would have no clearance at all (issue #310), so postnatal age is
    # floored at one day.
    v1 <-  155 * Fv # liters
    v2 <-  105 * Fv # liters
    postnatalDays <- max(age * 365, 1)
    cl1 <- 623 * Fcl * postnatalDays / (1.77 + postnatalDays) / 1000 # (L / min)
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
    lowerTypical <- 0.4
    upperTypical <- 0.8
    MEAC <- 0
    reference <- "Zuppa AF et al., Br J Anaesth 2019;123(6):839-852. https://pubmed.ncbi.nlm.nih.gov/31623840/"
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
