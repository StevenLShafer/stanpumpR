# -----------------------------------------------------------------------------
# Dexmedetomidine in infants on cardiopulmonary bypass (Zuppa 2019)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in mcg,
# concentrations in ng/mL.
#
# Zuppa AF, Nicolson SC, Wilder NS, et al.  Results of a phase 1 multicentre
# investigation of dexmedetomidine bolus and infusion in corrective infant
# cardiac surgery.  Br J Anaesth 2019;123(6):839-852.  PMID 31623840.
# 122 infants, 0 to 180 days, at corrective cardiac surgery: two compartments, with
# separate parameter sets before, during (by temperature, 36 to 31 C) and
# after cardiopulmonary bypass, entered as events on the Events panel.
#
# Until 2026-11-17 these sets were the infant branch (age 1 year or under) of
# the dexmedetomidine drug, whose default at every age is now Morse 2020.
# They describe a condition, bypass, rather than a population, so they are
# kept as a drug of their own, as heparinCPB is beside heparin (Claude Code,
# at Steven L. Shafer's request to incorporate Morse; the split is a
# [decision] for him to confirm).  The parameters, the size scaling and the
# effect site are unchanged and are offered at every age, the model being
# an extrapolation past 180 days.
# -----------------------------------------------------------------------------

#' Dexmedetomidine in infants on cardiopulmonary bypass
#'
#' Zuppa et al. (2019): two compartments with parameter sets for bypass.  See
#' the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales by fat-free mass;
#'   \code{FALSE} reproduces Zuppa's total-body-weight scaling.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
dexmedetomidineInfantCPB <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  {
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
