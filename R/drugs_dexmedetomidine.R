# -----------------------------------------------------------------------------
# Dexmedetomidine: Morse 2020 above one year; Zuppa 2019, with its bypass sets,
# at one year or under
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in mcg
# of dexmedetomidine (the product is the hydrochloride, labelled as the base),
# concentrations in ng/mL.
#
# SOURCE
# ======
# Morse JD, Cortinez LI, Anderson BJ.  A universal pharmacokinetic model for
# dexmedetomidine in children and adults.  J Clin Med 2020;9(11):3480.
# PMID 33126702.  https://doi.org/10.3390/jcm9113480
# Read in full from the PDF Steven L. Shafer supplied, 2026-11-17 (the
# PubMed Central text lacks the tables).  The Supplementary material (Table
# S1, Figures S1 and S2) was not read.
#
# Five data sets pooled, 202 people, 2145 plasma concentrations, 40.6 weeks
# postmenstrual age to 70.8 years and 3.1 to 152 kg: Hannivoort 2015 (18
# volunteers, here SIMULATED from that paper's model, not its measurements),
# Potts 2009 (children after cardiac surgery, pooled with Petroz 2006 and
# Vilo 2008), Cortinez 2015 and Rolle 2018 (obese and non-obese adults at
# laparoscopic surgery, fat-free mass by DXA) and Talke 2018 (10
# volunteers).  NONMEM 7.4, FOCE-I.  Table 1 (bootstrap medians):
#
#     V1 = 25.2  x (NFM/NFMstd)        L          NFM = FFM + FfatV x (WT - FFM)
#     V2 = 34.4  x (NFM/NFMstd)        L          FfatV = 0.293
#     V3 = 65.4  x (NFM/NFMstd)        L
#     CL = 0.897 x (FFM/FFMstd)^0.75 x MF   L/min   (FfatCL fixed at 0)
#     Q2 = 1.68  x (FFM/FFMstd)^0.75        L/min
#     Q3 = 0.62  x (FFM/FFMstd)^0.75        L/min
#     MF = PMA / (TM50 + PMA),  TM50 52.4 weeks, Hill fixed at 1
#
# FFMstd is 56.1 kg, "expected for a male with a TBM of 70 kg and height of
# 1.76 m" (ffmAlSallami() gives 56.13 at 35 y), and NFMstd the same man's NFM,
# 56.1 + 0.293 x 13.9 = 60.17 kg.
#
# Three readings the paper leaves to the reader, each recorded here:
#
# 1. Fat on the intercompartmental clearances.  Table 1 prints one FfatCL,
#    fixed at 0, and the text says FFM was "a better PK size descriptor for
#    clearance"; Q2 and Q3 are taken to share it (FFM^0.75), as the Eleveld
#    and Holford models these authors follow do.
# 2. Maturation on CL only.  Equation 16 is unnormalised and the text puts
#    maturation on clearance ("changes in CL due to age"), so Q2 and Q3 carry
#    none.  MF is used as printed: 0.43 at term birth and 0.79 at three years
#    are the "42% of adult values, reaching 80% by 3 years" the paper states,
#    so its "adult" is MF = 1.  An adult's MF is still below 1 (0.954 at 20
#    years, 0.986 at 70), a 3% drift with age that is the printed equation's.
# 3. Fat-free mass.  The authors measured it by DXA in two data sets and
#    predicted it in the rest, with Al-Sallami 2015 for the children
#    (Equations 9 and 10) and Janmahasatian 2005 for Talke's adults; this
#    model uses ffmAlSallami() at every age, which is Janmahasatian in adults.
#
# CHECK AGAINST THE PAPER'S TABLE 3: its clearance column runs 2% (20 years)
# to 17% (3 years) above Equations 15 and 16 with Table 1's values, whichever
# sex, and above the same model scaled by total weight; no single factor or
# normalisation of MF reproduces it, so Table 3 was not computed from the
# printed model (perhaps from the total-weight run of Figure S2, which was
# not read).  The maintenance rates of Table 3 follow its own clearances
# (1 mcg/L x CL / weight), so the column is internally consistent.  The
# printed equations are implemented, and Table 3 is recorded, not reproduced.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariates (fat-free mass and normal fat
# mass), as the Eleveld models do, so adjustToFFM is accepted for a uniform
# signature and not used.  The reference man (70 kg, 170 cm, 35 y) has 54.5 kg
# of fat-free mass, so he receives CL 0.853 L/min, where the 70 kg, 176 cm
# man of 35 receives 0.873 (0.897 x MF, MF 0.973 at 35 years).
#
# POSTMENSTRUAL AGE
# =================
# The Patient Profile's Gestational age at birth (blank = term) plus the
# postnatal age, postmenstrualAge().  The youngest patient studied was 40.6
# weeks; a preterm newborn is an extrapolation of the maturation function,
# which stays positive and finite at every legal entry.
#
# EFFECT SITE AND BAND
# ====================
# Morse estimated no effect site.  tPeak 14.9 min, as before: Colin 2017's
# MOAA/S ke0 (0.0428 /min) on the Hannivoort kinetics it was estimated with,
# carried as the time to peak effect it gives there (Minto 2003), and solved
# again on this model by getDrugPK().  The band is the 0.4-0.8 ng/mL sedation
# range, a library band (inst/extdata/therapeuticRanges.csv); Morse's own
# simulations target 1 mcg/L.
#
# WHICH MODEL
# ===========
# Morse is used above one year of age.  At one year or under the drug keeps
# Zuppa 2019's infant model (dexmedetomidineZuppa() below), whose parameter
# sets for cardiopulmonary bypass Morse's model cannot represent: Steven L.
# Shafer's decision of 2026-11-17, over making Morse the default at every age
# with Zuppa's sets split into a drug of their own.  The step between the two
# models at one year remains (the help page gives its size).  Because Morse
# is used only above one year, its maturation factor runs from 0.64 (a
# term-born one-year-old) upward.
#
# HISTORY
# =======
# Until 2026-11-17 the branch above one year was fixed parameters close to
# Dyck 1993's pooled estimates (V1 8.06, V2 12.8, V3 178 L; CL 0.445, Q2 2.08,
# Q3 1.99 L/min), its height term on clearance not applied, scaled by
# fat-free mass.  Morse replaced it at Steven L. Shafer's request, 2026-11-17.
# -----------------------------------------------------------------------------

# Morse 2020, Table 1
DEXMEDETOMIDINE_V1    <- 25.2     # L / 70 kg (NFM)
DEXMEDETOMIDINE_V2    <- 34.4     # L / 70 kg (NFM)
DEXMEDETOMIDINE_V3    <- 65.4     # L / 70 kg (NFM)
DEXMEDETOMIDINE_CL1   <- 0.897    # L/min / 70 kg (FFM^0.75), at MF = 1
DEXMEDETOMIDINE_CL2   <- 1.68     # L/min / 70 kg (FFM^0.75)
DEXMEDETOMIDINE_CL3   <- 0.62     # L/min / 70 kg (FFM^0.75)
DEXMEDETOMIDINE_FFAT_V <- 0.293
DEXMEDETOMIDINE_TM50  <- 52.4     # weeks postmenstrual age
DEXMEDETOMIDINE_HILL  <- 1        # fixed
DEXMEDETOMIDINE_FFM_STD <- 56.1   # kg: 70 kg, 176 cm man
DEXMEDETOMIDINE_WT_STD  <- 70     # kg
DEXMEDETOMIDINE_NFM_STD <- DEXMEDETOMIDINE_FFM_STD +
  DEXMEDETOMIDINE_FFAT_V * (DEXMEDETOMIDINE_WT_STD - DEXMEDETOMIDINE_FFM_STD)

#' Dexmedetomidine pharmacokinetics
#'
#' Morse et al. (2020) above one year of age: three compartments, volumes on
#' normal fat mass, clearances on fat-free mass to the 0.75 power, and a
#' postmenstrual-age maturation of clearance.  Zuppa et al. (2019) at one
#' year or under, with parameter sets for cardiopulmonary bypass.  See the
#' file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM not used by Morse's model, which carries its own size
#'   covariates; at one year or under, \code{TRUE} scales Zuppa's model by
#'   fat-free mass and \code{FALSE} by total body weight, as published.
#' @param gestationalAge gestational age at birth in weeks, or \code{NULL} for
#'   a term birth.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
dexmedetomidine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            gestationalAge = NULL)
{
  if (age <= 1) return(dexmedetomidineZuppa(weight, height, age, sex, adjustToFFM))
  dexmedetomidineMorse(weight, height, age, sex, gestationalAge)
}

# Morse 2020 (see the header); used above one year of age.
dexmedetomidineMorse <- function(weight, height, age, sex, gestationalAge = NULL)
{
  ffm <- ffmAlSallami(weight, height, age, sex)
  nfm <- ffm + DEXMEDETOMIDINE_FFAT_V * (weight - ffm)
  fv  <- nfm / DEXMEDETOMIDINE_NFM_STD
  fcl <- (ffm / DEXMEDETOMIDINE_FFM_STD)^0.75
  pma <- postmenstrualAge(age, gestationalAge)
  mf  <- pma^DEXMEDETOMIDINE_HILL /
    (DEXMEDETOMIDINE_TM50^DEXMEDETOMIDINE_HILL + pma^DEXMEDETOMIDINE_HILL)

  default <- list(
    v1  = DEXMEDETOMIDINE_V1 * fv,
    v2  = DEXMEDETOMIDINE_V2 * fv,
    v3  = DEXMEDETOMIDINE_V3 * fv,
    cl1 = DEXMEDETOMIDINE_CL1 * fcl * mf,
    cl2 = DEXMEDETOMIDINE_CL2 * fcl,
    cl3 = DEXMEDETOMIDINE_CL3 * fcl
  )

  list(
    PK = list(default = default),
    tPeak = 14.9,     # Colin 2017 MOAA/S ke0 on Hannivoort's kinetics
    MEAC = 0,
    typical = 0.6,    # midpoint of the 0.4-0.8 ng/mL sedation range
    upperTypical = 0.8,
    lowerTypical = 0.4,
    reference = paste0(
      "Morse JD, Cortinez LI, Anderson BJ, J Clin Med 2020;9(11):3480. ",
      "https://doi.org/10.3390/jcm9113480 (three compartments, volumes on ",
      "normal fat mass, clearances on fat-free mass^0.75, postmenstrual-age ",
      "maturation of clearance; tPeak 14.9 min from Colin PJ et al., ",
      "Br J Anaesth 2017;119:200-210)"
    )
  )
}

# Zuppa 2019, the infant model with its cardiopulmonary-bypass parameter sets;
# used at one year or under.  Zuppa AF et al., Br J Anaesth 2019;123:839-852
# (122 infants, 0 to 180 days, at corrective cardiac surgery), unchanged from
# the infant branch this drug had before 2026-11-17.
#
# The parameters are Table 7's ALLOMETRIC model (per 70 kg: CL 1240 mL/min
# before bypass, 74.1 on it, 623 after; maturation Age/(1.77 + Age), days),
# re-read on the page image (printed p. 848) on 2026-11-17 and on 2026-10-27
# (papers/digests/PMID_31623840.md, which also records that Shafer built the
# model from the final NONMEM control file).  The abstract's "DEX clearance
# after CPB was 1240 ml min-1 70 kg-1" is the LINEAR model's post-bypass
# clearance (Table 7, linear column; the Results say "623 (allometric) or 1240
# (linear)"), which only happens to equal the allometric model's pre-bypass
# 1240.  The abstract's "50% maximum clearance ... approximately 2 days" is the
# allometric 1.77 days rounded; its "90% ... 18 days" is normalised to a
# 180-day-old (Results), which Age/(1.77 + Age) is not (it gives 15.9 days
# un-normalised, 14.5 normalised; the digest card).  The authors preferred the
# linear model by AIC (16318 against 16328); the library uses the allometric
# one, a choice, not an error.
dexmedetomidineZuppa <- function(weight, height, age, sex, adjustToFFM = TRUE)
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

  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = tPeak,
    MEAC = MEAC,
    typical = typical,
    upperTypical = upperTypical,
    lowerTypical = lowerTypical,
    reference = reference
  )
}
