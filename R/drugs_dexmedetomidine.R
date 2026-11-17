# -----------------------------------------------------------------------------
# Dexmedetomidine: the universal model of Morse 2020, neonates to obese adults
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
# HISTORY
# =======
# Until 2026-11-17 this drug was two models: fixed parameters close to Dyck
# 1993's pooled estimates above one year (height term not applied, scaled by
# fat-free mass), and Zuppa 2019's infant model at or below one year, with
# separate parameter sets for cardiopulmonary bypass.  Morse replaces both as
# the default at every age, the program choosing the model (AGENTS.md: its
# 202 people span term neonates to obese adults of 70 years, wider than
# either source).  Zuppa's bypass sets describe a condition, not a population, so
# they are kept as their own drug, dexmedetomidineInfantCPB, as heparinCPB is
# kept beside heparin.  Both by Claude Code at Steven L. Shafer's request,
# 2026-11-17; the split is a [decision] for him to confirm.
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
#' Morse et al. (2020): three compartments, volumes on normal fat mass,
#' clearances on fat-free mass to the 0.75 power, and a postmenstrual-age
#' maturation of clearance.  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM accepted for a uniform signature and not used: the model
#'   carries its own size covariates.
#' @param gestationalAge gestational age at birth in weeks, or \code{NULL} for
#'   a term birth.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @keywords internal
dexmedetomidine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            gestationalAge = NULL)
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
