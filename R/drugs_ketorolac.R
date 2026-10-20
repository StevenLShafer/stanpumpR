# -----------------------------------------------------------------------------
# Ketorolac: S and R enantiomers as parallel systems (Cloesmeijer 2021)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of ketorolac free acid, S + R.
#
# DISPOSITION
# ===========
# Cloesmeijer et al. pooled 1020 S- and R-ketorolac concentrations from 80
# subjects given one intravenous dose (33 infants, median 0.83 years; 2
# children; 45 adults, median 30 years, among them women 4-5 months
# postpartum; 5.4-99 kg) and fitted each enantiomer separately.  Table 2,
# per 70 kg:
#
#            CL       V1      Q2      V2      Q3      V3
#     S     3.97     4.03    1.86    43.3    19.7    5.90     (L/h, L)
#     R     1.45     4.43    1.90    5.18      -       -
#
# clearances scaled by (W / 70)^0.75 and volumes by W / 70, fixed a priori.
# No maturation or other covariate was retained; renal function was not
# tested.  Half-lives at 70 kg: S 0.08, 1.3 and 24 h (a large, slowly
# equilibrating V2, with a wide bootstrap interval, 22.4-58.7 L, so the low
# S tail is uncertain); R 0.7 and 5.8 h.
#
# THE PLOTTED CONCENTRATION IS TOTAL KETOROLAC, S + R
# ===================================================
# The two enantiomers do not interconvert to any extent that matters and have
# dispositions of their own, so total ketorolac is the sum of two linear
# systems, five exponentials in all, which no single mammillary model holds.
# The S system is this model's own PK; R is a parallel system
# (parallelSystems, getDrugPK()), and simCpCe() runs both on the same doses
# and adds them, which is exact.  The S enantiomer carries nearly all the
# cyclo-oxygenase inhibition.
#
# Doses are entered as labelled ketorolac TROMETHAMINE.  Cloesmeijer converted
# them to free acid by molecular weight (255.273 / 376.409), and the racemate
# is half of each enantiomer, so each system receives
#     doseFraction = 0.5 x 255.273 / 376.409 = 0.3391
# of every dose: 30 mg tromethamine gives 10.17 mg of each enantiomer.
#
# ORAL ROUTE (PROVISIONAL)
# ========================
# Not fitted with this model.  The intravenous disposition is used with one
# shared first-order oral depot: complete bioavailability, as the
# intravenous/IM/oral crossover and the oral label report for the racemate,
# and the 3.8 min absorption half-life of Mroszczak et al. treated as
# first-order (ka = ln 2 / 3.8 min = 10.9 /h), no lag.  Neither was estimated
# jointly with the disposition or for each enantiomer separately; treat oral
# curves as provisional, especially with food (which delays absorption) or
# renal impairment.  They peak early and high: 10 mg by mouth peaks here at
# 1.01 mg/L at 12 min in the reference man, against a mean peak of about
# 0.8 mg/L at a mean of 0.9 h in Jung's crossover.  On this disposition no
# single first-order rate reproduces both (a 23 min absorption half-life gives
# the 0.9 h peak but only 0.61 mg/L), so the source's rate is kept as given.
#
# EFFECT SITE
# ===========
# None: tPeak = 0, and the plotted concentration is plasma.  No band is
# drawn.  The time-until-threshold level (endCe in the CSV) is 0.37 mg/L of
# racemic ketorolac, the adult analgesic EC50 Cloesmeijer et al. cite; their
# simulations put it at 0.057 mg/L of S-ketorolac in adults, and, because
# the S:R ratio changes with age, an infant needs 0.41 mg/L of racemate for
# the same S concentration.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The library's fat-free-mass scaling on the 70 kg parameters, for both
# systems; with the switch off, the published allometry on total weight
# (legacyVolume = W / 70, legacyClearance = (W / 70)^0.75).
#
# NOT MODELLED
# ============
# Renal impairment (ketorolac and its glucuronide are cleared by the kidney,
# and the label limits dosing in renal impairment and the elderly).  No
# CYP2D6 adjustment: the model fitted none.  Target-controlled infusion is not
# offered: the controller inverts a single system.
#
# PREGNANCY
# =========
# Kulo et al. gave 30 mg of racemic ketorolac intravenously after cesarean
# delivery (Int J Obstet Anesth 2012; n = 39, 8 of them studied again 4-5
# months postpartum, and 8 nonpregnant controls).  Clearance was 2.11 L/h/m2
# at delivery against 1.43 late postpartum and 1.07 in the controls, and the
# volume of distribution 0.24 L/kg against 0.16 and 0.17.  In the eight
# paired women (J Biomed Biotechnol 2012) clearance fell from 3.80 to 2.53
# L/h after the pregnancy.  Their enantiomer-specific analysis of the same
# cohort (Bosn J Basic Med Sci 2017) found the clearance and the steady-state
# volume of both S and R higher at delivery, S more than R, with the
# half-lives unchanged.
#
# The per-enantiomer factors are the BSA- and weight-normalised ratios of
# Kulo 2017's Table (delivery, n = 31, against the healthy female
# volunteers, n = 8), which remove the weight difference between the 73 kg
# delivery group and the 63 kg volunteers:
#     S-ketorolac: CL/BSA 3.47 / 2.06 = 1.68, Vss/BW 0.18 / 0.15 = 1.20
#     R-ketorolac: CL/BSA 1.18 / 0.59 = 2.00, Vss/BW 0.12 / 0.09 = 1.33
# S-ketorolac carries the analgesia, and its clearance rises less than R's.
# (The racemic 2012 ratio was a flat 1.5; the 2017 enantiomer analysis of
# the same cohort supersedes it.)
#
# With the Patient Profile's Pregnant field on, each enantiomer's clearance
# is multiplied by its own CL factor and each volume by its own V factor;
# the intercompartmental (distribution) clearances scale with the volume, so
# the distribution phase is unchanged and only the elimination clearance and
# the steady-state volume move.  Because clearance rises more than volume,
# the terminal half-life shortens a little (S x 1.20/1.68, R x 1.33/2.00),
# matching Kulo's delivery half-life point estimates (numerically shorter,
# not significant in n = 8).  (The R system's placeholder third volume, with
# no clearance to it, stays 1.)  The oral absorption is unchanged.  Ketorolac
# is highly bound to albumin, which falls in pregnancy; part of the rise may
# be a higher free fraction, in which case the unbound concentration changes
# less than the total plotted here, and surgery may contribute.  Claude Code,
# 2026-10-19.  See R/pregnancy.R.
#
# References
# ----------
# Cloesmeijer ME et al., Br J Clin Pharmacol 2021;87:1443-1454.
#   https://doi.org/10.1111/bcp.14547
# Jung D et al., Eur J Clin Pharmacol 1988;35:423-425 (intravenous, IM and oral
#   crossover). https://doi.org/10.1007/BF00561376
# Mroszczak EJ et al., Pharmacotherapy 1990;10:33S-39S.
#   https://pubmed.ncbi.nlm.nih.gov/2082311/
# Kulo A et al.  Pharmacokinetics of intravenous ketorolac following
#   caesarean delivery.  Int J Obstet Anesth 2012;21:334-338.
#   https://doi.org/10.1016/j.ijoa.2012.06.001 (pregnancy)
# Kulo A et al.  The impact of caesarean delivery on paracetamol and
#   ketorolac pharmacokinetics: a paired analysis.  J Biomed Biotechnol
#   2012;2012:437639.  https://doi.org/10.1155/2012/437639 (pregnancy)
# Kulo A et al.  Enantiomer-specific ketorolac pharmacokinetics in young
#   women, including pregnancy and postpartum period.  Bosn J Basic Med Sci
#   2017;17:54-60.  https://doi.org/10.17305/bjbms.2016.1515 (pregnancy)
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

KETOROLAC_MW_TROMETHAMINE <- 376.409   # g/mol, the labelled salt
KETOROLAC_MW_BASE         <- 255.273   # g/mol, ketorolac free acid
# Share of each labelled dose reaching each enantiomer as free acid
KETOROLAC_ENANTIOMER_FRACTION <- 0.5 * KETOROLAC_MW_BASE / KETOROLAC_MW_TROMETHAMINE
KETOROLAC_KA_PO <- log(2) / 3.8        # 1/min, Mroszczak 1990, provisional

# Pregnancy (see the header): enantiomer-specific, Kulo 2017's BSA/weight-
# normalised delivery-vs-volunteer ratios.  Clearance scales each enantiomer's
# elimination clearance; volume scales its volumes and distribution clearances.
KETOROLAC_PREGNANCY_S_CL_FACTOR <- 1.68   # S CL/BSA 3.47 / 2.06
KETOROLAC_PREGNANCY_S_V_FACTOR  <- 1.20   # S Vss/BW 0.18 / 0.15
KETOROLAC_PREGNANCY_R_CL_FACTOR <- 2.00   # R CL/BSA 1.18 / 0.59
KETOROLAC_PREGNANCY_R_V_FACTOR  <- 1.33   # R Vss/BW 0.12 / 0.09

#' Ketorolac pharmacokinetics (S + R enantiomers)
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, apply the changes of late pregnancy:
#'   every clearance and volume of both enantiomers x 1.5 (see the file's
#'   header)
#' @returns a list in the shape \code{getDrugPK()} expects, with the R
#'   enantiomer as a parallel system
#' @export
ketorolac <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)
  # Pregnancy (see the header): enantiomer-specific factors, Kulo 2017.  The
  # elimination clearance carries the CL factor; the volumes and the
  # intercompartmental (distribution) clearances carry the V factor, so the
  # distribution phase is unchanged and the terminal half-life shortens
  # because clearance rises more than volume.
  sClF <- sVF <- rClF <- rVF <- 1
  if (isTRUE(pregnant)) {
    sClF <- KETOROLAC_PREGNANCY_S_CL_FACTOR
    sVF  <- KETOROLAC_PREGNANCY_S_V_FACTOR
    rClF <- KETOROLAC_PREGNANCY_R_CL_FACTOR
    rVF  <- KETOROLAC_PREGNANCY_R_V_FACTOR
  }

  oral <- list(ka_PO = KETOROLAC_KA_PO, bioavailability_PO = 1, tlag_PO = 0)

  # S-ketorolac, three compartments
  default <- c(list(
    v1  = 4.03 * size$volume * sVF,
    v2  = 43.3 * size$volume * sVF,
    v3  = 5.90 * size$volume * sVF,
    cl1 = 3.97 / 60 * size$clearance * sClF,
    cl2 = 1.86 / 60 * size$clearance * sVF,
    cl3 = 19.7 / 60 * size$clearance * sVF
  ), oral)

  # R-ketorolac, two compartments
  rSet <- c(list(
    v1  = 4.43 * size$volume * rVF,
    v2  = 5.18 * size$volume * rVF,
    v3  = 1,
    cl1 = 1.45 / 60 * size$clearance * rClF,
    cl2 = 1.90 / 60 * size$clearance * rVF,
    cl3 = 0
  ), oral)

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Cloesmeijer ME et al., Br J Clin Pharmacol 2021;87:1443-1454 (S and R ",
    "enantiomers fitted separately, summed; doses as ketorolac tromethamine; ",
    "oral absorption provisional, Mroszczak 1990). ",
    "https://doi.org/10.1111/bcp.14547"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("S-ketorolac CL x ", KETOROLAC_PREGNANCY_S_CL_FACTOR, " and V x ",
             KETOROLAC_PREGNANCY_S_V_FACTOR, ", R-ketorolac CL x ",
             KETOROLAC_PREGNANCY_R_CL_FACTOR, " and V x ",
             KETOROLAC_PREGNANCY_R_V_FACTOR),
      paste0("Kulo A et al., Bosn J Basic Med Sci 2017;17:54-60. ",
             "https://doi.org/10.17305/bjbms.2016.1515")))
  }

  list(
    PK = PK,
    doseFraction = KETOROLAC_ENANTIOMER_FRACTION,
    parallelSystems = list(
      list(name = "R-ketorolac", doseFraction = KETOROLAC_ENANTIOMER_FRACTION,
           PK = list(default = rSet))
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
