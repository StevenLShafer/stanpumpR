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
# References
# ----------
# Cloesmeijer ME et al., Br J Clin Pharmacol 2021;87:1443-1454.
#   https://doi.org/10.1111/bcp.14547
# Jung D et al., Eur J Clin Pharmacol 1988;35:423-425 (intravenous, IM and oral
#   crossover). https://doi.org/10.1007/BF00561376
# Mroszczak EJ et al., Pharmacotherapy 1990;10:33S-39S.
#   https://pubmed.ncbi.nlm.nih.gov/2082311/
# (Claude Code, 2026-10-10, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

KETOROLAC_MW_TROMETHAMINE <- 376.409   # g/mol, the labelled salt
KETOROLAC_MW_BASE         <- 255.273   # g/mol, ketorolac free acid
# Share of each labelled dose reaching each enantiomer as free acid
KETOROLAC_ENANTIOMER_FRACTION <- 0.5 * KETOROLAC_MW_BASE / KETOROLAC_MW_TROMETHAMINE
KETOROLAC_KA_PO <- log(2) / 3.8        # 1/min, Mroszczak 1990, provisional

#' Ketorolac pharmacokinetics (S + R enantiomers)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects, with the R
#'   enantiomer as a parallel system
#' @export
ketorolac <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyClearance = (weight / 70)^0.75)

  oral <- list(ka_PO = KETOROLAC_KA_PO, bioavailability_PO = 1, tlag_PO = 0)

  # S-ketorolac, three compartments
  default <- c(list(
    v1  = 4.03 * size$volume,
    v2  = 43.3 * size$volume,
    v3  = 5.90 * size$volume,
    cl1 = 3.97 / 60 * size$clearance,
    cl2 = 1.86 / 60 * size$clearance,
    cl3 = 19.7 / 60 * size$clearance
  ), oral)

  # R-ketorolac, two compartments
  rSet <- c(list(
    v1  = 4.43 * size$volume,
    v2  = 5.18 * size$volume,
    v3  = 1,
    cl1 = 1.45 / 60 * size$clearance,
    cl2 = 1.90 / 60 * size$clearance,
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
