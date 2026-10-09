# -----------------------------------------------------------------------------
# Clonazepam: oral only, two compartments with a lag (dos Santos 2009)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL, total plasma.
#
# SOURCE
# ======
# dos Santos FM et al. gave 23 healthy men (19-45 y, within 15% of ideal body
# weight) a single 4 mg dose as two 2 mg immediate-release tablets after an
# overnight fast, sampled plasma for 72 h (16 samples), and fitted one- and
# two-compartment models with and without a lag (MONOLIX, SAEM).  The final
# pharmacokinetic model, Table 1, "2-Compartment With First Order Oral
# Absorption and Lag-Time", all apparent (oral) values:
#
#     tlag 0.369 h (RSE 8%)   ka 2.21 /h (21%)   Vc 141 L (9%)
#     k10 0.0207 /h (8%)      k12 0.0725 /h (27%)   k21 0.294 /h (26%)
#
# from which CL/F = k10 Vc = 2.92 L/h, Q/F = k12 Vc = 10.2 L/h and V2/F =
# k12 Vc / k21 = 34.8 L; the half-lives are 1.87 h and 42.2 h.  The
# simultaneous PK/PD fit (Table 2) re-estimated these within their errors
# (Vc 130 L, k10 0.0214 /h); Table 1 is used.  Read from the full paper.
#
# WHY NOT KRUIZINGA 2022
# ======================
# The ChatGPT specification this file was built from proposed Kruizinga MD et
# al. (Br J Clin Pharmacol 2022;88:2236-2245): 20 young adults given 0.5 or 1
# mg of oral SOLUTION, sampled to 48 h; two compartments, CL/F 2.98 L/h,
# V1/F 109.5 L, Q/F 61.37 L/h, V2/F 130.6 L, and an absorption mixture (75%
# slow, ka 1.106 /h; the rest fast, ka fixed at 100 /h).  Every value was
# verified against its Table 2.  It is not used because it describes tablets
# less well.  For 2 mg in the reference man:
#
#                       peak, ng/mL   at, h   AUC 0-72 h   terminal t1/2, h
#     dos Santos            12.5       2.0        475            42
#     Kruizinga (slow)       9.9       1.6        394            57
#     observed, 2 mg tablets: peak 14.9 at 1.7 h, AUC 0-inf 561 (Crevoisier
#     2003, n = 12); about 13 at about 3 h, AUC 0-72 about 360 (Genis-Najera
#     2024, n = 30); 16.9 (7.1-23.6) at 1-4 h, AUC 0-inf 581 (Berlin 1975, n
#     = 8); half-life 30-40 h (label), 38 h (Crevoisier), 19-60 h (Berlin).
#
# Kruizinga sampled only to 48 h, which overstates the half-life, and so the
# accumulation in long-term dosing, by about a third.  dos Santos's peak and
# half-life both fall within the observations; its AUC to infinity is about
# 20% above Crevoisier's and Berlin's (CL/F 2.92 against about 3.5 L/h).
#
# APPARENT SCALE, ORAL ONLY
# =========================
# dos Santos had oral data only, so every parameter is divided by an unknown
# bioavailability, which is about 0.9 (Crevoisier 2003: 90% oral, 93% IM;
# Berlin 1975: 0.98, 0.56-1.60).  These predict oral concentrations, not
# intravenous ones, and the intravenous studies do not fill the gap: Berlin
# and Dahlstrom gave 2 mg intravenously to 8 volunteers but sampled from 10
# min, so their two-compartment fits put the central volume at 48-241 L, and
# they note that resolving the first phase "requires very frequent sampling
# during the first 30 min, which was not done".  Two minutes after 0.5 mg
# IV, 27 +/- 18 ng/mL has been measured (Schols-Hendriks 1995), a central
# volume near 20 L.  No published model describes the first half hour after
# an injection, so clonazepam is offered by mouth only, with
# bioavailability_PO = 1 because the apparent scale already contains F.
#
# The lag of 22 min (0.369 h, RSE 8%) is an estimated parameter, "compatible
# with the time needed for disintegration, dissolution, and gastric emptying"
# of a tablet, and is kept, as gabapentin's and pregabalin's are: time until
# threshold reads blank for those minutes after each dose
# (test-recovery-lag.R).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# dos Santos had no covariates and enrolled men within 15% of ideal body
# weight; the values are read as the reference adult's.  Volumes scale with
# fat-free mass relative to the 70 kg, 170 cm reference male and clearances
# with that ratio ^ 0.75; adjustToFFM = FALSE uses the published values for
# everyone.
#
# NO EFFECT SITE
# ==============
# dos Santos: "the PK and PD of clonazepam were closely related within the
# first hours. This observation was the first indication that the site of
# action of clonazepam is not kinetically distinguishable from the plasma
# compartment so that an effect-compartment was a priori not relevant", and
# "the peak plasma concentration and peak effect occurred at the same time".
# A model with both an effect compartment and tolerance "was not retained".
# Only the plasma concentration is plotted, and it is the concentration that
# drives the effect.  The DSST fell by 72 +/- 3.7% at most, 1.5-4 h after the
# dose; the final PD model (Table 2) is a sigmoid Emax on plasma
# concentration, EC50 9.33 ng/mL, Hill 3.57, with acute tolerance raising
# the EC50 by up to 15.4 ng/mL with a half-time of about 21.5 h, so effects
# wane faster than concentrations fall.  MEAC 0.
#
# BAND
# ====
# 20-70 ng/mL, the reference range for epilepsy (ILAE, Patsalos 2008, as
# quoted by Kacirova 2016); typical 40, its midpoint, not a target.  For
# panic disorder and anxiety lower concentrations suffice: the AGNP
# reference range for anxiolytic use is 4-80 ng/mL (quoted by Kacirova 2016),
# and dos Santos's EC50 for psychomotor impairment in healthy men, before
# tolerance, is 9.3 ng/mL.  With this model 0.5 mg twice daily averages about
# 14 ng/mL at steady state, below the band.  No time-until-threshold level
# (endCe 0).
#
# NOT MODELLED
# ============
# Women (dos Santos enrolled men; Kruizinga found no sex effect, with weight
# fixed allometrically), the solution and orally disintegrating tablets,
# enzyme induction by other antiepileptic drugs (clearance 22-75% higher,
# Yukawa 2002), the elderly, hepatic disease, the 7-amino metabolite
# (inactive), and acute tolerance.
#
# References
# ----------
# dos Santos FM et al., Ther Drug Monit 2009;31:566-574.
#   https://doi.org/10.1097/FTD.0b013e3181b1dd76
# Kruizinga MD et al., Br J Clin Pharmacol 2022;88:2236-2245.
#   https://doi.org/10.1111/bcp.15152
# Berlin A, Dahlstrom H, Eur J Clin Pharmacol 1975;9:155-159.
#   https://doi.org/10.1007/BF00614012
# Crevoisier C et al., Eur Neurol 2003;49:173-177.
#   https://doi.org/10.1159/000069089
# Genis-Najera L, Sanudo-Maury ME, Neurol Ther 2024;13:141-152 (2 mg tablets).
#   https://doi.org/10.1007/s40120-023-00567-5
# Schols-Hendriks MW et al., Br J Clin Pharmacol 1995;39:449-451.
#   https://doi.org/10.1111/j.1365-2125.1995.tb04476.x
# Kacirova I et al., Medicine (Baltimore) 2016;95:e2881.
#   https://doi.org/10.1097/MD.0000000000002881
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Yukawa E et al., J Clin Pharmacol 2002;42:81-88.
#   https://doi.org/10.1177/0091270002042001009
# Klonopin (clonazepam) prescribing information.
#
# Drafted with Claude Code at the request of Steven L. Shafer, 2026-10-09,
# from a ChatGPT specification whose references and values were checked
# against the sources first (dos Santos and Berlin in full).
# -----------------------------------------------------------------------------

# dos Santos 2009, Table 1, final PK model: apparent values, per hour
CLONAZEPAM_VC   <- 141       # L
CLONAZEPAM_K10  <- 0.0207    # 1/h
CLONAZEPAM_K12  <- 0.0725    # 1/h
CLONAZEPAM_K21  <- 0.294     # 1/h
CLONAZEPAM_KA   <- 2.21      # 1/h
CLONAZEPAM_TLAG <- 0.369     # h

#' Clonazepam pharmacokinetics (oral)
#'
#' dos Santos et al. (2009): two compartments with first-order absorption
#' after a lag, apparent oral parameters, from immediate-release tablets.
#' Plasma only: the published effect is a direct function of plasma
#' concentration.  See the file's header.
#'
#' @inheritParams cefazolin
#' @param adjustToFFM \code{TRUE} (the default) scales volumes to fat-free mass
#'   and clearances to its 0.75 power; \code{FALSE} uses the published values
#'   for everyone.
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
clonazepam <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Size scaling (see the header): fixed published values, read as the
  # reference adult's.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  # Clearances and the peripheral volume from the published rate constants
  cl1Ref <- CLONAZEPAM_K10 * CLONAZEPAM_VC / 60      # 2.92 L/h, in L/min
  cl2Ref <- CLONAZEPAM_K12 * CLONAZEPAM_VC / 60      # 10.2 L/h, in L/min
  v2Ref  <- CLONAZEPAM_K12 * CLONAZEPAM_VC / CLONAZEPAM_K21   # 34.8 L

  default <- list(
    v1 = CLONAZEPAM_VC * size$volume,
    v2 = v2Ref * size$volume,
    v3 = 1,                                        # two compartments
    cl1 = cl1Ref * size$clearance,
    cl2 = cl2Ref * size$clearance,
    cl3 = 0,
    ka_PO = CLONAZEPAM_KA / 60,                    # 1/min
    bioavailability_PO = 1,                        # apparent (/F) parameters
    tlag_PO = CLONAZEPAM_TLAG * 60                 # 22.1 min
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Band, ng/mL plasma: the epilepsy reference range, typical its midpoint.
  typical      <- 40
  upperTypical <- 70
  lowerTypical <- 20

  reference <- paste0(
    "dos Santos FM et al., Ther Drug Monit 2009;31:566-574. ",
    "Two compartments with a lag, apparent oral parameters from ",
    "immediate-release tablets; plasma only (direct effect); oral only. ",
    "https://doi.org/10.1097/FTD.0b013e3181b1dd76"
  )

  return(
    list(
      PK = PK,
      # No effect site: dos Santos found none (see the header).
      tPeak = 0,
      tPeakRoute = ROUTE_PO,
      MEAC = 0,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
