# -----------------------------------------------------------------------------
# Lamotrigine: one compartment, apparent oral (immediate release)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Milosheska D et al. (Br J Clin Pharmacol 2016;82:399-411, PMC4972156): 100
# Slovenian adults with epilepsy on lamotrigine alone or with other drugs;
# one compartment, NONMEM.  The base model, from the abstract:
#
#     ka 1.96 /h   CL/F 2.32 L/h   V/F 77.6 L
#
# The final model adds UGT2B7 genotype, weight, renal function, smoking and
# co-medication; those terms are not used (the app has none of them except
# weight and creatinine, and the equations are in tables not retrieved).
#
# WHY NOT THE HANDOFF'S MODEL
# ===========================
# The handoff proposed Chavez-Castillo 2020 (J Pharm Sci): CL = 1.82 (1 -
# 0.465 VPA)(1 + 0.841 CBZ) L/h with V fixed at 1.8 L/kg.  The clearance is
# confirmed (its AAN 2019 precursor), but 1.8 L/kg gives a monotherapy half-
# life of about 48 h against the 24-33 h observed (Cohen 1987 measured V/F 1.2
# L/kg).  Milosheska's base model is complete in one source and consistent.
#
# CHECKS (70 kg adult)
# ====================
# Half-life 23 h (Cohen 1987: 24 h single dose, 25.5 h multiple dose in
# healthy volunteers); 200 mg/day averages 3.6 mg/L at steady state (COMPASS
# neutral group: about 5.9 mg/L at 200 mg/day... 142 mg.h/L over 24 h).  The
# curve is a MIXED co-medication population: valproate roughly halves
# clearance (half-life 59-70 h) and enzyme inducers roughly double it (13-15
# h); neither is applied.
#
# NOT OFFERED: Lamictal XR (no published input model; COMPASS gives peak 4-11
# h) and oestrogen contraceptives (which raise clearance about a third).
# Pregnancy, listed here until 2026-10-19, is now applied: see PREGNANCY.
#
# PREGNANCY
# =========
# The best-characterised antiseizure drug in pregnancy.  Karanam and
# colleagues (Pharmacotherapy 2025;45:33-42, read in full; the MONEAD
# population model, 170 pregnant and 51 nonpregnant women, 1227 samples)
# found a baseline CL/F of 2.79 L/h, the same in both groups, and in about
# 91% of pregnant women an Emax increase of 4.9 L/h with half the maximum at
# 12 weeks' gestation (to 275% of baseline at saturation); about 9% had no
# significant change.  Clearance returned to baseline after delivery with a
# rate constant of 1.27 /week.  The earlier population model of Polepally and
# colleagues (Ann Clin Transl Neurol 2014;1:99-106; 60 women, 64
# pregnancies) gave 2.16 to 6.88 L/h by the end of pregnancy in 77%; Pennell
# 2008 found total clearance 94% (free 89%) higher in the third trimester;
# MONEAD's dose-normalized concentrations fell by up to 56.1% (15.60 to 6.85
# ug/L/mg); Duan 2026 found the third-trimester concentration/dose ratio
# 63.2% lower.
#
# With the Patient Profile's Pregnant field on, CL/F (cl1) is multiplied by
# LAMOTRIGINE_PREGNANCY_CL_FACTOR = 2.3, Karanam's model at 36 weeks in the
# 91% whose clearance rises: the increase 4.9 x 36 / (12 + 36) = 3.68 L/h,
# and (2.79 + 3.68) / 2.79 = 2.32 (2.33 at 38 weeks).  Weighting by the 9%
# with no change gives 1 + 0.91 x 1.32 = 2.20; the cross-checks are MONEAD
# 15.60 / 6.85 = 2.28, Pennell 2008 1.94, Duan 1 / (1 - 0.632) = 2.72 and
# Polepally 6.88 / 2.16 = 3.19 (in 77%).  V/F and the absorption are
# unchanged (Karanam could not identify a change in V/F).  Free and total
# change alike (Pennell 2008: +89% and +94%; about 55% bound).
#
# Karanam scaled CL/F allometrically on the last NONPREGNANT weight; this
# library scales on the weight entered, the pregnant weight, so with the
# fat-free-mass switch on the weight gained in pregnancy already raises CL/F
# a little and the factor, measured against nonpregnant weight, counts that
# small part twice.  The clearance is the third-trimester value, and the
# about one woman in ten whose clearance does not rise is not represented: a
# measured level should guide the dose.  See R/pregnancy.R.  (Claude Code,
# 2026-10-19.)
#
# BODY SIZE: the base model's fixed values scaled to fat-free mass; with the
# switch off, the published values for everyone (legacyVolume = 1).
#
# BAND: 3-15 mcg/mL (ILAE, Patsalos 2008, 2.5-15), typical 8.
#
# References
# ----------
# Milosheska D et al., Br J Clin Pharmacol 2016;82:399-411.
#   https://doi.org/10.1111/bcp.12984
# Cohen AF et al., Clin Pharmacol Ther 1987;42:535-541.
#   https://doi.org/10.1038/clpt.1987.193
# Tompson DJ et al., Epilepsia 2008;49:410-417 (COMPASS).
#   https://doi.org/10.1111/j.1528-1167.2007.01274.x
# Karanam A, Pennell PB, Meador KJ, Long Y, Birnbaum AK.  Pharmacotherapy
#   2025;45:33-42.  https://doi.org/10.1002/phar.4640 (pregnancy)
# Polepally AR, Pennell PB, Brundage RC, et al.  Ann Clin Transl Neurol
#   2014;1:99-106.  https://doi.org/10.1002/acn3.29 (pregnancy)
# Pennell PB, Peng L, Newport DJ, et al.  Neurology 2008;70:2130-2136.
#   https://doi.org/10.1212/01.wnl.0000289511.20864.2a (pregnancy)
# Pennell PB, Karanam A, Meador KJ, et al.  JAMA Neurol 2022;79:370-379.
#   https://doi.org/10.1001/jamaneurol.2021.5487 (pregnancy, MONEAD)
# Duan et al.  Neurology 2026;107:e218456.
#   https://doi.org/10.1212/WNL.0000000000218456 (pregnancy)
# -----------------------------------------------------------------------------

# CL/F at 36 weeks relative to the nonpregnant value: Karanam 2025's Emax
# model, (2.79 + 4.9 x 36 / (12 + 36)) / 2.79 = 2.32 (see the header).
LAMOTRIGINE_PREGNANCY_CL_FACTOR <- 2.3

#' Lamotrigine pharmacokinetics (oral, immediate release)
#'
#' Milosheska et al. (2016) base model; see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies CL/F by
#'   \code{LAMOTRIGINE_PREGNANCY_CL_FACTOR} (Karanam 2025; see the file's
#'   header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lamotrigine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                        pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  # Pregnancy (see the header): CL/F only.
  clPregnancy <- if (isTRUE(pregnant)) LAMOTRIGINE_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = 77.6 * size$volume, v2 = 1, v3 = 1,
    cl1 = 2.32 * size$clearance / 60 * clPregnancy, cl2 = 0, cl3 = 0,
    ka_PO = 1.96 / 60,
    bioavailability_PO = 1,    # apparent
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Milosheska D et al., Br J Clin Pharmacol 2016;82:399-411, base model: ",
    "one compartment, apparent oral, ka 1.96/h, CL/F 2.32 L/h, V/F 77.6 L; ",
    "mixed co-medication, none applied. https://doi.org/10.1111/bcp.12984"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", LAMOTRIGINE_PREGNANCY_CL_FACTOR, " (36 weeks)"),
      paste0("Karanam A et al., Pharmacotherapy 2025;45:33-42. ",
             "https://doi.org/10.1002/phar.4640")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 8, upperTypical = 15, lowerTypical = 3,
    reference = reference
  )
}
