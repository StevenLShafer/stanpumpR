# -----------------------------------------------------------------------------
# Lacosamide: one compartment; oral and intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma (under 15% bound).
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer; see
# the antiseizure registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# BMC Pharmacol Toxicol 2026 (PMC13067655): 180 Chinese adults with epilepsy,
# 294 trough levels, doses 50-225 mg/day; one compartment, NONMEM.  Table 2,
# read in full:
#
#     CL/F = 1.86 L/h x 1.48^CBZ x 0.875^SEX x (CLCR / 119)^0.311
#     V    = 0.6 L/kg, FIXED from the literature
#     ka   = 6.47 /h, FIXED from the literature
#
# SEX is 1 for women; CBZ 1 with carbamazepine (not applied: the app has no
# co-medication field); CLCR is Cockcroft-Gault in mL/min, centred on 119.
# V and ka are not estimates (the data were troughs); they are part of the
# published model and are recorded as fixed in the registry.
#
# ROUTES
# ======
# Oral bioavailability is complete (about 100%, Cawello 2015) and the label
# makes intravenous and oral doses interchangeable mg for mg, so the
# intravenous route uses the same parameters with F = 1: an assumption
# anchored to the label, recorded as such.  With ka 6.47 /h the oral peak
# falls at about 0.6 h, earlier than the label's 1-4 h; the steady state is
# unaffected.
#
# RENAL FUNCTION: Cockcroft-Gault at the patient's creatinine (a child's on
# the adult scale) or, if none is entered, an assumed normal creatinine for
# age and sex (R/renalFunction.R).  About 40% of a dose is excreted unchanged.
#
# CHECKS (70 kg, 40 y man, assumed creatinine 0.9 mg/dL: CLCR 108)
# =================================================================
# CL/F 1.80 L/h, V 42 L, half-life 16 h (label 13 h).  Healthy Korean men,
# 100 and 200 mg twice daily: CL/F 1.92 and 1.78 L/h (AUC over the interval
# 52.1 and 112.4 mg.h/L; the model gives 55.5 and 111).
#
# BODY SIZE: V per kilogram and Cockcroft-Gault at the pharmacokinetic weight
# with the switch on, at total weight with it off.
#
# PHARMACODYNAMICS NOT PLOTTED: the FDA review's 12-hour-AUC Emax model for
# focal seizures was not retrievable with its coefficients.
#
# BAND: 10-20 mcg/mL, the commonly quoted range (ILAE gives no formal range
# for lacosamide; Patsalos 2018, 10-20 mg/L), typical 15.
#
# PREGNANCY
# =========
# MONEAD (Pennell and colleagues, JAMA Neurol 2022;79:370-379; prospective,
# pregnant women against the same women postpartum) found the dose-normalized
# lacosamide concentration fell by up to 39.9% (26.14 to 15.71 ug/L/mg, p <
# .001), declining with gestational week.  A pregnancy PBPK model evaluated
# against the MONEAD lacosamide data (Barry and colleagues, Epilepsia
# 2025;66:346-355; 16 pregnant and 11 nonpregnant women) predicts clearance
# 48.2% higher at 40 weeks.  A Chinese multicentre cohort (Duan 2026) found
# only a 10.8% fall, in the second trimester, and a six-patient Dutch series
# (Ter Horst 2025) concentration/dose ratios of 48-146% of baseline.
#
# With the Patient Profile's Pregnant field on, the whole CL/F (cl1) is
# multiplied by LACOSAMIDE_PREGNANCY_CL_FACTOR = 1.5: the PBPK estimate at
# term is 1.482 and MONEAD's maximal 26.14 / 15.71 = 1.66, and 1.5 sits
# between them, nearer the estimate at term.  The factor is applied to the
# clearance directly, NOT through the creatinine-clearance term: the 50% rise
# in filtration carried by that term would give only 1.5^0.311 = 1.13, and
# only about 40% of lacosamide is excreted unchanged, the rest demethylated
# by CYP2C19, 3A4 and 2C9.  The Cockcroft-Gault term reads the creatinine as
# entered; a pregnant woman's measured creatinine, lower than the assumed
# nonpregnant value used when the field is blank, raises it a little on top of
# the factor (at most 1.13-fold).  The volume and the absorption are
# unchanged.  Lacosamide is under 15% bound, so total and unbound change
# alike.  The source population (Chinese adults) was not pregnant.  See
# R/pregnancy.R.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# BMC Pharmacol Toxicol 2026, lacosamide in Chinese adults.
#   https://doi.org/10.1186/s40360-026-01114-2
# Cawello W, Clin Pharmacokinet 2015;54:901-914.
#   https://doi.org/10.1007/s40262-015-0276-0
# Patsalos PN et al., Ther Drug Monit 2018;40:526-548.
#   https://doi.org/10.1097/FTD.0000000000000546
# Pennell PB, Karanam A, Meador KJ, et al.  JAMA Neurol 2022;79:370-379.
#   https://doi.org/10.1001/jamaneurol.2021.5487 (pregnancy, MONEAD)
# Barry JM, et al.  Epilepsia 2025;66:346-355.
#   https://doi.org/10.1111/epi.18184 (pregnancy, PBPK)
# Duan et al.  Neurology 2026;107:e218456.
#   https://doi.org/10.1212/WNL.0000000000218456 (pregnancy)
# Ter Horst PGJ, Truong T, Mian P, Wegner I.  Eur J Clin Pharmacol 2025;82:10.
#   https://doi.org/10.1007/s00228-025-03924-5 (pregnancy)
# -----------------------------------------------------------------------------

# Whole CL/F at term relative to the published value: between the PBPK
# estimate at 40 weeks (1.482, Barry 2025) and MONEAD's maximal fall in the
# dose-normalized concentration (26.14 / 15.71 = 1.66); see the header.
LACOSAMIDE_PREGNANCY_CL_FACTOR <- 1.5

#' Lacosamide pharmacokinetics (oral and intravenous)
#'
#' One compartment with renal and sex covariates (BMC Pharmacol Toxicol
#' 2026); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the whole
#'   CL/F by \code{LACOSAMIDE_PREGNANCY_CL_FACTOR}, not through the renal term
#'   (MONEAD 2022, Barry 2025; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lacosamide <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  crcl <- creatinineClearanceCG(pkW, age, sex,
                                adultEquivalentCreatinine(creatinine, age, sex))
  female <- sex == SEX_FEMALE
  # Pregnancy (see the header): the whole clearance, not the renal term.
  clPregnancy <- if (isTRUE(pregnant)) LACOSAMIDE_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = 0.6 * pkW, v2 = 1, v3 = 1,
    cl1 = 1.86 * (if (female) 0.875 else 1) * (crcl / 119)^0.311 / 60 *
      clPregnancy,
    cl2 = 0, cl3 = 0,
    ka_PO = 6.47 / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "BMC Pharmacol Toxicol 2026 (180 Chinese adults): one compartment, ",
    "CL/F 1.86 L/h x 0.875 (women) x (Cockcroft-Gault/119)^0.311 from the ",
    "entered or an assumed normal creatinine, V 0.6 L/kg and ka 6.47/h ",
    "fixed; intravenous on the label's 1:1 conversion. ",
    "https://doi.org/10.1186/s40360-026-01114-2"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", LACOSAMIDE_PREGNANCY_CL_FACTOR),
      paste0("Pennell PB et al. (MONEAD), JAMA Neurol 2022;79:370-379. ",
             "https://doi.org/10.1001/jamaneurol.2021.5487; Barry JM et al., ",
             "Epilepsia 2025;66:346-355. https://doi.org/10.1111/epi.18184")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 15, upperTypical = 20, lowerTypical = 10,
    reference = reference
  )
}
