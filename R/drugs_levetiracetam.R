# -----------------------------------------------------------------------------
# Levetiracetam: one compartment, oral and intravenous
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma (under 10% bound).
# Drafted by Claude Code, 2026-10-10, at the request of Steven L. Shafer; see
# the antiseizure registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE AND REDUCTION
# ====================
# No single retrievable population model carries both intravenous disposition
# and an explicit creatinine-clearance equation, so a model is assembled from
# sourced parts, and the assembly is recorded in the registry as a reduction:
#
#   - STRUCTURE AND TYPICAL CLEARANCE: Rhee SJ et al. (Epilepsy Res
#     2017;132:8-15), 425 Korean adults, one compartment, first-order
#     absorption, ka 2.44 /h FIXED, CL/F 3.9 L/h at a typical adult; weight
#     on both parameters, estimated glomerular filtration rate on clearance.
#   - VOLUME: 0.6 L/kg, the label's (Keppra: "close to the volume of
#     intracellular and extracellular water", 0.5-0.7 L/kg, with a half-life
#     of 7 +/- 1 h in adults).  Rhee's V/F, 65.3 L, is from 483 mostly trough
#     concentrations in 425 patients, which barely identify a volume; with
#     the clearance it gave a half-life of 11.6 h, against 7 +/- 1 h in every
#     rich-data study and the label.  Until 2026-10-14 the file carried 65.3 L
#     and its header mis-stated the resulting half-life as 8 h.
#   - RENAL SPLIT: the label and mass-balance work put about 66% of a dose
#     into the urine unchanged and about 24% through non-renal hydrolysis.
#     Clearance is therefore split 0.66 renal, proportional to Cockcroft-Gault
#     creatinine clearance against the reference patient's, and 0.34 non-renal
#     and fixed.  This reproduces the proportional fall in clearance with
#     renal function that the label and the critically ill models (Bilbao-
#     Meseguer 2021) show, without importing an intensive-care intercept.
#   - ROUTES: absolute oral bioavailability is essentially complete, so the
#     apparent parameters are read as absolute and the intravenous route (the
#     injection is interchangeable with the tablet mg for mg) uses them with
#     F = 1.
#
# The reference patient (70 kg, 40 y man, assumed creatinine 0.9 mg/dL) has a
# Cockcroft-Gault clearance of 108 mL/min, at which total clearance is Rhee's
# 3.9 L/h.
#
# CHECKS (70 kg adult)
# ====================
# Half-life ln 2 x 42 L / 3.9 L/h = 7.5 h (label 7 +/- 1 h).  Ramael 2006,
# 1500 mg in healthy adults (73 kg): intravenous clearance 3.82 L/h and oral
# 3.51 L/h, AUC 392 and 428 mg.h/L; the model gives a clearance of 3.9 L/h
# and an AUC of 385 mg.h/L for 1500 mg.  Ramael's end-of-infusion peak after
# 1500 mg over 15 min, 50.5 mcg/mL, is above the one-compartment model's 35
# mcg/mL: the early distribution phase is not in a one-compartment model, so
# the peak after a fast intravenous dose is understated.  Levetiracetam
# accumulates in renal impairment, which the renal split captures;
# haemodialysis is not represented.
#
# NOT OFFERED: the extended-release tablet (Keppra XR; a published input model
# was not confirmed) and the weak phenytoin interaction.
#
# RENAL FUNCTION: Cockcroft-Gault at the patient's creatinine (a child's on
# the adult scale) or an assumed normal one (R/renalFunction.R).
#
# BODY SIZE: V per kilogram (0.6 L/kg) and the renal clearance through
# Cockcroft-Gault, both at the pharmacokinetic weight with the switch on.
#
# BAND: 6-46 mcg/mL, typical 30: the AGNP update 2026 (Hart et al.,
# Pharmacopsychiatry 2026;59:166-295, Table 5; 12 h after a dose), which
# lowered ILAE's (Patsalos 2008) lower limit of 12.
#
# PREGNANCY
# =========
# Clearance rises early, often to its maximum by the first or second
# trimester, and stays raised to term.  In the third trimester: Tomson and
# colleagues (Epilepsia 2007;48:1111-1116; prospective, n = 7) found
# concentrations 40% of baseline outside pregnancy; Westin and colleagues
# (Seizure 2008;17:192-198; 21 pregnancies, n = 11 in the third trimester)
# a concentration/dose ratio 50% of baseline; Lopez-Fraile 2009 (n = 5) 62%
# of late postpartum levels; Duan 2026 (China, prospective) a ratio 31.0%
# lower (40.1% in the second trimester); MONEAD (Pennell 2022) a dose-
# normalized concentration lower by up to 36.8% (11.33 to 7.16 ug/L/mg).
# Voinescu 2018 reports a maximal 1.71-fold clearance in the first trimester
# (n = 18), Reisinger 2013 a mean PEAK clearance 207% higher (n = 15).
#
# With the Patient Profile's Pregnant field on, the TOTAL clearance (cl1,
# renal and non-renal together) is multiplied by
# LEVETIRACETAM_PREGNANCY_CL_FACTOR = 1.6.  The ratios are the largest
# gestational fall each study reports: Duan's third-trimester 1 / (1 - 0.310)
# = 1.45, MONEAD's second-trimester 11.33 / 7.16 = 1.58 (its third-trimester
# median is 7.6, a smaller 1.49), Lopez-Fraile's third-trimester 1 /
# 0.62 = 1.61, Westin 1 / 0.50 = 2.0 and Tomson 1 / 0.40 = 2.5: the two large
# prospective cohorts give 1.45-1.58, the three small series 1.6-2.5
# (geometric mean of all five 1.79), and 1.6 is weighted toward the large
# cohorts.  The factor is applied to the clearance directly, NOT through the
# renal split: the 50% rise in filtration would give only 0.66 x 1.5 + 0.34 =
# 1.33, which undershoots every study.  The Cockcroft-Gault term reads the
# creatinine as entered; a pregnant woman's measured creatinine, lower than
# the assumed nonpregnant value used when the field is blank, raises the
# renal part a little on top of the factor.  The volume and the absorption
# are unchanged.  Levetiracetam is under 10% bound, so total and unbound are
# the same.  Clearance returns to baseline within weeks of delivery (Westin),
# which is not represented.  See R/pregnancy.R.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# Rhee SJ et al., Epilepsy Res 2017;132:8-15.
#   https://doi.org/10.1016/j.eplepsyres.2017.02.011
# Bilbao-Meseguer I et al., Pharmaceutics 2021;13:1690.
#   https://doi.org/10.3390/pharmaceutics13101690
# Ramael S et al., Clin Ther 2006;28:734-744.
#   https://doi.org/10.1016/j.clinthera.2006.05.004
# Patsalos PN et al., Epilepsia 2008;49:1239-1276.
#   https://doi.org/10.1111/j.1528-1167.2008.01561.x
# Keppra (levetiracetam) US prescribing information, UCB: volume of
#   distribution 0.5-0.7 L/kg, half-life 7 +/- 1 h, 66% renal unchanged.
# Tomson T, Palm R, Kallen K, et al.  Epilepsia 2007;48:1111-1116.
#   https://doi.org/10.1111/j.1528-1167.2007.01032.x (pregnancy)
# Westin AA, Reimers A, Helde G, Nakken KO, Brodtkorb E.  Seizure
#   2008;17:192-198.  https://doi.org/10.1016/j.seizure.2007.11.027
#   (pregnancy)
# Lopez-Fraile IP, Cid AO, Juste AO, Modrego PJ.  Epilepsy Behav
#   2009;15:372-375.  https://doi.org/10.1016/j.yebeh.2009.04.006 (pregnancy)
# Duan et al.  Neurology 2026;107:e218456.
#   https://doi.org/10.1212/WNL.0000000000218456 (pregnancy)
# Pennell PB, Karanam A, Meador KJ, et al.  JAMA Neurol 2022;79:370-379.
#   https://doi.org/10.1001/jamaneurol.2021.5487 (pregnancy, MONEAD)
# Voinescu PE, Park S, Chen LQ, et al.  Neurology 2018;91:e1228-e1236.
#   https://doi.org/10.1212/WNL.0000000000006240 (pregnancy)
# Reisinger TL, Newman M, Loring DW, Pennell PB, Meador KJ.  Epilepsy Behav
#   2013;29:13-18.  https://doi.org/10.1016/j.yebeh.2013.06.026 (pregnancy)
# -----------------------------------------------------------------------------

#' Fraction of levetiracetam clearance that is renal
#' @keywords internal
LEVETIRACETAM_RENAL_FRACTION <- 0.66

#' Levetiracetam volume of distribution, L/kg (the label's 0.5-0.7 L/kg)
#' @keywords internal
LEVETIRACETAM_V_PER_KG <- 0.6

# Total clearance in the third trimester relative to the model: Duan 2026
# 1.45, MONEAD 1.58, Lopez-Fraile 1.61, Westin 2.0, Tomson 2.5, weighted
# toward the two large cohorts (see the header).
LEVETIRACETAM_PREGNANCY_CL_FACTOR <- 1.6

#' Levetiracetam pharmacokinetics (oral and intravenous)
#'
#' One compartment, clearance split renal (proportional to creatinine
#' clearance) and non-renal; see the header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the total
#'   clearance by \code{LEVETIRACETAM_PREGNANCY_CL_FACTOR}, not through the
#'   renal split (see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levetiracetam <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  crcl    <- creatinineClearanceCG(pkW, age, sex,
                                   adultEquivalentCreatinine(creatinine, age, sex))
  crclRef <- creatinineClearanceCG(70, 40, SEX_MALE, adultCreatinine(SEX_MALE))
  f <- LEVETIRACETAM_RENAL_FRACTION
  clTotal <- 3.9 * (f * (crcl / crclRef) + (1 - f))     # L/h
  # Pregnancy (see the header): the total clearance, not the renal split.
  if (isTRUE(pregnant)) clTotal <- clTotal * LEVETIRACETAM_PREGNANCY_CL_FACTOR

  default <- list(
    v1  = LEVETIRACETAM_V_PER_KG * pkW, v2 = 1, v3 = 1,
    cl1 = clTotal / 60, cl2 = 0, cl3 = 0,
    ka_PO = 2.44 / 60,
    bioavailability_PO = 1,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Rhee SJ et al., Epilepsy Res 2017;132:8-15 (one compartment, ka 2.44/h ",
    "fixed, CL/F 3.9 L/h), the volume 0.6 L/kg and the 66% renal / 34% ",
    "non-renal clearance split (renal proportional to Cockcroft-Gault) from ",
    "the Keppra label, which gives a half-life of 7.5 h; intravenous on the ",
    "label's 1:1 conversion. https://doi.org/10.1016/j.eplepsyres.2017.02.011"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("total clearance x ", LEVETIRACETAM_PREGNANCY_CL_FACTOR),
      paste0("Tomson T et al., Epilepsia 2007;48:1111-1116. ",
             "https://doi.org/10.1111/j.1528-1167.2007.01032.x; Westin AA et ",
             "al., Seizure 2008;17:192-198; Pennell PB et al. (MONEAD), JAMA ",
             "Neurol 2022;79:370-379")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 30, upperTypical = 46, lowerTypical = 6,
    reference = reference
  )
}
