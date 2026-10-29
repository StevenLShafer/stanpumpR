# -----------------------------------------------------------------------------
# Digoxin, intravenous (Cheng 1997, with Hornestam 2003's time to peak effect)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-10-19, at the request of Steven L. Shafer, from
# full texts he supplied.  An absolute intravenous parameter set, separate
# from the apparent oral digoxin (Yukawa 2001), as haloperidol and
# haloperidolIV are.
#
# THE PHARMACOKINETICS: CHENG 1997
# ================================
# Cheng JW, Charland SL, Shaw LM, Kobrin S, Goldfarb S, Stanek EJ, Spinler SA.
# Is the volume of distribution of digoxin reduced in patients with renal
# dysfunction?  Determining digoxin pharmacokinetics by fluorescence
# polarization immunoassay.  Pharmacotherapy 1997;17:584-590.  Eighteen
# adults (mean 44 years) in three groups of six by measured creatinine
# clearance (<10, 10-50, >50 mL/min), given 7 or 10 mcg/kg intravenously,
# 15 serum samples over 5-7 days, two compartments fitted per group.  The
# paper's regressions on creatinine clearance (Results):
#
#     CL  (L/h/kg) = 0.0003 x CLcr + 0.0676      (r2 0.41)
#     Vss (L/kg)   = 0.0115 x CLcr + 5.289       (r2 0.24)
#
# The central volume was 0.6 L/kg in the groups below 10 and above 50 mL/min
# (0.7 between), and is fixed at 0.6.  The intercompartmental clearance is
# not printed; it is solved, 0.615 L/h/kg, so that the normal-renal group's
# own means (CL 0.11, Vc 0.6, Vss 6.4 L/kg) give its distribution rate
# constant, alpha 1.3 /h.  That system's terminal half-life is 46 h against
# the 49 h printed, and Q is held at that value for every patient.
#
# This was chosen over Hornestam 2003's intravenous clearance, CL 9.88 L/h x
# (1 + 0.02 x (CrCL - 70)), because Hornestam's slope was fixed at the bound
# that kept clearance non-negative over its data and so gives zero
# clearance at 20 mL/min; Cheng's clearance has a non-renal floor.
#
# Renal function is Cockcroft-Gault from an adult-equivalent creatinine
# (Cheng measured it; Cockcroft-Gault is the bedside estimate), at the
# pharmacokinetic weight, held to 0-140 mL/min (the studied range reaches the
# mid-130s).  A blank creatinine field means an assumed normal creatinine for
# age and sex.
#
# THE EFFECT SITE: HORNESTAM 2003
# ===============================
# Hornestam B, Jerling M, Karlsson MO, Held P.  Intravenously administered
# digoxin in patients with acute atrial fibrillation: a population
# pharmacokinetic/pharmacodynamic analysis based on the Digitalis in Acute
# Atrial Fibrillation trial.  Eur J Clin Pharmacol 2003;58:747-755.  In 105
# patients the fall in ventricular rate followed an effect compartment with
# ke0 0.184 /h (half-time 3.8 h).  With Hornestam's own typical plasma model
# (CL 9.88 L/h, Vc 27.8 L, Q 71.8 L/h, Vp 444 L) that ke0 puts the peak
# effect 87 minutes after a bolus, and the effect site here is solved from
# that time to peak, so it carries over to Cheng's plasma curve.  That is a
# model-derived, cross-model anchor, not a peak observed on Cheng's curve:
# on Cheng's plasma model (70 kg man, assumed normal creatinine) 87 minutes
# needs ke0 0.44 /h, 2.4 times Hornestam's, and Hornestam's own 0.184 /h
# would put the peak at about 138 minutes.  The two structures disagree that
# much because Cheng's distribution is slower.  It is the effect site for
# RATE CONTROL.  Inotropy is far slower: Weiss M, Kang W,
# Pharm Res 2004;21:231-236 (doi 10.1023/b:pham.0000016236.36210.a6), fitting
# Kramer 1979's 1 mg bolus data in healthy men, found an equilibration
# half-time of 13 h for shortening of electromechanical systole, attributed
# to slow receptor binding and dependent on the dosing schedule.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# Per kilogram of actual weight, as Cheng dosed and reported: total weight
# with the switch off, the pharmacokinetic weight with it on.
#
# BAND
# ====
# 0.5-2 ng/mL, the serum range used for the oral digoxin, meaningful after
# distribution (6 h or more after a dose).
#
# PREGNANCY
# =========
# Hebert and colleagues gave 0.25 mg of oral digoxin to 13 women at 28 to 32
# weeks of gestation and again to the same women 6 to 10 weeks postpartum:
# the unbound renal clearance was 272 +/- 45 mL/min in pregnancy against
# 183 +/- 37 mL/min postpartum (x 1.49), and the unbound net secretion 109
# against 58 mL/min (x 1.88), so both filtration and renal P-glycoprotein
# rise.  The curve is TOTAL serum digoxin, so the ratio that matters is the
# total renal clearance: 181 +/- 25 against 115 +/- 25 mL/min, x 1.574, with
# the unbound fraction 67% against 63% (Hebert's Table 2, read from the
# supplied paper on 2026-10-27; the creatinine clearances, 163 +/- 23
# against 125 +/- 22 mL/min, are in the Results text).  An earlier version took 163
# against 125 mL/min as the total renal clearance; those are Hebert's
# CREATININE clearances, and applied the unbound ratio 1.49 to total
# clearance, giving 1.35.  About 73% of digoxin's clearance is renal in
# healthy adults (25 to 28% nonrenal, Iisalo 1977) -- an assumption carried
# over to Cheng's patients, not measured in them -- so the total clearance
# rises by 0.73 x 1.574 + 0.27 = 1.42.  Hebert's Discussion puts the renal
# share at 80 to 85%, uncited, which would give 1.46 to 1.49; Iisalo's
# measured fraction is kept.  With the Patient Profile's Pregnant
# field on, the elimination clearance CL1 is multiplied by
# DIGOXIN_IV_PREGNANCY_CL_FACTOR = 1.42, directly; V1, V2 and Q are
# unchanged.  Hebert did not estimate digoxin's systemic clearance (no
# reliable extrapolation beyond 48 h), so 1.42 is a derived factor.
#
# The factor is NOT carried through Cheng's regressions on creatinine
# clearance, for two reasons.  Cheng's slope gives renal function only about
# 31% of the clearance at 100 mL/min (0.03 of 0.0976 L/h/kg), so a 1.5-fold
# creatinine clearance would raise the clearance only about 1.15-fold, less
# than the measured renal change, which includes the rise in secretion that
# creatinine does not see; and Cheng's Vss rises with creatinine clearance
# too, which the rise in filtration of pregnancy should not do.  For the same
# reason the factor is applied whatever the creatinine field holds: a
# pregnant woman's own low creatinine moves Cheng's clearance only by its
# small renal slope (and moves Vss with it, as it does for anyone).  The
# 87-minute time to peak effect is unchanged.  Digoxin-like immunoreactive
# substances rise in pregnancy and cross-react in some immunoassays, so a
# measured serum digoxin may read higher than the curve.  Late pregnancy is
# the reference state.  (Claude Code, 2026-10-19.)
#
# References
# ----------
# Cheng JW, Charland SL, Shaw LM, et al.  Pharmacotherapy 1997;17:584-590.
#   https://pubmed.ncbi.nlm.nih.gov/9165563/
# Hornestam B, Jerling M, Karlsson MO, Held P.  Eur J Clin Pharmacol
#   2003;58:747-755.  https://doi.org/10.1007/s00228-002-0553-3
# Hebert MF, Easterling TR, Kirby B, et al.  Effects of pregnancy on CYP3A
#   and P-glycoprotein activities as measured by disposition of midazolam and
#   digoxin: a University of Washington specialized center of research
#   study.  Clin Pharmacol Ther 2008;84:248-253.
#   https://doi.org/10.1038/clpt.2008.1 (pregnancy)
# Iisalo E.  Clinical pharmacokinetics of digoxin.  Clin Pharmacokinet
#   1977;2:1-16.  https://doi.org/10.2165/00003088-197702010-00001
#   (pregnancy: the nonrenal fraction)
# -----------------------------------------------------------------------------

# Cheng 1997
DIGOXIN_IV_CL_INTERCEPT <- 0.0676   # L/h/kg
DIGOXIN_IV_CL_SLOPE     <- 0.0003   # L/h/kg per mL/min
DIGOXIN_IV_VSS_INTERCEPT <- 5.289   # L/kg
DIGOXIN_IV_VSS_SLOPE     <- 0.0115  # L/kg per mL/min
DIGOXIN_IV_V1  <- 0.6               # L/kg
DIGOXIN_IV_Q   <- 0.615             # L/h/kg, solved from group 3's alpha
DIGOXIN_IV_CRCL_RANGE <- c(0, 140)  # mL/min
# Hornestam 2003
DIGOXIN_IV_TPEAK <- 87              # min

# Elimination clearance in late pregnancy relative to the published value:
# Hebert 2008's x 1.574 in total renal clearance (181 / 115 mL/min) on the
# 73% of clearance that is renal (Iisalo 1977), 0.73 x 1.574 + 0.27 = 1.42
# (see the header).
DIGOXIN_IV_PREGNANCY_CL_FACTOR <- 1.42

#' Digoxin, intravenous (Cheng 1997; Hornestam 2003's time to peak effect)
#'
#' Two compartments per kilogram with clearance and steady-state volume
#' rising with creatinine clearance, and an effect site peaking 87 minutes
#' after a bolus; see the header of \code{R/drugs_digoxinIV.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine in mg/dL, or NULL for the assumed normal
#'   value
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   elimination clearance by 1.42 (Hebert 2008; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
digoxinIV <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL, pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- creatinineClearanceCG(pkW, age, sex, scr)              # mL/min
  crcl <- min(max(crcl, DIGOXIN_IV_CRCL_RANGE[1]), DIGOXIN_IV_CRCL_RANGE[2])

  clPerKg  <- DIGOXIN_IV_CL_INTERCEPT + DIGOXIN_IV_CL_SLOPE * crcl     # L/h/kg
  vssPerKg <- DIGOXIN_IV_VSS_INTERCEPT + DIGOXIN_IV_VSS_SLOPE * crcl   # L/kg
  # Pregnancy (see the header): elimination clearance only, not through the
  # regressions on creatinine clearance.
  clPregnancy <- if (isTRUE(pregnant)) DIGOXIN_IV_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = DIGOXIN_IV_V1 * pkW,
    v2  = (vssPerKg - DIGOXIN_IV_V1) * pkW,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = clPerKg * pkW / 60 * clPregnancy,
    cl2 = DIGOXIN_IV_Q * pkW / 60,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  creatinineNote <- if (is.null(creatinine))
    paste0("assumed normal serum creatinine ", format(scr), " mg/dL")
  else
    paste0("serum creatinine ", format(creatinine), " mg/dL")
  reference <- paste0(
    "Cheng JW, Charland SL, Shaw LM, et al., Pharmacotherapy 1997;17:584-590 ",
    "(https://pubmed.ncbi.nlm.nih.gov/9165563/): ",
    "two compartments, CL 0.0676 + 0.0003 x CLcr L/h/kg, Vss 5.289 + 0.0115 x ",
    "CLcr L/kg, Vc 0.6 L/kg (Cockcroft-Gault CLcr ", round(crcl), " mL/min from ",
    creatinineNote, ").  Time to peak effect 87 min, for the fall in ",
    "ventricular rate in acute atrial fibrillation (not inotropy), from ",
    "Hornestam B et al., Eur J Clin Pharmacol 2003;58:747-755 ",
    "(https://doi.org/10.1007/s00228-002-0553-3)."
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", DIGOXIN_IV_PREGNANCY_CL_FACTOR),
      paste0("Hebert MF et al., Clin Pharmacol Ther 2008;84:248-253. ",
             "https://doi.org/10.1038/clpt.2008.1")))
  }

  list(
    PK = PK,
    tPeak = DIGOXIN_IV_TPEAK,
    MEAC = 0,
    typical = 1.0,
    upperTypical = 2.0,
    lowerTypical = 0.5,
    reference = reference
  )
}
