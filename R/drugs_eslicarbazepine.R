# -----------------------------------------------------------------------------
# Eslicarbazepine: one compartment, apparent oral (from eslicarbazepine acetate)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-10, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# Eslicarbazepine acetate (Aptiom) is a prodrug: hydrolysed almost completely
# on first pass to eslicarbazepine, the active moiety (systemic prodrug is
# about 0.01% of exposure).  The curve is eslicarbazepine.
#
# Falcao A et al. (CNS Drugs 2012;26:79-91) fitted 641 phase III patients,
# one compartment, first-order absorption:
#
#     CL/F = (2.36 + ... co-medication ...) x (WT/70)^0.75 L/h
#     V/F, ka from the Simulations Plus AAPS 2013 poster (P3.243): V/F 61.3 L,
#     absorption half-life 0.296 h, i.e. ka 2.34 /h, with CL/F 2.43 L/h at
#     the base.
#
# The handoff said enzyme inducers add 1.24 L/h to CL/F; the peer-reviewed
# value is +1.41 L/h (Falcao).  Neither is applied: the app has no
# co-medication field, so the curve is eslicarbazepine without inducers.
#
# DOSE BASIS
# ==========
# The dose is entered as mg of eslicarbazepine ACETATE (the product strength).
# Eslicarbazepine is 0.858 of that by molecular weight (254.3 / 296.3); since
# the apparent parameters were fitted to eslicarbazepine-equivalent dosing,
# the acetate dose is multiplied by 0.858 through bioavailability_PO.  That is
# a mass conversion on an apparent scale, recorded in the registry, not a
# measured bioavailability.
#
# CHECKS: half-life 17 h (label 13-20 h); once-daily dosing gives a trough of
# about 7.4 mg/L at 800 mg and 11 mg/L at 1200 mg (Sunkaraneni 2018), which
# the model reproduces with the 0.858 factor.
#
# RENAL: clearance falls in renal impairment (eslicarbazepine is renally
# cleared), but Falcao found no creatinine-clearance term in this cohort, so
# none is applied.
#
# BODY SIZE: the model's own allometric weight, at the pharmacokinetic weight
# with the switch on.
#
# BAND: 3-35 mcg/mL, typical 15: the AGNP update 2026 (Hart et al.,
# Pharmacopsychiatry 2026;59:166-295, Table 5; trough 24 h after a dose),
# based on Jacob 2016's expert guideline and oxcarbazepine's MHD range.
# Norway's eslicarbazepine-specific 3-26 (Svendsen 2017, Reimers 2018) is
# narrower, on the ground that eslicarbazepine is 80% of racemic MHD.
#
# PREGNANCY
# =========
# There are NO pregnancy pharmacokinetic data for eslicarbazepine acetate
# itself.  The evidence applied is oxcarbazepine's: oxcarbazepine is reduced
# to its monohydroxy derivative (MHD, licarbazepine), about 80% of it the
# S-enantiomer, which IS eslicarbazepine, so the MHD measured in pregnant
# women is mostly the molecule plotted here.  Mazzucchelli and colleagues
# (Epilepsia 2006;47:504-509) found S-(+)-MHD "the most abundant compound in
# plasma" and its concentration markedly lower in pregnancy.  The dose-
# corrected MHD concentration in the third trimester was 64% of baseline in
# Christensen and colleagues (Neurology 2006;67:1497-1499; 9 pregnancies),
# 38.2% lower in Petrenaite and colleagues (Epilepsy Res 2009;84:245-249; 14
# pregnancies) and 44.3% lower in Duan 2026; MONEAD (Pennell 2022) found it
# lower by up to 32.6% (total) and 30.6% (unbound), and Voinescu 2018 a
# maximal 1.63-fold clearance in the second trimester.
#
# With the Patient Profile's Pregnant field on, CL/F (cl1) is multiplied by
# ESLICARBAZEPINE_PREGNANCY_CL_FACTOR = 1.6, a SURROGATE from oxcarbazepine's
# MHD: Christensen 1 / 0.64 = 1.56, Petrenaite 1 / (1 - 0.382) = 1.62, Duan 1
# / (1 - 0.443) = 1.80, MONEAD 11.55 / 7.79 = 1.48, geometric mean of the
# four 1.61.  V/F, the absorption and the 0.858 acetate-to-eslicarbazepine
# mass factor are unchanged.  Elimination is glucuronidation (UGT, induced in
# pregnancy as lamotrigine's is) and renal excretion; MHD is about 40% bound,
# and total and unbound fell alike in MONEAD.  Low confidence: no study of
# eslicarbazepine acetate in pregnancy exists.  See R/pregnancy.R.  (Claude
# Code, 2026-10-19.)
#
# References
# ----------
# Falcao A et al., CNS Drugs 2012;26:79-91.
#   https://doi.org/10.2165/11596290-000000000-00000
# Sunkaraneni S et al., J Pharmacokinet Pharmacodyn 2018;45(4):649-658.
#   https://doi.org/10.1007/s10928-018-9596-7
# Patsalos PN et al., Ther Drug Monit 2018;40:526-548.
#   https://doi.org/10.1097/FTD.0000000000000546
# Christensen J, Sabers A, Sidenius P.  Neurology 2006;67:1497-1499.
#   https://doi.org/10.1212/01.wnl.0000240047.11166.0e (pregnancy, MHD)
# Petrenaite V, Sabers A, Hansen-Schwartz J.  Epilepsy Res 2009;84:245-249.
#   https://doi.org/10.1016/j.eplepsyres.2009.01.011 (pregnancy, MHD)
# Mazzucchelli I, Onat FY, Ozkara C, et al.  Epilepsia 2006;47:504-509.
#   https://doi.org/10.1111/j.1528-1167.2006.00459.x (pregnancy, MHD
#   enantiomers)
# Duan et al.  Neurology 2026;107:e218456.
#   https://doi.org/10.1212/WNL.0000000000218456 (pregnancy, MHD)
# Pennell PB, Karanam A, Meador KJ, et al.  JAMA Neurol 2022;79:370-379.
#   https://doi.org/10.1001/jamaneurol.2021.5487 (pregnancy, MONEAD, MHD)
# -----------------------------------------------------------------------------

#' Mass of eslicarbazepine in a unit mass of eslicarbazepine acetate
ESLICARBAZEPINE_ACETATE_FRACTION <- 254.3 / 296.3

# CL/F in the third trimester relative to the model: a surrogate from
# oxcarbazepine's monohydroxy derivative, mostly eslicarbazepine (geometric
# mean of four studies, 1.61; see the header).
ESLICARBAZEPINE_PREGNANCY_CL_FACTOR <- 1.6

#' Eslicarbazepine pharmacokinetics (oral, from eslicarbazepine acetate)
#'
#' Falcao et al. (2012) with the Simulations Plus poster absorption; see the
#' header of the file.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies CL/F by
#'   \code{ESLICARBAZEPINE_PREGNANCY_CL_FACTOR}, from oxcarbazepine's
#'   monohydroxy derivative (see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
eslicarbazepine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  # Pregnancy (see the header): CL/F only.
  clPregnancy <- if (isTRUE(pregnant)) ESLICARBAZEPINE_PREGNANCY_CL_FACTOR else 1

  default <- list(
    v1  = 61.3 * (w / 70), v2 = 1, v3 = 1,
    cl1 = 2.43 * (w / 70)^0.75 / 60 * clPregnancy, cl2 = 0, cl3 = 0,
    ka_PO = 2.34 / 60,
    bioavailability_PO = ESLICARBAZEPINE_ACETATE_FRACTION,   # acetate -> moiety
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Falcao A et al., CNS Drugs 2012;26:79-91, with absorption from the ",
    "Simulations Plus AAPS 2013 poster: one compartment, apparent oral, the ",
    "active eslicarbazepine from eslicarbazepine acetate, CL/F 2.43 L/h ",
    "(no inducers), V/F 61.3 L, ka 2.34/h. ",
    "https://doi.org/10.2165/11596290-000000000-00000"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("clearance x ", ESLICARBAZEPINE_PREGNANCY_CL_FACTOR,
             ", from oxcarbazepine's monohydroxy derivative, mostly ",
             "eslicarbazepine"),
      paste0("Christensen J et al., Neurology 2006;67:1497-1499. ",
             "https://doi.org/10.1212/01.wnl.0000240047.11166.0e; Petrenaite V ",
             "et al., Epilepsy Res 2009;84:245-249")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 15, upperTypical = 35, lowerTypical = 3,
    reference = reference
  )
}
