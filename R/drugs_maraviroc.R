# -----------------------------------------------------------------------------
# Maraviroc (Selzentry): oral, two compartments with a lag; the fraction
# absorbed and the absorption rate rise with the size of each dose (Chan 2008)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, amounts in mg,
# concentrations in ng/mL of maraviroc in plasma.  Concentration only: no
# CCR5 occupancy or antiviral effect is plotted.  Drafted by Claude Code,
# 2026-11-28, at the request of Steven L. Shafer (docs/library-expansion/
# plan.csv, Antivirals; "Yes to all drugs in group 1", 2026-11-28).  The
# candidate came through the nlmixr2lib cross-check (docs/nlmixr2lib/);
# nlmixr2lib's files were not used, and every number below was read on the
# PDF.
#
# SOURCE
# ======
# Chan PLS, Weatherley B, McFadyen L.  A population pharmacokinetic
# meta-analysis of maraviroc in healthy volunteers and asymptomatic
# HIV-infected subjects.  Br J Clin Pharmacol 2008;65(Suppl 1):76-85 (PMID
# 18333869; card papers/digests/PMID_18333869.md; read on the PDF, Tables 2-5
# and Equations 1-8).  17 Pfizer studies, 413 subjects (365 healthy, 48 with
# asymptomatic HIV infection; median 30 years, 18-54, and 71 kg, 46-109; 317
# men and 96 women; 95 Asian), single and multiple oral doses of 100 to 1200
# mg (to 900 mg twice daily), mostly fasted; 8951 concentrations.
#
# The model (Equations 1-5, Table 4):
#
#     F    = FABS x FHEP
#     FABS = Dose^1.39 / (51.2^1.39 + Dose^1.39)        (ABSEmax 1, fasted)
#     FHEP = 1 - EH,  EH = 0.662
#     CL   = FQ x EH + CLR = 59.59 x 0.662 + 12 = 51.45 L/h
#            (FQ, hepatic plasma flow, and CLR fixed in the source)
#     Vc   = 132 L
#     CLic = 16.4 L/h x (age/30)^0.349
#     Vp   = 277 L
#     ka   = 0.277 /h x Dose^0.173,  Tlag = 0.198 h
#
# The disposition parameters are systemic, not apparent: F was partitioned into
# FABS and FHEP through a hepatic plasma flow fixed at 59.59 L/h (hepatic blood
# flow 101 L/h and a blood:plasma ratio of 0.59) and a renal clearance fixed
# at 12 L/h, both from a mass-balance analysis the paper says would be
# published separately; every dataset was oral.
# Table 4's ka of 0.277 /h is the value at 1 mg (Equation 5): Table 5 gives
# 0.614 /h at 100 mg, 0.277 x 100^0.173.  The Results call 0.277 the 100 mg
# value, a slip.  F is 0.242 at 100 mg, 0.311 at 300 mg and 0.327 at 600 mg
# (Table 5; the abstract: 24% to 33%).
#
# DOSE SIZE.  FABS and ka depend on the size of each dose, so the model takes
# `dose` (mg) and declares doseDependent: simCpCe() runs each dose size as its
# own linear system with its own F and ka (doseSizePK(), R/getDrugPK.R) and
# adds them.  With no dose the reference is 300 mg, the label's dose without
# interacting drugs.  The dose in both terms is held to the studied unit
# doses, 100 to 1200 mg (MARAVIROC_DOSE_RANGE): below 100 mg FABS and ka fall
# toward 0, which no study measured.
#
# NOT APPLIED.  Race (Asian: EH x exp(-0.0948), and terms on Vp and CLic whose
# form is not printed) has no Patient Profile field; the curve is a
# non-Asian's.  Food (a high-fat meal: ka x 0.547, ABSEmax x exp(-0.258), ED50
# x exp(0.594)) is not applied: the reference is fasted, as most of the data
# were (93.3% of profiles), and the label lets maraviroc be taken with or
# without food; the paper finds a high-fat meal lowers the AUC by 26 to 43%.
# Drug interactions, which set the label's dose (150 mg twice daily with a
# strong CYP3A inhibitor, 600 mg with an inducer), are not modelled: a dose
# entered is simulated as if given alone.
#
# BODY SIZE.  Weight, sex and HIV status were tested and none was retained.
# By the library's rule every parameter is scaled all the same (docs/
# weight-adjustment.md, *Every parameter is scaled*): Vc and Vp by the volume
# factor, CL (FQ x EH + CLR) and CLic by the clearance factor, so EH and F
# are unchanged.  The age term on CLic is the source's, floored at 18 years,
# the youngest studied.
#
# BODY SIZE ANCHOR: the reference man, not the cohort.  Table 2 prints the
# median weight (71 kg) and age (30 years) and Table 3 the sexes (317 men, 96
# women), but no height or BMI, and the cohort is multinational (23% Asian),
# so its typical height would need NCD-RisC's heights for several countries
# (as ceftolozane's anchor, R/drugs_ceftolozane.R).  At 71 kg and 30 years,
# 77% men, the cohort is close to the reference man, so a cohort factor would
# move the parameters little.
#
# PREGNANCY
# =========
# Colbers A, Best B, Schalkwijk S, et al.  Maraviroc pharmacokinetics in
# HIV-1-infected pregnant women.  Clin Infect Dis 2015;61:1582-1589 (PMID
# 26202768; abstract read, PMC gives no full text).  18 women, each with a
# steady-state 12-hour profile in the third trimester and again 2 weeks or
# more after delivery (12 on 150 mg twice daily with a protease inhibitor, 2
# on 300 mg without one): the geometric mean ratio, pregnancy to postpartum,
# is 0.72 (90% CI 0.60-0.88) for AUCtau and 0.70 (0.58-0.85) for Cmax; the
# trough fell 15%.  AUC and Cmax falling alike is the signature of a fall in
# bioavailability rather than a rise in clearance, which lowers the AUC more
# than the peak, so the factor is applied to F: with the Patient Profile's
# Pregnant field on, F x 0.72 (MARAVIROC_PREGNANCY_F).  The trough's smaller
# fall is not reproduced by either mechanism alone.
#
# CHECKS
# ======
# Table 5's AUC after a single fasted dose (non-Asian): 471, 805, 1815 and 3817
# ng.h/mL at 100, 150, 300 and 600 mg, which is Dose x F / CL; the test pins
# the model to them.  The terminal half-life, 15.9 h (Results), is the
# model's.  At 300 mg twice daily the model's steady state in a 30-year-old
# man of 70 kg is an AUC12 of 1815 ng.h/mL, Cmax 345 and Cmin 39 ng/mL, the
# peak 1.9 h after the dose.  The Selzentry label (Table 11, not fitted
# here): 2908, 888 and 43.1 in 64 healthy volunteers (phase 1), 2550, 618
# and 33.6 in 8 asymptomatic HIV patients (phase 2a), and 1513-1865, 266-287
# and 37-60 in the phase 2b/3 trials.  The trough agrees with all of them;
# the AUC agrees with the patients' trials and is 38% below the volunteers';
# the peak is well below the richly sampled phase 1 and 2a means and above
# the sparse phase 2b/3 estimates.  The absorption phase is where the
# source's residual error peaked (74% at 0.95 h after a dose, a
# time-dependent error model), so the peak is the least certain part of the
# curve.
#
# References
# ----------
# Chan PLS, Weatherley B, McFadyen L, Br J Clin Pharmacol 2008;65(Suppl 1):76-85.
#   https://doi.org/10.1111/j.1365-2125.2008.03139.x
# SELZENTRY (maraviroc) tablets and oral solution, prescribing information,
#   ViiV Healthcare, NDA 022128 and 208984, revised 6/2026 (DailyMed setid
#   46f30ac5-c96b-429e-976d-8c5ee1c0761b).
# -----------------------------------------------------------------------------
MARAVIROC_EH      <- 0.662    # hepatic extraction ratio, non-Asian
MARAVIROC_FQ      <- 59.59    # L/h, hepatic plasma flow, fixed in the source
MARAVIROC_CLR     <- 12       # L/h, renal clearance, fixed in the source
MARAVIROC_VC      <- 132      # L
MARAVIROC_CLIC    <- 16.4     # L/h at 30 years
MARAVIROC_CLIC_AGE <- 0.349
MARAVIROC_AGE_REF <- 30       # years, the cohort's median
MARAVIROC_AGE_MIN <- 18       # years, the youngest studied
MARAVIROC_VP      <- 277      # L
MARAVIROC_KA_1MG  <- 0.277    # /h at 1 mg
MARAVIROC_KA_DOSE <- 0.173    # power of dose on ka
MARAVIROC_TLAG    <- 0.198    # h
MARAVIROC_ED50    <- 51.2     # mg
MARAVIROC_GAMMA   <- 1.39
MARAVIROC_DOSE_REF <- 300     # mg, the help tables' dose
MARAVIROC_DOSE_RANGE <- c(100, 1200)   # mg, the studied unit doses
MARAVIROC_PREGNANCY_F <- 0.72  # Colbers 2015, AUCtau third trimester / postpartum

#' Maraviroc pharmacokinetics (oral)
#'
#' Two compartments with first-order absorption after a lag; the fraction
#' absorbed and the absorption rate depend on the size of each dose (Chan
#' 2008); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param dose the size of one oral dose, mg (NULL: 300 mg)
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the oral
#'   bioavailability by 0.72 (Colbers 2015)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
maraviroc <- function(weight, height, age, sex, adjustToFFM = TRUE, dose = NULL,
                      pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  d <- if (is.null(dose)) MARAVIROC_DOSE_REF else dose
  d <- min(max(d, MARAVIROC_DOSE_RANGE[1]), MARAVIROC_DOSE_RANGE[2])
  fabs <- d^MARAVIROC_GAMMA / (MARAVIROC_ED50^MARAVIROC_GAMMA + d^MARAVIROC_GAMMA)
  ageTerm <- (max(age, MARAVIROC_AGE_MIN) / MARAVIROC_AGE_REF)^MARAVIROC_CLIC_AGE

  default <- list(
    v1  = MARAVIROC_VC * size$volume,
    v2  = MARAVIROC_VP * size$volume,
    v3  = 1,
    cl1 = (MARAVIROC_FQ * MARAVIROC_EH + MARAVIROC_CLR) * size$clearance / 60,
    cl2 = MARAVIROC_CLIC * ageTerm * size$clearance / 60,
    cl3 = 0,
    ka_PO = MARAVIROC_KA_1MG * d^MARAVIROC_KA_DOSE / 60,
    bioavailability_PO = fabs * (1 - MARAVIROC_EH) *
      (if (isTRUE(pregnant)) MARAVIROC_PREGNANCY_F else 1),
    tlag_PO = MARAVIROC_TLAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    doseDependent = TRUE,
    reference = paste0(
      "Chan PLS, Weatherley B, McFadyen L, Br J Clin Pharmacol 2008;65(Suppl 1):76-85, ",
      "Table 4 (413 healthy volunteers and asymptomatic HIV-infected subjects, 100-1200 mg, ",
      "fasted): two compartments, CL 51.45 L/h (hepatic extraction 0.662 of 59.59 L/h plus ",
      "renal 12 L/h), Vc 132 L, CLic 16.4 L/h x (age/30)^0.349, Vp 277 L; F = FABS x 0.338 ",
      "with FABS = Dose^1.39/(51.2^1.39 + Dose^1.39), ka 0.277 /h x Dose^0.173, lag 0.198 h, ",
      "each dose size its own system, the dose held to 100-1200 mg; non-Asian, fasted; no ",
      "interactions. https://doi.org/10.1111/j.1365-2125.2008.03139.x",
      if (isTRUE(pregnant)) paste0("; ", pregnancyReference(
        "oral bioavailability x 0.72",
        "Colbers 2015, Clin Infect Dis 61:1582-1589, AUCtau third trimester/postpartum")) else "")
  )
}
