# -----------------------------------------------------------------------------
# Mirabegron (Myrbetriq): three compartments, sequential zero- and first-order
# oral absorption from the extended-release (OCAS) tablet
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL.  Plasma concentration only: no bladder, heart-rate
# or blood-pressure effect is plotted.  Drafted by Claude Code, 2026-11-19, at
# the request of Steven L. Shafer.
#
# SOURCE
# ======
# There is no population model of mirabegron in adults in the journal
# literature (PubMed, 2026-11-19: Rittig 2020, PMID 31787582, is in children,
# and the rest are PBPK).  The source is the sponsor's population analysis
# (Astellas report 178-PK-015) as the FDA reproduced it in the Clinical
# Pharmacology and Biopharmaceutics Review of NDA 202611 (Myrbetriq, 2012;
# Reference ID 3129601), Appendix "Population PK Analyses of Mirabegron",
# printed pages 144-153 (PDF pages 158-167), read on the rendered pages:
# https://www.accessdata.fda.gov/drugsatfda_docs/nda/2012/202611Orig1s000ClinPharmR.pdf
#
# The structure was fitted in two steps.  First, a three-compartment model to
# the INTRAVENOUS data of 101 healthy subjects (studies 178-CL-033 and -076,
# 7.5 to 50 mg infused over 2 h; Table 7, page 147):
#
#     CL  = 50.5 L/h     V1  = 19.8 L
#     K12 = 2.39 /h      K21 = 0.0399 /h      (the slow compartment)
#     K13 = 3.15 /h      K31 = 0.374 /h       (the fast compartment)
#     CL and V1 x (WT / WTref)^0.50 (fixed) x (1 + 0.142 MALE)
#
# Then that disposition, every parameter fixed, was carried into the oral
# model fitted to 4081 patients with overactive bladder in six phase 2 and 3
# studies (15,106 samples; 25 to 200 mg of the OCAS tablet once daily; Table
# 8, pages 148-149), which added "sequential zero- and first-order absorption
# processes" (page 144) and the covariates:
#
#     D1  = 2.79 h  (zero-order release into the gut)     Ka = 0.109 /h
#     F1  = 0.220 at 50 mg x (DOSE/50)^0.443 x (1 - 0.170 MALE)
#           x (AGE/AGEref)^0.245 x (CrCL/CrCLref)^-0.258
#     V1  x (DOSE/50)^-0.453
#     (and terms for proton pump inhibitors on CL, weak or moderate CYP3A4
#     inhibitors and food in the non-Japanese studies on V1, not applied:
#     see NOT APPLIED)
#
# Covariates enter as PAR = theta1 (COVAR / TVCOVAR)^theta2 for a continuous
# covariate and theta1 (1 + theta2 COVAR) for a categorical one (page 147),
# so the reference is a woman (MALE = 0).
#
# ABSOLUTE PARAMETERS.  The disposition was fitted to intravenous data, so
# the volumes and clearances are absolute and F1 is a true bioavailability.
# Intravenous mirabegron is not a product, so the drug offers oral units only.
#
# IN LIBRARY TERMS the source's rate constants are fixed, so every volume and
# intercompartmental clearance follows V1: v2 (fast) = V1 K13 / K31, cl2 =
# V1 K13; v3 (slow) = V1 K12 / K21, cl3 = V1 K12 (the library numbers the
# faster peripheral compartment 2).  At the reference V2 is 167 L and V3 1186
# L, Vss 1373 L for a woman and 1567 L for a man; the label's Vss is about
# 1670 L and its intravenous clearance about 57 L/h (model 50.5 for a woman,
# 57.7 for a man).  The terminal half-life at 50 mg is 35.0 h; the label's
# "approximately 50 hours in patients" is non-compartmental.
#
# THE DOSE TERMS.  F1 and V1 depend on the size of each tablet dose.
#
#   * F1 (DOSE/50)^0.443 is carried by an oralSaturation block of the power
#     form, (DOSE/50)^0.443, which simCpCe() applies to each oral dose at its
#     own size before the release into the gut, with bioavailability_PO the
#     value at 50 mg.  That is the source's F1 exactly, dose by dose, and
#     reproduces the review's statement that AUCtau rises 20-fold from 25 to
#     200 mg (8^1.443 = 20.1, page 144).  The term is applied as fitted up to
#     200 mg, the largest studied dose; above it F1 is extrapolated, and it
#     reaches 1 near 600 mg in an old woman with poor renal function.
#
#   * V1 (DOSE/50)^-0.453 is NOT applied: V1 is held at its value at 50 mg,
#     the usual and the largest labelled dose.  The library carries a
#     per-dose parameter only by running each dose size as its own system
#     (doseDependent, as lixisenatide does), and simCpCe() cannot yet do that
#     for a zero-order oral release (it expands the release into rate rows
#     before splitting the doses by size).  Clearance, and so the AUC, does
#     not depend on V1, so every AUC is the source's; the peak is not.
#     Measured against a matrix-exponential solution of the source's
#     equations at the reference, at steady state on once-daily dosing:
#     Cmax 7% high and Ctrough 9% low at 25 mg, Cmax 7% low and Ctrough 11%
#     high at 100 mg, Cmax 13% low and Ctrough 24% high at 200 mg, exact at
#     50 mg; after a single dose, Cmax 22% high at 25 mg and 17% low at 100
#     mg.  The label gives Cmax rising 2.9-fold and AUCtau 2.6-fold from 50
#     to 100 mg, and 8.4- and 6.5-fold to 200 mg; the source model, with its
#     V1 term, gives Cmax 2.92- and 8.54-fold, and this one 2.72- and 7.39-.
#     A [decision] for Shafer: whether to extend simCpCe() so that the V1
#     term can be applied.
#
# The OCAS tablet's zero-order release into the gut runs through
# zeroOrderPO, which carries the oral doses on the tissue depot
# (R/subcutaneous.R), exactly.  With V1 held, the terminal half-life is
# 35.0 h at every dose (the source's model, whose rate constants are fixed,
# would give 41.8 h at 25 mg and 26.6 h at 200 mg).
#
# THE REFERENCE VALUES are not printed.  The review's own simulations of the
# covariate effects are anchored at a body weight of 70 kg, an age of 60
# years and a creatinine clearance of 92 mL/min (page 144), and the effects
# it states follow from the exponents at exactly those anchors (CrCL 25:
# (25/92)^-0.258 = 1.3996, "increased by 39.9%"; 142: 0.894, "decreased by
# 10.6%"; weight 30 kg: (30/70)^-0.5 = 1.528, "increased by 52.8%").  Age 60
# and a creatinine clearance of 91 mL/min are the medians of the oral data
# (Table 5, page 145).  These anchors are taken as the references, and the
# sponsor's simulated typical woman at 50 mg (Figure 20) is reproduced at
# them to within reading the figure (CHECKS, below); the weight anchor is
# the one the figure cannot separate from the others, and remains a
# [decision] for Shafer.
#
# RENAL FUNCTION: the Cockcroft-Gault creatinine clearance (the review prints
# "CrCL" in mL/min beside a separate GFR), from the entered serum creatinine
# or an assumed normal creatinine for the patient's age and sex when the
# field is blank, at the pharmacokinetic weight as for lixisenatide; held to
# the studied 27 to 262 mL/min (Table 5).  It acts on F1, not on clearance,
# as fitted: renal clearance is about a quarter of the total (label 12.3).
#
# AGE acts on F1 as (AGE/60)^0.245, held to the studied 18 to 91 years
# (Table 5): the age floor rule (AGENTS.md) applies to a term that would be
# 0 at age 0.  The model was fitted in adults; children (Rittig 2020) are
# not described.
#
# BODY SIZE: the model's own weight covariate on CL and V1, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off,
# as canagliflozin and lixisenatide do; its sex term is the source's own.
#
# NOT APPLIED.  Proton pump inhibitors (CL x 0.908) and weak or moderate
# CYP3A4 inhibitors (V1 x 1.426), and their missing-data indicators: no field
# exists.  Food (V1 x 1.187, non-Japanese studies only): held at the
# reference; the label says the tablet is taken with or without food, and
# the reviewer found the food result confounded (page 153).  CYP2D6
# genotype: not in the model (poor metabolisers' AUC 17% higher, review
# Appendix II, not significant in the population analysis).
#
# CHECKS.  The sponsor's own simulations of this model at 50 mg once daily
# at steady state (review Figure 2.3.1.2.1, page 28, and Figure 20, page
# 152, read off the rendered figures) peak near 12.8 ng/mL at about 3.5 h
# in the typical woman and 9.4 in the typical man, with troughs near 5.8
# and 4.1; this model gives 13.11 and 9.42, troughs 5.91 and 4.25, peaking
# at 3.6 h (label: Tmax about 3.5 h).  Figure 20's dose panel, normalised to
# 50 mg, peaks near 8.8, 12.8, 18.5 and 27 at 25, 50, 100 and 200 mg; the
# source's equations with its V1 term give 8.78, 12.75, 18.61 and 27.21 at
# the references assumed above, which is what confirms them.  The engine's
# curve matches a matrix-exponential solution of the implemented system to
# 1e-13, and its AUC is F x dose / CL.
#
# WHAT THE CURVE DESCRIBES.  F1 was fitted in patients dosing as they do,
# 72% of samples recorded fed (Table 6), and is 0.220 at 50 mg for the
# reference woman (0.183 for a man): lower than the 35% the label gives from
# the fasted absolute-bioavailability study in healthy subjects.  The oral
# data were sparse (one to three samples a visit) and the reviewer warned
# that "precisely characterizing the absorption PK profile of the oral
# formulation was challenging" (page 152): the model's absorption is slow
# (ka 0.109 /h) and its curve flat.  In healthy volunteers taking 50 mg once
# daily (study 178-CL-072, page 27), steady-state Cmax was 54.4 and 58.1
# ng/mL and AUC 413 and 471 ng.h/mL in young men and women, against this
# model's 9.4 and 13.1 ng/mL and 161 and 224 ng.h/mL; single 50 mg doses
# gave Cmax 29-33 ng/mL and AUC 297-340 ng.h/mL by age group (Table
# 2.3.1.1.1), against the model's 5.6 to 7.8 ng/mL and 158 to 218 ng.h/mL.
# Fasting raised AUC by 17% over a high-fat and by about 50% over a low-fat
# meal, and Cmax by 45% and 75% (study 178-CL-041), which explains part of
# the gap in the area and less of it in the peak.  So the curve is the
# sponsor's description of exposure in patients with overactive bladder,
# good for the area and the trough, and it understates the peak a fasting
# volunteer reaches by a factor of about four (AGENTS.md, Adherence: the
# outpatient model is preferred; a [decision] for Shafer whether this
# model's peak is acceptable).
#
# PREGNANCY: no human pharmacokinetics (label 8.1; PubMed 2026-11-19);
# registry row `unknown`.
#
# References
# ----------
# FDA Center for Drug Evaluation and Research.  Clinical Pharmacology and
#   Biopharmaceutics Review, NDA 202611 (mirabegron extended-release tablets,
#   Myrbetriq), 2012, Reference ID 3129601, Appendix (population PK, report
#   178-PK-015).
# Myrbetriq (mirabegron) prescribing information, Astellas, DailyMed setid
#   ba9e9e15-e666-4c56-9271-2e24739cfa2d.
# -----------------------------------------------------------------------------

MIRABEGRON_WEIGHT_REF <- 70          # kg (review's simulation anchor; [decision])
MIRABEGRON_AGE_REF    <- 60          # years (pooled median, Table 5)
MIRABEGRON_CRCL_REF   <- 92          # mL/min (review's simulation anchor)
MIRABEGRON_DOSE_REF   <- 50          # mg per administration (F1 is printed at 50 mg)
MIRABEGRON_DOSE_POWER <- 0.443       # Gamma_F1_Dose (Table 8)
MIRABEGRON_AGE_RANGE  <- c(18, 91)   # years, studied (Table 5)
MIRABEGRON_CRCL_RANGE <- c(27, 262)  # mL/min, studied (Table 5)
MIRABEGRON_D1_HOURS   <- 2.79        # h, zero-order release into the gut

#' Mirabegron pharmacokinetics (oral extended-release tablet)
#'
#' Three compartments fitted to intravenous data, sequential zero- and
#' first-order absorption, and a bioavailability that rises with the dose
#' (the FDA review of NDA 202611); see the header of the file.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
mirabegron <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  pkW  <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  male <- sex != SEX_FEMALE
  crcl <- creatinineClearanceCG(pkW, age, sex, adultEquivalentCreatinine(creatinine, age, sex))
  crcl <- min(max(crcl, MIRABEGRON_CRCL_RANGE[1]), MIRABEGRON_CRCL_RANGE[2])
  ageF <- min(max(age, MIRABEGRON_AGE_RANGE[1]), MIRABEGRON_AGE_RANGE[2])

  size <- (pkW / MIRABEGRON_WEIGHT_REF)^0.50 * (if (male) 1.142 else 1)
  cl   <- 50.5 * size             # L/h
  v1   <- 19.8 * size             # L, at the reference dose, 50 mg (see the header)
  k12  <- 2.39;  k21 <- 0.0399    # slow peripheral compartment (library v3)
  k13  <- 3.15;  k31 <- 0.374     # fast peripheral compartment (library v2)
  # F1 at 50 mg; the dose term is the oralSaturation block below
  f50  <- 0.220 * (if (male) 1 - 0.170 else 1) *
    (ageF / MIRABEGRON_AGE_REF)^0.245 * (crcl / MIRABEGRON_CRCL_REF)^-0.258

  default <- list(
    v1  = v1, v2 = v1 * k13 / k31, v3 = v1 * k12 / k21,
    cl1 = cl / 60, cl2 = v1 * k13 / 60, cl3 = v1 * k12 / 60,
    ka_PO = 0.109 / 60,
    bioavailability_PO = f50,
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    # Each tablet is released into the gut over D1, then absorbed first order
    zeroOrderPO = list(duration = MIRABEGRON_D1_HOURS * 60),
    # F1 x (dose / 50 mg)^0.443, each dose at its own size
    oralSaturation = list(form = "power", exponent = MIRABEGRON_DOSE_POWER,
                          Dref = MIRABEGRON_DOSE_REF, exampleDoses = c(25, 50, 100, 200),
                          on = "bioavailability"),
    reference = paste0(
      "FDA Clinical Pharmacology Review of NDA 202611 (Myrbetriq, 2012), sponsor's ",
      "population model (178-PK-015; 101 healthy subjects given intravenous and 4081 ",
      "patients with overactive bladder given oral mirabegron): three compartments, ",
      "CL 50.5 L/h, V1 19.8 L, K12 2.39/h, K21 0.0399/h, K13 3.15/h, K31 0.374/h, CL ",
      "and V1 x (weight/70)^0.5 x 1.142 in men; release into the gut over 2.79 h, ka ",
      "0.109/h, F 0.220 at 50 mg x (dose/50)^0.443 x 0.830 in men x (age/60)^0.245 x ",
      "(Cockcroft-Gault CrCL/92)^-0.258 from the entered or an assumed normal ",
      "creatinine; references 70 kg, 60 y and 92 mL/min assumed (unprinted); V1 held ",
      "at its 50 mg value (its dose term not applied); proton pump inhibitor, CYP3A4 ",
      "inhibitor and food terms not applied; plasma concentration only. ",
      "https://www.accessdata.fda.gov/drugsatfda_docs/nda/2012/202611Orig1s000ClinPharmR.pdf"
    )
  )
}
