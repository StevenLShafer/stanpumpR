# -----------------------------------------------------------------------------
# Insulin degludec (Tresiba U-100 and U-200), subcutaneous, once daily
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, doses in
# units (U), concentrations in mcU/mL (= mU/L).  Concentration only: no
# glucose, glucose infusion rate or HbA1c is plotted.  Drafted by Claude Code,
# 2026-11-18, at the request of Steven L. Shafer.
#
# SOURCE
# ======
# Tang D, Gao X, Zhu M, Shu C, Dong Y, Yu X, Chen H, Li X, Zhang X, Cai D,
# Feng S.  Population pharmacokinetic and pharmacodynamic modeling to support
# molar dose ratio determination of long-acting insulin analogs.  Clin
# Pharmacokinet 2026;65:417-429 (read in full, the PDF supplied by Shafer;
# the online supplement, with Tables S2-S4, was not).  Jiangsu Hengrui's
# joint population model of its analogue INS068 and of insulin degludec
# (IDeg), NONMEM FOCE-I, 307 subjects, 7989 concentrations by LC-MS/MS:
#
#   INS068-101-US  25 healthy men 18-55 y, BMI 18-27, single doses, IDeg 3.6
#                  and 7.2 nmol/kg, sampled to 96 h
#   INS068-102-US  99 adults with type 1 diabetes, 18-65 y, BMI 18-30,
#                  crossover, IDeg 0.4, 0.6 and 0.8 U/kg once daily for 9
#                  days, sampled to 120 h after the last dose
#   INS068-103     healthy men, INS068 only
#   INS068-201     179 adults with type 2 diabetes, troughs of INS068 only
#                  (the paper's own limitation: "no PK concentrations of IDeg
#                  were collected" in it)
#
# so degludec itself contributed 102 subjects and 3588 concentrations, from
# healthy men and adults with type 1 diabetes.  One compartment, first-order
# absorption after a lag, linear elimination.  Table 2 (final model):
#
#   ka 0.0476 /h, CL/F 1.93 L/h, Vc/F 20.9 L, ALAG1 0.4 h (INS068, healthy,
#   70 kg, "the typical subject"), with
#   IDeg on ka x 0.837; T1DM on ka x 0.85, on CL/F x 0.978; T2DM on ka x 0.18,
#   on CL/F x 0.764; T1DM/T2DM on Vc/F x 0.537; body weight "included as a
#   power function" (footnote a): CL/F x (WT/70)^1.04, Vc/F x (WT/70)^1.54.
#
# The text agrees with the table: "The ka of IDeg (0.0398 /h)" = 0.0476 x
# 0.837 = 0.03984; "the ka, CL/F, and Vc/F were reduced by 15, 2.2 and 46.3%
# ... in patients with T1DM ... (decreases of 82% [ka], 23.6% [CL/F] and 46.3%
# [Vc/F] ...) in T2DM"; the reference weight, 70 kg, is the forest plot's
# typical subject.  No parameter of degludec differs from INS068's but ka.
#
# THE PATIENT THE MODEL DESCRIBES (Shafer's decisions of 2026-11-18)
# ==================================================================
# * Population.  The Patient Profile has no diabetes field, so one population
#   is chosen: type 1 diabetes, the population in which Tang measured
#   degludec at steady state (study 102, the bulk of its degludec data) and
#   the one that takes the drug.  The choice matters little: at 70 kg and
#   0.4 U/kg daily the healthy values give an average steady-state level 2%
#   lower (CL/F 1.93 against 1.89 L/h), with a slightly smaller swing
#   (peak/trough 3.94/3.06 against 4.17/2.94 nmol/L), the type 1 volume being
#   smaller.  ka = 0.0476 x 0.837 x 0.85 = 0.033865 /h, CL/F = 1.93 x
#   0.978 = 1.88754 L/h, V/F = 20.9 x 0.537 = 11.2233 L at 70 kg.
#   Tang's type 2 diabetes factors are NOT applied: they were estimated from
#   INS068 troughs alone, ka's has a bootstrap interval of 0.123-1.15, and
#   for degludec they would put its absorption half-life at 97 h and its
#   steady state weeks away (and raise the average level 28%), which the
#   label (steady state in 3-4 days) and the FDA review ("the PK profile was
#   consistent between healthy, Type 1 and Type 2 diabetic subjects")
#   contradict.  Shafer agreed to leave them out (2026-11-18).  The healthy-subject values
#   (Table 2 without the T1DM factors: ka 0.03984 /h, CL/F 1.93 L/h, V/F
#   20.9 L) are used only in the checks below.
# * Weight.  The power terms are applied to the pharmacokinetic weight (fat-
#   free-mass equivalent with the switch on, total weight with it off).  Tang
#   prints only the 5th-95th percentiles of its 307 subjects' weights, 54.4-
#   93.1 kg ("the 90% distribution range"; the demographics are in Table S2,
#   in the supplement, not supplied).  Outside that range the two ends are
#   treated differently, on Shafer's decision of 2026-11-18:
#
#   ABOVE 93.1 kg the weight is held at 93.1.  Tang's exponent on CL/F, 1.04,
#   is steeper than Novo Nordisk's in adults: Kapitza 2015 (type 2 diabetes,
#   to 156.8 kg) measured exposure 0.77 at 156.8 kg and 1.38 at 41 kg against
#   85.8 kg, an exponent of 0.43-0.44, and the adult population model of
#   trial 3586 (the pediatric review below, its Table 2) has 0.76.  Holding
#   the weight reproduces Kapitza's measurement: at 150 kg and 0.4 U/kg
#   daily the held model's average level is 5.91 nmol/L against 5.68 by
#   Kapitza's exponent, where Tang's term carried on would give 3.60, 37%
#   too low.
#
#   BELOW 54.4 kg Tang's terms carry on, unheld.  Tang studied no child, but
#   Tresiba is approved from 1 year, and Novo Nordisk's pediatric population
#   model, which the FDA reviewed and accepted, puts weight on CL/F with an
#   exponent of 0.977 (95% CI 0.815-1.14), Tang's 1.04 inside it (FDA
#   Clinical Pharmacology Review of NDA 203314/S-03, 2016, Table 5, read in
#   full: 205 subjects aged 1.5-57 years weighing 11.2-102 kg, Table 4; age
#   was not a covariate once weight was in; "When dosed per kg body weight,
#   the exposure becomes independent of body weight").  The label agrees
#   (12.3: "After adjusting for body weight, the total exposure of TRESIBA at
#   steady state was independent of age").  Holding a child at 54.4 kg had
#   put a 20 kg child's level at a third of what per-kilogram dosing gives
#   (1.38 against 3.90 nmol/L at 0.4 U/kg).  Two residuals, stated on the
#   help page: below 11.2 kg the term is an extrapolation (with an exponent
#   near 1, per-kilogram dosing still gives the adult level); and Tang's
#   exponent on V/F, 1.54, is just outside that review's interval for V/F
#   (1.01, 0.49-1.52), which leaves the average level unchanged but makes an
#   11.2 kg child's peak 10% higher and trough 12% lower than an exponent of
#   1.01 would (4.92/2.81 against 4.47/3.19 nmol/L).
# * Units.  Doses in units: 1 U of degludec is 6 nmol (FDA OCP review of NDA
#   203314: "0.4 U/kg (2.4 nmol/kg)"; Tang: INS068's unit is 6 nmol "similar
#   to IDeg"); the molecular weight is 6.104 kDa (label, section 11).  The
#   model's volumes and clearances are amount-free, so a dose in U gives
#   U/L, x1000 mcU/mL.  A degludec mcU/mL is 6 pmol/L of total (more than 99%
#   protein-bound, label 12.3) degludec, and is NOT comparable with a human-
#   insulin concentration in mcU/mL: its affinity for the insulin receptor
#   is low and it is protein-bound, so "higher IDeg systemic levels ... are
#   necessary in vivo to achieve the glucodynamic effect" (FDA OCP review).
#   Tang's Table 4 labels its simulated steady-state exposures pg/mL and
#   does not print the simulated dose.  The numbers fit nmol/L: a healthy
#   70 kg subject at 0.6 U/kg (252 nmol) gives AUCtau,ss 252 / 1.93 =
#   130.6 nmol.h/L (Table 4: 130) and Cmax,ss 5.91 nmol/L (Table 4 median:
#   5.98), consistent with a molar amount basis shared by dose and
#   concentration, as the label's clearance (below) also is.
# * Bioavailability is inside the apparent CL/F and V/F (F = 1).
#
# FLIP-FLOP.  At 70 kg the elimination rate CL/V = 0.168 /h exceeds ka, so
# the terminal half-life is absorption's, ln 2 / 0.033865 = 20.5 h, as the
# label says ("determined primarily by the rate of absorption").
#
# CHECKS NOT USED IN THE FIT (computed independently, tests pin them)
# ===================================================================
# * Label (TRESIBA, revised 7/2022): type 1 diabetes, 0.4 U/kg once daily for
#   8 days: Cmax 4472 pmol/L at a median 9 h.  Model, 70 kg: 4174 pmol/L
#   (696 mcU/mL) at 8.1 h, 6.7% low.  Steady state "after 3-4 days": the
#   model's trough reaches 92% of steady state after the third dose and 97%
#   after the fourth.  Half-life "approximately 25 hours": the model's 20.5 h
#   is shorter (Tang sampled to 120 h after the last dose; Heise 2015,
#   abstract read, measured 25.4 h at steady state in type 1 diabetes).
# * FDA OCP review of NDA 203314, Table 7 (trial NN1250-3769, healthy, 0.4
#   U/kg single dose, geometric means): Cmax 2083 pmol/L, tmax 14 h (10-22),
#   CL/F 0.029 L/h/kg, Vd/F 0.667 L/kg, t1/2 16 h.  The healthy set at 70 kg:
#   Cmax 1833 pmol/L at 16.4 h (12% low), CL/F 0.0276 L/h/kg, CL/F / ka =
#   0.692 L/kg, t1/2 17.4 h.
# * Cao 2026 (Naunyn Schmiedebergs Arch Pharmacol, doi 10.1007/s00210-026-
#   06074-2, read in full, supplied by Shafer; 30 healthy Chinese men, 54.7-
#   75.6 kg, one 0.4 U/kg dose sampled only to 24 h): CL/F 1.49 L/h, V/F
#   25.13 L (Table 3; 25.3 in the abstract), ka 0.10 /h (Table 3 prints h-1;
#   the online abstract's "h" is a lost superscript), ALAG 0.72 h, BMI on
#   CL/F (1.21) and weight on V/F (3.03), whose functional form and reference
#   values are not printed.  Not used: the covariate model cannot be read,
#   and 24 h of sampling cannot identify a 17-25 h absorption half-life (its
#   own Discussion says so).  Tang's healthy set at its median 62.3 kg gives
#   CL/F 1.71 L/h, 15% above Cao's 1.49.
# * Kapitza 2015 (J Clin Pharmacol 55:1369-1377, supplied by Shafer; Novo
#   Nordisk's population PK of degludec in type 2 diabetes, model in an
#   appendix not supplied): exposure falls with weight, by less than Tang's
#   exponent says (above); age, sex (male/female 0.96), race, ethnicity and
#   injection site had no relevant effect (all within 0.80-1.25).
#
# PREGNANCY
# =========
# No pharmacokinetic study of degludec in pregnancy was found (PubMed, 2026-
# 11-18: EXPECT, Mathiesen 2023, is an efficacy trial; the rest are case
# series).  With the Patient Profile's Pregnant field on, CL/F x
# INSULIN_PREGNANCY_CL_FACTOR (1.25; Catalano 1998, the metabolic clearance
# rate of infused insulin at 34-36 weeks against before conception), the
# factor every insulin in the library takes, on Shafer's decision of
# 2026-11-18: "Insulin formulation should probably not materially affect
# clearance (unless specific data state otherwise)."  None do for degludec.
# Absorption and V/F are unchanged.
#
# References
# ----------
# Tang D et al.  Clin Pharmacokinet 2026;65:417-429.
#   https://doi.org/10.1007/s40262-025-01598-x
# Cao Y et al.  Pop PK-PD model for evaluating the effects of insulin
#   degludec in healthy adult Chinese males.  Naunyn Schmiedebergs Arch
#   Pharmacol 2026.  https://doi.org/10.1007/s00210-026-06074-2 (check)
# Kapitza C et al.  J Clin Pharmacol 2015;55:1369-1377.
#   https://doi.org/10.1002/jcph.549 (check)
# FDA Clinical Pharmacology Review, NDA 203314 (2012).
# FDA Clinical Pharmacology Review, NDA 203314/S-03 (pediatric, 2016).
#   https://www.accessdata.fda.gov/drugsatfda_docs/pediatric/203314-Insulin-degludec-clinpharm-prea.pdf
# Catalano PM, Drago NM, Amini SB.  Diabetes Care 1998;21:403-408.
#   https://doi.org/10.2337/diacare.21.3.403 (pregnancy)
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2015/203313Orig1s000_203314Orig1s000ClinPharmR.pdf
# -----------------------------------------------------------------------------

DEGLUDEC_REFERENCE <- "https://doi.org/10.1007/s40262-025-01598-x"

# Tang 2026 Table 2: the typical INS068 subject (healthy, 70 kg)
DEGLUDEC_TANG_KA <- 0.0476          # /h
DEGLUDEC_TANG_CL <- 1.93            # L/h
DEGLUDEC_TANG_V <- 20.9             # L
DEGLUDEC_TANG_LAG <- 0.4            # h
DEGLUDEC_TANG_IDEG_KA <- 0.837      # IDeg on ka
# Type 1 diabetes on ka, CL/F; type 1 or 2 diabetes on Vc/F
DEGLUDEC_TANG_T1DM <- c(ka = 0.85, cl = 0.978, v = 0.537)
# Body weight: power terms about 70 kg, held at the printed 95th percentile
# of Tang's subjects above it and unheld below (Shafer, 2026-11-18; see the
# header)
DEGLUDEC_TANG_WEIGHT <- list(reference = 70, cl = 1.04, v = 1.54,
                             upper = 93.1)

#' Insulin degludec (Tresiba U-100 and U-200): pharmacokinetics
#'
#' Tang 2026's joint population model of INS068 and insulin degludec: one
#' compartment, first-order absorption after a 0.4 h lag, at the type 1
#' diabetes typical values, body weight on CL/F and V/F held at 93.1 kg above.
#' Concentrations in mcU/mL of total degludec (1 U = 6 nmol).  See the file's
#' header for the source, the population chosen and the checks.
#'
#' @inheritParams cefazolin
#' @param pregnant when \code{TRUE}, CL/F x INSULIN_PREGNANCY_CL_FACTOR (see
#'   the file's header)
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
insulinDegludec <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            pregnant = FALSE)
{
  w <- DEGLUDEC_TANG_WEIGHT
  pkW <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight
  pkW <- min(pkW, w$upper)

  t1 <- DEGLUDEC_TANG_T1DM
  ka <- DEGLUDEC_TANG_KA * DEGLUDEC_TANG_IDEG_KA * t1[["ka"]]          # /h
  cl <- DEGLUDEC_TANG_CL * t1[["cl"]] * (pkW / w$reference)^w$cl      # L/h
  if (isTRUE(pregnant)) cl <- cl * INSULIN_PREGNANCY_CL_FACTOR
  v <- DEGLUDEC_TANG_V * t1[["v"]] * (pkW / w$reference)^w$v          # L

  default <- list(
    v1 = v, v2 = 1, v3 = 1,
    cl1 = cl / 60, cl2 = 0, cl3 = 0,
    ka_SC = ka / 60,
    bioavailability_SC = 1,   # apparent CL/F and V/F
    tlag_SC = DEGLUDEC_TANG_LAG * 60
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
      "Tang 2026 (Clin Pharmacokinet 65:417-429), joint population model of ",
      "INS068 and insulin degludec, at the type 1 diabetes typical values: one ",
      "compartment, ka 0.0476 x 0.837 (degludec) x 0.85 /h, lag 0.4 h, CL/F 1.93 ",
      "x 0.978 x (WT/70)^1.04 L/h, V/F 20.9 x 0.537 x (WT/70)^1.54 L, weight held ",
      "at 93.1 kg above; total degludec, 1 U = 6 nmol, concentration only. ",
      DEGLUDEC_REFERENCE)
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("CL/F x ", INSULIN_PREGNANCY_CL_FACTOR),
      paste0("Catalano PM et al., Diabetes Care 1998;21:403-408. ",
             "https://doi.org/10.2337/diacare.21.3.403")))
  }

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 0, upperTypical = 0, lowerTypical = 0,
    reference = reference
  )
}
