# -----------------------------------------------------------------------------
# Tinzaparin: subcutaneous two-compartment anti-Xa model in DVT patients
# (Barrett 2001)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min, anti-Xa
# activity in IU/mL.  Doses are entered in anti-Xa international units (IU)
# subcutaneously; the model is apparent (CL/F, V/F), so the activity is read
# directly from the SC dose, as for enoxaparin.
#
# SOURCE
# ======
# Barrett JS, Gibiansky E, Hull RD, Planes A, Pentikis H, Hainer JW, Hua TA,
# Gastonguay M.  Population pharmacodynamics in patients receiving tinzaparin
# for the prevention and treatment of deep vein thrombosis.  Int J Clin
# Pharmacol Ther 2001;39:431-446.  PMID 11680668.  425 patients (2,631 anti-Xa
# observations) from two Phase III trials, NONMEM FO; a two-compartment model
# with first-order absorption and a constant endogenous anti-Xa activity.  The
# model used here is the FINAL model fitted to the full DVT-treatment data set
# (Run 101, Hull 1992; 175 anti-Xa IU/kg once-daily subcutaneous), Table 3,
# read in full on 2026-11-26:
#
#     CL  = 0.0176 L/h/kg (apparent, CL/F)      V3 (peripheral) = 0.864 L/kg
#     Vc  = 0.0981 L/kg   (central, CL/F)       Q               = 0.007 L/h/kg
#     KA  = 0.206 /h                            ENDO            = 0.098 IU/mL
#     CL_CREA = -0.213    CL_PIBW = -0.006
#
# All of CL, Vc, Q and V3 are weight-normalised (per kg), so they carry the
# patient's weight directly; KA is a rate constant.  The prophylaxis study
# (Planes 1999, Table 4) gave a similar structure but no covariates and was
# the extension of this base model across dose and indication, so the fuller
# treatment model is used (Steven L. Shafer's choice, 2026-11-26).
#
# WHAT THIS REPRESENTS
# ====================
# The adult DVT patient on weight-based tinzaparin.  The fit spans a broad
# adult range (age 21-92, weight 37-151 kg, plasma creatinine 0.29-5.5 mg/dL,
# PIBW -37.6 to +120%; Table 1).  Bioavailability is 1: the apparent
# parameters already carry the subcutaneous absorbed fraction (tinzaparin's
# absolute bioavailability is ~86.7% by anti-Xa activity, Fossler 2001, but
# that fraction is folded into CL/F and V/F and must not be applied again).
#
# COVARIATES ON CLEARANCE (Barrett 2001, Table 3, Eq 10)
# ======================================================
# The final model carries two continuous covariates on apparent clearance,
# applied as the published exponential, centred on the population medians
# (Steven L. Shafer's decision, 2026-11-26, to apply both as published):
#
#     CL/F = 0.0176 x WT x exp( -0.213 x (CREA - 1) - 0.006 x (PIBW - 14) )
#
# where CREA is the RAW plasma creatinine in mg/dL (not a creatinine
# clearance, unlike enoxaparin's and fondaparinux's MDRD term) centred on the
# median 1.0 mg/dL, and PIBW is the percent above ideal body weight centred on
# the median 14%.  The authors judged both effects "probably not clinically
# significant" over the range studied, but they do shift clearance at the
# extremes (Table 5: CL falls to 77% of typical at the 95th-percentile
# creatinine 2.2 mg/dL, and to 81% at the 95th-percentile PIBW 50%), so they
# are kept.  Ideal body weight is the Devine form the paper used (Eqs 1-2):
#
#     IBW_male   = 50 + (2.3/2.5) x max(0, height_cm - 152)
#     IBW_female = 45 + (2.3/2.5) x max(0, height_cm - 152)
#     PIBW       = (weight - IBW) / IBW x 100
#
# CREATININE
# ==========
# The plasma creatinine covariate reads the Patient Profile's serum creatinine
# on the adult-equivalent scale (adultEquivalentCreatinine(), R/renalFunction.R),
# so a child's value is placed on the adult scale the model was fitted on; with
# the field blank it is the ASSUMED NORMAL creatinine for age and sex
# (assumedCreatinine(): 0.9 mg/dL men, 0.8 mg/dL women from age 18), which is
# close to the model's median of 1.0 mg/dL, so a blank field leaves clearance
# near its typical value.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The model carries its own size covariate: every disposition parameter is
# per kilogram, so weight is the size term.  With the fat-free-mass switch on
# it is evaluated at the pharmacokinetic weight (size$pkWeight) and with it off
# at total body weight, as enoxaparin's weight terms are.  The PIBW covariate
# is a clinical index of the entered patient, so it always reads the actual
# weight and height, not the pharmacokinetic weight (as enoxaparin's creatinine
# term always reads the entered creatinine); with the switch off the model
# reproduces Barrett's published per-kilogram scaling exactly.
#
# ENDOGENOUS ANTI-Xa ACTIVITY
# ===========================
# The model includes a constant endogenous anti-Xa activity ENDO = 0.098 IU/mL
# (the baseline present before any dose).  The plotted curve is the DRUG-
# induced anti-Xa activity only (the two-compartment output), so ENDO is NOT
# added (Steven L. Shafer's decision, 2026-11-26), consistent with enoxaparin
# and fondaparinux, which carry no baseline; the drug's own contribution is
# exactly the compartment output.
#
# EFFECT SITE AND THRESHOLDS
# ==========================
# None: tPeak = 0 and the plotted curve is anti-Xa activity, not an effect
# site.  No band, no MEAC and no time until threshold: a simulated anti-Xa
# activity is not a bleeding or clotting risk or a procedure-safe time
# (the Anticoagulants rule, 2026-10-27; these are research and teaching
# models, not validated clinical decision support).
#
# PREGNANCY
# =========
# Recorded UNQUANTIFIED, not applied (R/pregnancy.R): Norris 2004 (PMID
# 15467910, 24 pregnant women on 50-100 IU/kg) found "the pharmacokinetics of
# tinzaparin are affected by pregnancy", with peak and 24-hour anti-Xa cover
# low and 46% of women on 50 IU/kg needing a dose increase, and Ni Ainle 2008
# (PMID 18832911, 37 high-risk pregnancies on 175 IU/kg) needed dose
# modification by peak anti-Xa in 45% -- so pregnancy lowers tinzaparin
# exposure (apparent clearance rises), as for the LMWH class, but neither
# reports a quantified pregnant-vs-nonpregnant pharmacokinetic ratio, so no
# factor is applied.  US US-market note: Innohep (tinzaparin) was withdrawn
# from the United States market in 2011; it remains marketed in Europe and
# Canada.
#
# References
# ----------
# Barrett JS, Gibiansky E, Hull RD, et al.  Int J Clin Pharmacol Ther
#   2001;39:431-446.  PMID 11680668.
# Fossler MJ, Barrett JS, Hainer JW, et al.  Anticoagulant pharmacodynamics of
#   tinzaparin following fixed intravenous and subcutaneous administration
#   relative to subcutaneous heparin in healthy volunteers.  Am J Health Syst
#   Pharm 2001;58:1614-1621 (absolute bioavailability).
# Norris LA, Bonnar J, Smith MP, Steer PJ, Savidge G.  Thromb Haemost
#   2004;92:791-796.  https://doi.org/10.1160/TH03-10-0618 (pregnancy)
# Ni Ainle F, Wong A, Appleby N, et al.  Blood Coagul Fibrinolysis
#   2008;19:689-692.  https://doi.org/10.1097/MBC.0b013e32830b14ef (pregnancy)
#
# (Claude Code, 2026-11-26, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

# Barrett 2001, Table 3 (final model, DVT-treatment full data set), per kg.
TINZAPARIN_CL <- 0.0176   # L/h/kg, apparent clearance (CL/F)
TINZAPARIN_VC <- 0.0981   # L/kg, apparent central volume (Vc/F)
TINZAPARIN_KA <- 0.206    # 1/h, first-order subcutaneous absorption
TINZAPARIN_Q  <- 0.007    # L/h/kg, intercompartmental clearance
TINZAPARIN_V3 <- 0.864    # L/kg, apparent peripheral volume (V3/F)
TINZAPARIN_ENDO <- 0.098  # IU/mL, endogenous baseline (documented, not added)

# Covariates on clearance (Table 3; Eq 10), centred on the population medians.
TINZAPARIN_CL_CREA <- -0.213   # on raw plasma creatinine (mg/dL)
TINZAPARIN_CL_PIBW <- -0.006   # on percent above ideal body weight
TINZAPARIN_CREA_REF <- 1.0     # mg/dL, median plasma creatinine
TINZAPARIN_PIBW_REF <- 14      # %, median percent above ideal body weight

# Ideal body weight intercepts (Devine; Barrett 2001, Eqs 1-2), kg.
TINZAPARIN_IBW_INTERCEPT <- c(male = 50, female = 45)

#' Tinzaparin pharmacokinetics (subcutaneous, anti-Xa, DVT patients)
#'
#' Barrett 2001; see the header of \code{R/drugs_tinzaparin.R}.
#'
#' @inheritParams apixaban
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tinzaparin <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight

  # Covariates on CL (Barrett 2001, Table 3; Eq 10): raw plasma creatinine in
  # mg/dL on the adult-equivalent scale (blank = assumed normal) centred at
  # the median 1.0, and percent above ideal body weight centred at 14.  PIBW
  # is a clinical index of the entered patient, so it reads the actual weight
  # and height, not the pharmacokinetic weight.
  crea <- adultEquivalentCreatinine(creatinine, age, sex)
  ibw  <- TINZAPARIN_IBW_INTERCEPT[[sex]] + (2.3 / 2.5) * max(0, height - 152)
  pibw <- (weight - ibw) / ibw * 100
  clFactor <- exp(TINZAPARIN_CL_CREA * (crea - TINZAPARIN_CREA_REF) +
                  TINZAPARIN_CL_PIBW * (pibw - TINZAPARIN_PIBW_REF))

  default <- list(
    v1  = TINZAPARIN_VC * pkW,                 # central (Vc)
    v2  = TINZAPARIN_V3 * pkW,                 # peripheral (V3)
    v3  = 1,                                   # two compartments
    cl1 = TINZAPARIN_CL * pkW * clFactor / 60, # elimination, L/min
    cl2 = TINZAPARIN_Q  * pkW / 60,            # intercompartmental Q, L/min
    cl3 = 0,
    ka_SC = TINZAPARIN_KA / 60,                # 1/min
    tlag_SC = 0,
    bioavailability_SC = 1                     # apparent scale
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Barrett JS et al., Int J Clin Pharmacol Ther 2001;39:431-446 ",
    "(PMID 11680668: subcutaneous first-order-input two-compartment anti-Xa ",
    "model in DVT patients; apparent per-kilogram parameters, clearance on ",
    "raw plasma creatinine and percent above ideal body weight, from the ",
    "entered creatinine or an assumed normal value for age and sex). ",
    "Research and teaching model, not validated clinical decision support."
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
