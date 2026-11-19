# -----------------------------------------------------------------------------
# Tamsulosin (Flomax)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Drafted by Claude Code, 2026-11-19, at the request of Steven L. Shafer, from
# the full text of Tsuda Y, Tatami S, Yamamura N, Tadayasu Y, Sarashina A,
# Liesenfeld K-H, Staab A, Schaefer H-G, Ieiri I, Higuchi S.  Population
# pharmacokinetics of tamsulosin hydrochloride in paediatric patients with
# neuropathic and non-neuropathic bladder.  Br J Clin Pharmacol
# 2010;70:88-101 (PMID 20642551, PMC2909811), read on the archive's PDF, its
# Table 3 checked on the page image.  Tests:
# tests/testthat/test-drugs-tamsulosin.R.
#
# WHAT THE SOURCE IS
# ==================
# 189 children aged 2 to 16 years (12.1 to 92.2 kg, median 25.0 kg; 105 boys,
# 84 girls) with neuropathic (144) or non-neuropathic (45) bladder, from three
# Boehringer Ingelheim trials: frequent sampling in trials 1 and 2 and sparse
# sampling in trial 3.  Single and repeated once-daily doses of 0.1 to 0.8 mg
# tamsulosin hydrochloride, the capsule's modified-release pellets sprinkled on
# a teaspoon of applesauce or yogurt, FED (a biowaiver bridged the pellets to
# the commercial capsule on dissolution).  NONMEM, FOCE with interaction.
#
# The model (Table 3) is ONE compartment with first-order absorption after a
# lag, every parameter apparent in F:
#
#   CL/F = 2.28 L/h x (WT/70)^0.75 x (AAG/20.0)^-0.844
#   V/F  = 37.5 L   x (WT/70)      x (AAG/20.0)^-0.663
#   Ka   = 0.368 /h,  ALAG1 = 0.957 h
#
# The authors tested two compartments (no better on AIC), zero-order and
# sequential zero- then first-order absorption (worse, or unstable), and kept
# first order with a lag.  So the modified-release capsule is carried by the
# slow ka (half-time of absorption 1.9 h) and the 57-minute lag, which the
# engine represents exactly: no oralZeroOrder or transit chain is needed.
#
# THE SOURCE IS IN CHILDREN; THE DRUG IS FOR MEN WITH BPH
# =======================================================
# This is the only population model of tamsulosin read in full.  Nikolic 2024
# (World J Urol 42:427, PMID 39037497), in 65 men with benign prostatic
# hyperplasia, is the population the drug is for, but it is not in the archive
# or in PubMed Central, so it could not be read: its abstract prints a
# clearance of 0.719 L/h and a steady-state volume of 32 L for a two-
# compartment model fitted to steady-state levels, a clearance a third of
# Tsuda's at 70 kg, which nothing in the abstract explains and which would
# give three times the label's exposure.  It is a [source] to obtain.
#
# Tsuda's model is used for adults because its size model was built for the
# purpose (fixed allometry, the estimated exponents 0.768 and 0.807 bracketing
# the fixed ones) and, more to the point, because it reproduces the US label's
# adult measurements, which were never in its fit.  The Flomax label (NDA
# 020579, s033, 2019), Table 2: 0.4 mg once daily with a light breakfast in 23
# healthy men aged 18 to 32, at steady state, Cmax 10.1 ng/mL, Cmin 4.0 ng/mL,
# Tmax 6.0 h, AUCtau 151 ng.h/mL.  This model at 70 kg gives Cmax 10.25,
# Cmin 3.87, Tmax 5.96 h and AUCtau 175 (16% high), and an elimination
# half-life of 11.4 h against the label's apparent 9 to 13 h in healthy
# volunteers.  The label's 0.8 mg arm in men aged 55 to 75 gives 440 ng.h/mL,
# 220 per 0.4 mg, so the model's 175 lies between the young and the old men.
# The paper itself compares its simulations with these two adult studies
# (Figure 5) and calls the children's exposure comparable.
#
# BODY SIZE
# =========
# The source's allometry, weight^0.75 on CL/F and weight^1 on V/F at a 70 kg
# standard, is the library's, so with the switch off the published model is
# reproduced exactly (legacyVolume = WT/70, legacyClearance = (WT/70)^0.75)
# and with it on the same exponents act on fat-free mass
# (docs/weight-adjustment.md).  The model carries its own size covariate on
# every parameter, so there is no cohort anchor to apply.  Ka and the lag are
# not scaled: the paper's equations name CL/F and V/F only.
#
# ALPHA-1-ACID GLYCOPROTEIN IS HELD AT THE SOURCE'S REFERENCE
# ===========================================================
# Tamsulosin is 94 to 99% bound, mostly to AAG (label), and AAG lowers both
# CL/F and V/F of the TOTAL drug.  The Patient Profile has no AAG field, so
# AAG is held at the paper's reference, 20.0 uM (its median, 19.95), and the
# terms drop out.  The paper's own simulation puts the steady-state AUC at
# 40 uM 3.2 times that at 10 uM.  AAG is raised in older men, in renal and
# hepatic impairment and after surgery: the label says that in renal and
# hepatic impairment the TOTAL concentration changes with AAG while the
# unbound concentration does not.  The curve is total tamsulosin.
#
# WHAT IS NOT APPLIED, AND WHY
# ============================
# Age: the label's cross-study comparison gives 40% more exposure at 55-75
# than at 20-32 years from a fall in intrinsic clearance; no fitted age term
# exists, and the source studied children.  Food: the source was fed and the
# label doses the capsule half an hour after the same meal each day; fasting
# raises the AUC by 30% and Cmax by 40-70% (label), which this model does not
# show.  CYP2D6: paroxetine raised the AUC 1.6-fold (label, measured), and the
# label EXPECTS a similar rise in poor metabolisers without measuring it; no
# source quantifies a phenotype on a parameter, so the model does not read the
# Patient Profile's CYP2D6 field (AGENTS.md, pharmacogenetic covariates).
# Renal function: under 10% is excreted unchanged (label); no renal term.
#
# DOSE BASIS
# ==========
# The product is tamsulosin HYDROCHLORIDE, the source doses and measures
# tamsulosin hydrochloride, and the label's concentrations are of tamsulosin
# hydrochloride, so a dose is entered as labelled with no conversion
# (Shafer, 2026-11-10: the approved product's basis).
#
# PREGNANCY
# =========
# Flomax is not indicated in women.  It is used off label as medical
# expulsive therapy for ureteral stones, including in pregnancy (Theriault
# 2020, a safety series, no concentrations).  No pharmacokinetics in
# pregnancy were found, so nothing is applied and the model does not name
# `pregnant` (inst/extdata/pregnancyRegistry.csv, group `unknown`).
#
# NO EFFECT SITE, NO BAND
# =======================
# No concentration-effect relation is used and no therapeutic range is
# established: tPeak, MEAC and the band are zero.
# -----------------------------------------------------------------------------

# Tsuda 2010, Table 3, at 70 kg and AAG 20.0 uM, with relative standard errors.
TAMSULOSIN_CL_F    <- 2.28    # L/h  (4.21% RSE)
TAMSULOSIN_V_F     <- 37.5    # L    (6.35% RSE)
TAMSULOSIN_KA      <- 0.368   # 1/h  (12.0% RSE)
TAMSULOSIN_LAG_H   <- 0.957   # h    (1.14% RSE)
TAMSULOSIN_CL_EXP  <- 0.75    # fixed allometric exponent on CL/F
# Held at the paper's reference, 20.0 uM: the terms (AAG/20.0)^-0.844 on CL/F
# and (AAG/20.0)^-0.663 on V/F are then 1 (see the header).
TAMSULOSIN_AAG_REFERENCE <- 20.0   # uM
TAMSULOSIN_AAG_CL        <- -0.844
TAMSULOSIN_AAG_V         <- -0.663

#' Tamsulosin pharmacokinetics (oral modified-release capsule, apparent)
#'
#' Tsuda et al. (2010), one compartment with first-order absorption after a
#' lag, allometric in weight, alpha-1-acid glycoprotein held at the source's
#' reference; see the file header.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tamsulosin <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # The source's allometry is the library's (weight^0.75 on CL/F, weight on
  # V/F, at 70 kg), so the legacy factors reproduce the published model with
  # the switch off.
  size <- pkSizeFactors(
    weight, height, age, sex, adjustToFFM,
    legacyVolume    = weight / FFM_REFERENCE_WEIGHT,
    legacyClearance = (weight / FFM_REFERENCE_WEIGHT)^TAMSULOSIN_CL_EXP)

  aag <- TAMSULOSIN_AAG_REFERENCE / TAMSULOSIN_AAG_REFERENCE   # held: 1
  clF <- TAMSULOSIN_CL_F * aag^TAMSULOSIN_AAG_CL
  vF  <- TAMSULOSIN_V_F  * aag^TAMSULOSIN_AAG_V

  default <- list(
    v1  = vF * size$volume,
    v2  = 1,                                   # one compartment
    v3  = 1,
    cl1 = clF / 60 * size$clearance,           # L/h -> L/min
    cl2 = 0,
    cl3 = 0,
    ka_PO = TAMSULOSIN_KA / 60,                # 1/min
    bioavailability_PO = 1,                    # apparent (/F) parameters
    tlag_PO = TAMSULOSIN_LAG_H * 60            # min
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 0    # no concentration-effect relation is used
  MEAC  <- 0

  # No therapeutic range is established for tamsulosin.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste0(
    "Tsuda Y, Tatami S, Yamamura N, Tadayasu Y, Sarashina A, Liesenfeld K-H, ",
    "Staab A, Schaefer H-G, Ieiri I, Higuchi S. Population pharmacokinetics ",
    "of tamsulosin hydrochloride in paediatric patients with neuropathic and ",
    "non-neuropathic bladder. Br J Clin Pharmacol 2010;70:88-101. One ",
    "compartment, apparent in bioavailability, from 189 children aged 2-16 ",
    "years given the capsule's pellets fed: CL/F 2.28 L/h and V/F 37.5 L at ",
    "70 kg, weight^0.75 on CL/F and weight on V/F, ka 0.368 /h after a lag ",
    "of 0.957 h. Alpha-1-acid glycoprotein, which lowers CL/F and V/F of total ",
    "drug, is held at the source's reference of 20 uM (no Patient Profile ",
    "field). Used for adults because it reproduces the Flomax label's ",
    "steady-state Cmax, Cmin and Tmax at 0.4 mg once daily in young men, with ",
    "an AUC 16% above the label's; fasting (+30% AUC), age, CYP2D6 and renal ",
    "function are not applied. Doses and concentrations are tamsulosin ",
    "hydrochloride. https://doi.org/10.1111/j.1365-2125.2010.03662.x"
  )

  list(
    PK = PK,
    tPeak = tPeak,
    MEAC = MEAC,
    typical = typical,
    upperTypical = upperTypical,
    lowerTypical = lowerTypical,
    reference = reference
  )
}
