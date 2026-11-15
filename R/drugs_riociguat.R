# -----------------------------------------------------------------------------
# Riociguat (Adempas): one compartment, oral, from Saleh 2016's population
# model in pulmonary hypertension (PATENT-1/2 and CHEST-1/2)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-15, at the request of Steven L. Shafer, who
# asked for the drugs of pulmonary arterial hypertension.  Source, read in
# full (PMC4860535): Saleh S, Becker C, Frey R, Muck W.  Population
# pharmacokinetics and the pharmacokinetic/pharmacodynamic relationship of
# riociguat in patients with pulmonary arterial hypertension or chronic
# thromboembolic pulmonary hypertension.  Pulm Circ 2016;6(Suppl 1):S86-S96.
# doi 10.1086/685404.
#
# THE SOURCE
# ==========
# 5,245 sparse plasma concentrations from 698 patients (PATENT-1/2, PAH, 438;
# CHEST-1/2, CTEPH, 260; 79% and 66% women; ages 18-80; 0.5 to 2.5 mg three
# times a day), NONMEM, one compartment.  Table 2, part A:
#
#     absorption rate 2.17 /h (fixed, from earlier work)
#     clearance 1.81 L/h            volume 32.3 L
#     volume: +0.619% per kg of body weight above or below the median (68 kg)
#     clearance: +120% in smokers; +35.6% with bosentan;
#                +13.6% per mg/dL of bilirubin above or below 0.58 mg/dL;
#                +0.394% per mL/min of baseline creatinine clearance above
#                or below 81.2 mL/min
#
# The paper does not say whether the covariate terms add or multiply, nor
# which equation gave the creatinine clearance.  They are applied here as
# linear terms that multiply,
#
#     CL = 1.81 (1 + 0.00394 (CrCl - 81.2)) (1 + 0.136 (bili - 0.58)) ...
#     V  = 32.3 (1 + 0.00619 (WT - 68))
#
# with the creatinine clearance by Cockcroft-Gault (creatinineClearanceCG(),
# R/renalFunction.R), the usual estimator of Bayer's riociguat programme.
# Both are assumptions, stated on the help page.  The patient is held at a
# non-smoker, not taking bosentan, with a bilirubin of 0.58 mg/dL (the median),
# so CL is 1.81 L/h at a creatinine clearance of 81.2 mL/min.  Creatinine
# clearance is held within the range studied, 15.4 to 187 mL/min (Table 1,
# baseline): the line stays positive everywhere, but beyond those it is an
# extrapolation.
#
# The parameters are apparent (/F); the absolute bioavailability is "about
# 94%" (Adempas label; Becker 2016), so they are close to absolute.
#
# CHECKS
# ======
# The Adempas label: clearance "about 1.8 L/h in patients with PAH and about
# 3.4 L/h in healthy subjects", terminal half-life "about 12 hours in
# patients and 7 hours in healthy subjects", Vss about 30 L.  This model, at
# the median patient: 1.81 L/h, 12.4 h, 32.3 L.  Saleh's Table 3 (the same
# patients, so not independent): steady-state AUC over 8 h on 2.5 mg tid,
# geometric mean 1,275 ug.h/L in PATENT-1 (5% smokers, 29% on bosentan);
# this model gives 2500 / 1.81 = 1,381.  Healthy subjects clear riociguat
# about twice as fast (Becker 2016, intravenous 1 mg: CL 3.1 L/h), so the
# model over-predicts a healthy subject's exposure about twofold.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on V, and Cockcroft-Gault's, evaluated at the
# pharmacokinetic weight with the switch on and at total weight with it off.
#
# ACTIVE METABOLITE
# =================
# M1 (BAY 60-4552) has a third to a tenth of the parent's potency and about
# half its concentration in PAH.  Saleh fitted it as its own apparent
# one-compartment model from the dose (ka 0.26 /h, CL 3.16 L/h, V 124 L), not
# formed from the parent's compartment; it is not plotted.
#
# DOSE
# ====
# Adempas tablets, 0.5 to 2.5 mg of riociguat (no salt), three times a day.
#
# PREGNANCY AND THE BAND
# ======================
# Contraindicated in pregnancy (embryo-fetal toxicity); no pregnancy
# pharmacokinetics (registry row).  No therapeutic range: Saleh found no
# relation of the 6-minute walk distance to exposure.
# -----------------------------------------------------------------------------

# Saleh 2016, Table 2, part A
RIOCIGUAT_KA      <- 2.17    # /h
RIOCIGUAT_CL      <- 1.81    # L/h
RIOCIGUAT_V       <- 32.3    # L
RIOCIGUAT_V_PER_KG <- 0.00619   # fractional change per kg
RIOCIGUAT_WT_REF  <- 68         # kg
RIOCIGUAT_CL_PER_CRCL <- 0.00394  # fractional change per mL/min
RIOCIGUAT_CRCL_REF <- 81.2      # mL/min
RIOCIGUAT_CRCL_MIN <- 15.4      # mL/min, Table 1 baseline range
RIOCIGUAT_CRCL_MAX <- 187       # mL/min

#' Riociguat (Saleh 2016)
#'
#' One compartment, oral, in pulmonary hypertension; see the header of
#' \code{R/drugs_riociguat.R}.
#'
#' @inheritParams cefazolin
#' @param creatinine serum creatinine, mg/dL, or NULL for an assumed normal
#'   value
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
riociguat <- function(weight, height, age, sex, adjustToFFM = TRUE,
                      creatinine = NULL)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  pkW  <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  scr  <- adultEquivalentCreatinine(creatinine, age, sex)
  crcl <- min(max(creatinineClearanceCG(pkW, age, sex, scr), RIOCIGUAT_CRCL_MIN),
              RIOCIGUAT_CRCL_MAX)
  cl <- RIOCIGUAT_CL * (1 + RIOCIGUAT_CL_PER_CRCL * (crcl - RIOCIGUAT_CRCL_REF))
  # positive above 68 - 1 / 0.00619 = -93.6 kg
  v  <- RIOCIGUAT_V * (1 + RIOCIGUAT_V_PER_KG * (pkW - RIOCIGUAT_WT_REF))

  default <- list(
    v1  = v,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = RIOCIGUAT_KA / 60,
    bioavailability_PO = 1,     # apparent parameters
    tlag_PO = 0
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
    reference = paste0(
      "Saleh S, Becker C, Frey R, Muck W, Pulm Circ 2016;6(Suppl 1):S86-S96, Table 2: ",
      "one compartment in pulmonary hypertension, ka 2.17 /h, CL/F 1.81 L/h ",
      "(+0.394% per mL/min of Cockcroft-Gault creatinine clearance about 81.2; ",
      "non-smoker, no bosentan, bilirubin 0.58 mg/dL), V/F 32.3 L (+0.619% per kg ",
      sprintf("about 68 kg); creatinine clearance %.0f mL/min. ", crcl),
      "https://doi.org/10.1086/685404")
  )
}
