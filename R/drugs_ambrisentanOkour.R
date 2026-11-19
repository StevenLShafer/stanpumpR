# -----------------------------------------------------------------------------
# Ambrisentan (Letairis): two compartments, oral, allometric in weight, from
# Okour 2022's population model in children with pulmonary arterial
# hypertension
# -----------------------------------------------------------------------------
# A CANDIDATE OF THE AMBRISENTAN GROUP (2026-11-18): the drug a user doses is
# ambrisentan, whose model the program chooses (R/modelSelection.R).  This
# model is chosen from 8 years and, as the nearest, for adults; younger
# children get Miura 2026 (R/drugs_ambrisentanMiura.R), on Shafer's decision
# of 2026-11-18 ("Use Miura's model for children").
#
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-16, at the request of Steven L. Shafer, who
# supplied the paper that day.  Source, read in full: Okour M, Thapar MM,
# Farrell C, Lukas MA, Beghetti M, Beerahee M.  Pediatric population
# pharmacokinetic modeling and exposure-response analysis of ambrisentan in
# pulmonary arterial hypertension and comparison with adult data.  J Clin
# Pharmacol 2023;63:593-603.  doi 10.1002/jcph.2199.
#
# THE SOURCE
# ==========
# 211 concentrations from 39 children aged 8 to 16 years with pulmonary
# arterial hypertension (AMB112529), 2.5 to 10 mg once daily, NONMEM.  Two
# compartments, first-order absorption after a lag; no covariate but weight,
# allometric about a 70-kg reference with fixed exponents.  Table 2:
#
#     CL/F 1.17 L/h x (WT/70)^0.75        Vc/F 12.3 L x WT/70
#     Q/F 0.457 L/h x (WT/70)^0.75        Vp/F 81.3 L x WT/70
#     ka 2.46 /h                          lag 0.525 h (fixed)
#
# Apparent values: "The absolute bioavailability of ambrisentan is not known"
# (label).
#
# ONE MODEL FOR EVERY AGE
# =======================
# The adult population model the paper compares with is GSK's, unpublished,
# with its parameters in the paper's Supplementary Table S3, which has not
# been read.  The paper reports CL/F, Vc/F, ka and the lag "similar to those
# from the adult population PK model", and the steady-state exposures of
# children and adults similar once weight is allowed for (10 mg: children's
# AUCss 9.15 against adults' 9.12 ug.h/mL).  Its peripheral volume differed
# from the adult model's ("Despite Vp/F being statistically different"; Vp/F
# has 86% shrinkage), which gives this model a slow phase of 174 hours.  So
# the library offers this model at every age, the help page saying it was
# fitted in children, until Table S3 is read and an adult model can be a
# candidate for the selector (R/modelSelection.R); recorded as a decision in
# the PR (2026-11-16).
#
# CHECKS (not used in the fit)
# ============================
# Letairis label, adults with pulmonary arterial hypertension: oral clearance
# "19 mL/min" (1.14 L/h), peak "approximately 2 hours" after a dose, trough
# "about 15% of the mean peak" and accumulation "about 1.2" at steady state,
# terminal half-life 15 h.  This model at 70 kg: CL/F 1.17 L/h; on 10 mg
# daily a peak 1.7 h after the dose, trough 17% of the peak, AUC
# accumulation 1.43 (the long slow phase), steady-state AUC 8.5 ug.h/mL
# (Okour's adult model: 9.12).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own allometric weight terms, at the pharmacokinetic weight with
# the switch on and total weight with it off.
#
# DOSE
# ====
# Letairis tablets, 5 and 10 mg of ambrisentan (no salt), once daily.
# Cyclosporine doubles exposure (label); not modelled.
#
# PREGNANCY AND THE BAND
# ======================
# Contraindicated in pregnancy; no pregnancy pharmacokinetics (registry row).
# No therapeutic range: Okour found no relation of exposure to the 6-minute
# walk distance or to adverse events.
# -----------------------------------------------------------------------------

# Okour 2022, Table 2
AMBRISENTAN_CL   <- 1.17    # L/h at 70 kg
AMBRISENTAN_V1   <- 12.3    # L
AMBRISENTAN_Q    <- 0.457   # L/h
AMBRISENTAN_V2   <- 81.3    # L
AMBRISENTAN_KA   <- 2.46    # /h
AMBRISENTAN_TLAG <- 0.525   # h
AMBRISENTAN_WT_REF <- 70    # kg

#' Ambrisentan (Okour 2022)
#'
#' Two compartments, oral, allometric in weight; see the header of
#' \code{R/drugs_ambrisentan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ambrisentanOkour <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  f <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / AMBRISENTAN_WT_REF

  default <- list(
    v1  = AMBRISENTAN_V1 * f,
    v2  = AMBRISENTAN_V2 * f,
    v3  = 1,
    cl1 = AMBRISENTAN_CL * f^0.75 / 60,
    cl2 = AMBRISENTAN_Q * f^0.75 / 60,
    cl3 = 0,
    ka_PO = AMBRISENTAN_KA / 60,
    bioavailability_PO = 1,     # apparent parameters
    tlag_PO = AMBRISENTAN_TLAG * 60
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
      "Okour M, Thapar MM, Farrell C, Lukas MA, Beghetti M, Beerahee M, J Clin Pharmacol ",
      "2023;63:593-603, Table 2: two compartments fitted in children of 8-16 years with ",
      "pulmonary arterial hypertension, CL/F 1.17 L/h, Q/F 0.457 L/h x (WT/70)^0.75, ",
      "Vc/F 12.3 L, Vp/F 81.3 L x WT/70, ka 2.46 /h, lag 0.525 h. ",
      "https://doi.org/10.1002/jcph.2199")
  )
}
