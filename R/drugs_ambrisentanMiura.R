# -----------------------------------------------------------------------------
# Ambrisentan in young children with pulmonary arterial hypertension: one
# compartment, oral, from Miura 2026.  A candidate of the ambrisentan group
# (R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer
# ("Use Miura's model for children", 2026-11-18).  Source, read in full
# (Table 3 and its footnote on the page image): Miura M, Tanaka S, Aihara R,
# et al.  Development of a population pharmacokinetic model using combined
# paediatric and adult data for four pulmonary arterial hypertension drugs.
# Br J Clin Pharmacol 2026;92:3611-3621.  doi 10.1002/bcp.70676.
#
# THE SOURCE
# ==========
# 177 plasma concentrations from 29 Japanese patients with pulmonary arterial
# hypertension (18 children aged 0.4-8.8 years, 7.1-21.6 kg, 66 samples; 11
# adults aged 29.4-69 years, 37.4-62 kg, 111 samples), sampled at steady
# state in routine care; LC-MS; NONMEM FOCE-I.  One compartment with
# first-order absorption (Table 3):
#
#     CL/F = 0.594 L/h x (WT/13.4)^0.561
#     V/F 12.4 L        ka 1.03 /h
#
# The footnote's "BWmedian" is taken as the ambrisentan cohort's median
# weight in Table 1, 13.4 kg, as for bosentan (R/drugs_bosentan.R).  V/F has
# no covariate.
#
# WHO GETS IT (R/modelSelection.R)
# ================================
# The selector's table gives this model the range of the source's CHILDREN
# (0.4-8.8 years, 7.1-21.6 kg), on Shafer's decision that Miura's model is the
# children's.  The library's other ambrisentan model, Okour 2022
# (ambrisentanOkour()), was fitted in 39 children of 8-16 years and is
# offered to adults as the nearest; so children under 8 get this model,
# children from 8 years and adults Okour's (from 8 to 8.8 years both include
# the child, and Okour's is the larger study).
#
# CHECKS
# ======
# The source's median steady-state concentrations of its patients'
# individual fits (Css = dose / CL/F; Bayesian, so not independent):
# children 164 ng/mL, adults 298 ng/mL.  This model at the children's median
# 10.1 kg on 0.2 mg/kg a day gives 166 ng/mL, and at the adults' median 51.4
# kg on 10 mg a day 330 ng/mL.  Against the Letairis label (adults, not
# fitted): oral clearance "19 mL/min" (1.14 L/h); this model at 70 kg, 1.50
# L/h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on CL/F, at the pharmacokinetic weight with
# the switch on and total weight with it off.
#
# DOSE, PREGNANCY AND THE BAND
# ============================
# Letairis tablets, ambrisentan (no salt), as in the source.  Pregnancy and
# the band: the ambrisentan group's rows.
# -----------------------------------------------------------------------------

# Miura 2026, Table 3 and its footnote; the median weight from Table 1
AMBRISENTAN_MIURA_CL     <- 0.594   # L/h at the median weight
AMBRISENTAN_MIURA_WT_EXP <- 0.561
AMBRISENTAN_MIURA_WT_REF <- 13.4    # kg
AMBRISENTAN_MIURA_V      <- 12.4    # L
AMBRISENTAN_MIURA_KA     <- 1.03    # /h

#' Ambrisentan in young children with pulmonary arterial hypertension (Miura 2026)
#'
#' One compartment, oral, weight on apparent clearance.  A candidate of the
#' ambrisentan group.  See the header of \code{R/drugs_ambrisentanMiura.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
ambrisentanMiura <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- AMBRISENTAN_MIURA_CL * (w / AMBRISENTAN_MIURA_WT_REF)^AMBRISENTAN_MIURA_WT_EXP

  default <- list(
    v1  = AMBRISENTAN_MIURA_V,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = AMBRISENTAN_MIURA_KA / 60,
    bioavailability_PO = 1      # apparent parameters
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
      "Miura M, Tanaka S, Aihara R, et al., Br J Clin Pharmacol 2026;92:3611-3621, ",
      "Table 3: one compartment fitted at steady state in Japanese children and adults ",
      "with pulmonary arterial hypertension, CL/F 0.594 L/h x (WT/13.4)^0.561, ",
      "V/F 12.4 L, ka 1.03 /h. https://doi.org/10.1002/bcp.70676")
  )
}
