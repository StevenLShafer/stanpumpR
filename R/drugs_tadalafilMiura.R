# -----------------------------------------------------------------------------
# Tadalafil in young children with pulmonary arterial hypertension: one
# compartment, oral, from Miura 2026.  A candidate of the tadalafil group
# (R/modelSelection.R)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer
# ("Use Miura's model for children", 2026-11-18).  Source, read in full
# (Table 2 and its footnote on the page image): Miura M, Tanaka S, Aihara R,
# et al.  Development of a population pharmacokinetic model using combined
# paediatric and adult data for four pulmonary arterial hypertension drugs.
# Br J Clin Pharmacol 2026;92:3611-3621.  doi 10.1002/bcp.70676.
#
# THE SOURCE
# ==========
# 157 plasma concentrations from 31 Japanese patients with pulmonary arterial
# hypertension (20 children aged 0.1-8.3 years, 5.4-23.9 kg, 62 samples; 11
# adults aged 15.5-74.2 years, 32.5-73.5 kg, 95 samples), sampled at steady
# state in routine care; LC-MS; NONMEM FOCE-I.  One compartment with
# first-order absorption (Table 2):
#
#     CL/F = 1.76 L/h x (WT/14.2)^0.602 x (1 - 0.400)^SPIRONOLACTONE
#     V/F 80.6 L        ka 0.84 /h, fixed from an earlier report
#
# The footnote's "BWmedian" is taken as the tadalafil cohort's median weight
# in Table 1, 14.2 kg, as for bosentan (R/drugs_bosentan.R).  Spironolactone
# is held at none (13 of the 31 took it; the source reads it as a marker of
# right heart failure, not an interaction); with it, clearance is 40% lower.
# V/F has no covariate.
#
# WHO GETS IT (R/modelSelection.R)
# ================================
# The selector's table gives this model the range of the source's CHILDREN
# (0.1-8.3 years, 5.4-23.9 kg), on Shafer's decision that Miura's model is the
# children's.  The library's other tadalafil model, Ferguson-Sells 2022
# (tadalafilFergusonSells(), with study LVBF's model without pulmonary
# hypertension), was fitted in 324 patients aged 2.5-90.3 years, children
# among them, so by the selector's rule (the larger study among those that
# include the patient) children from 2.5 years keep Ferguson-Sells and this
# model is chosen below 2.5 years.  Whether Miura should be chosen for every
# child instead is a [decision] for Shafer, put to him on 2026-11-18.  Where
# it is chosen, it is used whatever the Pulmonary hypertension field says.
#
# CHECKS
# ======
# The source's median steady-state concentrations of its patients'
# individual fits (Css = dose / CL/F; Bayesian, so not independent):
# children 461 ng/mL, adults 768 ng/mL.  This model without spironolactone at
# the children's median 10.7 kg on 1 mg/kg a day gives 300 ng/mL, and at the
# adults' median 52.3 kg on 40 mg a day 432 ng/mL; with spironolactone, 501
# and 720.  The source's medians mix the two (13 of 31 on spironolactone).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on CL/F, at the pharmacokinetic weight with
# the switch on and total weight with it off.
#
# DOSE, PREGNANCY AND THE BAND
# ============================
# Doses as labelled (Adcirca 20 mg tablets of tadalafil), as in the source.
# Pregnancy and the band: the tadalafil group's rows.
# -----------------------------------------------------------------------------

# Miura 2026, Table 2 and its footnote; the median weight from Table 1
TADALAFIL_MIURA_CL     <- 1.76    # L/h at the median weight
TADALAFIL_MIURA_WT_EXP <- 0.602
TADALAFIL_MIURA_WT_REF <- 14.2    # kg
TADALAFIL_MIURA_V      <- 80.6    # L
TADALAFIL_MIURA_KA     <- 0.84    # /h, fixed

#' Tadalafil in young children with pulmonary arterial hypertension (Miura 2026)
#'
#' One compartment, oral, weight on apparent clearance.  A candidate of the
#' tadalafil group.  See the header of \code{R/drugs_tadalafilMiura.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tadalafilMiura <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- TADALAFIL_MIURA_CL * (w / TADALAFIL_MIURA_WT_REF)^TADALAFIL_MIURA_WT_EXP

  default <- list(
    v1  = TADALAFIL_MIURA_V,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = TADALAFIL_MIURA_KA / 60,
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
      "Table 2: one compartment fitted at steady state in Japanese children and adults ",
      "with pulmonary arterial hypertension, CL/F 1.76 L/h x (WT/14.2)^0.602 (no ",
      "spironolactone), V/F 80.6 L, ka 0.84 /h. https://doi.org/10.1002/bcp.70676")
  )
}
