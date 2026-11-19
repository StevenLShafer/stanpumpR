# -----------------------------------------------------------------------------
# Sildenafil in children with pulmonary arterial hypertension: one
# compartment, oral, from Miura 2026; intravenous doses on the FDA review's
# model.  A candidate of the sildenafil group (R/modelSelection.R)
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
# 297 plasma concentrations from 24 Japanese patients with pulmonary arterial
# hypertension (15 children aged 0.4-14.8 years, 6.5-54.9 kg, 37 samples; 9
# adults aged 38-69 years, 37.4-58.7 kg, 260 samples), sampled at steady
# state in routine care; LC-MS; NONMEM FOCE-I.  One compartment with
# first-order absorption (Table 2):
#
#     CL/F = 60.6 L/h x (WT/32.3)^0.537 x (1 + 0.633)^BOSENTAN
#     V/F 322 L        ka 4.51 /h, fixed from an earlier report
#
# The footnote's "BWmedian" is taken as the sildenafil cohort's median weight
# in Table 1, 32.3 kg, as for bosentan (R/drugs_bosentan.R).  Bosentan is held
# at none (the library has no interactions among these drugs); with it,
# clearance is 1.63 times higher.  V/F has no covariate, so a small child has
# the adult's apparent volume: the source's model, sampled sparsely in the
# children.
#
# WHO GETS IT (R/modelSelection.R)
# ================================
# The selector's table gives this model the range of the source's CHILDREN
# (0.4-14.8 years, 6.5-54.9 kg) and the adults' FDA model 18 years and over,
# on Shafer's decision that Miura's model is the children's.  A child gets it
# whatever the Pulmonary hypertension field says: sildenafil is given to
# children for pulmonary arterial hypertension.
#
# INTRAVENOUS DOSES
# =================
# The source fitted oral doses only, with apparent parameters, so this
# model's own system takes only oral doses (routes = PO).  An intravenous dose
# (Revatio injection) is run on the FDA review's absolute model
# (sildenafilFDA(), R/drugs_sildenafilFDA.R) for the same patient, scaled to
# the child by the library's size factors, as a parallel system that takes
# only intravenous doses; the two are added.
#
# CHECKS
# ======
# The source's median steady-state concentrations of its patients'
# individual fits (Css = dose / CL/F; Bayesian, so not independent): children
# 25.4 ng/mL, adults 34.3 ng/mL.  This model at the children's median 16.5 kg
# on 30 mg a day (the Japanese dose for 8-20 kg) gives 29.6 ng/mL, and at the
# adults' median 48.5 kg on 60 mg a day 33.2 ng/mL.  Its apparent clearance
# at 70 kg, 91.8 L/h, against the FDA review's 29.5 L/h over an oral F of
# 0.355 at 20 mg, 83.1 L/h.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The source's own weight term on CL/F, at the pharmacokinetic weight with
# the switch on and total weight with it off.
#
# DOSE, PREGNANCY AND THE BAND
# ============================
# Doses as labelled (Revatio 20 mg tablets of sildenafil, as citrate), as in
# the source.  Pregnancy and the band: the sildenafil group's rows.
# -----------------------------------------------------------------------------

# Miura 2026, Table 2 and its footnote; the median weight from Table 1
SILDENAFIL_MIURA_CL     <- 60.6    # L/h at the median weight
SILDENAFIL_MIURA_WT_EXP <- 0.537
SILDENAFIL_MIURA_WT_REF <- 32.3    # kg
SILDENAFIL_MIURA_V      <- 322     # L
SILDENAFIL_MIURA_KA     <- 4.51    # /h, fixed

#' Sildenafil in children with pulmonary arterial hypertension (Miura 2026)
#'
#' One compartment, oral, weight on apparent clearance; intravenous doses on
#' the FDA review's model.  A candidate of the sildenafil group.  See the
#' header of \code{R/drugs_sildenafilMiura.R}.
#'
#' @inheritParams sildenafilFDA
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
sildenafilMiura <- function(weight, height, age, sex, adjustToFFM = TRUE,
                            pulmonaryHypertension = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- SILDENAFIL_MIURA_CL * (w / SILDENAFIL_MIURA_WT_REF)^SILDENAFIL_MIURA_WT_EXP

  default <- list(
    v1  = SILDENAFIL_MIURA_V,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = SILDENAFIL_MIURA_KA / 60,
    bioavailability_PO = 1      # apparent parameters
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))
  iv <- sildenafilFDA(weight, height, age, sex, adjustToFFM,
                      pulmonaryHypertension = pulmonaryHypertension)
  # The intravenous system takes no oral dose: it carries the FDA model's
  # disposition with this model's oral absorption, only to share the time
  # line (as meloxicam's does; the FDA model's lag would add knots this
  # model's oral system does not have).
  ivSet <- c(iv$PK[[PK_EVENT_DEFAULT]][c("v1", "v2", "v3", "cl1", "cl2", "cl3")],
             default[c("ka_PO", "bioavailability_PO")])

  list(
    PK = PK,
    routes = ROUTE_PO,
    parallelSystems = list(
      list(name = "Intravenous (FDA review of NDA 22-473)", routes = ROUTE_IV,
           PK = list(default = ivSet))
    ),
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Oral: Miura M, Tanaka S, Aihara R, et al., Br J Clin Pharmacol 2026;92:3611-3621, ",
      "Table 2: one compartment fitted at steady state in Japanese children and adults ",
      "with pulmonary arterial hypertension, CL/F 60.6 L/h x (WT/32.3)^0.537 (no ",
      "bosentan), V/F 322 L, ka 4.51 /h; intravenous doses on the sildenafilFDA model ",
      "(the FDA review of NDA 22-473), scaled to the patient. ",
      "https://doi.org/10.1002/bcp.70676")
  )
}
