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
# V/F has no covariate in the source; see BODY SIZE for its scaling.
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
# model is chosen below 2.5 years.  Shafer confirmed this on 2026-11-18:
# "Use Ferguson-Sell's study in larger children.  That is consistent with
# the general rule of trying to match the patient to the study population."
# Where
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
# CL/F keeps the source's own weight term exactly as fitted, at the
# pharmacokinetic weight with the switch on and total weight with it off, and
# takes nothing more.
#
# V/F has no covariate in the source, not because size was found not to
# matter but because it was never tested: V/F's shrinkage exceeded 30% for
# every drug, so the authors explored covariates on CL/F only (Discussion).
# Under Steven L. Shafer's first-principles rule of 2026-11-19 (a covariate
# not detected is not evidence of no size effect), with the switch on V/F is
# scaled by the fat-free-mass ratio (size$volume), anchored at the cohort's
# typical patients (TADALAFIL_MIURA_COHORT, below; Shafer's choice of 2026-11-20: two
# anchors, the children's median subject and the adults', each of both
# sexes), so that the cohort's typical patient receives the published 80.6 L.
# The cohort factor is 0.2725 on volume (0.3772 on clearance): the geometric
# mean of the children's and the adults' fat-free-mass factors, weighted by
# their numbers, sits that far below the 70 kg reference man.  So V/F is
# 296 L in a 70 kg, 170 cm, 35-year-old man, about 47 L at the
# children's median subject and 192 and 250 L at the adults' median woman and
# man.  Al-Sallami's fat-free mass is an extrapolation below 3 years, where this model is chosen (under 2.5 years).
# With the switch off V/F is 80.6 L for everyone, as before.
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

# Miura 2026, Table 1 (the tadalafil column), the cohort V/F is anchored at
# (see the header's BODY SIZE): 31 patients, 20 children (median 1.9 years,
# 10.7 kg) and 11 adults (median 37.8 years, 52.3 kg), 12 men and 19 women.
# Table 1 does not split the sexes by age group, so each group is ASSUMED to
# have the cohort's sex ratio (12:19).  No height, BMI or BSA is printed
# (nor any supplement).  The children's heights are the median (50th
# percentile) height for age of the Japanese 2000 reference (Japanese Society
# for Pediatric Endocrinology, Appendix Table 2, Kato N et al. 2004;
# http://jspe.umin.jp/medical/files/fuhyo2.pdf, read on the page image
# 2026-11-20), interpolated linearly between its months at 1.9 years: boys
# 84.8 cm, girls 83.6 cm.  The adults' heights are NCD-RisC's mean height at 18
# for Japan, birth cohort 1978 (about 2016 less 37.8: the patients were
# sampled in routine care in Hamamatsu from about 2009, the registered
# studies' UMIN entries, to 2023, the last ethics approval; no dates are
# printed, and 2009 or 2023 would move these heights by under 1 cm), less
# 0.2 cm (men) or 0.3 cm (women) a decade after 18: men 171.1 cm, women 157.9
# cm.  The sexes and groups enter at their counts (fractional where the
# assumed ratio makes them so).
TADALAFIL_MIURA_COHORT <- list(groups = list(
  list(weight = 10.7, height = 84.8, age = 1.9, men = 12 * 20 / 31, women = 0),
  list(weight = 10.7, height = 83.6, age = 1.9, men = 0, women = 19 * 20 / 31),
  list(weight = 52.3, height = 171.1, age = 37.8, men = 12 * 11 / 31, women = 0),
  list(weight = 52.3, height = 157.9, age = 37.8, men = 0, women = 19 * 11 / 31)))

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
  # size$pkWeight (for the fitted weight term on CL/F) does not depend on the
  # cohort; size$volume scales the size-free V/F, anchored at
  # TADALAFIL_MIURA_COHORT, and is 1 with the switch off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, TADALAFIL_MIURA_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- TADALAFIL_MIURA_CL * (w / TADALAFIL_MIURA_WT_REF)^TADALAFIL_MIURA_WT_EXP

  default <- list(
    v1  = TADALAFIL_MIURA_V * size$volume,
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
      "spironolactone), V/F 80.6 L (no covariate; with the fat-free-mass switch on, scaled ",
      "by the fat-free-mass ratio anchored at the cohort's median child and adult), ",
      "ka 0.84 /h. https://doi.org/10.1002/bcp.70676")
  )
}
