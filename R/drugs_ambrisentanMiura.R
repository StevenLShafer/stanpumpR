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
# no covariate in the source; see BODY SIZE for its scaling.
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
# typical patients (AMBRISENTAN_MIURA_COHORT, below; Shafer's choice of 2026-11-20: two
# anchors, the children's median subject and the adults', each of both
# sexes), so that the cohort's typical patient receives the published 12.4 L.
# The cohort factor is 0.2731 on volume (0.3778 on clearance): the geometric
# mean of the children's and the adults' fat-free-mass factors, weighted by
# their numbers, sits that far below the 70 kg reference man.  So V/F is
# 45.4 L in a 70 kg, 170 cm, 35-year-old man, about 7.0 L at the
# children's median subject and 29.1 and 37.9 L at the adults' median woman and
# man.  Al-Sallami's fat-free mass is an extrapolation below 3 years, which includes the children's median subject (1.8 years).
# With the switch off V/F is 12.4 L for everyone, as before.
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

# Miura 2026, Table 1 (the ambrisentan column), the cohort V/F is anchored at
# (see the header's BODY SIZE): 29 patients, 18 children (median 1.8 years,
# 10.1 kg) and 11 adults (median 44.6 years, 51.4 kg), 11 men and 18 women.
# Table 1 does not split the sexes by age group, so each group is ASSUMED to
# have the cohort's sex ratio (11:18).  No height, BMI or BSA is printed
# (nor any supplement).  The children's heights are the median (50th
# percentile) height for age of the Japanese 2000 reference (Japanese Society
# for Pediatric Endocrinology, Appendix Table 2, Kato N et al. 2004;
# http://jspe.umin.jp/medical/files/fuhyo2.pdf, read on the page image
# 2026-11-20), interpolated linearly between its months at 1.8 years: boys
# 83.9 cm, girls 82.7 cm.  The adults' heights are NCD-RisC's mean height at 18
# for Japan, birth cohort 1971 (about 2016 less 44.6: the patients were
# sampled in routine care in Hamamatsu from about 2009, the registered
# studies' UMIN entries, to 2023, the last ethics approval; no dates are
# printed, and 2009 or 2023 would move these heights by under 1 cm), less
# 0.2 cm (men) or 0.3 cm (women) a decade after 18: men 170.7 cm, women 157.6
# cm.  The sexes and groups enter at their counts (fractional where the
# assumed ratio makes them so).
AMBRISENTAN_MIURA_COHORT <- list(groups = list(
  list(weight = 10.1, height = 83.9, age = 1.8, men = 11 * 18 / 29, women = 0),
  list(weight = 10.1, height = 82.7, age = 1.8, men = 0, women = 18 * 18 / 29),
  list(weight = 51.4, height = 170.7, age = 44.6, men = 11 * 11 / 29, women = 0),
  list(weight = 51.4, height = 157.6, age = 44.6, men = 0, women = 18 * 11 / 29)))

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
  # size$pkWeight (for the fitted weight term on CL/F) does not depend on the
  # cohort; size$volume scales the size-free V/F, anchored at
  # AMBRISENTAN_MIURA_COHORT, and is 1 with the switch off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, AMBRISENTAN_MIURA_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  cl <- AMBRISENTAN_MIURA_CL * (w / AMBRISENTAN_MIURA_WT_REF)^AMBRISENTAN_MIURA_WT_EXP

  default <- list(
    v1  = AMBRISENTAN_MIURA_V * size$volume,
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
      "V/F 12.4 L (no covariate; with the fat-free-mass switch on, scaled by the ",
      "fat-free-mass ratio anchored at the cohort's median child and adult), ka 1.03 /h. ",
      "https://doi.org/10.1002/bcp.70676")
  )
}
