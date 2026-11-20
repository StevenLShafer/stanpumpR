# -----------------------------------------------------------------------------
# Bosentan (Tracleer): one compartment, oral, from Miura 2026's population
# model in Japanese children and adults with pulmonary arterial hypertension
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL (= mcg/L), total plasma.
#
# Drafted by Claude Code, 2026-11-18, at the request of Steven L. Shafer, who
# supplied the paper on 2026-11-17 and asked that day for bosentan to be
# built from it.  Source, read in full (Table 3 and its footnote on the page
# image, which keeps the minus signs the text extraction drops): Miura M,
# Tanaka S, Aihara R, Yamamoto S, Nakashima Y, Kashiwagura Y, Hakamata A,
# Miyakawa S, Tatsuguchi M, Odagiri K, Namiki N, Inui N, Watanabe H, Uchida
# S.  Development of a population pharmacokinetic model using combined
# paediatric and adult data for four pulmonary arterial hypertension drugs.
# Br J Clin Pharmacol 2026;92:3611-3621.  doi 10.1002/bcp.70676.
#
# THE SOURCE
# ==========
# 189 plasma concentrations from 16 Japanese patients with pulmonary arterial
# hypertension (7 children aged 2.2-9.2 years, 6.9-54.9 kg; 9 adults aged
# 15.6-69 years, 38.5-58.7 kg; 170 of the samples from the adults), on
# bosentan for at least 4 weeks, sampled at steady state in routine care;
# LC-MS; NONMEM FOCE-I.  One compartment with first-order absorption after a
# lag (Table 3):
#
#     CL/F = 8.85 L/h x (WT/42.2)^0.472 x (ALT/16.5)^-0.203
#     V/F 41.0 L        ka 2.47 /h        lag 0.379 h
#
# The footnote writes the reference values as "BWmedian" and "ALTmedian"
# without printing them; they are taken as the bosentan cohort's medians in
# Table 1, 42.2 kg and 16.5 IU/L (an interpretation: the median of the
# drug's own analysis set; CHECKS cannot tell it from the adults' 48.5 kg).
# V/F has no covariate in the source; see BODY SIZE for its scaling.  The
# sildenafil model of the same paper gives
# sildenafil's clearance with bosentan 1.63 times higher (bosentan induces
# CYP3A); that interaction is not modelled.
#
# ALT IS HELD AT ITS MEDIAN
# =========================
# The Patient Profile has no ALT field, so the ALT term is evaluated at the
# median, 16.5 IU/L, where it is 1.  Its effect is small within the studied
# 7-50 IU/L: clearance x 1.19 at 7 and x 0.80 at 50.
#
# STEADY STATE ONLY: AUTOINDUCTION IS NOT MODELLED
# ================================================
# Every patient had taken bosentan for at least 4 weeks, so the clearance is
# the induced one.  Bosentan induces its own metabolism (CYP2C9 and CYP3A):
# "Upon multiple oral dosing, plasma concentrations in healthy adults
# decrease gradually to 50-65% of those seen after single dose
# administration ... Steady-state is reached within 3-5 days" (Tracleer
# label, sec. 12.3).  So this model under-predicts the first days of
# treatment, by up to about half, and is right from about the fifth day.
# The source gives no induction parameters, so the library's autoinduction
# engine cannot be used; the help page says so.
#
# CHECKS
# ======
# The source's own medians of the individual steady-state concentrations
# (Css = dose / individual CL/F, Figure 5; Bayesian, so not independent of
# the model): adults 1221 ng/mL on 250 mg/day, children 339 ng/mL on 4
# mg/kg/day.  This model at the adults' median weight 48.5 kg gives 1102
# ng/mL (-10%), and at the children's median 12 kg (48 mg/day) 409 ng/mL
# (+21%).  With 48.5 kg as the reference weight instead, 1177 (-4%) and 437
# (+29%): the check does not choose between the two readings.  Against
# the label (not used in the fit): clearance after an intravenous dose is
# "about 4 L/h in patients with PAH" and oral bioavailability "about 50%" in
# healthy volunteers, so about 8 L/h before induction, and steady-state
# concentrations in healthy adults are 50-65% of the first dose's; this
# model's 11.2 L/h at 70 kg lies 9% below that 8 / 0.65 to 8 / 0.50 (12.3-16
# L/h), the label's figures being healthy volunteers' bioavailability and
# induction.  The label's peak "within 3-5 hours" against this model's 1.4 h (ka 2.47 /h and the 0.38-h lag) is a sparse-sampling
# limit the source names ("most samples were obtained during the elimination
# phase").  The label's "terminal elimination half-life is about 5 hours in
# healthy adult subjects" against this model's 2.5 h at 70 kg.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# CL/F keeps the source's own weight term exactly as fitted (and its ALT
# term, held at the median), at the pharmacokinetic weight with the switch on
# and total weight with it off, and takes nothing more.
#
# V/F has no covariate in the source, not because size was found not to
# matter but because it was never tested: V/F's shrinkage exceeded 30% for
# every drug, so the authors explored covariates on CL/F only (Discussion).
# Under Steven L. Shafer's first-principles rule of 2026-11-19 (a covariate
# not detected is not evidence of no size effect), with the switch on V/F is
# scaled by the fat-free-mass ratio (size$volume), anchored at the cohort's
# typical patients (BOSENTAN_COHORT, below; Shafer's choice of 2026-11-20: two
# anchors, the children's median subject and the adults', each of both
# sexes), so that the cohort's typical patient receives the published 41.0 L.
# The cohort factor is 0.3889 on volume (0.4925 on clearance): the geometric
# mean of the children's and the adults' fat-free-mass factors, weighted by
# their numbers, sits that far below the 70 kg reference man.  So V/F is
# 105 L in a 70 kg, 170 cm, 35-year-old man, about 21 L at the
# children's median subject and 65.0 and 84.6 L at the adults' median woman and
# man.  Al-Sallami's fat-free mass is an extrapolation below 3 years.
# With the switch off V/F is 41.0 L for everyone, as before.
#
# The studied weights were
# 6.9-58.7 kg, so a 70-kg adult is an extrapolation by the power law, which
# the help page says.  Fitted in children from 2.2 years and adults, offered
# at every age.  The label says exposure in children "reaches a plateau at
# lower doses ... doses higher than 2 mg/kg twice daily do not increase the
# exposure"; the model is linear and does not carry that.
#
# DOSE
# ====
# "Each TRACLEER 62.5 mg tablet contains 64.54 mg of bosentan monohydrate,
# equivalent to 62.5 mg of anhydrous bosentan" (label, sec. 11), and the
# source dosed 62.5 mg tablets (Table 1), so doses are milligrams of
# anhydrous bosentan, as labelled, with no conversion.
#
# PREGNANCY AND THE BAND
# ======================
# Contraindicated in pregnancy; no pregnancy pharmacokinetics (registry row).
# No therapeutic range is established (therapeutic-range row).
# -----------------------------------------------------------------------------

# Miura 2026, Table 3 and its footnote; the medians from Table 1
BOSENTAN_CL      <- 8.85    # L/h at the median weight and ALT
BOSENTAN_WT_EXP  <- 0.472
BOSENTAN_WT_REF  <- 42.2    # kg, the bosentan cohort's median
BOSENTAN_ALT_EXP <- -0.203
BOSENTAN_ALT_REF <- 16.5    # IU/L, the bosentan cohort's median
BOSENTAN_V       <- 41.0    # L
BOSENTAN_KA      <- 2.47    # /h
BOSENTAN_TLAG    <- 0.379   # h

# Miura 2026, Table 1 (the bosentan column), the cohort V/F is anchored at
# (see the header's BODY SIZE): 16 patients, 7 children (median 4.8 years,
# 12 kg) and 9 adults (median 47 years, 48.5 kg), 5 men and 11 women.
# Table 1 does not split the sexes by age group, so each group is ASSUMED to
# have the cohort's sex ratio (5:11).  No height, BMI or BSA is printed
# (nor any supplement).  The children's heights are the median (50th
# percentile) height for age of the Japanese 2000 reference (Japanese Society
# for Pediatric Endocrinology, Appendix Table 2, Kato N et al. 2004;
# http://jspe.umin.jp/medical/files/fuhyo2.pdf, read on the page image
# 2026-11-20), interpolated linearly between its months at 4.8 years: boys
# 105.6 cm, girls 105.4 cm.  The adults' heights are NCD-RisC's mean height at 18
# for Japan, birth cohort 1969 (about 2016 less 47: the patients were
# sampled in routine care in Hamamatsu from about 2009, the registered
# studies' UMIN entries, to 2023, the last ethics approval; no dates are
# printed, and 2009 or 2023 would move these heights by under 1 cm), less
# 0.2 cm (men) or 0.3 cm (women) a decade after 18: men 170.5 cm, women 157.4
# cm.  The sexes and groups enter at their counts (fractional where the
# assumed ratio makes them so).
BOSENTAN_COHORT <- list(groups = list(
  list(weight = 12, height = 105.6, age = 4.8, men = 5 * 7 / 16, women = 0),
  list(weight = 12, height = 105.4, age = 4.8, men = 0, women = 11 * 7 / 16),
  list(weight = 48.5, height = 170.5, age = 47, men = 5 * 9 / 16, women = 0),
  list(weight = 48.5, height = 157.4, age = 47, men = 0, women = 11 * 9 / 16)))

#' Bosentan (Miura 2026)
#'
#' One compartment, oral, with weight on apparent clearance; steady-state
#' (induced) clearance.  See the header of \code{R/drugs_bosentan.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
bosentan <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # size$pkWeight (for the fitted weight term on CL/F) does not depend on the
  # cohort; size$volume scales the size-free V/F, anchored at
  # BOSENTAN_COHORT, and is 1 with the switch off
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1,
                        cohort = do.call(cohortSizeFactors, BOSENTAN_COHORT))
  w <- if (isTRUE(adjustToFFM)) size$pkWeight else weight
  # ALT held at the median (no Patient Profile field), where its term is 1
  cl <- BOSENTAN_CL * (w / BOSENTAN_WT_REF)^BOSENTAN_WT_EXP

  default <- list(
    v1  = BOSENTAN_V * size$volume,
    v2  = 1,
    v3  = 1,
    cl1 = cl / 60,
    cl2 = 0,
    cl3 = 0,
    ka_PO = BOSENTAN_KA / 60,
    bioavailability_PO = 1,     # apparent parameters
    tlag_PO = BOSENTAN_TLAG * 60
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
      "with pulmonary arterial hypertension, CL/F 8.85 L/h x (WT/42.2)^0.472 (ALT held ",
      "at its median), V/F 41.0 L (no covariate; with the fat-free-mass switch on, scaled ",
      "by the fat-free-mass ratio anchored at the cohort's median child and adult), ",
      "ka 2.47 /h, lag 0.379 h; autoinduction over the ",
      "first days not modelled. https://doi.org/10.1002/bcp.70676")
  )
}
