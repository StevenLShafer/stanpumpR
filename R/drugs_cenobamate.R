# -----------------------------------------------------------------------------
# Cenobamate: two compartments, apparent oral, with an absorption lag
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma.  Drafted by Claude Code,
# 2026-10-18, at the request of Steven L. Shafer; see the antiseizure
# registry, inst/extdata/antiseizureRegistry.csv.
#
# SOURCE
# ======
# The sponsor's population model (SK Life Science report SK16007): 960
# subjects, 8700 concentrations, from eight phase 1 studies in healthy and
# special populations, two phase 2 trials and one phase 3 trial in adults with
# focal seizures; single doses of 5 to 750 mg and daily doses of 50 to 600 mg,
# two thirds of the subjects at 200 mg/day.  Published by Vashi and colleagues
# (Epilepsy Res 2023;195:107185; read in full with its supplement).  The
# parameters are the paper's Supplemental Table 2, identical to Table 2 of the
# FDA clinical pharmacology review of NDA 212839 (2019, Reference ID 4488162),
# which reproduces the report's pages 53-54:
#
#     ka 2.74 /h   lag 0.240 h   CL/F 0.533 L/h   V1/F 42.4 L
#     Q/F 1.84 L/h   V2/F 7.27 L
#
# Two compartments, first-order absorption after a lag, linear elimination.
#
# COVARIATES NOT APPLIED
# ======================
# Every continuous covariate enters linearly about a centring value,
# P = P_TV x (1 + theta x (X - X~)), and every categorical one as
# P_TV x (1 + theta) (Supplemental Methods).  X~ is "typically the median",
# and no median is printed anywhere in the paper, its supplement, the FDA
# review or the EMA assessment report, so no continuous term can be
# evaluated exactly:
#   - daily dose on CL/F, "exponential", -0.0280 (Supplemental Table 2 gives
#     its unit as m^-2, copied from the BSA row).  The exponential form is
#     not written out and the reference dose is not printed.  Read as a power of the daily dose about
#     200 mg it moves CL/F by under 4% between 100 and 400 mg/day, which
#     agrees with the multiple-dose studies: apparent clearance at steady
#     state was 0.40-0.52 L/h at every daily dose from 50 to 500 mg, with no
#     trend (AUC proportional to dose; Vernillet 2020, Table 3 and
#     Supplemental Table 4, power-model slope 1.04).  The single-dose
#     nonlinearity is not reproduced: after one dose the apparent clearance
#     fell from 1.26-1.42 L/h at 5-10 mg to 0.40-0.48 L/h at 400-750 mg
#     (Vernillet 2020, Table 2), so the model overpredicts the total exposure
#     to a single small dose (AUC0-inf 2.2 times at 5 mg, 1.2 at 100 mg) and
#     underpredicts it after a single large one (0.73 times at 750 mg).  The
#     first day of daily dosing is reproduced (below), and dosing is daily.
#   - body surface area on CL/F (0.611 per m^2) and weight on V1/F (0.0120
#     per kg), medians unprinted.  Replaced by the library's fat-free-mass
#     scaling, as for ethosuximide.  Centred on the 70 kg, 170 cm reference
#     man (Du Bois BSA), the published slopes and the fat-free-mass factors
#     agree within 5% for men near that size (80 kg: V1 x1.12 against x1.08,
#     CL/F x1.07 against x1.06; 60 kg: x0.88 against x0.92, x0.93 against
#     x0.94).  They part for women, whom the source gave no sex term (60 kg,
#     160 cm: V1 x0.88 against x0.72), and in obesity (110 kg: V1 x1.48
#     against x1.26), where the fat-free-mass scaling is the library's choice.
#   - bilirubin on CL/F (units given as "IU"), African American (CL/F
#     -14.7%, V1/F -9.35%) and Hispanic (-13.3%) ethnicity: no Patient
#     Profile fields.  Supplemental Table 2 labels the Hispanic effect as on
#     CL/F, but in Supplemental Table 1 it improved the fit on V1/F (run 22,
#     dOFV -22.8) and not on CL/F (runs 19 and 23, -0.2 and -0.7), so the
#     label is probably wrong.
#   - clobazam (CL/F -19%) and carbamazepine (+15%): no co-medication field.
#     Phenytoin lowers cenobamate exposure 27-28% (label) and is not a model
#     covariate either.  The curve is cenobamate without these drugs, in a
#     non-Hispanic, non-African-American adult of the median size and
#     bilirubin of the analysis population.
#
# CHECKS (reference adult)
# ========================
# Terminal half-life 65 h (label 50-60 h; Vernillet 2020 30-76 h, rising with
# dose).  Vss 49.7 L (label Vd/F 40-50 L).  Peak 1.6 h after an oral dose
# (label median Tmax 1-4 h).  At 200 mg once daily the steady-state average
# is 15.6 mg/L, trough 13.7 and peak 17.9.  The phase 3 mean steady-state
# concentration at 200 mg/day was 15.5 mg/L (as quoted by Greene 2022), the
# population the model mostly describes.
#
# Healthy volunteers (Vernillet 2020: ascending-dose studies, capsules).
# These studies are in the pooled fit (Vashi Table 1; the single-dose study
# alone gave 5895 of the 8700 concentrations), so this is a consistency
# check, not an independent validation:
#   - the first day of daily dosing (Supplemental Table 5, 50-300 mg): the
#     model's AUC0-24 is 0.87-1.05 of the observed geometric means and its
#     peak 0.75-0.95, so absorption and distribution are right;
#   - steady state (Table 3, day 14 or 17, 50-500 mg/day): the model's AUCtau
#     is 0.73-0.94 of observed (geometric mean ratio 0.80), its peak 0.74 and
#     its trough 0.76, with no trend with dose.  The observed steady-state
#     clearance is about 0.43 L/h against the model's 0.533.
# Other healthy-volunteer studies agree: a median trough of 18.4 mg/L at
# 200 mg/day after titration (Greene 2022, two thirds African American) and a
# mean peak of 23.1 mg/L at 200 mg/day and 64.0 at 500 mg/day (Darpo 2022).
# So the curve is about 20% low for a healthy adult and right for a patient
# on adjunctive therapy, whose other antiseizure drugs (phenytoin among them,
# not a covariate) raise cenobamate clearance; the source's ethnicity terms
# are of the same size.
#
# NOT USED: the later sponsor model (Krishna, Jonsson, Abdallah and Vashi, AES
# 2025 abstract 2.366; 1101 subjects, two compartments, no lag) prints CL/F
# 0.282 L/h, V1/F 38.6 L and V2/F 6.72 L and nothing else (no ka, Q or
# covariate equations).  Taken as typical values those give a terminal
# half-life of at least 95 h (V1/F x ln 2 / CL/F, whatever Q is) and a
# steady-state average of 30 mg/L at 200 mg/day, about twice
# every observation above, so they must be conditional on covariate
# reference values the abstract does not give.
#
# EMA ASSESSMENT REPORT (Ontozry, EMA/CHMP/160820/2021, read in full)
# ====================================================================
# Summarises SK16007 without equations or medians, so it settles nothing
# above, but it adds four things the curve does not carry:
#   - weight: "subjects with low weights may have ~25% higher cenobamate
#     exposures and those with high weights may have ~20% lower exposures
#     than those with mean weights" (p 59; the weights are not defined).
#     The fat-free-mass scaling gives a 50 kg man (170 cm) 16% more
#     exposure than the 70 kg reference and a 110 kg man 16% less, the same
#     direction and nearly the same size.
#   - renal impairment: AUC 1.4-1.5 times normal after 200 mg in mild and
#     moderate impairment; severe impairment, studied at 100 mg, showed no
#     increase, which the report calls unexplained (p 49).  Creatinine
#     clearance was tested in SK16007 and not retained, so the model reads
#     no creatinine; the label caps renal impairment at 300 mg/day.
#   - hepatic impairment: AUC 1.9 times normal (mild) and 2.3 times
#     (moderate) after 200 mg; not modelled (no hepatic field).  The FDA
#     label of August 2025 adds the post-marketing study in severe (Child-
#     Pugh C) impairment: 4.2 times normal after a single 100 mg dose, so
#     use there is not recommended (both agencies cap Child-Pugh A and B at
#     200 mg/day).
#   - "multiple late peaks were observed suggesting enterohepatic
#     recirculation" (p 52); not modelled.
#
# Steady state takes about two weeks (label).  The label titration is 12.5
# mg/day for two weeks, then 25, 50, 100, 150 and 200 mg/day, each for two
# weeks.
#
# BIOAVAILABILITY: at least 88% absorbed (mass balance); the parameters are
# apparent (/F), so bioavailability_PO is 1.  Food has no clinically
# significant effect, and a crushed tablet orally or by nasogastric tube is
# bioequivalent (Vashi 2024), so one oral input serves all.
#
# BODY SIZE: the fixed values scaled to fat-free mass in place of the source's
# linear weight and body-surface-area terms; with the switch off, the
# published values for everyone (legacyVolume = 1).
#
# PATIENTS IN CLINICS (independent of the fit)
# ============================================
# Two therapeutic drug monitoring series, in adults with refractory epilepsy
# on other antiseizure drugs, give the concentration per mg/day of dose.  The
# model's steady-state average is 1 / (0.533 L/h x 24 h) = 0.078 mg/L per
# mg/day at every dose:
#   - Kork, Germany (Steinhoff 2024; 101 patients, 100-400 mg/day, morning
#     samples, which the authors measured to be within 10% of the day's
#     other levels): seizure-free patients 17.8 mg/L at a mean 240.3 mg/day
#     (0.074), non-responders 20.9 mg/L at 260.8 mg/day (0.080);
#     concentration linear in dose (r 0.70).
#   - Oslo, Norway (Johannessen Landmark 2026, read in full; 258 patients,
#     drug-fasting morning samples at steady state): 53.7 umol/L at a mean
#     164 mg/day, which is 14.4 mg/L at 267.67 g/mol (0.088 as a ratio of
#     means).  The paper's Figure 1D gives the mean of the patients' own
#     ratios, in umol/L per mg, by co-medication: 0.339 with neither a
#     strong inducer nor an inhibitor (n 129; 0.091 mg/L per mg/day), 0.311
#     with carbamazepine, phenytoin or phenobarbital (n 33; 0.083) and 0.384
#     with valproate or cannabidiol (n 96; 0.103).  Within-patient
#     coefficient of variation 18%; between-patient range 34-fold.
# The Kork figures are ratios of means.  The model is within 12% of the
# Kork figures and of Oslo's ratio of means, 14% below Oslo's patients on
# neither an inducer nor an inhibitor (a mean of ratios, which a skewed
# distribution pulls up) and 6% below those on an inducer; the 24% gap
# for valproate or cannabidiol is an inhibition the model has no term for.
#
# BAND: 10-25 mg/L, typical 18 (its midpoint): the therapeutic reference
# range newly introduced by the AGNP update 2026 (Hart et al.,
# Pharmacopsychiatry 2026;59:166-295, Table 5; trough 24 h after a dose;
# laboratory alert level 50 mg/L; level of recommendation 4), the mean
# +/- SD in 36 patients seizure-free at a median 200 mg/day.  It replaces
# the 5-35 mg/L responder range below, which this file drew until the
# consensus table was read (docs/therapeutic-ranges.md: the class
# consensus guideline comes first).  The model's troughs at 100 and
# 400 mg/day (6.8, 27.4) fall outside the narrower range, and the trials'
# seizure-free patients on 100 mg/day (central 90% 5.1-9.7) lie wholly
# below it: the consensus range describes the usual 200 mg/day.
#
# The sponsor's analysis of steady-state
# troughs in the maintenance phase of the two controlled trials (C013 and
# C017; Greene and Kamin, AAN 2021 abstract 1377, read in full): 95% of the
# patients with a 50% or greater seizure reduction, or seizure freedom, lay
# between 5 and 35 mg/L, which the authors propose as "a trough plasma
# concentration reference range (not a target therapeutic range)"; no toxic
# range was established.  The central 90% of the 50% responders were 4.6-9.3
# mg/L at 100 mg/day (n 42), 11.3-18.7 at 200 (n 95) and 20.8-35.2 at 400
# (n 50), and of the seizure-free 5.1-9.7, 10.8-19.8 and 21.9-35.8.  The
# model's steady-state troughs, 6.8, 13.7 and 27.4 mg/L, lie inside every
# one of those ranges, at 0.89-0.98 of their midpoints.  The phase 3 safety
# study (C021; post hoc, 10 US sites, AAN 2022 abstract S13.004, read in
# full) agrees: mean trough at the first maintenance visit on 200 mg/day
# 15.1 mg/L in the 177 patients still taking cenobamate, 15.2 in the 158
# 50% responders and 14.5 in the 60 seizure-free for 12 months or more,
# against the model's 13.7 (0.90-0.94).  It is a reference range in the
# sense of the ILAE (the concentrations at which patients respond), not a
# therapeutic range: in the Kork series seizure-free patients (3.8-33.7
# mg/L, mean 17.8) and non-responders (3.9-54.6, mean 20.9) did not differ,
# adverse effects did not track the concentration, and the authors
# concluded that no general therapeutic range can be defined and
# recommended individual reference concentrations instead.  Patients responding at 100 mg/day or
# less in an Italian cohort had 0.5-17.6 mg/L, median about 5-6 (Operto
# 2025).  The Oslo laboratory uses 15-130 umol/L, which is 4.0-34.8 mg/L
# (the paper prints "10-40 mg/L", which does not match its own umol/L), on
# the strength of the Kork series, and 85% of its measurements fell inside
# it.
#
# EFFECT SITE: none, deliberately.  The one study that timed an effect
# (photoparoxysmal EEG response, 6 evaluable patients, single doses of
# 100-400 mg; Kasteleijn-Nolst Trenite 2019) found the response already
# reduced 1 h after 250 and 400 mg, before the plasma peak, and still
# reduced more than 24 h later.  No hysteresis is visible on that time
# scale and no ke0 can be estimated, and with a 65 h half-life an
# equilibration of minutes would not change any curve a daily-dosed drug
# draws.  Plasma only.
#
# References
# ----------
# Vashi V et al., Epilepsy Res 2023;195:107185.
#   https://doi.org/10.1016/j.eplepsyres.2023.107185  (and Supplementary
#   Material: Methods, Tables 1-2)
# FDA, NDA 212839 Clinical Pharmacology Review (Xcopri), 2019, Table 2.
#   https://www.accessdata.fda.gov/drugsatfda_docs/nda/2019/212839Orig1s000ClinPharmR.pdf
# Vernillet L et al., Clin Pharmacol Drug Dev 2020;9:428-443, and its
#   supplement.  https://doi.org/10.1002/cpdd.769
# EMA, Ontozry assessment report, EMA/CHMP/160820/2021 (2021).
#   https://www.ema.europa.eu/en/documents/assessment-report/ontozry-epar-public-assessment-report_en.pdf
# Krishna G et al., AES 2025 abstract 2.366 (not used; see above).
#   https://aesnet.org/abstractslisting/cenobamate-population-pharmacokinetic-analyses-exposures-in-adult-and-elderly-focal-seizure-subjects
# Greene SA et al., Clin Transl Sci 2022;15:899-911.
#   https://doi.org/10.1111/cts.13204
# Darpo B et al., Clin Pharmacol Drug Dev 2022;11:523-534.
#   https://doi.org/10.1002/cpdd.1077
# Vashi V et al., J Clin Pharmacol 2024;64:922-931.
#   https://doi.org/10.1002/jcph.2439
# Operto FF et al., Front Pharmacol 2025;16:1668382.
#   https://doi.org/10.3389/fphar.2025.1668382
# Greene S, Kamin M, AAN 2021 abstract 1377, Neurology 2021;96(15 Suppl).
#   https://doi.org/10.1212/WNL.96.15_supplement.1377
# AAN 2022 abstract S13.004 (C021 post hoc troughs), Neurology 2022;98(18
#   Suppl).  https://doi.org/10.1212/WNL.98.18_supplement.537
# FDA, Xcopri prescribing information, revised 8/2025 (Reference ID 5644857).
#   https://www.accessdata.fda.gov/drugsatfda_docs/label/2025/212839s013lbl.pdf
# Steinhoff BJ et al., J Clin Med 2024;13:2757.
#   https://doi.org/10.3390/jcm13102757
# Johannessen Landmark C et al., Epilepsia 2026;67:2820-2835.
#   https://doi.org/10.1002/epi.70184
# Kasteleijn-Nolst Trenite DGA et al., Neurology 2019;93:e559-e567.
#   https://doi.org/10.1212/WNL.0000000000007894
# -----------------------------------------------------------------------------

#' Cenobamate pharmacokinetics (oral)
#'
#' The sponsor's population model (Vashi et al., 2023), two compartments,
#' apparent oral, with an absorption lag; parameters from its Supplemental
#' Table 2.  See the header of the file.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
cenobamate <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 42.4 * size$volume, v2 = 7.27 * size$volume, v3 = 1,
    cl1 = 0.533 * size$clearance / 60, cl2 = 1.84 * size$clearance / 60, cl3 = 0,
    ka_PO = 2.74 / 60,
    bioavailability_PO = 1,     # apparent (/F)
    tlag_PO = 0.240 * 60        # min
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, tPeak = 0, MEAC = 0,
    typical = 18, upperTypical = 25, lowerTypical = 10,
    reference = paste0(
      "Vashi V et al., Epilepsy Res 2023;195:107185, Supplemental Table 2: two ",
      "compartments, apparent oral, lag 0.24 h, CL/F 0.533 L/h, V1/F 42.4 L, ",
      "Q/F 1.84 L/h, V2/F 7.27 L, ka 2.74/h; dose, size, bilirubin, ",
      "ethnicity and co-medication terms not applied. ",
      "https://doi.org/10.1016/j.eplepsyres.2023.107185"
    )
  )
}
