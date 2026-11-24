# -----------------------------------------------------------------------------
# Lumefantrine: oral, a chain of transit compartments, then two compartments,
# with a bioavailability that falls as the dose per kilogram rises
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg, so the plotted concentration is mcg/mL (= mg/L) of lumefantrine in
# plasma.
#
# Drafted by Claude Code, 2026-11-24, at the request of Steven L. Shafer, as
# one of the two components of Coartem (R/combination-products.R; "For
# Coartem, both components are active. Both should appear.", 2026-11-24).
# Lumefantrine is given in the United States only in Coartem, so this model
# has no dose units of its own and no Category: a Coartem dose reaches it as
# mg of lumefantrine, 120 mg a tablet.  Every number below was read on the
# PDF of the source.
#
# SOURCE: DING 2026, TABLE 4
# ==========================
# Ding J, Hoglund RM, van der Pluijm RW, Callery JJ, Peto TJ, Tripura R, et
# al.  Population pharmacokinetics of artemether-lumefantrine plus
# amodiaquine in patients with uncomplicated Plasmodium falciparum malaria.
# Br J Clin Pharmacol 2026;92(2):589-605.  doi 10.1002/bcp.70301 (PMID
# 41052792).  1,448 lumefantrine concentrations (lower limit 9.71 ng/mL) from
# 885 patients, 79 densely sampled, of the TRACII (575; Bangladesh and
# elsewhere) and TACT-CV (310; Vietnam and elsewhere) trials; ages 1.9-65
# years, 9.0-101 kg (Table 1); NONMEM 7.5, FOCE-I, the samples below the
# limit as censored (M3).  The arms with and without amodiaquine did not
# differ.
#
# Table 4, for a "typical" adult patient weighing 45 kg, with a baseline
# parasite density of 10^4.56 /uL and a temperature of 37.5 C, receiving
# 9.8 mg/kg of lumefantrine a dose:
#
#   MTT 5.43 h, five transit compartments, F 1 (fixed)
#   CL/F 4.35 L/h, VC/F 101 L, Q/F 1.65 L/h, VP/F 311 L
#   on F: dose (mg/kg)  -6.47 %, as F x [1 + theta (Dose - 9.8)]
#         parasitaemia  -11.6 %, as F x [1 + theta (log(PBM) - 4.56)]
#         temperature   -12.2 %, as F x [1 + theta (TEMP - 37.5)]
#   study (TACT) on VC/F -28.1 %
#
# The signs are on the page (the text layer drops them); the dose term's unit
# is per mg/kg, the table printing the per cent as a fraction of 1 per mg/kg
# (a literal -6.47 would make F negative 0.16 mg/kg above the reference).
# Weight acts on every clearance and volume with exponents FIXED at 0.75 and
# 1 (Methods 2.7), normalised to 45 kg.
#
# The transit chain is read as R/drugs_artemether.R reads artemether's: the
# dose depot and five transit compartments, every step at (5 + 1) / MTT.
#
# WHAT IS APPLIED, AND WHAT IS HELD
# =================================
# * The dose per kilogram, applied to each dose through the oralSaturation
#   block's linear form (R/routes.R), on the patient's actual weight: a
#   45 kg patient's 480 mg is 10.7 mg/kg, F 0.94; a 70 kg patient's, 6.9
#   mg/kg, F 1.19.  Ding: "this linear dose-covariate relationship on
#   relative bioavailability is not suitable for extrapolation beyond the
#   dose range studied here", so the dose is held within the studied range,
#   4.75 to 16.0 mg/kg a dose (Table 1: 9.5 to 32.0 mg/kg a day in two
#   doses), where F runs from 1.33 to 0.60.
# * Parasitaemia and temperature are held at the reference (10^4.56 /uL,
#   37.5 C): the Patient Profile has neither.  A patient with ten times the
#   parasites absorbs 11.6% less; with a fever of 39.5 C, 24% less.
# * The study term on VC/F is held at TRACII's, the table's reference.
# * Desbutyl-lumefantrine, under 1% of the parent's exposure (label 12.3),
#   is not drawn.
# * Food: every patient took Coartem with food or milk; fasting lowers
#   lumefantrine's bioavailability about 16-fold (label 12.3).  The curve is
#   the fed patient's.
# * Interactions (CYP3A4: ketoconazole, lopinavir/ritonavir, rifampin,
#   efavirenz) are not modelled.
#
# PREGNANCY
# =========
# Juma E, Ding J, Ongas M, et al.  Population pharmacokinetics of
# lumefantrine in pregnant and non-pregnant women with uncomplicated
# Plasmodium falciparum malaria in Western Kenya.  Br J Clin Pharmacol
# 2026;92(3):911-921 (PMID 41152149, PMC12930023; read in full from PubMed
# Central, 2026-11-24).  The same group's model in 75 women given the
# standard six doses, 50 in the second or third trimester (gestational age
# 13 to 40 weeks, median 26) and 25 not pregnant, densely sampled in venous
# plasma.  Pregnancy status, chosen over gestational age by the lower
# objective function, entered the final model as theta x (1 + theta_cov) on
# CL/F (+23.2%, 95% CI 10.8-34.8) and Vc/F (+28.1%, 7.3-51.8), Table 2;
# trimester had no significant effect.  So with the Pregnant field ticked,
# CL/F x 1.232 and VC/F x 1.281 (LUMEFANTRINE_PREGNANCY_CL, _VC).  The
# pregnant women's exposure was lower (AUC GMR 0.76, day-7 concentration
# 0.69).  Kloprogge 2018's pooled estimate (ka x 1.35 in pregnancy, 20%
# lower day-7 levels) has the same direction; its first-order absorption is
# not this model's, so it is recorded, not applied.
#
# WHY DING AND NOT KLOPROGGE 2018
# ===============================
# Kloprogge 2018 (PLoS Med 15:e1002579, PMID 29894518, read on the PDF),
# the WWARN pooled analysis of 1,347 patients, is larger, but it describes
# absorption with one first-order rate of 0.0386 /h (a half-time of 18 h),
# fitted to mostly sparse day-7 samples; after a single dose that puts the
# peak at the end of the third day, where the label and every dense study put
# it at 6 to 8 hours.  Ding's five transit compartments were fitted with 79
# densely sampled patients and give the label's peak time.  The two agree on
# what the plot mostly shows, the clearance and the day-7 level: Kloprogge's
# CL/F 1.35 L/h at 42 kg over its saturable F at 9.8 mg/kg (3.86 / (3.86 +
# 9.8) = 0.283) is 4.8 L/h, against Ding's 4.35 at 45 kg, and both models put
# the typical day-7 concentration near 460 ng/mL.  Kloprogge's pregnancy term
# (ka x 1.35) is recorded in the pregnancy registry, not applied: it belongs
# to its first-order absorption, which this model does not have.
#
# CHECKS (tests/testthat/test-drugs-lumefantrine.R)
# =================================================
# * Table 4's typical values, both switch positions, and the dose term.
# * The typical 45 kg patient on the label's six doses of 480 mg: day-7
#   concentration 462 ng/mL and AUC 591 h.mcg/mL, against Table 4's medians
#   of 452 and 600.  Table 4's median peak, 15.2 mcg/mL, is twice the typical
#   patient's 7.2; the peak was reported only for the 79 densely sampled
#   patients, and the reason is not resolved.  The label's patients (Table 4,
#   six doses) had mean peaks of 5.6 to 9.0 mcg/mL, which this model matches.
# -----------------------------------------------------------------------------

LUMEFANTRINE_MTT        <- 5.43    # h, mean transit time
LUMEFANTRINE_TRANSITS   <- 5L      # transit compartments, besides the depot
LUMEFANTRINE_CL         <- 4.35    # L/h, CL/F at 45 kg
LUMEFANTRINE_VC         <- 101     # L, VC/F at 45 kg (TRACII)
LUMEFANTRINE_Q          <- 1.65    # L/h, Q/F at 45 kg
LUMEFANTRINE_VP         <- 311     # L, VP/F at 45 kg
LUMEFANTRINE_DOSE_F     <- -0.0647 # per mg/kg above the reference dose
LUMEFANTRINE_DOSE_REF   <- 9.8     # mg/kg a dose
LUMEFANTRINE_DOSE_RANGE <- c(4.75, 16.0)  # mg/kg a dose, Table 1 (per day / 2)
LUMEFANTRINE_PREGNANCY_CL <- 1.232  # Juma 2026, Table 2: CL/F x (1 + 0.232)
LUMEFANTRINE_PREGNANCY_VC <- 1.281  # Juma 2026, Table 2: Vc/F x (1 + 0.281)

#' Lumefantrine pharmacokinetics
#'
#' A component of Coartem: see R/combination-products.R.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
lumefantrine <- function(weight, height, age, sex, adjustToFFM = TRUE,
                         pregnant = FALSE)
{
  f <- dingWeightFactors(weight, height, age, sex, adjustToFFM)
  pCl <- if (isTRUE(pregnant)) LUMEFANTRINE_PREGNANCY_CL else 1
  pVc <- if (isTRUE(pregnant)) LUMEFANTRINE_PREGNANCY_VC else 1
  ktr <- transitRateConstant(LUMEFANTRINE_MTT, LUMEFANTRINE_TRANSITS)   # /h

  default <- list(
    v1  = LUMEFANTRINE_VC * f$volume * pVc,
    v2  = LUMEFANTRINE_VP * f$volume,
    v3  = 1,
    cl1 = LUMEFANTRINE_CL * f$clearance * pCl / 60,
    cl2 = LUMEFANTRINE_Q * f$clearance / 60,
    cl3 = 0,
    ka_PO = ktr / 60,
    bioavailability_PO = 1,     # apparent parameters at the reference dose
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
    transitChain = list(
      n   = LUMEFANTRINE_TRANSITS + 1L,
      ktr = ktr / 60,
      kMetabolic = 0
    ),
    # F x [1 + theta (dose/WT - 9.8)], the dose per kg on the actual weight,
    # written per mg for this patient and held within the studied doses.
    oralSaturation = list(
      form  = ORAL_SATURATION_LINEAR,
      slope = LUMEFANTRINE_DOSE_F / weight,
      Dref  = LUMEFANTRINE_DOSE_REF * weight,
      Dmin  = LUMEFANTRINE_DOSE_RANGE[1] * weight,
      Dmax  = LUMEFANTRINE_DOSE_RANGE[2] * weight,
      # the help page's examples: one to four tablets of Coartem
      exampleDoses = c(120, 240, 360, 480)
    ),
    reference = paste0(
      "Ding J, Hoglund RM, van der Pluijm RW, et al. Population pharmacokinetics ",
      "of artemether-lumefantrine plus amodiaquine in patients with uncomplicated ",
      "Plasmodium falciparum malaria. Br J Clin Pharmacol 2026;92:589-605, Table 4 ",
      "(885 patients). A COMPONENT OF COARTEM. Five transit compartments behind the ",
      "dose depot (MTT 5.43 h, each step 6/MTT), then two compartments: CL/F 4.35 ",
      "L/h and Q/F 1.65 L/h x (WT/45)^0.75, VC/F 101 L and VP/F 311 L x WT/45. ",
      "Bioavailability falls 6.47% per mg/kg a dose above 9.8 mg/kg, held within ",
      "the studied 4.75-16.0 mg/kg; parasitaemia and temperature held at the ",
      "reference (10^4.56 /uL, 37.5 C). Fed patients; no interaction is modelled. ",
      "https://doi.org/10.1002/bcp.70301",
      if (isTRUE(pregnant)) paste0("; ", pregnancyReference(
        "CL/F x 1.232, VC/F x 1.281",
        "Juma 2026, Br J Clin Pharmacol 92:911-921, Table 2")))
  )
}
