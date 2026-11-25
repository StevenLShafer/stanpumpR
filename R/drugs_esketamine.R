# -----------------------------------------------------------------------------
# Esketamine (S(+)-ketamine), intravenous: Sigtermans 2009
# -----------------------------------------------------------------------------
# Sigtermans M, Dahan A, Mooren R, Bauer M, Kest B, Sarton E, Olofsen E.
# S(+)-ketamine effect on experimental pain and cardiac output: a population
# pharmacokinetic-pharmacodynamic modeling study in healthy volunteers.
# Anesthesiology 2009;111:892-903.  doi:10.1097/ALN.0b013e3181b437b1, PMID
# 19741495.  Every number below was read on the PDF (stanpumpRPapers
# PMID_19741495.pdf; card papers/digests/PMID_19741495.md, from the digest run
# of 2026-11-25, PR #903).
#
# WHY A DRUG OF ITS OWN (Steven L. Shafer, 2026-11-25: "Please see if the
# pharmacokinetics of ketamine accurately describe Esketamine ... I don't
# think the pharmacokinetics are very different"; then "Implement esketamine
# from Sigtermans 2009").  They are different.  The library's ketamine is
# racemic (Domino 1984, R/drugs_ketamine.R), with a clearance of 55 L/h at
# 70 kg; S(+)-ketamine given alone is cleared at 75 (men) to 97 (women) L/h
# per 70 kg here, because R(-)-ketamine inhibits the elimination of
# S(+)-ketamine when the two are given together (Ihmsen 2001, PMID 11719729,
# still to obtain).  At steady state the racemic model would predict
# esketamine concentrations about 1.4 to 1.8 times too high for a given rate.
#
# STUDY.  20 healthy volunteers, 10 men and 10 women, 18-30 years, BMI below
# 28 (Table 1: 69.4 kg, men 75.2, women 63.6; 177 cm; 21.7 years).  A 2-h
# stepped infusion raising the concentration by 40 ng/mL every 15 min to 320
# ng/mL (66.5 mg in 70 kg); arterial plasma for 3 h after it; HPLC-UV, LLOQ
# 10 ng/mL.
#
# MODEL.  Table 2, the combined S(+)-ketamine-S(+)-norketamine model (the
# paper's final model; the ketamine-only fit beside it agrees on V1, V2, V3
# and CL1 but swaps which peripheral compartment takes the larger
# intercompartmental clearance, which the paper does not discuss):
#   V1 15.4, V2 26.6, V3 121 L per 70 kg
#   CL1 75.1 (men), 97.3 (women), CL2 80.6, CL3 146 L/h per 70 kg
# Norketamine (S(+)-desmethylketamine) is not carried: its central volume
# was not identifiable and was set equal to ketamine's, and the library has
# no norketamine model; it added no consistent effect on any end point.
#
# SEX.  On CL1 only.  The abstract and Discussion say clearance is 20% higher
# in women; that is the ketamine-only fit (98.0/81.4); the combined model's
# Table 2 values used here give 30% (97.3/75.1).
#
# BODY SIZE (docs/weight-adjustment.md).  The doses were per 70 kg, so every
# volume and clearance is per 70 kg, linear in weight; the weight-scaled
# parameterisation beat the unscaled one (delta MOFV 21).  That is a size
# term the source estimated, so it is kept as fitted: the parameters are
# evaluated at the pharmacokinetic weight with the fat-free-mass switch on
# (size$pkWeight) and at total body weight with it off, as landiolol's are,
# and are not anchored at the cohort.
#
# DOSE BASIS.  The paper gives doses as "mg S(+)-ketamine" (Fig 1A) and does
# not name the salt.  By the library's rule (AGENTS.md, Dose basis) they are
# read as the labelled product, esketamine base (Ketanest S is labelled as
# esketamine; Spravato, the US product, is too), and doses are entered as
# labelled with no conversion.
#
# EFFECT SITE: tPeak 3 MINUTES, THE SAME AS THE LIBRARY'S RACEMIC KETAMINE
# (Steven L. Shafer, 2026-11-25: "The peak effect of esketamine will be the
# same as for ketamine if it is given IV").  Sigtermans did not estimate ke0
# (Discussion, PDF p8); they saw the effects follow the plasma closely and
# cite Herd's t1/2 ke0 of 11 s (racemic ketamine, children, sedation scores;
# Herd 2008, PMID 18095964; Sigtermans quote its 95% CI as 7-20 s, Herd's
# abstract prints 0.07-20 s) and Schuttler's S(+)-ketamine EEG data.  On this
# model 11 s would put the peak at 0.7 min.  The library's ketamine assumes
# 3 min (R/drugs_ketamine.R), and ke0 is solved from it here
# (ESKETAMINE_TPEAK = KETAMINE's 3 min), so the two drugs peak together after
# a bolus.
#
# NO BAND.  The paper's C50s are for experimental pain (heat pain intensity
# 373 ng/mL, electrical pain tolerance 2.2 mcg/mL) and a 20% rise in cardiac
# output (134 ng/mL), not a clinical target; they are given on the help page.
#
# PREGNANCY.  No pharmacokinetic study of esketamine in pregnancy (PubMed,
# 2026-11-25; the 2026 review of peripartum esketamine, Zhong et al., Drug
# Des Devel Ther, says so); recorded unknown.
# -----------------------------------------------------------------------------

#' Sigtermans 2009 S(+)-ketamine parameters, per 70 kg (Table 2, combined model)
#'
#' Volumes in L/70 kg, clearances in L/h per 70 kg.
#' @keywords internal
ESKETAMINE_SIGTERMANS <- c(v1 = 15.4, v2 = 26.6, v3 = 121,
                           cl1Male = 75.1, cl1Female = 97.3,
                           cl2 = 80.6, cl3 = 146.0)

#' Time to peak effect of esketamine after a bolus, minutes: the library's
#' racemic ketamine's (Shafer, 2026-11-25)
#' @keywords internal
ESKETAMINE_TPEAK <- 3

#' Esketamine (S(+)-ketamine), intravenous, Sigtermans 2009
#'
#' Three compartments per 70 kg in healthy volunteers, with women's clearance
#' higher than men's.  See the file header.
#' @inheritParams ketamine
#' @export
esketamine <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  w70  <- (if (isTRUE(adjustToFFM)) size$pkWeight else weight) / 70
  p    <- ESKETAMINE_SIGTERMANS
  cl1  <- if (identical(sex, "female")) p[["cl1Female"]] else p[["cl1Male"]]

  default <- list(
    v1  = p[["v1"]] * w70,
    v2  = p[["v2"]] * w70,
    v3  = p[["v3"]] * w70,
    cl1 = cl1 / 60 * w70,
    cl2 = p[["cl2"]] / 60 * w70,
    cl3 = p[["cl3"]] / 60 * w70
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK,
    tPeak = ESKETAMINE_TPEAK,  # the same as racemic ketamine; see the header
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = paste0(
      "Sigtermans M et al., Anesthesiology 2009;111:892-903 (S(+)-ketamine, ",
      "20 healthy volunteers, arterial plasma; doses as esketamine base). ",
      "Time to peak effect 3 min, as for racemic ketamine. ",
      "https://doi.org/10.1097/ALN.0b013e3181b437b1")
  )
}
