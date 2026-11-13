# -----------------------------------------------------------------------------
# Tacrolimus (Prograf, twice daily; Woillard 2011): whole blood, ng/mL
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is mcg/L (ng/mL) in whole blood.
#
# Drafted by Claude Code, 2026-11-12, at the request of Steven L. Shafer, who
# asked for the common drugs with narrow therapeutic windows that the library
# lacked.  Every number below was read from the full text of the source in
# PubMed Central (PMC3045548: Methods, Results, Table 1 and Table 4).
#
# THE SOURCE
# ==========
# Woillard JB, de Winter BCM, Kamar N, Marquet P, Rostaing L, Rousseau A.
# Population pharmacokinetic model and Bayesian estimator for two tacrolimus
# formulations - twice daily Prograf and once daily Advagraf.  Br J Clin
# Pharmacol 2011;71:391-402 (doi 10.1111/j.1365-2125.2010.03837.x).  Adult
# kidney transplant recipients: 32 on Prograf (145 profiles at weeks 1 and 2
# and months 1, 3 and 6 after transplantation; age 55, 18-69; weight 65,
# 46-97 kg; haematocrit 32.3%, 20.9-46.6) and 41 on Advagraf more than a year
# after.  Whole blood by TFC-MS/MS, LLOQ 1 ug/L, at 0, 0.33, 0.66, 1, 1.5, 2,
# 3, 4, 6, 9 (and 12) h.  NONMEM, two compartments with first-order
# elimination, the absorption "an Erlang absorption model with three transit
# compartments" (ADVAN5).  Table 4, the final model on the whole data set:
#
#     Ktr  = 3.34 /h x 1.53^STUDY          (STUDY 1 for Prograf)
#     CL/F = 21.2 L/h x (HT/35)^-1.14 x 2.00^CYP   (CYP 1 for a CYP3A5
#                                                    expresser)
#     Q/F  = 79 L/h
#     Vc/F = 486 L x 0.29^STUDY
#     Vp/F = 271 L
#
# So for Prograf: Ktr 5.11 /h, Vc/F 141 L.  The Discussion's "apparent volume
# of distribution was 205 l in patients treated with Prograf" matches no table
# value; Table 4 is used.  The parameters are APPARENT (divided by an
# unmeasured bioavailability), so only the oral route is offered: the Prograf
# label's absolute bioavailability, 17 +/- 10% in kidney transplant patients,
# would turn them into intravenous ones only by borrowing a number from a
# different study, which docs/adding-a-drug.md forbids for the dose basis it
# would set.
#
# THE ERLANG ABSORPTION
# =====================
# Three compartments in series, each emptying at Ktr, the last into the
# central compartment: a gamma input of shape 3.  A repeated eigenvalue no
# closed form can carry, so the drug declares a transit chain of three with
# ka_PO = Ktr and runs on the exact matrix-exponential engine
# (R/advanceTransitChain.R), as enalapril and levodopa do.
#
# HELD AT THE REFERENCE
# =====================
#   CYP3A5: the app has no CYP3A5 field.  The reference is a NON-EXPRESSER
#     (CYP3A5*3/*3), as 67 of these 73 patients were and about 85% of white
#     Europeans are; an expresser (*1 carrier, the majority in people of
#     African descent) clears tacrolimus twice as fast (CL/F x 2.00).
#     [decision, Shafer] Whether to add a CYP3A5 field to the Patient Profile.
#   Haematocrit: held at the model's reference, 35%.  CL/F falls as it rises
#     (25.3 L/h at 30% and 18.2 at 40%, the Discussion's own figures).
#   Time since transplantation, corticosteroid dose, interacting drugs (the
#     azoles, macrolides, diltiazem, rifampin): not modelled.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No size covariate was retained (body weight was tested and dropped), so the
# published fixed values are scaled by the library's fat-free-mass factors
# with the switch on, and fixed with it off (legacyVolume = legacyClearance
# = 1).
#
# THE BAND
# ========
# The Prograf label (NDA 050708, 2023), Table 1, adult kidney transplant,
# whole blood trough: with mycophenolate and an IL-2 receptor antagonist,
# "Month 1-12: 4-11 ng/mL" (with azathioprine, 7-20 ng/mL in months 1-3 and
# 5-15 in months 4-12).  The MMF regimen, the usual one, is drawn.
#
# PREGNANCY
# =========
# Zheng S, et al.  Pharmacokinetics of tacrolimus
# during pregnancy.  Ther Drug Monit 2012;34:660-670 (PMC3498613, read in
# full): ten transplant recipients on Prograf studied at steady state in
# early, mid (22-26 weeks) and late (34-38 weeks) pregnancy and five again
# more than 3 months postpartum, mixed-effects comparison: "The mean
# tacrolimus CL/F based on whole blood concentration was higher by 39% during
# mid/late pregnancy compared to postpartum (47.4 +/- 12.6 L/h vs. 34.2 +/-
# 14.8 L/h, p < 0.03)".  Applied: whole blood CL/F x 47.4/34.2 = 1.386,
# volumes and absorption unchanged.  The same study found the UNBOUND
# clearance unchanged: the rise in whole blood clearance comes from the fall
# in albumin and red cells, so unbound tacrolimus rises when the dose is
# raised to hold the whole blood trough (unbound trough +112%).
# -----------------------------------------------------------------------------

# Woillard 2011, Table 4 (whole data set), for Prograf
TACROLIMUS_KTR  <- 3.34 * 1.53   # 1/h
TACROLIMUS_CL   <- 21.2          # L/h at haematocrit 35%, CYP3A5 non-expresser
TACROLIMUS_Q    <- 79            # L/h
TACROLIMUS_VC   <- 486 * 0.29    # L
TACROLIMUS_VP   <- 271           # L
TACROLIMUS_CYP3A5_EXPRESSER_FACTOR <- 2.00   # not applied: no CYP3A5 field
# Zheng 2012: whole blood CL/F in mid/late pregnancy over postpartum
TACROLIMUS_PREGNANCY_CL_FACTOR <- 47.4 / 34.2

#' Tacrolimus, Prograf (Woillard 2011)
#'
#' Two compartments with a three-compartment Erlang absorption, apparent oral
#' parameters, whole blood; see the header of \code{R/drugs_tacrolimus.R}.
#'
#' @inheritParams cefazolin
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the whole
#'   blood CL/F by \code{TACROLIMUS_PREGNANCY_CL_FACTOR} (Zheng 2012).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
tacrolimus <- function(weight, height, age, sex, adjustToFFM = TRUE,
                       pregnant = FALSE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  ktr <- TACROLIMUS_KTR / 60

  default <- list(
    v1  = TACROLIMUS_VC * size$volume,
    v2  = TACROLIMUS_VP * size$volume,
    v3  = 1,                          # placeholder: two compartments
    cl1 = TACROLIMUS_CL / 60 * size$clearance *
      (if (isTRUE(pregnant)) TACROLIMUS_PREGNANCY_CL_FACTOR else 1),
    cl2 = TACROLIMUS_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = ktr,
    bioavailability_PO = 1,           # apparent parameters
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Woillard JB et al., Br J Clin Pharmacol 2011;71:391-402: Prograf in ",
    "adult kidney transplant recipients, two compartments after a ",
    "three-compartment Erlang absorption (Ktr 5.11 /h), CL/F 21.2 L/h at a ",
    "haematocrit of 35% in a CYP3A5 non-expresser (an expresser x 2), Q/F 79 ",
    "L/h, Vc/F 141 L, Vp/F 271 L; whole blood. ",
    "https://doi.org/10.1111/j.1365-2125.2010.03837.x"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      "whole blood CL/F x 1.39 (mid and late pregnancy)",
      paste0("Zheng S et al., Ther Drug Monit 2012;34:660-670. ",
             "https://doi.org/10.1097/FTD.0b013e3182708edf")))
  }

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 7.5,
    upperTypical = 11,
    lowerTypical = 4,
    reference = reference,
    transitChain = list(n = 3L, ktr = ktr, kMetabolic = 0)
  )
}
