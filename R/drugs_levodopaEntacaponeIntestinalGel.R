# -----------------------------------------------------------------------------
# Levodopa-entacapone-carbidopa intestinal gel (Lecigon): one compartment,
# apparent, with allometric weight, infused into the jejunum
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L) of levodopa.
#
# Drafted by Claude Code, 2026-10-20, at the request of Steven L. Shafer, who
# supplied the article (inst/help/models/parkinsons.md,
# inst/extdata/parkinsonsRegistry.csv).
#
# THE SOURCE
# ==========
# Senek M, Nyholm D, Nielsen EI.  Population pharmacokinetics of levodopa gel
# infusion in Parkinson's disease: effects of entacapone infusion and genetic
# polymorphism.  Sci Rep 2020;10:18057 (PMID 33093598), read in full.  Eleven
# patients in Uppsala with advanced disease (age 63-76, weight 51-99 kg, 7
# men), each given levodopa-carbidopa intestinal gel (LCIG, Duodopa) one day
# and levodopa-entacapone-carbidopa intestinal gel (LECIG, Lecigon) the other,
# 14 hours of morning dose, continuous rate and extra doses, sampled to 17 h.
# NONMEM 7.3, FOCE-I.  Table 2, one compartment with first-order absorption:
#
#     CL/F (LCIG)      27.9 L/h x (WT/70)^0.75
#     LECIG shift      -0.365 on CL/F          (so 17.7 L/h at 70 kg)
#     Vc/F             74.5 L x WT/70           (the abstract rounds to 74.4)
#     ka               50 /h, fixed ("the lowest value which did not give a
#                      significant increase in OFV")
#     Frel             1, fixed, for both gels
#
# The weight exponents were fixed (the allometric power model), not
# estimated.  A two-compartment model lowered the objective function by 23
# but was unstable.  The drug is the LECIG arm: the LCIG arm's levodopa is
# already the library's levodopaIntestinalGel (Othman & Dutta 2014, a larger
# study with a peripheral compartment), which Senek's 27.9 L/h corroborates
# (Othman's 24.8).
#
# Entacapone itself is not plotted.  Its effect on levodopa is the 36.5% fall
# in clearance, the same in every patient (inter-individual variability of
# the shift 11%), and the paper's conclusion follows from it: a continuous
# rate must be about 35% lower on LECIG than on LCIG for the same steady
# state (tests).  The study's own 20% reduction let the concentration climb
# through the day.
#
# GENOTYPE
# ========
# Every patient was genotyped for COMT rs4680 (Val158Met: 3 AA low activity, 4
# AG, 4 GG high) and DDC rs921451 and rs3837091.  The comparison of the
# individual clearances by genotype was graphical only ("a formal covariate
# analysis ... was not performed, due to the low number of included
# subjects"): no trend by COMT genotype, either in clearance or in the
# entacapone shift, and a tendency to higher clearance with higher DDC
# activity.  No genotype term is applied; this drug reads no
# pharmacogenetic field.
#
# WHERE TO BE CAREFUL
# ===================
# The model over-predicts the first hours on LECIG (the authors' Figure 1);
# the cause is unknown.  Protein meals (served at 1, 4, 7, 10 and 13 h) moved
# the concentrations but no food effect could be fitted.  Eleven patients.
# Lecigon is approved in Sweden and other European countries, not in the US.
#
# DOSING: as for levodopaIntestinalGel, the morning dose and extra doses are
# "mg PO" and the continuous rate "mg/hr PO", both levodopa into the jejunum
# (the drug declares enteralInfusion; R/transit.R).  Absorption at 50/h is
# all but immediate (half-time 50 s).
#
# BODY SIZE (docs/weight-adjustment.md): the model's own allometric weight
# terms, evaluated at the pharmacokinetic weight with the fat-free-mass switch
# on and at total body weight with it off.
#
# EFFECT SITE, THRESHOLD AND BAND: as for levodopa and its other gel, Mao
# 2013's motor effect site (UPDRS III ke0 1.80/h, EC50 0.81 mcg/mL, fitted to
# measured plasma levodopa in advanced disease, so independent of the plasma
# model and of entacapone, which acts on levodopa's elimination, not on its
# action) and the AGNP band, 0.9 to 2.0 mcg/mL.
#
# References
# ----------
# Senek M, Nyholm D, Nielsen EI. Sci Rep 2020;10:18057.
#   https://doi.org/10.1038/s41598-020-75052-2
# Mao Z et al. J Clin Pharmacol 2013;53:523-531. https://doi.org/10.1002/jcph.63
# -----------------------------------------------------------------------------

# Senek 2020, Table 2
LECIG_CL_LCIG  <- 27.9     # L/h at 70 kg, levodopa-carbidopa gel
LECIG_CL_SHIFT <- -0.365   # fractional change in CL/F with entacapone gel
LECIG_V        <- 74.5     # L at 70 kg
LECIG_KA       <- 50       # /h, fixed

#' Levodopa-entacapone-carbidopa intestinal gel (Lecigon)
#'
#' One compartment with allometric weight (Senek 2020), levodopa's apparent
#' clearance 36.5% lower than on levodopa-carbidopa gel, given as doses
#' ("mg PO") and a continuous enteral rate ("mg/hr PO"), with levodopa's
#' motor effect site (Mao 2013).  See the header of
#' \code{R/drugs_levodopaEntacaponeIntestinalGel.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
levodopaEntacaponeIntestinalGel <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  w <- if (isTRUE(adjustToFFM))
    pkSizeFactors(weight, height, age, sex, TRUE)$pkWeight else weight

  default <- list(
    v1  = LECIG_V * w / 70, v2 = 1, v3 = 1,
    cl1 = LECIG_CL_LCIG * (1 + LECIG_CL_SHIFT) * (w / 70)^0.75 / 60,
    cl2 = 0, cl3 = 0,
    ka_PO = LECIG_KA / 60,
    bioavailability_PO = 1,          # apparent parameters
    tlag_PO = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  list(
    PK = PK, ke0 = LEVODOPA_KE0 / 60, MEAC = 0,
    typical = 1.4, upperTypical = 2.0, lowerTypical = 0.9,
    enteralInfusion = TRUE,
    reference = paste0(
      "Senek M et al., Sci Rep 2020;10:18057 (11 patients, levodopa-carbidopa and ",
      "levodopa-entacapone-carbidopa intestinal gel): one compartment, ka 50/h ",
      "(fixed), CL/F 27.9 L/h x (weight/70)^0.75 on levodopa-carbidopa gel and ",
      "36.5% lower with entacapone (17.7 L/h), V/F 74.5 L x weight/70; motor ",
      "effect site ke0 1.80/h (Mao 2013). https://doi.org/10.1038/s41598-020-75052-2"
    )
  )
}
