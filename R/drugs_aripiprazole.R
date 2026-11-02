# -----------------------------------------------------------------------------
# Aripiprazole, oral only, with dehydroaripiprazole as its active metabolite
# -----------------------------------------------------------------------------
# Provenance
# ----------
# Drafted by Claude Code, 2026-10-09, at the request of Steven L. Shafer, from
# the antipsychotic implementation brief, with only the abstract of Kim 2008
# available.  Checked there: one compartment, ka 1.06 /h fixed, plasma, 80
# patients on 10 to 30 mg/day, intermediate metabolisers' CL/F about 60% of
# normal.  NOT checked: the genotype-class clearances, the metabolite's
# CLm/fm and Vm/fm, and the formation convention.
#
# Kim 2012's full text was read (2026-10-12; Table 1 re-read on the PDF
# 2026-11-01) and confirms ke0 0.725 /h (95% CI 0.542-0.908), EC50 8.63 ng/mL
# and Emax 100%.  THE KE0 TRANSFER (adjudicated 2026-11-01): Kim fitted ke0
# with his own two-compartment PK (CL/F 5.8 L/h, V1/F 130 L, Q/F 37.1 L/h,
# V2/F 274 L, ka 0.293 /h) in 18 healthy men given one oral dose, PET at 3,
# 45 and 120 h, not with the one-compartment Kim 2008 set this file runs it
# on (CL/F 3.15 L/h, V/F 192 L, ka 1.06 /h fixed).  Only the 3-h scan
# precedes equilibrium, so what the data identify is how far Ce trails Cp
# about 3 h after an ORAL dose.  On this file's PK, ke0 0.725 gives Ce/Cp
# 0.771 at 3 h against 0.728 in Kim's own model (10 mg); the ke0 matching it
# exactly is 0.632, a least-squares fit of Ce/Cp over 0-24 h gives 0.657, and
# matching Kim's oral Ce peak (6.03 h; 6.80 h here, the one-compartment
# plasma peak being flatter) gives 0.932 -- all inside or at the edge of
# Kim's interval, so ke0 is carried as published.  The bolus tPeak (2.19 h in
# Kim's model, 5.35 h here) is NOT carried: it reflects a distribution phase
# no oral dose produces, and its ke0 (2.26 /h) would erase most of the
# hysteresis seen at 3 h (+16 occupancy points at 1 h, +3 at 3 h after
# 10 mg).  The ke0 matters only in the first ~3 h after a dose or dose
# change.  At periodic steady state on once-daily dosing, replacing 0.725 by
# 0.632 or 0.932 moves occupancy by at most 0.4 points at any time in the
# interval at 2 mg (0.13 at 10 mg, 0.05 at 30 mg), and by the bolus ke0 2.26
# by at most 1.7 points at 2 mg (1.5 h after the dose; 0.5 at 10 mg).
#
# The larger difference is 3 h after a single dose: this model gives
# 46/81/93% occupancy after 2/10/30 mg where Kim's gives 32/71/88% (observed
# mean maxima 30/72/82%).  Clearance explains almost none of it (Kim's
# 5.8 L/h on this model moves the 10 mg value from 81.1 to 80.8%).  It is
# absorption: ka 1.06 /h, fixed in Kim 2008's sparse steady-state data,
# against 0.293 /h fitted in Kim 2012's rich single-dose data, so 3 h after
# a dose this model's plasma concentration is 1.7 times Kim's; with Kim's ka
# this model gives 69.2% against Kim's 70.5%.  That is a difference between
# the two sources' absorption models, not of ke0, and is recorded, not
# reconciled.
#
# V/F: the brief gives 193 L from Table 3; the abstract says 192 L.  The
# abstract is the text that could be read, so 192 L is used.
#
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in ng/mL (plasma).
#
# THE SOURCE
# ==========
# Kim J-R et al., Br J Clin Pharmacol 2008;66:802-810: 141 steady-state plasma
# concentrations from 80 Korean psychiatric patients.  One compartment each for
# parent and metabolite, APPARENT oral parameters:
#
#     ka 1.06 /h (fixed), V/F 192 L
#     CL/F by CYP2D6 genotype class: 3.15 L/h (two functional alleles), 2.66
#       (functional + partially deficient), 2.27 (functional + null), 1.83
#       (intermediate metabolisers)
#     CLm/fm 8.02 L/h, Vm/fm 587 L
#
# CYP2D6 PHENOTYPE
# ================
# normal = 3.15 L/h and intermediate = 1.83 L/h, the source's two named
# groups.  Poor and ultrarapid metabolisers were NOT studied; they take the
# nearest studied value (1.83 and 3.15).  For a poor metaboliser that
# overstates clearance and understates exposure.
#
# FORMATION
# =========
# The metabolite parameters are divided by fm, and the whole of the parent's
# apparent clearance forms the scaled metabolite, kFormation = (CL/F)/(V/F).
# That convention is assumed, not read from the paper; it is supported by the
# abstract's finding that the dehydroaripiprazole concentration-to-dose ratio
# did not differ between genotypes, which is what it predicts (the metabolite's
# steady state is dose rate / (CLm/fm) whatever the parent clearance).  It
# predicts a steady-state metabolite-to-parent ratio of (CL/F)/(CLm/fm), 0.39
# in normal and 0.23 in intermediate metabolisers, against 0.20 to 0.34
# observed.  The scaled parameters live in "dehydroaripiprazole", which is
# never dosed directly.  Amounts in aripiprazole mass (mwRatio = 1).
#
# Body size: no weight covariate, so the published values are the 70 kg
# reference adult's and scale to fat-free mass (legacyVolume = 1).
#
# EFFECT SITE
# ===========
# Kim E 2012: [11C]raclopride PET with serial plasma aripiprazole in 18 healthy
# men, fitted with an effect compartment driven by PARENT plasma: ke0 0.725 /h,
# EC50 8.63 ng/mL, Emax 100%.  ke0 is supplied directly, so the plotted
# effect-site concentration is that model's Ce, and D2 occupancy is
# 100 x Ce / (8.63 + Ce) -- of parent only.  Adding dehydroaripiprazole to the
# 8.63 ng/mL denominator would be wrong.  The PK and PD come from different
# populations, which antipsychoticProfiles() records.  Occupancy is a receptor
# biomarker, not a target: MEAC is zero.  The band is the serum therapeutic
# reference range of parent aripiprazole, 120 to 270 ng/mL (Hart et al.,
# Psychopharmacology 2022;239:3377-3391; inst/extdata/therapeuticRanges.csv),
# a trough concentration at steady state to read against the plasma curve.
#
# PREGNANCY
# =========
# Westin 2017 (Clin Pharmacol Ther) followed 14 aripiprazole pregnancies
# through routine therapeutic drug monitoring, each against the same woman's
# own baseline outside pregnancy: the dose-adjusted active moiety
# (aripiprazole + dehydroaripiprazole) was 52% lower in the middle of the
# third trimester (95% CI -62% to -39%, p < 0.001), aripiprazole alone fell
# similarly, and the parent-to-metabolite ratio fell 31% (p < 0.001).  Zheng
# 2020's physiologically based model predicts a 61% lower active-moiety AUC
# in the third trimester.  CYP2D6, which forms dehydroaripiprazole, is
# induced in pregnancy.  With the Patient Profile's Pregnant field on, the
# PARENT's apparent clearance CL/F is multiplied by
# ARIPIPRAZOLE_PREGNANCY_CL_FACTOR = 2.1 (1 / 0.48 = 2.08, rounded as the
# registry gives it), on top of the CYP2D6 phenotype's value; V/F, ka and
# ke0 are unchanged.  Because the whole of the parent's clearance forms the
# metabolite (FORMATION above), kFormation = cl1 / v1 rises with it, and
# the metabolite's own parameters (CLm/fm, Vm/fm, in R/drugs_dehydroaripiprazole.R)
# are NOT changed.  So aripiprazole falls by 52% at steady state, as
# Westin's parent did, and D2 occupancy, which is driven by the parent, falls
# with it; but the metabolite's steady state, dose rate / (CLm/fm), does not
# move, so the plotted active moiety falls by less than the measured 52%
# (38% in a normal metaboliser), and the parent-to-metabolite ratio by 52%
# rather than 31%.  Westin's ratio implies the metabolite itself fell by
# about 30%, which would need its own clearance raised; no factor for it was
# measured, and none is applied.  Late pregnancy (the third trimester) is the
# state described.  (Claude Code, 2026-10-19, at the request of Steven L.
# Shafer; see R/pregnancy.R and inst/extdata/pregnancyRegistry.csv.)
#
# References
# ----------
# Kim JR et al., Br J Clin Pharmacol 2008;66:802-810.
#   https://doi.org/10.1111/j.1365-2125.2008.03223.x
# Kim E et al., J Cereb Blood Flow Metab 2012;32:759-768.
#   https://doi.org/10.1038/jcbfm.2011.180 (effect site)
# Westin AA, Brekke M, Molden E, Skogvoll E, Castberg I, Spigset O.  Clin
#   Pharmacol Ther 2018;103:477-484.  https://doi.org/10.1002/cpt.770
#   (pregnancy)
# Zheng L, Tang S, Tang R, Xu M, Jiang X, Wang L.  Clin Pharmacokinet
#   2021;60:623-635.  https://doi.org/10.1007/s40262-020-00962-3
#   (pregnancy, physiologically based model)
# -----------------------------------------------------------------------------

ARIPIPRAZOLE_V   <- 192     # L, apparent (abstract; the brief says 193)
ARIPIPRAZOLE_KA  <- 1.06    # 1/h, fixed in the source
ARIPIPRAZOLE_KE0 <- 0.725   # 1/h, Kim 2012

# Parent apparent clearance in pregnancy relative to Kim's value: Westin
# 2017, active moiety 52% lower in the third trimester, 1 / 0.48 = 2.1 (see
# the header).  Not applied to the metabolite.
ARIPIPRAZOLE_PREGNANCY_CL_FACTOR <- 2.1

ARIPIPRAZOLE_CL_CYP2D6 <- c(   # L/h, apparent; see the header
  poor         = 1.83,
  intermediate = 1.83,
  normal       = 3.15,
  ultrarapid   = 3.15
)

#' Aripiprazole pharmacokinetics (oral)
#'
#' Kim et al. (2008): one compartment, apparent oral parameters, CYP2D6 on
#' clearance, with dehydroaripiprazole formed as an active metabolite; effect
#' site from Kim et al. (2012).
#'
#' @inheritParams cefazolin
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}
#' @param pregnant is the patient pregnant?  \code{TRUE} multiplies the
#'   parent's apparent clearance by 2.1 (Westin 2017; see the file's header).
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
aripiprazole <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                         adjustToFFM = TRUE, pregnant = FALSE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }

  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  v1  <- ARIPIPRAZOLE_V * size$volume
  cl1 <- unname(ARIPIPRAZOLE_CL_CYP2D6[[cyp2d6]]) / 60 * size$clearance   # L/min
  # Pregnancy (see the header): the parent's apparent clearance, and with it
  # kFormation below; the metabolite's own clearance is not touched.
  if (isTRUE(pregnant)) cl1 <- cl1 * ARIPIPRAZOLE_PREGNANCY_CL_FACTOR

  default <- list(
    v1 = v1,
    v2 = 1,                                        # one compartment
    v3 = 1,
    cl1 = cl1,
    cl2 = 0,
    cl3 = 0,
    ka_PO = ARIPIPRAZOLE_KA / 60,                  # 1/min
    bioavailability_PO = 1,                        # apparent (/F)
    tlag_PO = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Kim JR et al., Br J Clin Pharmacol 2008;66:802-810. ",
    "https://doi.org/10.1111/j.1365-2125.2008.03223.x (apparent oral ",
    "parameters); effect site: Kim E et al., J Cereb Blood Flow Metab ",
    "2012;32:759-768. https://doi.org/10.1038/jcbfm.2011.180"
  )
  if (isTRUE(pregnant)) {
    reference <- paste0(reference, "; ", pregnancyReference(
      paste0("parent apparent clearance x ", ARIPIPRAZOLE_PREGNANCY_CL_FACTOR,
             ", metabolite unchanged"),
      paste0("Westin AA et al., Clin Pharmacol Ther 2018;103:477-484. ",
             "https://doi.org/10.1002/cpt.770")))
  }

  list(
    PK = PK,
    tPeak = 0,                        # ke0 is supplied, not solved
    ke0 = ARIPIPRAZOLE_KE0 / 60,      # 1/min
    MEAC = 0,
    typical = 195,                    # Hart 2022, ng/mL; mirrors the CSV
    upperTypical = 270,
    lowerTypical = 120,
    reference = reference,
    metabolite = list(
      name              = "dehydroaripiprazole",
      kFormation        = cl1 / v1,   # all apparent clearance, fm-scaled
      firstPassFraction = 0,
      mwRatio           = 1           # scaled amounts in aripiprazole mass
    )
  )
}
