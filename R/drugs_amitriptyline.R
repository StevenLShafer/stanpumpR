# -----------------------------------------------------------------------------
# Amitriptyline (Koh 2018), forming nortriptyline: two compartments, apparent
# oral, CYP2C19 on the formation and CYP2D6 on the other pathways
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mcg, so the plotted concentration is ng/mL.
#
# Drafted by Claude Code, 2026-11-13, at the request of Steven L. Shafer, who
# supplied the source's PDF and on 2026-11-13 agreed the five choices marked
# [Shafer 2026-11-13] below.  The numbers were read in that PDF (Methods,
# Figure 2, Table 3 on the page image), in Hart 2026 (Tables 5 and 6) and in
# the amitriptyline hydrochloride tablets label (ANDA 085968, 2025).
#
# THE SOURCE
# ==========
# Koh A, et al.  Quantitative modeling analysis demonstrates the impact of
# CYP2C19 and CYP2D6 genetic polymorphisms on the pharmacokinetics of
# amitriptyline and its metabolite, nortriptyline.  J Clin Pharmacol
# 2019;59:532-540 (doi 10.1002/jcph.1344).  24 healthy Korean men (25 +/- 4
# years, 72 +/- 7 kg), six in each of four genotype groups, a single 25 mg
# oral dose, plasma to 96 h (373 amitriptyline and 347 nortriptyline
# concentrations, LC-MS/MS, molar), NONMEM.  Two compartments for each
# moiety; nortriptyline formed from amitriptyline's central compartment
# (no first-pass formation).  Table 3 (apparent oral parameters):
#
#     CL_M (formation of nortriptyline)  61.5 L/h; 17 L/h in CYP2C19 PM
#     CL_O (other pathways)              30.6 L/h; 18.8 L/h in CYP2D6 "PM"
#     Vc 155 L    Q 109 L/h    Vp 1670 L
#     ka 0.125 /h; the dose released zero-order over D1 1.13 h into the
#     depot after a lag of 0.414 h
#     nortriptyline: CL 58.2 L/h, K56 1.09 /h, K65 0.168 /h, its central
#     volume fixed equal to amitriptyline's "due to an identifiability
#     problem"
#
# so a CYP2C19 and CYP2D6 normal metaboliser's apparent clearance is 92.1
# L/h, terminal half-life 23.8 h.  Enterohepatic recirculation (K24, K41,
# MTIME) is a single-dose, meal-timed feature that changes the area by 0.7%;
# it is not modelled.
#
# GENOTYPES [Shafer 2026-11-13]
# =============================
# Koh's "RM" and "PM" are not the CPIC terms.  CYP2C19 (on formation): the
# poor metaboliser (*2 or *3 on both alleles) 17 L/h; every other phenotype
# 61.5 L/h, Koh having pooled *1/*1 with *1/*2 and *1/*3 and not genotyped
# *17, so rapid and ultrarapid take the normal value (extrapolated).
# CYP2D6 (other pathways): normal and ultrarapid 30.6 L/h; intermediate 18.8
# L/h (Koh's low group was *10/*10 and *10/*5, CPIC intermediates); poor
# also 18.8 L/h, an extrapolation that likely overstates a true poor
# metaboliser's clearance (no *5/*5 subject was enrolled).  The CYP2D6
# effect was not statistically significant in the source.
#
# THE ABSORPTION [Shafer 2026-11-13]
# ==================================
# The library's metabolite route cannot run a zero-order release, so the
# release over D1 is discretised as four quarter doses at the midpoints of
# its quarters, after the lag (oralPulses with ORAL_PULSE_DEFAULT_FORM,
# R/oral-pulses.R), each absorbed at ka.  Measured against a 400-pulse
# discretisation (the exact release to the precision shown) in
# test-drugs-amitriptyline.R: within 1.3% of the peak for amitriptyline and
# 0.03% for nortriptyline after a single dose.
#
# NORTRIPTYLINE: READ FROM THE LIBRARY'S MODEL [Shafer 2026-11-13]
# ================================================================
# The curve of the nortriptyline formed is drawn with the library's
# nortriptyline model (Vos 2025, R/drugs_nortriptyline.R), so the formation
# is rescaled to it (AGENTS.md, "Parents and their metabolites must agree";
# METABOLITE_FORMATION_BASIS "reads").  With nortriptyline's central volume
# fixed to amitriptyline's, Koh's split between formation and nortriptyline
# clearance is arbitrary; what the data identify is their ratio, R = CL_M /
# CL_NOR (1.0567 in a CYP2C19 normal metaboliser, 0.2921 in a poor one), the
# molar ratio of the two moieties' areas.  The formation clearance is
# therefore R times the library's nortriptyline clearance for this patient,
# applied to the plasma amitriptyline the model draws, with the molecular-
# weight ratio 263.38 / 277.40.  This keeps the steady state (the formed
# nortriptyline's trough per daily dose, 0.389 ng/mL per mg against Hart
# 2026's 0.376, +3%), but the library's nortriptyline is slower and larger
# than Koh's, so after a single dose its peak is about half Koh's.
#
# AGAINST UNFITTED DATA (Gupta 1999, 14 healthy men, 75 mg of the
# hydrochloride; Hart 2026 Table 6)
# ==================================================================
# Gupta: amitriptyline AUC 688 ng.h/mL, peak 26.8 ng/mL; this model at 77 kg,
# 677 and 30.9.  Nortriptyline AUC 648 and peak 12.0; this model 675 and 5.9
# (the half-height peak above).  Hart's 24-h trough per daily dose:
# amitriptyline 0.399 ng/mL per mg, this model 0.232 (its average over the
# day, 0.40, matches: Koh's small central volume and fast distribution leave
# a deep trough after a once-daily dose); nortriptyline formed 0.376, this
# model 0.389.
#
# THE DOSE BASIS [Shafer 2026-11-13]
# ==================================
# US tablets are amitriptyline hydrochloride ("10, 25, 50, 75, 100, or 150 mg
# amitriptyline hydrochloride"), entered as prescribed.  Koh fitted molar
# concentrations without naming the salt of its 25 mg dose; read as the
# hydrochloride converted to base, the model gives Gupta 1999's single-dose
# area to 5% (and as the hydrochloride itself, 18% high).  So each dose is
# taken as hydrochloride and converted to base, 277.40 / 313.86 = 0.884,
# through the bioavailability (the parameters being apparent).
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# No covariate besides genotype; the values are written for the library's
# reference man and scaled by fat-free mass with the switch on, fixed with it
# off.
#
# THE BAND [Shafer 2026-11-13]
# ============================
# Not drawn.  Hart 2026 (AGNP update, Table 5): 80-200 ng/mL for amitriptyline
# PLUS nortriptyline, 12 h after the dose, alert level 300 ng/mL; with the
# two moieties about equal (Table 7: nortriptyline/amitriptyline 0.2-1.8), the
# parent does not reliably carry most of the sum (AGENTS.md, therapeutic
# ranges), so the band is recorded (sum_not_drawn), not drawn.
#
# PREGNANCY
# =========
# Not applied (unquantified): concentration per dose fell through pregnancy
# in Leutritz 2023 (2-4 women per trimester), and a PBPK model (Scherf-Clavel
# 2025, abstract) predicts the parent rising and nortriptyline falling.
# -----------------------------------------------------------------------------

# Koh 2018, Table 3
AMITRIPTYLINE_CLM        <- 61.5    # L/h, formation, CYP2C19 not poor
AMITRIPTYLINE_CLM_PM     <- 17      # L/h, formation, CYP2C19 poor
AMITRIPTYLINE_CLO        <- 30.6    # L/h, other pathways, CYP2D6 normal
AMITRIPTYLINE_CLO_LOW    <- 18.8    # L/h, other pathways, CYP2D6 intermediate/poor
AMITRIPTYLINE_VC         <- 155     # L
AMITRIPTYLINE_Q          <- 109     # L/h
AMITRIPTYLINE_VP         <- 1670    # L
AMITRIPTYLINE_KA         <- 0.125   # 1/h
AMITRIPTYLINE_LAG        <- 0.414   # h
AMITRIPTYLINE_D1         <- 1.13    # h, zero-order release into the depot
AMITRIPTYLINE_CL_NOR_KOH <- 58.2    # L/h, Koh's nortriptyline clearance
AMITRIPTYLINE_MW         <- 277.40  # base, C20H23N
AMITRIPTYLINE_HCL_MW     <- 313.86  # hydrochloride
NORTRIPTYLINE_MW         <- 263.38  # base, C19H21N

#' Amitriptyline (Koh 2018), forming nortriptyline
#'
#' Two compartments, apparent oral, CYP2C19 on the formation of
#' nortriptyline and CYP2D6 on the other pathways; see the header of
#' \code{R/drugs_amitriptyline.R}.
#'
#' @inheritParams cefazolin
#' @param cyp2c19 CYP2C19 phenotype, one of \code{CYP2C19_VALUES}
#' @param cyp2d6 CYP2D6 phenotype, one of \code{CYP2D6_VALUES}
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
amitriptyline <- function(weight, height, age, sex, adjustToFFM = TRUE,
                          cyp2c19 = CYP2C19_DEFAULT, cyp2d6 = CYP2D6_DEFAULT)
{
  if (!(length(cyp2c19) == 1 && cyp2c19 %in% CYP2C19_VALUES))
    stop("Invalid cyp2c19 for amitriptyline: ", paste(cyp2c19, collapse = ", "))
  if (!(length(cyp2d6) == 1 && cyp2d6 %in% CYP2D6_VALUES))
    stop("Invalid cyp2d6 for amitriptyline: ", paste(cyp2d6, collapse = ", "))
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  clm <- if (cyp2c19 == "poor") AMITRIPTYLINE_CLM_PM else AMITRIPTYLINE_CLM
  clo <- if (cyp2d6 %in% c(CYP2D6_INTERMEDIATE, CYP2D6_POOR))
    AMITRIPTYLINE_CLO_LOW else AMITRIPTYLINE_CLO
  cl <- (clm + clo) / 60 * size$clearance
  v1 <- AMITRIPTYLINE_VC * size$volume

  default <- list(
    v1 = v1,
    v2 = AMITRIPTYLINE_VP * size$volume,
    v3 = 1,
    cl1 = cl,
    cl2 = AMITRIPTYLINE_Q / 60 * size$clearance,
    cl3 = 0,
    ka_PO = AMITRIPTYLINE_KA / 60,
    # the hydrochloride as entered, converted to base (parameters apparent)
    bioavailability_PO = AMITRIPTYLINE_MW / AMITRIPTYLINE_HCL_MW,
    tlag_PO = 0                        # the lag is carried by the pulses
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # Formation read from the library's nortriptyline model for this patient:
  # Koh's identified ratio CL_M / CL_NOR times the library's clearance.
  clNor <- nortriptyline(weight, height, age, sex,
                         adjustToFFM = adjustToFFM)$PK$default$cl1        # L/min
  formation <- clm / AMITRIPTYLINE_CL_NOR_KOH * clNor                    # L/min

  # The zero-order release over D1 after the lag, as four quarter doses at
  # the midpoints of its quarters
  pulseDelay <- (AMITRIPTYLINE_LAG + AMITRIPTYLINE_D1 * (seq_len(4) - 0.5) / 4) * 60

  reference <- paste0(
    "Koh A et al., J Clin Pharmacol 2019;59:532-540: two compartments, ",
    "apparent oral, CL/F ", sprintf("%.1f", clm + clo), " L/h (formation ",
    clm, " for CYP2C19 ", cyp2c19, ", other ", clo, " for CYP2D6 ", cyp2d6,
    "), Vc 155 L, Q 109 L/h, Vp 1670 L, ka 0.125 /h after a 0.414 h lag and a ",
    "1.13 h release; doses as the hydrochloride (x 0.884); nortriptyline ",
    "formed in Koh's ratio to the library's nortriptyline clearance. ",
    "https://doi.org/10.1002/jcph.1344"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    prodrug = FALSE,
    oralPulses = stats::setNames(
      list(list(fraction = rep(0.25, 4), delay = pulseDelay)),
      ORAL_PULSE_DEFAULT_FORM),
    metabolite = list(
      name              = "nortriptyline",
      kFormation        = formation / v1,
      firstPassFraction = 0,
      mwRatio           = NORTRIPTYLINE_MW / AMITRIPTYLINE_MW
    )
  )
}
