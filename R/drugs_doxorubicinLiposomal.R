# -----------------------------------------------------------------------------
# Doxorubicin hydrochloride liposome injection (pegylated liposomal
# doxorubicin, Doxil/Caelyx), intravenous (Hsu 2018): liposome-encapsulated
# doxorubicin, releasing free doxorubicin (doxorubicinLiposomalFree)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min; amounts in
# mg and concentrations in mcg/mL (= mg/L); free doxorubicin in ng/mL.
#
# Drafted by Claude Code, 2026-11-16.  Steven L. Shafer decided (2026-11-16)
# that liposomal doxorubicin be built with free doxorubicin as a model of its
# own, not the library's doxorubicin.  Every number below was read on the
# pages of the PDF.
#
# THE SOURCE
# ==========
# Hsu LF.  Investigation of the discriminatory ability of pharmacokinetic
# metrics for the bioequivalence assessment of PEGylated liposomal
# doxorubicin.  Pharm Res 2018;35:106 (doi 10.1007/s11095-018-2387-4).  A
# randomized two-way crossover bioequivalence study from a dossier submitted
# to the Taiwan regulatory authority: women with ovarian cancer (number not
# printed), the innovator product (Doxil/Caelyx) 50 mg/m2 over 1 h, samples to
# 336 h; encapsulated and free doxorubicin fitted together (NONMEM, ADVAN6,
# FOCE-I).
#
# THE MODEL (Table I)
# ===================
# Encapsulated doxorubicin: one compartment, V1 2.52 L, emptied only by the
# release of free doxorubicin, Rel 0.0212 L/h (uptake by the
# reticuloendothelial system, CLres, estimated near 0 and fixed there); a
# release half-life of 82 h.  Encapsulation E 98% (fixed, from the
# literature): 2% of the dose enters free doxorubicin directly.  Free
# doxorubicin: two compartments, V2 9.52 L, V3 35.1 L, Q 2.87 L/h, CLfree
# 2.68 L/h (R/drugs_doxorubicinLiposomalFree.R).  No covariates.
#
# IN THIS LIBRARY'S INTERFACE: free doxorubicin is the metabolite, formed from
# the encapsulated amount at the release rate, and the unencapsulated 2% is
# the metabolite block's directFraction (metaboliteCoefficients(), added for
# this drug).  The encapsulated compartment receives the other 98%, which is
# written exactly by reparameterising it: v1 = V1 / 0.98 and cl1 = Rel / 0.98
# (so k10 = Rel / V1 is unchanged and a dose gives 0.98 x dose / V1), and the
# formation rate constant 0.98 x Rel / V1 (so the amount released per minute
# is Rel x the encapsulated concentration, as in the paper).  The help tables
# therefore show a central volume of 2.57 L: the paper's V1 is 2.52 L.
#
# FREE DOXORUBICIN IS NOT THE LIBRARY'S DOXORUBICIN.  CLfree 2.68 L/h is
# about a twentieth of conventional doxorubicin's clearance (Joerger 2007, 108
# L/h for the hydrochloride): in a joint fit of release and elimination only
# their ratio is identifiable, and the "free" analyte here is the released
# pool as the assay measures it.  So the metabolite is a model of its own,
# METABOLITE_FORMATION_BASIS "joint", and conventional doxorubicin's
# parameters are not borrowed (the registry's rule for this product).
#
# CHECKS: test-drugs-doxorubicinLiposomal.R holds both curves to an
# independent integration of Hsu's states (1e-4).  The label (DOXIL,
# 2026-06-03) gives total doxorubicin at 10 and 20 mg/m2: clearance 0.056 and
# 0.041 L/h/m2, half-lives 52 and 55 h, and says the kinetics are nonlinear,
# with lower clearance and a longer half-life at 50 mg/m2, Hsu's dose: Hsu's
# release, 0.0212 L/h (about 0.012 L/h/m2), and its 82-h half-life are in that
# direction.  The curve is therefore a 50 mg/m2 curve; at lower doses it
# underestimates clearance.  Free doxorubicin's area is about 1% of the
# encapsulated, as the paper reports.
#
# DOSE BASIS: mg of doxorubicin hydrochloride, as labelled and as the paper
# doses it; no conversion.
#
# BODY SIZE (docs/weight-adjustment.md): no covariate was reported.  Until
# 2026-11-24 every parameter was fixed, after the precedent of Crombag's
# paclitaxel, which Shafer's first-principles rule of 2026-11-19 overturned
# (*Every parameter is scaled*; Gemini's chemotherapy audit of 2026-11-23,
# F5).  With the switch on, the volumes (encapsulated V1, free V2 and V3) take
# the fat-free-mass volume factor and the release and the free drug's Q and
# clearance the clearance factor, here and in doxorubicinLiposomalFree, the
# fit being joint; with it off they are the published values.
#
# BODY SIZE ANCHOR: at the reference (the 70 kg man), not at the cohort:
# Hsu 2018 prints neither the number nor the size of its women
# (chemotherapyRegistry.csv), so there is no cohort to anchor at.  A typical
# woman therefore receives smaller volumes and clearances than published.
# [decision] for Shafer: whether to anchor at a sex-typical woman in the
# absence of printed demographics, or to keep the reference anchor until the
# paper's (or the NDA's) demographics are read.
#
# PREGNANCY: see its registry row.  NO EFFECT SITE, NO BAND.
# -----------------------------------------------------------------------------

# Hsu 2018, Table I
DOXORUBICIN_LIPOSOMAL_HSU <- list(
  v1 = 2.52,          # L, encapsulated
  rel = 0.0212,       # L/h, release
  encapsulated = 0.98,
  v2 = 9.52, v3 = 35.1, q = 2.87, clFree = 2.68   # free doxorubicin, L and L/h
)

#' Pegylated liposomal doxorubicin, intravenous (Hsu 2018)
#'
#' Encapsulated doxorubicin in one compartment releasing free doxorubicin,
#' carried as the metabolite \code{doxorubicinLiposomalFree}; see the header
#' of \code{R/drugs_doxorubicinLiposomal.R}.
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
doxorubicinLiposomal <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  p <- DOXORUBICIN_LIPOSOMAL_HSU
  e <- p$encapsulated
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM,
                        legacyVolume = 1, legacyClearance = 1)
  v1 <- p$v1 * size$volume
  rel <- p$rel * size$clearance

  default <- list(
    v1  = v1 / e,                            # 98% of the dose: see the header
    v2  = 1,                                 # one compartment
    v3  = 1,
    cl1 = rel / e / 60,                      # k10 = Rel / V1
    cl2 = 0,
    cl3 = 0
  )
  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Hsu LF. Investigation of the discriminatory ability of pharmacokinetic metrics for the ",
    "bioequivalence assessment of PEGylated liposomal doxorubicin. Pharm Res 2018;35:106. ",
    "Women with ovarian cancer, Doxil/Caelyx 50 mg/m2: encapsulated doxorubicin in one ",
    "compartment (V1 2.52 L) releasing free doxorubicin at 0.0212 L/h, 98% encapsulated; ",
    "free doxorubicin two compartments (V 9.52 and 35.1 L, Q 2.87, CL 2.68 L/h); volumes ",
    "and clearances scaled to fat-free mass at the reference anchor (no cohort printed). Liposomal ",
    "product only. https://doi.org/10.1007/s11095-018-2387-4"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference,
    metabolite = list(
      name              = "doxorubicinLiposomalFree",
      kFormation        = e * rel / v1 / 60,       # /min: Rel x encapsulated conc.
      firstPassFraction = 0,
      directFraction    = 1 - e,                    # the unencapsulated 2%
      mwRatio           = 1                         # the paper's amounts, as dosed
    )
  )
}
