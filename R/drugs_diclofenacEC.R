# -----------------------------------------------------------------------------
# Enteric-coated diclofenac: apparent two-compartment oral model (Bartels 2010)
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min,
# concentrations in mcg/mL (= mg/L), total plasma diclofenac.
#
# WHY A SEPARATE DRUG
# ===================
# The library's diclofenac (R/drugs_diclofenac.R, Standing 2011) has a
# systemic disposition and the dispersible tablet; its estimates do not apply
# to the enteric-coated tablet, the commonest adult form.  The only population
# fit of enteric-coated diclofenac found is oral-only, with apparent
# parameters, so it cannot share Standing's disposition without a
# bioavailability bridged across studies.  Like amiodaroneIV, it is offered as
# a drug of its own, every value from the one source.  It is plotted as its
# own line: a dispersible or intravenous dose entered under "diclofenac" is
# not added to it.
#
# SOURCE
# ======
# Bartels C, Armogida M, Hamren B (Novartis).  Population PK model for pooled
# data of different oral diclofenac formulations.  Poster, Population Approach
# Group Europe (PAGE), Athens, 7-10 June 2010.  NONMEM VI, FOCE, log-
# transformed data from healthy subjects with rich sampling: immediate release
# (117 subjects, single dose), mixed release (21, b.i.d. for 5 days), slow
# release (21, single dose) and enteric coated (12, t.i.d. for 5 days), fitted
# together with one apparent two-compartment disposition (Table 2):
#
#     CL/F 40.3 L/h    Vc/F 23.5 L    Q/F 10.6 L/h    Vp/F 21.3 L
#
# F is the unmeasured bioavailability of the immediate-release reference arm.
# Each formulation has a bioavailability RELATIVE to that arm (Frelative,
# Table 3), applied once to the dose.  Enteric coated: Frelative 0.784, all of
# the dose through the lagged fast path (Ffast 1, fixed), kfast 0.503 /h, lag
# 0.932 h.  The fast path also runs through two transition compartments at
# 100 /h each (Figure 2), added to keep NONMEM's derivatives continuous at the
# lag; they are carried here as their mean transit time, 2 / 100 h = 1.2 min,
# added to the lag (0.952 h in all).  No covariate analysis was done.
#
# Checked against the poster PDF on 2026-10-11 (Tables 2-5 and Figure 2).
# The poster does not name the salt or product of each arm, so the dose is
# entered as labelled; enteric-coated diclofenac is usually the sodium salt.
#
# VARIABILITY
# ===========
# Large, and not shown: interindividual variability of CL/F 14.9%,
# interoccasion variability of the absorption rates 143% (Table 4), and a
# residual SD of 96.4% on the log scale for the enteric-coated arm (Table 5),
# against 57.7% for immediate release.  The plotted curve is the typical
# patient's, with the median absorption rate; a given tablet may be absorbed
# much faster or slower, and the poster notes multiple peaks in some
# profiles.
#
# EFFECT SITE
# ===========
# None: tPeak = 0, and the plotted concentration is plasma.  No band and no
# recovery threshold, as for diclofenac.
#
# BODY SIZE (docs/weight-adjustment.md)
# =====================================
# The poster reports no size covariate.  The library's fat-free-mass scaling
# with the switch on; with it off, the published 70 kg values unscaled
# (legacyVolume = 1).
#
# References
# ----------
# Bartels C, Armogida M, Hamren B.  Poster, PAGE, Athens, June 2010.
#   https://www.page-meeting.org/wp-content/uploads/pdf_assets/3455-213_v5_ChristianBartels_final.pdf
# (Claude Code, 2026-10-11, at the request of Steven L. Shafer.)
# -----------------------------------------------------------------------------

DICLOFENAC_EC_F_REL <- 0.784
DICLOFENAC_EC_LAG <- 0.932 + 2 / 100   # h, lag plus the two transit states

#' Enteric-coated diclofenac pharmacokinetics (apparent oral)
#'
#' @inheritParams cefazolin
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
diclofenacEC <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = 23.5 * size$volume,
    v2  = 21.3 * size$volume,
    v3  = 1,                                     # two compartments
    cl1 = 40.3 / 60 * size$clearance,
    cl2 = 10.6 / 60 * size$clearance,
    cl3 = 0,
    # Apparent parameters, scaled to the immediate-release arm: the enteric-
    # coated tablet's relative bioavailability is applied once.
    ka_PO              = 0.503 / 60,             # 1/min
    tlag_PO            = DICLOFENAC_EC_LAG * 60, # min
    bioavailability_PO = DICLOFENAC_EC_F_REL
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  reference <- paste0(
    "Bartels C et al., poster, PAGE, Athens, 2010 (apparent oral ",
    "two-compartment model pooled over four release types; enteric-coated ",
    "tablet, bioavailability 0.784 relative to immediate release). ",
    "https://www.page-meeting.org/wp-content/uploads/pdf_assets/3455-213_v5_ChristianBartels_final.pdf"
  )

  list(
    PK = PK,
    tPeak = 0,
    MEAC = 0,
    typical = 0,
    upperTypical = 0,
    lowerTypical = 0,
    reference = reference
  )
}
