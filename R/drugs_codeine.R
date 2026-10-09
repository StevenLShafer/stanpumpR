# -----------------------------------------------------------------------------
# Codeine: a prodrug whose analgesia is its metabolite's
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# Codeine has negligible affinity for the mu receptor.  Its analgesia is
# morphine's, formed by CYP2D6 O-demethylation, so codeine carries NO effect
# site of its own: tPeak is zero, getDrugPK leaves ke0 at zero, and the plotted
# codeine row is plasma only.  The effect appears on the morphine row, which
# receives the formed metabolite.
#
# WHAT IS ANCHORED AND WHAT IS CALIBRATED
# =======================================
#
# Disposition -- ONE COMPARTMENT, and deliberately so.
# ----------------------------------------------------
# Persson 1992 gave intravenous codeine and reports total plasma clearance of
# 10.8 mL/min/kg.  Guay 1988 gave intravenous codeine and reports a mean
# residence time of 3.90 h.  Together those fix clearance and steady-state
# volume, which is all the published literature identifies.
#
# A second compartment is indicated -- Guay's terminal half-life of 4.04 h
# exceeds the 2.70 h a one-compartment model with that MRT predicts -- but it
# is NOT identifiable from the published summaries.  Clearance, Vss and a
# terminal slope give three equations for the three unknowns V1, V2 and Q only
# if the distribution half-life is also known, and it is not reported.  Rather
# than invent a central/peripheral split, this model uses one compartment,
# which reproduces the correct AUC, the correct mean residence time and the
# correct oral peak, and understates the terminal half-life.  Codeine is given
# orally in essentially all real use, where absorption governs the early curve
# and the distribution phase is largely invisible.
#
# Resolving this needs individual concentration-time data after intravenous
# codeine, not another summary table.
#
# Absorption -- first order.
# --------------------------
# ka is set so the plasma peak falls at 60 min, matching Chen 1991 (0.97 h) and
# Shah 1990 (1.2 h).  Ashraf 2024 fitted ka = 8.74 /h, which with this
# disposition would peak near 24 min, far earlier than observed; that estimate
# belongs to their own one-compartment morphine model and does not transfer.
#
# An inverse Gaussian absorption density is the better lumped description of
# tablet disintegration, dissolution and transit, and stanpumpR has a dormant
# implementation of it.  It is not used here: the available codeine data do not
# establish that it predicts better than first order, and activating it is its
# own piece of work.
#
# Bioavailability is 0.5, from Spahn 1985 (59%) and Hull 1982 (50-55% relative
# to intramuscular).  Persson found 12-84% between subjects, so this is a
# central value for a genuinely wide distribution.
#
# Morphine formation -- RESCALED, and this is the subtle part.
# ------------------------------------------------------------
# Ashraf 2024 is the best phenotype evidence, but its formation estimates were
# fitted alongside a morphine clearance of 357.5 L/h.  stanpumpR's morphine is
# Lotsch Model A, whose clearance is 75.3 L/h.  For a formed metabolite only
# the RATIO of formation to clearance is identifiable, so pairing Ashraf's
# fraction with Lotsch's clearance unchanged would overpredict morphine by
# 357.5/75.3, a factor of 4.75.
#
# The fraction is therefore rescaled by that ratio:
#
#     f_normal = (0.16/1.16) * 75.3/357.5 = 0.02905
#
# where 0.16/1.16 is Ashraf's activity-score expression at a score of 2, taking
# the supplement's sign convention over the main text's.  The rescaled value
# predicts a morphine:codeine AUC ratio of 0.018 after intravenous codeine,
# against observed oral ratios of 0.027 (Shah 1990) and 0.020 (Yue 1991).
# Unrescaled it would predict 0.130.
#
# Phenotype weights come from Ashraf's group medians, transformed to odds and
# normalised to the normal metaboliser.  They are an algebraic transformation
# of published group summaries, not published clearance ratios.
#
# First pass -- small, bounded by the observed peak time, and the weakest
# number in this file.
# -----------------------------------------------------------------------
# Hull 1982 found a higher morphine:codeine ratio after oral than after
# intramuscular codeine, so some morphine is formed before the parent reaches
# the systemic circulation.  Its size is not separately identifiable from oral
# data, which constrain only the sum of the two routes.
#
# What does constrain it is the TIME of the morphine peak.  First-pass
# metabolite arrives with the absorption kernel, so it peaks early; systemic
# metabolite peaks near 2 h.  Lafolie 1996 reports every measured compound,
# morphine included, peaking 1-2 h after oral codeine.  Raising the first-pass
# fraction above about 0.002 of the dose makes the simulated morphine curve
# bimodal and moves its peak to 20 min, which no study reports.  The value
# here is the top of the range that keeps the peak inside the observed window,
# and it is scaled by the same phenotype weights because both routes are
# CYP2D6-mediated.
#
# HOW WELL THIS MATCHES, AND WHERE IT DOES NOT
# ============================================
# For 60 mg of oral codeine in a 70 kg normal metaboliser the model gives a
# codeine peak of 131 ng/mL at 60 min and a morphine peak of 1.4 ng/mL near
# 110 min.  Shah 1990 observed 88 ng/mL and 2.7 ng/mL after 60 mg of codeine
# phosphate, about 45 mg of base.
#
# The morphine:codeine AUC ratio comes to 0.014 over the first six hours,
# against 0.027 observed by Shah 1990 and 0.020 by Yue 1991.  The model
# therefore runs roughly 30-50% below observed morphine.  Matching the observed
# ratio exactly would need a formation fraction near 0.038 rather than the
# 0.029 that Ashraf implies once rescaled.  That is a 32% disagreement between
# two independent routes to the same quantity, which is unremarkable for this
# kind of cross-study reconciliation, and it is left standing rather than tuned
# away: the Ashraf value is traceable and phenotype-resolved, and the
# observations it disagrees with are two studies of eight and ten subjects
# whose AUCs are truncated at six hours.
#
# A separate caution for anyone comparing with urinary recovery: Chen 1991
# recovered 7.1% of an oral dose as total morphine, far above the ~1.5% of the
# dose that becomes systemic morphine here.  The gap is most plausibly morphine
# formed presystemically and glucuronidated without ever circulating as
# morphine, which would also explain the late peak, but it is unresolved.
#
# NOT MODELLED
# ============
# Codeine-6-glucuronide, morphine-3-glucuronide, norcodeine and
# morphine-6-glucuronide are omitted.  The first three carry no assigned human
# analgesic potency, so they cannot affect anything stanpumpR displays.
# Morphine-6-glucuronide IS active, and is the obvious next addition, but it
# needs a two-stage cascade (codeine to morphine to M6G) and a drug row of its
# own.
#
# References
# ----------
# Persson K et al., Eur J Clin Pharmacol 1992;42(6):663-666.
#   https://doi.org/10.1007/BF00265933
# Guay DR et al., Clin Pharmacol Ther 1988;43(1):63-71.
#   https://doi.org/10.1038/clpt.1988.12
# Chen ZR et al., Br J Clin Pharmacol 1991;31(4):381-390.
#   https://doi.org/10.1111/j.1365-2125.1991.tb05550.x
# Yue QY et al., Br J Clin Pharmacol 1991;31(6):635-642.
#   https://doi.org/10.1111/j.1365-2125.1991.tb05585.x
# Shah JC, Mason WD, J Clin Pharmacol 1990;30(8):764-766.
#   https://doi.org/10.1002/j.1552-4604.1990.tb03641.x
# Hull JH et al., Drug Intell Clin Pharm 1982;16(11):849-854.
#   https://doi.org/10.1177/106002808201601107
# Spahn H et al., Arzneimittelforschung 1985;35(6):973-976. PMID 4026924.
# Ashraf MW et al., Clin Pharmacokinet 2024;63:1547-1560.
#   https://doi.org/10.1007/s40262-024-01433-9
# Lotsch J et al., Clin Pharmacol Ther 2002;72(2):151-162.
#   https://doi.org/10.1067/mcp.2002.126172
# -----------------------------------------------------------------------------


# Relative CYP2D6 formation activity, normal metaboliser = 1.  Derived from the
# median apparent formation fractions in Ashraf 2024 section 3.3 (0.55, 6.82,
# 13.8 and 19.9 per cent) by converting each to odds and dividing by the normal
# group's.  See tests/testthat/test-drugs-codeine.R, which recomputes them.
CODEINE_CYP2D6_WEIGHT <- c(
  poor         = 0.0345450703507,
  intermediate = 0.4571827629864,
  normal       = 1.0,
  ultrarapid   = 1.5518464238542
)

#' Codeine pharmacokinetics
#'
#' @param weight weight in kg
#' @param height height in cm (not used)
#' @param age age in years (not used)
#' @param sex sex as a string (not used)
#' @param cyp2d6 CYP2D6 metaboliser phenotype, one of \code{CYP2D6_VALUES}.
#'   Scales morphine formation by both the systemic and the first-pass route.
#'
#' @returns a list in the shape \code{getDrugPK()} expects, carrying an
#'   additional \code{metabolite} element that names morphine as the formed
#'   active species
#' @export
codeine <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT,
                    adjustToFFM = TRUE)
{
  if (length(cyp2d6) != 1 || !cyp2d6 %in% CYP2D6_VALUES) {
    stop("Invalid cyp2d6: ", paste(cyp2d6, collapse = ", "),
         ". Must be one of: ", paste(CYP2D6_VALUES, collapse = ", "))
  }
  activity <- unname(CODEINE_CYP2D6_WEIGHT[[cyp2d6]])

  # --- Disposition, one compartment (see header) ---
  CL_PER_KG <- 10.8 / 1000 * 60   # Persson 1992, 10.8 mL/min/kg -> L/h/kg
  MRT       <- 3.90               # Guay 1988, hours

  # Size scaling (see docs/weight-adjustment.md): the published parameters
  # describe a 70 kg adult.  Volumes scale with fat-free mass relative to the
  # 70 kg, 170 cm reference male, clearances with that ratio ^ 0.75
  # (Al-Sallami 2015).  adjustToFFM = FALSE reproduces the former behaviour
  # exactly: clearance and volume both linear in weight
  # (per-kilogram sources), so both scaled with weight/70.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM)
  clTotal <- CL_PER_KG / 60 * 70 * size$clearance   # L/min
  v1      <- CL_PER_KG * MRT * 70 * size$volume     # L, = Vss * size at 70 kg

  # Formation clearance is a branch of total clearance, so changing the
  # phenotype changes the total as well.  The CYP2D6 branch is only about 3% of
  # codeine clearance, so the spread across phenotypes is about 4%, consistent
  # with Yue 1991 and Chen 1991 finding no significant difference in total
  # codeine clearance between extensive and poor metabolisers.
  FORMATION_FRACTION_NORMAL <- (0.16 / 1.16) * 75.3 / 357.5   # = 0.0290523
  clFormation <- clTotal * FORMATION_FRACTION_NORMAL * activity
  clOther     <- clTotal * (1 - FORMATION_FRACTION_NORMAL)
  cl1         <- clOther + clFormation

  # One compartment: no peripheral exchange.  getDrugPK's cube() reduces to the
  # single root k10 when k21 and k31 are both zero.
  v2  <- 1
  v3  <- 1
  cl2 <- 0
  cl3 <- 0

  # --- Absorption ---
  # Chosen so the plasma peak falls at 60 min with the disposition above.
  ka_PO              <- 0.0425953551   # 1/min, = 2.556 /h
  bioavailability_PO <- 0.5
  tlag_PO            <- 0

  # --- Morphine formation ---
  # kFormation is a first-order transfer out of the central compartment, so it
  # is the formation clearance over the central volume.  With both scaled on
  # total weight it was weight-independent; with the fat-free-mass scaling it
  # falls as the size ratio ^ -0.25, like every other k10-type constant.
  kFormation <- clFormation / v1

  # The largest first-pass fraction that keeps the simulated morphine peak
  # inside the 1-2 h window Lafolie 1996 observed.  Above about 0.002 the curve
  # becomes bimodal and peaks at 20 min.  Pinned by test-drugs-codeine.R.
  FIRST_PASS_NORMAL <- 0.0015
  firstPassFraction <- FIRST_PASS_NORMAL * activity

  tPeak <- 0        # prodrug: no effect site of its own
  MEAC  <- 0        # and therefore no minimum effective concentration

  # Display band, in ng/mL: plasma concentrations seen after ordinary
  # therapeutic oral doses (Kim 2002 peaked at 214 ng/mL after 60 mg).
  typical      <- 100
  upperTypical <- 150
  lowerTypical <- 50

  reference <- paste0(
    "Persson K et al., Eur J Clin Pharmacol 1992;42(6):663-666. ",
    "https://pubmed.ncbi.nlm.nih.gov/1623909/ (disposition); ",
    "Ashraf MW et al., Clin Pharmacokinet 2024;63:1547-1560. ",
    "https://pubmed.ncbi.nlm.nih.gov/39300028/ (CYP2D6), rescaled to the ",
    "Lotsch morphine model"
  )

  default <- list(
    v1 = v1,
    v2 = v2,
    v3 = v3,
    cl1 = cl1,
    cl2 = cl2,
    cl3 = cl3,
    ka_PO = ka_PO,
    bioavailability_PO = bioavailability_PO,
    tlag_PO = tlag_PO
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  return(
    list(
      PK = PK,
      tPeak = tPeak,
      MEAC = MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference,
      metabolite = list(
        name              = "morphine",
        kFormation        = kFormation,
        firstPassFraction = firstPassFraction,
        # Formation is molar but concentrations are reported by mass.
        # Morphine 285.34 g/mol, codeine 299.36 g/mol.
        mwRatio           = 285.34 / 299.36
      )
    )
  )
}
