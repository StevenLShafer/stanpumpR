# -----------------------------------------------------------------------------
# Oxymorphone: active in its own right, and oxycodone's active metabolite
# -----------------------------------------------------------------------------
# Units: time in minutes, volumes in litres, clearances in L/min.
#
# ONE COMPARTMENT, BECAUSE NO HUMAN CENTRAL VOLUME EXISTS
# =======================================================
# The only human intravenous oxymorphone disposition information is the
# manufacturer's summary carried in the Physicians' Desk Reference: clearance
# 2.0 +/- 0.5 L/min, steady-state volume 3.08 +/- 1.14 L/kg, terminal
# half-life 1.3 +/- 0.7 h.  It gives no central volume, no peripheral volume,
# no exchange clearance and no sample size.  Searching for a population
# analysis that supplies them found only animal studies.
#
# A two-compartment model therefore cannot be built: the central/peripheral
# split is simply not in the literature.  One compartment with the volume set
# to the reported steady-state volume reproduces clearance exactly, mean
# residence time exactly, and a terminal half-life of 1.245 h against the
# reported 1.3 h.  That agreement is good enough that oxymorphone behaves
# nearly mono-exponentially over the window these summaries describe.
#
# What it gets wrong is the first few minutes after an intravenous bolus,
# where a real central volume smaller than 216 L would give a much higher
# early peak.  Treat early intravenous predictions with suspicion.  Resolving
# this needs individual concentration-time data, not another summary.
#
# A cross-check worth knowing: 10 mg of the hydrochloride is 8.92 mg of base,
# and with the observed oral AUC of 9.10 ng.h/mL that implies apparent
# clearance near 980 L/h, so with bioavailability 0.10 the implied clearance
# is about 98 L/h rather than the 120 L/h anchor.  The two disagree by a
# fifth.  The intravenous anchor is kept, and the model's oral exposure comes
# out about 18% below the observed mean as a result, inside one standard
# deviation of it.
#
# ABSORPTION AND BIOAVAILABILITY
# ==============================
# Bioavailability is 0.10 from the product labelling.  A crossover abstract
# reported 3.8 to 7.2%, with poor area estimation at low concentrations, so
# 0.10 should be read as a round scenario rather than an exact figure.
#
# ka is set so the predicted peak matches the 1.93 ng/mL Adams and Ahdieh
# measured after 10 mg of immediate-release tablet.  The peak then falls at
# 1.4 h, which is a consequence of that calibration rather than a fitted time;
# no time to peak was available in the sources consulted.  Extended-release
# oxymorphone is a different input and is not represented.
#
# AS A METABOLITE OF OXYCODONE
# ============================
# Oxycodone names this drug as its metabolite, so a patient given oxycodone
# gets an oxymorphone row whether or not oxymorphone itself was given, and a
# patient given both sees the sum.  The formation constant lives in
# R/drugs_oxycodone.R, calibrated against the observed plasma ratio.
#
# Oxymorphone's own metabolites, the 3-glucuronide and 6-hydroxyoxymorphone,
# are not modelled.  The 6-hydroxy compound has animal analgesic activity and
# no human potency estimate; the glucuronide's activity has never been
# evaluated, so it must not be called inactive either.
#
# References
# ----------
# Endo, NUMORPHAN prescribing information, Physicians' Desk Reference 54th
#   edition, 2000.  Manufacturer summary, not a population analysis.
# Adams MP, Ahdieh H, Drugs R D 2005;6:91-99.
#   https://doi.org/10.2165/00126839-200506020-00004
# Adams MP, Ahdieh H, Pharmacotherapy 2004;24:468-476.
#   https://doi.org/10.1592/phco.24.5.468.33347
# Lalovic B et al., Clin Pharmacol Ther 2006;79:461-479.
#   https://doi.org/10.1016/j.clpt.2006.01.009
# Agema BC et al., Cancers 2021;13:2768.
#   https://doi.org/10.3390/cancers13112768
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# TO BE SUPPLIED
# -----------------------------------------------------------------------------
# No human concentration-effect vector for oxymorphone was found.  Babalonis
# 2016 measured experimental effects after oral immediate-release oxymorphone
# but reports no potency, and equal-milligram comparisons cannot supply one.
#
# Until these are set, oxymorphone is plotted as plasma only and contributes
# nothing to the opioid MEAC total, including when it arrives as oxycodone's
# metabolite.  Oxymorphone binds the mu receptor 8 to 44 times more tightly
# than oxycodone and is thought to carry 15 to 20% of oral oxycodone's
# analgesia, so leaving it at zero understates a real effect; those figures
# are affinity and attribution rather than an in-vivo potency, which is why
# they are quoted here and not used.
#
# Setting OXYMORPHONE_MEAC here also requires updating the MEAC column of
# inst/extdata/drugDefaults_global.csv, which is what the plot and the opioid
# total actually read.  test-drugs-oxymorphone.R checks that the two agree.

# tPeak: PROVISIONAL, AND NEEDS A LITERATURE REFERENCE AND VALIDATION
# -------------------------------------------------------------------
# Set by Steven L. Shafer on 2026-10-06.  No citation is attached to it yet,
# and none was found in the search behind this file.
#
# tPeak is the time to peak EFFECT SITE concentration after an intravenous
# bolus, which getDrugPK() back-solves into ke0.  Oxymorphone can be given
# intravenously, so unlike hydrocodone the quantity is directly observable in
# principle; what is missing is a study that observed it.  Babalonis 2016
# measured experimental effects after ORAL immediate-release oxymorphone,
# where absorption dominates the onset and cannot be separated from
# equilibration without modelling both.
#
# A caution on the disposition this sits on: the one-compartment reduction
# below has no distribution phase, so the early plasma curve after a bolus is
# wrong in exactly the window ke0 is most sensitive to.  A tPeak validated
# against a real two-compartment oxymorphone model would not transfer to this
# one unchanged.
OXYMORPHONE_TPEAK <- 20  # minutes; provisional, see above

# MEAC: PROVISIONAL, SET TO ONE TENTH OF MORPHINE'S, AND NEEDS LITERATURE
# EVALUATION
# -----------------------------------------------------------------------
# Set by Steven L. Shafer on 2026-10-06, taking oxymorphone as ten times as
# potent as morphine.  Morphine's is 0.008 in its own row, which is mcg/mL
# because morphine is reported in mcg/mL, so 8 ng/mL; a tenth of that is 0.8,
# and oxymorphone is reported in ng/mL, so the value here is 0.8 rather than
# 0.0008.  Check the unit before comparing the two rows.
#
# What would evaluate or replace it:
#   - A human concentration-effect study for oxymorphone.  None was found.
#   - Tenfold potency relative to morphine is a received equianalgesic ratio,
#     and an equianalgesic DOSE ratio is not a concentration ratio: it folds
#     in bioavailability, clearance and distribution, all of which differ
#     between the two drugs.  Oxymorphone's receptor affinity relative to
#     oxycodone is reported as 8 to 44 fold, which is a different comparison
#     again and should not be read as support for this number.
#   - Oxymorphone also arrives as oxycodone's metabolite, so this value now
#     feeds the opioid total whenever oxycodone is given.  At about 2% of
#     oxycodone concentrations and a tenth of its MEAC, the formed
#     contribution is small but no longer zero, which is worth confirming
#     against the 15 to 20% of oral oxycodone analgesia sometimes attributed
#     to oxymorphone.
OXYMORPHONE_MEAC  <- 0.8  # ng/mL; provisional, see above
# -----------------------------------------------------------------------------


#' Oxymorphone pharmacokinetics
#'
#' @param weight weight in kg
#' @param height height in cm (not used)
#' @param age age in years (not used)
#' @param sex sex as a string (not used)
#'
#' @returns a list in the shape \code{getDrugPK()} expects
#' @export
oxymorphone <- function(weight, height, age, sex)
{
  # --- Disposition, one compartment (see header) ---
  VSS_PER_KG <- 3.08          # L/kg, manufacturer summary
  CL_PER_KG  <- 120 / 70      # L/h/kg, 2.0 L/min at the 70 kg reference the
                              # summary's volume-per-kg implies

  v1  <- VSS_PER_KG * weight
  v2  <- 1                    # unused
  v3  <- 1                    # unused
  cl1 <- CL_PER_KG / 60 * weight
  cl2 <- 0
  cl3 <- 0

  # --- Absorption, calibrated to the observed immediate-release peak ---
  ka_PO              <- 0.0155970777   # 1/min, = 0.9358 /h
  bioavailability_PO <- 0.10
  tlag_PO            <- 0

  reference <- paste0(
    "Endo, NUMORPHAN prescribing information, Physicians' Desk Reference ",
    "54th ed., 2000 (intravenous clearance and steady-state volume); ",
    "Adams MP, Ahdieh H, Drugs R D 2005;6:91-99. ",
    "https://pubmed.ncbi.nlm.nih.gov/15777102/ (oral)"
  )

  # Display band, ng/mL: 10 mg immediate release peaked at 1.93 ng/mL.
  typical      <- 2
  upperTypical <- 4
  lowerTypical <- 1

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
      tPeak = OXYMORPHONE_TPEAK,
      MEAC = OXYMORPHONE_MEAC,
      typical = typical,
      upperTypical = upperTypical,
      lowerTypical = lowerTypical,
      reference = reference
    )
  )
}
