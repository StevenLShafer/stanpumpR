# Intravenous calcium: calcium chloride, and calcium gluconate in
# R/drugs_calciumGluconate.R, which shares everything here.
#
# What stanpumpR plots for either salt is not the concentration of the salt
# but the CHANGE in venous serum ionized calcium, in mmol/L, above the
# patient's own baseline.  That is the quantity Ansari and colleagues
# modelled: the measured ionized calcium at each time minus the patient's
# value before the infusion.  The baseline itself (about 1.15 mmol/L in their
# parturients) is not part of the model and is not plotted, so the panel
# starts at zero and the curve is what the dose adds.  Total serum calcium,
# which is mostly albumin bound, is not represented at all.
#
# Source.  Ansari JR, Conti DJ, Michel G, Yarmosh A, Cole NM, Shafer SL.
# Bioequivalence and pharmacokinetics of intravenous calcium during cesarean
# delivery.  Anesthesiology 2025;142:121-31.  A two-compartment model fitted
# by NONMEM to 311 ionized calcium measurements from 70 parturients who
# received 0.5 or 1 g of calcium chloride or 1.5, 2 or 3 g of calcium
# gluconate by 10-minute infusion after umbilical cord clamping at cesarean
# delivery (three trials: a bioequivalence trial, NCT05973747, and two
# placebo-controlled trials of calcium chloride for uterine atony,
# NCT03867383 and NCT05027048).  Sampling ran to 60 to 90 minutes.  Table 2:
#
#   systemic clearance       CL1  0.183 L/min   (95% CI 0.067 to 0.266)
#   central volume           V1   10.9  L       (9.27 to 12.6)
#   intercompartmental CL    CL2  1.25  L/min   (1.03 to 1.56)
#   peripheral volume        V2   16.5  L       (12.5 to 24.7)
#   dose ratio, gluconate to chloride  3.11     (2.77 to 3.48)
#
# The model is written in grams of calcium chloride dihydrate (CaCl2.2H2O,
# 147 g/mol): the authors' note to Table 2 says to divide a calcium
# gluconate dose by 3.11 to get the equivalent calcium chloride dose, and to
# multiply a predicted concentration in g/L by 1000/147 to get mmol/L.
# stanpumpR keeps the parameters as published and does both conversions on
# the dose instead (Concentration.Units "mmol" in the CSV and the `molar`
# block below): a dose in grams of the product becomes millimoles of calcium
# chloride equivalent, and dividing by the volumes in litres gives mmol/L
# directly.  Because the dose is in millimoles, the plotted mmol/L is the
# ionized calcium the dose adds, whichever salt was given.
#
# Population.  All 70 patients were pregnant women, 18 to 55 years, at or
# after 34 weeks' gestation, 55 to 100 kg (mean 79.8 kg, BMI 30.6 kg/m2,
# mean age 34), with a serum creatinine of 1.0 mg/dL or less, no cardiac
# disease, no digoxin or calcium channel blocker, no magnesium infusion, and
# they were bleeding (median blood loss about 800 mL) while the samples were
# drawn.  Maternal age, weight, blood loss and venous pH were tested as
# covariates and none improved the fit.  No men, children, elderly patients
# or patients with renal insufficiency were studied: the model is offered for
# every patient the app accepts, as the only pharmacokinetic description of
# intravenous calcium in adults, and every patient outside that population is
# an extrapolation (see the help page).  The authors note that their findings
# agree with the calcium kinetics Cote and colleagues (Anesthesiology
# 1987;66:465-70) reported in burned children and dogs.
#
# Elimination.  The slower half-life from these parameters is 109 minutes,
# but sampling stopped at 60 to 90 minutes, and the authors say that longer
# sampling would almost certainly have shown a slower terminal phase and a
# smaller clearance: calcium exchanges with bone and is regulated by
# parathyroid hormone, which this model does not contain.  Trust the first
# hour or two; beyond that the curve returns to zero faster than it should.
#
# Size scaling (see docs/weight-adjustment.md).  The source has no size
# covariate and found none, over 55 to 100 kg.  The published parameters are
# taken to describe the 70 kg reference adult and are scaled to fat-free mass
# in the library's usual way, so that a child or a very large patient gets a
# proportionate volume; this is the library's assumption, not the paper's.
# With the switch off the published values apply unscaled to everyone, which
# is how the paper reads: a typical study parturient (80 kg, 34 years,
# female) with the switch on has volumes 13% smaller than published and peaks
# about 15% higher than the paper's figure 5.
#
# Drafted by Claude Code, 2026-10-13, at the request of Steven L. Shafer.

# Grams of the product as dosed per mole of calcium chloride dihydrate
# equivalent, the scale the model is written in.
CALCIUM_CHLORIDE_DIHYDRATE_MW <- 147.01   # CaCl2.2H2O, g/mol (the paper rounds to 147)
# Fitted dose ratio: grams of calcium gluconate that raise ionized calcium as
# much as 1 g of calcium chloride (Ansari 2025, Table 2).  Stoichiometry gives
# 448.4 / 147.0 = 3.05 (calcium gluconate monohydrate over calcium chloride
# dihydrate); the fitted 3.11 is used as published, 2% apart.
CALCIUM_GLUCONATE_DOSE_RATIO <- 3.11

# The disposition both salts share.  Returns the list a drug model returns,
# without the `molar` block that names the salt.
calciumIonizedPK <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units **************
  # Time: Minutes
  # Volume: Liters
  # Amount: mmol of calcium chloride dihydrate equivalent, so Cp is the
  # change in ionized calcium in mmol/L

  v1Ref  <- 10.9
  v2Ref  <- 16.5
  cl1Ref <- 0.183
  cl2Ref <- 1.25

  # Size scaling: published parameters for the reference adult, scaled to
  # fat-free mass; legacyVolume = 1 because the source does not scale at all.
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1  = v1Ref  * size$volume,
    v2  = v2Ref  * size$volume,
    v3  = 1,                          # placeholder: a two-compartment model
    cl1 = cl1Ref * size$clearance,
    cl2 = cl2Ref * size$clearance,
    cl3 = 0
  )

  events <- c(PK_EVENT_DEFAULT)
  PK <- sapply(events, function(x) list(get0(x)))

  # No effect site: the ionized calcium itself is the end point, and no
  # concentration-effect relationship (on uterine tone, contractility or
  # the ECG) was modelled.  Plasma only.
  tPeak <- 0
  MEAC  <- 0

  # No band: the panel is a change above an unknown baseline, so no target
  # range applies.  The CSV's band columns are zero as well.
  typical      <- 0
  upperTypical <- 0
  lowerTypical <- 0

  reference <- paste(
    "Ansari JR, Conti DJ, Michel G, Yarmosh A, Cole NM, Shafer SL.",
    "Bioequivalence and pharmacokinetics of intravenous calcium during cesarean delivery.",
    "Anesthesiology 2025;142:121-31 (parturients at cesarean delivery; plotted as the",
    "change in ionized calcium, mmol/L). https://doi.org/10.1097/ALN.0000000000005248"
  )

  list(
    PK = PK,
    tPeak = tPeak,
    MEAC = MEAC,
    typical = typical,
    upperTypical = upperTypical,
    lowerTypical = lowerTypical,
    reference = reference
  )
}

calciumChloride <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  out <- calciumIonizedPK(weight, height, age, sex, adjustToFFM)
  # 1 g of calcium chloride dihydrate is 1000 / 147.01 = 6.80 mmol of calcium.
  out$molar <- list(molecularWeight = CALCIUM_CHLORIDE_DIHYDRATE_MW)
  out
}
