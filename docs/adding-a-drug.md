# Adding a drug to stanpumpR

stanpumpR is designed so that adding a drug is a small, self-contained change — the goal is to
let outside investigators contribute and maintain the pharmacokinetics for individual drugs.
A new drug touches **four** files, including a help page. None of the engine code needs to change.

> Every model must handle body size the stanpumpR way — see
> [weight-adjustment.md](weight-adjustment.md) and step 1 below. This is a requirement, not an option.

> Prerequisite: read the [architecture map](architecture.md) first if you haven't. You only
> need to understand the *drug library* pattern, not the closed-form solver.

## 1. The model — `R/drugs_<name>.R`

Create a file named after the drug (lowercase, matching the CSV `Drug` value). Export one
function `<name>(weight, height, age, sex, adjustToFFM = TRUE)` that returns a list. Keep the
full signature even if the model ignores some arguments.

Minimal example (`alfentanil`, whose published parameters are fixed values for a 70 kg adult):

```r
alfentanil <- function(weight, height, age, sex, adjustToFFM = TRUE)
{
  # Units: time in minutes, volumes in liters

  # Size scaling: the published parameters describe a 70 kg adult and formerly
  # did not scale at all (hence legacyVolume = 1).
  size <- pkSizeFactors(weight, height, age, sex, adjustToFFM, legacyVolume = 1)

  default <- list(
    v1 = 2.1853    * size$volume,  v2 = 6.698864 * size$volume,  v3 = 14.52582 * size$volume,
    cl1 = 0.1988623 * size$clearance, cl2 = 1.433557 * size$clearance, cl3 = 0.2469389 * size$clearance
  )

  events <- c("default")
  PK <- sapply(events, function(x) list(get0(x)))

  tPeak <- 1.4         # minutes to peak effect (drives ke0)
  MEAC  <- 39          # minimum effective analgesic/anesthetic concentration
  typical      <- MEAC * 1.2
  upperTypical <- MEAC * 0.8
  lowerTypical <- MEAC * 2.0
  reference    <- "JPET 1987;240:159-166"

  list(
    PK = PK, tPeak = tPeak, MEAC = MEAC,
    typical = typical, upperTypical = upperTypical, lowerTypical = lowerTypical,
    reference = reference
  )
}
```

Covariate-driven models simply compute `v1..v3` / `cl1..cl3` from the arguments before
building `default` — see `R/drugs_remifentanil.R` (branches on BMI between the Eleveld and Kim
models) or `R/drugs_propofol.R`.

### Body size scaling (required)

Every model must do one of two things, and say which in a comment:

1. **Carry its own size covariate**, as the Eleveld models do (`R/drugs_propofol.R`). Accept
   `adjustToFFM` and ignore it; note in the comment that the model has its own covariate.
2. **Inherit the library's fat-free-mass scaling.** Write the published parameters for the
   70 kg reference adult, call `pkSizeFactors(weight, height, age, sex, adjustToFFM, ...)`, and
   multiply every volume by `size$volume` and every clearance by `size$clearance`. The
   `legacyVolume` / `legacyClearance` arguments say what the model does when the user turns the
   switch off, and must reproduce the published scaling: `legacyVolume = 1` for fixed
   parameters, the default `weight / 70` for a per-kilogram V1 with fixed rate constants, or
   `legacyClearance = (weight / 70)^0.75` for a model published with allometric clearance.

A published model that was fitted on total body weight is **still** scaled to fat-free mass
(remimazolam is an example); only a model with its own fat-free-mass covariate is exempt.
Placeholders for a missing compartment (`v3 = 1`, `cl3 = 0`) are left unscaled.

**A model with its own weight or renal covariate** (vancomycin, gentamicin, sugammadex,
cefazolin, gabapentin, pregabalin) evaluates the published equations at the pharmacokinetic weight with the switch
on (`size$pkWeight`, which is 70 kg × FFM / FFM<sub>ref</sub>) and at total body weight with it
off, and scales any size-free parameter by the library factors. Write that choice out:
`if (isTRUE(adjustToFFM)) size$pkWeight else weight`, as `R/drugs_cefazolin.R` does. Do not
derive the weight from `70 * size$volume` unless the model's `legacyVolume` is `weight / 70`;
with `legacyVolume = 1` that expression is 70 kg for everyone when the switch is off. Renal
function comes from `R/renalFunction.R` (`creatinineClearanceCG()`, `egfrDeindexed()`).
Add `creatinine = NULL` to the model's signature and pass
`adultEquivalentCreatinine(creatinine, age, sex)` as the creatinine: that is the patient's
serum creatinine from the Patient Profile, or an **assumed normal creatinine** for the
patient's age and sex when the field is blank, and for a child it is put on the adult scale
(divided by the normal for age, times the adult value), because Cockcroft-Gault and CKD-EPI
overestimate a child's renal function from the child's own creatinine. Pass
`patientCreatinine(creatinine, age, sex)` instead only when the source applied its renal
equation to children's own creatinine, as pregabalin's did. Say so in the model's header
and in its `reference` string. See `R/drugs_vancomycin.R` and `R/renalFunction.R`.

**What the engine cannot represent.** The closed-form engine is linear and mammillary, with
first-order extravascular absorption. A source model with saturable protein binding
(cefazolin, hydrocortisone), reversible interconversion (prednisone and prednisolone) or an
apparent oral scale has to be reduced to that form, and the reduction must be written down
in the header: which part is exact, which is approximate, and what is plotted
(`R/drugs_cefazolin.R`, `R/drugs_hydrocortisone.R`, `R/drugs_prednisolone.R`).
The one exception is oral or sublingual bioavailability that falls with the size of the
dose: the engine applies that itself, dose by dose (see *saturable oral absorption* below).

### Return-value contract

| Field | Meaning |
|---|---|
| `PK` | named list of PK sets, one per event; each has `v1,v2,v3,cl1,cl2,cl3` (liters, L/min). A single-model drug uses one set named `default`. |
| `tPeak` | time (min) to peak effect site; `getDrugPK()` back-solves `ke0` from it. `0` means no effect-site model. |
| `MEAC` | reference effect concentration used for the MEAC plot / normalization (`0` if not applicable). |
| `typical`, `upperTypical`, `lowerTypical` | the shaded "typical range" band on the plot. |
| `reference` | literature citation (string). |
| `prodrug` | optional; `FALSE` marks an active parent that has a metabolite but no effect site, so the help does not call it a prodrug (see "An active parent with no effect-site model" below). |

**Optional — extravascular routes.** To support oral/sublingual/IM/intranasal dosing, or
regional anesthesia (RA, a local anesthetic injected into tissue), add absorption fields to
a PK set: `ka_PO`, `bioavailability_PO`, `tlag_PO` (and the `_SL` / `_IM` / `_IN` / `_RA`
equivalents).
An RA drug may add a slow second tissue depot absorbing in parallel: `ka_RA_slow` (1/min) and
`fraction_RA_slow`, the share of the absorbed dose that goes through it (`R/drugs_mepivacaine.R`). `getDrugPK()` builds the matching absorption coefficients and `simCpCe()` routes
those doses through `advanceClosedFormPO_IM_IN()`. Omit them for an IV-only drug.
The route is the suffix of the unit (`mg PO`, `mg SL`, `mg IM`, `mg IN`, `mg RA`; `doseRoute()` in `R/routes.R`),
so list those units in the drug's `Units` field; the dropdowns group them by route automatically.

**Optional — a second oral depot.** A formulation absorbed through two parallel first-order
paths, each with its own lag, adds `ka_PO2` (1/min), `fraction_PO2` (the share of the
**absorbed** oral dose that takes the second path) and optionally `tlag_PO2` (min; the oral lag
if absent) beside `ka_PO`, `bioavailability_PO` and `tlag_PO`. `bioavailability_PO` stays the
absolute bioavailability of the whole dose, applied once; `getDrugPK()` splits it between the
depots, and `simCpCe()` duplicates each oral dose row into the internal route `PO2`, as it
does for the slow RA depot. Diclofenac is the example (`R/drugs_diclofenac.R`); meloxicam uses
it with apparent parameters and `bioavailability_PO = 1`. Not available with an active
metabolite, several oral formulations, or a `tPeak` measured after an oral dose.

**Optional — parallel systems.** A drug whose plotted concentration is the sum of independent
linear systems sharing its doses (ketorolac: the S and R enantiomers, fitted separately)
returns `parallelSystems`, a list of `list(name, doseFraction, PK)` entries whose `PK` has the
same shape and event names as the drug's own, plus its own `doseFraction` (the share of each
dose its own system receives; a salt or racemate conversion goes here). `getDrugPK()` builds
each system's coefficients on the drug's effect site, and `simCpCe()` runs every system on the
same doses and adds the results, which is exact. Target-controlled infusion is refused for
such a drug, and it cannot have a metabolite, several oral formulations, saturable absorption
or an osmotic block. The help page lists the systems under *Parallel systems*. See
`R/drugs_ketorolac.R`.

A system can also be limited to some routes: `routes` (values of `DOSE_ROUTES`) on a
`parallelSystems` entry, and on the drug's own list for its own system. A system then gets the
other routes' doses as zero, so it stays on the same time line. Give every system the same
absorption parameters (the same lags) even where it receives none of those doses. Meloxicam
uses this to send oral doses to its apparent oral fit (`routes = "PO"`) and intravenous doses to
the separate ANJESO fit (`routes = "IV"`) (`R/drugs_meloxicam.R`). This is a way to offer a
route from a separate study without inventing a bioavailability that links the two fits.

**Optional — oral input as a constant daily rate.** A model fitted with each day's oral dose
spread evenly over the day, rather than absorbed first-order, offers the unit `mg/day PO`
(`poRateUnits` in `R/constants.R`). It is oral by route, so the help and the dropdowns call it
oral, but a rate by kind (`isRateUnit()` in `R/routes.R`): `simCpCe()` converts it to mg/min and
runs each row as the drug's running input rate, as it does an infusion, until the drug's next
rate row; `0 mg/day PO` stops it. It is applied to the model's parameters directly, with no
absorption rate constant and no bioavailability, so it suits **apparent** oral parameters (see
below) and the model needs no `ka_PO` at all. Amiodarone is the example: Pollak and colleagues
modelled a 400 mg/day dose as 16.7 mg/h for 24 hours. A source reporting clearances per day is
converted with `MINS_PER_DAY` in the drug file.

**Optional — time-varying PK.** Provide more than one named PK set (e.g. `default`,
`"CPB Start"`) to switch kinetics on a clinical event; `advanceClosedForm1()` handles the
transitions. Event names must exist in `inst/extdata/eventDefaults.csv`.

**Optional — a pharmacogenetic covariate.** A model whose kinetics depend on CYP2D6 adds
`cyp2d6` to its signature, with a default:

```r
codeine <- function(weight, height, age, sex, cyp2d6 = CYP2D6_DEFAULT)
```

`getDrugPK()` passes the phenotype only to models that name the argument, so no other drug
file changes. Valid values are in `CYP2D6_VALUES`: `poor`, `intermediate`, `normal`,
`ultrarapid`. Validate it and fail loudly on anything else.

CYP2C19 works the same way: a model adds `cyp2c19 = CYP2C19_DEFAULT` to its signature, and
gets the Patient Profile's **CYP 2C19** field. Valid values are in `CYP2C19_VALUES`, the five
CPIC terms (`poor`, `intermediate`, `normal`, `rapid`, `ultrarapid`). A source that estimated
fewer groups says in the drug file which group each of the five is given (escitalopram,
citalopram). The drug's help page tabulates its clearance by phenotype automatically.

**Optional — an osmotic agent.** A drug reported as the serum osmolality it produces, rather
than as its own concentration, adds `osmolality` to its signature (the patient's baseline,
mOsm/kg, passed only to models that name it) and returns an `osmotic` block:

```r
osmotic = list(baseline = osmolality, fraction = 0.555, molecularWeight = 182.17)
```

Its CSV row uses `mOsm` as `Concentration.Units`, so doses in g or mg are converted to mOsm
with the molecular weight, and `simCpCe()` plots `baseline + fraction * Cp`. The panel is
labelled mOsm/kg and its y-axis is not anchored at zero. `R/drugs_mannitol.R` is the example,
and [mannitol.md](mannitol.md) explains where its fraction comes from.

**Optional — an active metabolite.** A drug whose effect is carried by a metabolite returns
a `metabolite` block alongside the usual fields:

```r
metabolite = list(
  name              = "morphine",   # must be a drug in the CSV
  kFormation        = 1.24e-4,      # 1/min, first-order out of the central compartment
  firstPassFraction = 0.0015,       # of an oral dose, converted before reaching systemic
  mwRatio           = 285.34 / 299.36
)
```

`getDrugPK()` resolves the metabolite's own disposition and convolves the parent through it
(`metaboliteCoefficients()`); `simCpCe()` routes the drug through
`advanceClosedFormMetabolite()`; and `foldMetabolites()` adds the formed contribution to the
metabolite drug's own row after every drug has been simulated, creating that row if the
metabolite was never given directly. See `R/drugs_codeine.R` for a worked example.

Four things to know before using it:

- `kFormation` is **not** a share of the parent's elimination. The parent's published
  clearance already subsumes the metabolic loss, so formation is added on top of a
  disposition model that stands unchanged. Subtracting it again would double-count.
- A parent and its metabolite need not report in the same units. `getDrugPK()` applies
  `metaboliteUnitScale()` automatically from the two `Concentration.Units`; getting this
  wrong is a silent thousandfold error.
- Only one level is resolved. A cascade (codeine → morphine → M6G) would need a two-stage
  convolution and is not supported.
- Only the intravenous and oral routes carry metabolite coefficients. SL, IM and IN doses raise
  rather than silently dropping the metabolite, and a metabolite drug cannot also switch
  kinetics on a clinical event.

**A pure prodrug** sets `tPeak = 0`, so `ke0` is zero and the drug has no effect site. Its
plotted effect-site column is `NA`, which `simulationPlot()` drops, and the derived scalars
fall back to zero. The effect appears on the metabolite's row — including its "time until
threshold", which `foldMetabolites()` solves from the formed contribution's effect-site states
together with any of the metabolite drug that was given directly (`recoveryStates.R`). Nothing
in a drug model has to arrange that; `endCe` on the metabolite drug's defaults row is the
threshold it is measured against.

**An active parent with no effect-site model** looks the same to the engine (`tPeak = 0` and a
`metabolite` block), but is not a prodrug, and the generated help page would otherwise say
that its effect is the metabolite's. Such a model returns `prodrug = FALSE` alongside the
usual fields. The field is optional and read only by the help (`R/help-drugs.R`); leaving it
out keeps the prodrug description. Amiodarone, which is active itself and has no published
human ke0, is the example (`R/drugs_amiodarone.R`).

**A drug whose potency is not yet known** uses the same mechanism, but should say so. Put
`tPeak` and `MEAC` in named constants at the top of the file with a comment explaining what
is missing, and add a test asserting that the constant matches the CSV's `MEAC` column —
the plot and the opioid total read the CSV, not the drug function, so changing one without
the other fails silently. `R/drugs_hydrocodone.R` and `R/drugs_oxymorphone.R` are the
worked examples.

**Apparent parameters restrict the route.** A model fitted to oral data alone gives
clearance and volume divided by an unmeasured bioavailability. Those predict oral
concentrations correctly, because the unknown factor cancels, and intravenous ones wrong by
`1/F`. Such a drug must offer oral units only and carry `bioavailability_PO = 1`, since the
apparent scale already contains it. Hydrocodone is the example. A drug offered only as the
constant-rate oral unit `mg/day PO` (amiodarone) carries no absorption fields at all: the rate
is applied to the apparent parameters as it stands.

**Optional — a pulsed extended-release product.** A product designed as fixed
fractions released at fixed delays (Adderall XR: two bead populations, half at once and
half 4 h later) returns an `oralPulses` block naming the formulation word that selects it:

```r
oralPulses = list(XR = list(fraction = c(0.5, 0.5), delay = c(0, 240)))   # delay in minutes
```

and lists `mg PO XR` (and `mg PO XR qd`) in its CSV `Units`. `simCpCe()` replaces each
dose of that formulation by its pulses before anything else happens to it
(`expandOralPulses()`, `R/oral-pulses.R`; after the scheduled repeats are expanded): plain
`mg PO` doses of `fraction[i]` of it at its time plus `delay[i]`, each absorbed with the
drug's default `ka_PO`, `bioavailability_PO` and `tlag_PO`. The fractions must sum to one,
so the amount given is unchanged (`validateOralPulses()`). This describes release, not
absorption, and suits a product shown to be bioequivalent to its immediate-release form
given in split doses; a continuous release (an osmotic pump) is not represented this way.
The drug's help page must say how the pulses were chosen. `R/drugs_mixedAmphetamineSalts.R`
is the example.

**Optional — saturable oral absorption.** A drug absorbed by a carrier that saturates, so
that the fraction of an oral dose absorbed falls as the dose rises, returns an
`oralSaturation` block alongside the usual fields:

```r
oralSaturation = list(Imax = 0.906, ID50 = 571)   # ID50 in mg per administration
```

`simCpCe()` scales every oral dose by `1 - Imax * D / (ID50 + D)`, with D its dose in mg,
before it reaches the engine, and `bioavailability_PO` becomes the fraction absorbed in the
limit of a small dose. The hyperbolic `Dmax / (D50 + D)` form is the case `Imax = 1` with
`bioavailability_PO = Dmax / D50`. Each dose is then an ordinary input, so superposition
holds; what is not represented is saturation shared between doses taken together or close
in time. `Imax` must lie between 0 and 1 (`validateOralSaturation()`), and the drug's help
page tabulates the fraction at several doses. `R/drugs_gabapentin.R` is the example.

The block has two further forms, chosen with a `form` field (the one above is
`form = "saturable"`, the default):

- `list(form = "rising", D50 = 15.5)`: the fraction is `D / (D50 + D)`, so bioavailability
  **rises** with the dose towards `bioavailability_PO`, which is then its maximum. Sertraline
  (`R/drugs_sertraline.R`).
- `list(form = "power", exponent = 0.363, Dref = 25)`: the fraction is `(D / Dref)^exponent`.
  This carries an empirical power of the dose on **apparent clearance**,
  `CL/F × (D / Dref)^-exponent`, which a linear engine cannot hold: write the clearance at
  `Dref`, and the steady-state exposure `D / CL(D)` is reproduced by scaling the dose. The
  half-life stays that of the reference dose, and each administration is read as the day's dose.
  Paroxetine (`R/drugs_paroxetine.R`).

Any form may carry `exampleDoses`, the oral doses in mg the help page tabulates.

A **sublingual** bioavailability that falls with the dose is declared the same way, as a
`sublingualSaturation` block with the same two fields; `simCpCe()` scales every SL dose by
the same expression and `bioavailability_SL` becomes the small-dose limit. A source that
reports the dependence in another form (buprenorphine's power law) is fitted to this one
over the dose range the source covers, and the fit is documented in the drug header
(`R/drugs_buprenorphine.R`).

## 2. The metadata — `inst/extdata/drugDefaults_global.csv`

Add one row. Columns:

```
Drug,Concentration.Units,Bolus.Units,Infusion.Units,Default.Units,Units,Color,Lower,Upper,Typical,MEAC,endCe,Class,Category
```

- `Drug` — must exactly match the R function name (this CSV is the source of the drug list).
- `Concentration.Units` — `mcg` or `ng` per mL (sets the internal unit scaling in `simCpCe`),
  or `mOsm` for an osmotic agent (see above).
- `Bolus.Units` / `Infusion.Units` / `Default.Units` — units offered in the dose grid.
- `Units` — quoted comma-separated list of all selectable units, e.g. `"mcg,mcg/kg,mcg/kg/min"`.
- `Color` — hex color for this drug's curves (e.g. `#0000C0`).
- `Lower,Upper,Typical,MEAC,endCe` — plot band bounds, MEAC, and the "time until threshold"
  level: the effect-site concentration for a drug with an effect site, the plasma concentration
  for one without, and `0` for none. A drug with no established range, or none that applies
  to its model, sets all three band columns to `0`: no band is drawn, and its help page says so
  (desethylamiodarone; amiodaroneIV, whose chronic trough window does not describe intravenous
  loading). For an antibiotic, `endCe` is the plotted concentration at
  which **free** drug equals the MIC: the MIC itself if the model plots unbound drug, the MIC
  divided by the free fraction if it plots total drug. Add the antibiotic to
  `antibioticMicTable()` in `R/antibioticThresholds.R`, which records the organism, MIC, free
  fraction and sources, feeds the drug's help page, and is checked against this column by
  `test-antibiotic-thresholds.R`.
- `Class` — `IV` for an injected or swallowed drug, `gas` for an inhaled agent. The gases
  take a separate simulation path and have no `drugs_*.R` covariate function, so a new drug
  added by this procedure is `IV`.
- `Category` — the group the drug is listed under in the menu the app opens with: one of
  `DRUG_CATEGORIES` in `R/constants.R` (`Hypnotics and sedatives`, `Opioids`,
  `Oral analgesics`, `Neuromuscular blockade`, `Inhaled anesthetics`, `Antibiotics`,
  `Corticosteroids`, `Antidepressants`, `Stimulants`, `Local anesthetics`, `Other`). Left blank, the drug is not offered there; only a metabolite with no units of its
  own, and the carrier gases and ventilation, are blank. A new category goes into
  `DRUG_CATEGORIES`, and its checkbox id (`startupDrugs_<n>`) into `bookmarksToExclude` in
  `R/app_globals.R`. `test-startup-drugs.R` fails until both are done.

Example row (remifentanil):

```
remifentanil,ng,mcg,mcg/kg/min,mcg/kg/min,"mcg,mcg/kg,mcg/kg/min",#0000C0,0.8,2,1.2,1,1,IV,Opioids
```

A prodrug sets `MEAC` to zero and uses the band columns for its own plasma concentration,
since it has no effect site to band. Codeine's row is the example:

```
codeine,ng,mg,mg/hr,mg PO,"mg,mg/kg,mg/hr,mg PO",#4A6FE3,50,150,100,0,0,IV
```

## 3. The test — `tests/testthat/test-drugs-<name>.R`

Pin the returned values at a reference patient so future edits are intentional, with the
switch off (so the published parameters are what is pinned) and a second test with it on for
a non-reference patient. Mirror the existing drug tests:

```r
test_that("returns the correct calculations", {
  weight <- 70; height <- 171; age <- 50; sex <- "male"
  actual <- <name>(weight, height, age, sex, adjustToFFM = FALSE)

  expected <- list(
    PK = list(default = list(
      v1 = ..., v2 = ..., v3 = ...,
      cl1 = ..., cl2 = ..., cl3 = ...
    )),
    tPeak = ..., MEAC = ...,
    typical = ..., upperTypical = ..., lowerTypical = ...,
    reference = "..."
  )

  expect_equal_rounded(actual, expected)
})

test_that("scales to fat-free mass for a 120 kg man", {
  # 120 kg, 170 cm, 50 y male: volumes x 1.3049067, clearances x 1.2209126
  actual <- <name>(120, 170, 50, "male")
  expected <- list(v1 = ..., v2 = ..., v3 = ..., cl1 = ..., cl2 = ..., cl3 = ...)
  expect_equal_rounded(actual$PK$default[names(expected)], expected)
})
```

`expect_equal_rounded` is defined in `tests/testthat/helpers.R`. Work the scaled pins out from
the published numbers and the factors above, not by running the code under test.

## 4. The help page — `inst/help/drugs/<name>.md`

The in-app help (the **Help** tab) generates a page for every drug in the CSV: its parameters
at six reference patients, its citation, its units and typical range, all computed from the
files above so they cannot drift. What it cannot generate is the narrative — the population
the model was fitted in, which covariates it uses and how, where it is extrapolated, who did
the work. That goes in `inst/help/drugs/<name>.md`, starting at `###` headings (it is appended
under an "About this model" heading). `tests/testthat/test-help-drugs.R` fails if the file is
missing. See `inst/help/README.md` for the Markdown conventions and `inst/help/drugs/fentanyl.md`
for a short example.

Optionally, add a teaching scenario that uses the drug: a `helpScenario()` entry in
`R/help-scenarios.R` and its narrative in `inst/help/scenarios/<id>.md`.

## Verify

```r
devtools::load_all(".")
devtools::test(filter = "drugs-<name>")   # unit test
devtools::test(filter = "help")           # the help pages, including the new drug's
run_app()                                  # confirm it appears in the dose-grid dropdown and plots
```

Also run the broader `test-multi-PK` / `test-single-PK` suites, which exercise the full
`getDrugPK → simCpCe` path against the library.
