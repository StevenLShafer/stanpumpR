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
cefazolin) evaluates the published equations at the pharmacokinetic weight with the switch
on (`size$pkWeight`, which is 70 kg × FFM / FFM<sub>ref</sub>) and at total body weight with it
off, and scales any size-free parameter by the library factors. Write that choice out:
`if (isTRUE(adjustToFFM)) size$pkWeight else weight`, as `R/drugs_cefazolin.R` does. Do not
derive the weight from `70 * size$volume` unless the model's `legacyVolume` is `weight / 70`;
with `legacyVolume = 1` that expression is 70 kg for everyone when the switch is off. Renal
function comes from `R/renalFunction.R` (`creatinineClearanceCG()`, `egfrDeindexed()`).
Add `creatinine = NULL` to the model's signature and pass `patientCreatinine(creatinine, sex)`
as the creatinine: that is the patient's serum creatinine from the Patient Profile, or an
**assumed normal creatinine** for the patient's sex when the field is blank. Say so in the
model's header and in its `reference` string. See `R/drugs_vancomycin.R`.

**What the engine cannot represent.** The closed-form engine is linear and mammillary, with
first-order extravascular absorption. A source model with saturable protein binding
(cefazolin, hydrocortisone), reversible interconversion (prednisone and prednisolone) or an
apparent oral scale has to be reduced to that form, and the reduction must be written down
in the header: which part is exact, which is approximate, and what is plotted
(`R/drugs_cefazolin.R`, `R/drugs_hydrocortisone.R`, `R/drugs_prednisolone.R`).

### Return-value contract

| Field | Meaning |
|---|---|
| `PK` | named list of PK sets, one per event; each has `v1,v2,v3,cl1,cl2,cl3` (liters, L/min). A single-model drug uses one set named `default`. |
| `tPeak` | time (min) to peak effect site; `getDrugPK()` back-solves `ke0` from it. `0` means no effect-site model. |
| `MEAC` | reference effect concentration used for the MEAC plot / normalization (`0` if not applicable). |
| `typical`, `upperTypical`, `lowerTypical` | the shaded "typical range" band on the plot. |
| `reference` | literature citation (string). |

**Optional — extravascular routes.** To support oral/IM/intranasal dosing, add absorption
fields to a PK set: `ka_PO`, `bioavailability_PO`, `tlag_PO` (and the `_IM` / `_IN`
equivalents). `getDrugPK()` builds the matching absorption coefficients and `simCpCe()` routes
those doses through `advanceClosedFormPO_IM_IN()`. Omit them for an IV-only drug.
The route is the suffix of the unit (`mg PO`, `mg IM`, `mg IN`; `doseRoute()` in `R/routes.R`),
so list those units in the drug's `Units` field; the dropdowns group them by route automatically.

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
- Only the intravenous and oral routes carry metabolite coefficients. IM and IN doses raise
  rather than silently dropping the metabolite, and a metabolite drug cannot also switch
  kinetics on a clinical event.

**A pure prodrug** sets `tPeak = 0`, so `ke0` is zero and the drug has no effect site. Its
plotted effect-site column is `NA`, which `simulationPlot()` drops, and the derived scalars
fall back to zero. The effect appears on the metabolite's row — including its "time until
threshold", which `foldMetabolites()` solves from the formed contribution's effect-site states
together with any of the metabolite drug that was given directly (`recoveryStates.R`). Nothing
in a drug model has to arrange that; `endCe` on the metabolite drug's defaults row is the
threshold it is measured against.

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
apparent scale already contains it. Hydrocodone is the example.

## 2. The metadata — `inst/extdata/drugDefaults_global.csv`

Add one row. Columns:

```
Drug,Concentration.Units,Bolus.Units,Infusion.Units,Default.Units,Units,Color,Lower,Upper,Typical,MEAC,endCe,Class
```

- `Drug` — must exactly match the R function name (this CSV is the source of the drug list).
- `Concentration.Units` — `mcg` or `ng` per mL (sets the internal unit scaling in `simCpCe`),
  or `mOsm` for an osmotic agent (see above).
- `Bolus.Units` / `Infusion.Units` / `Default.Units` — units offered in the dose grid.
- `Units` — quoted comma-separated list of all selectable units, e.g. `"mcg,mcg/kg,mcg/kg/min"`.
- `Color` — hex color for this drug's curves (e.g. `#0000C0`).
- `Lower,Upper,Typical,MEAC,endCe` — plot band bounds, MEAC, and the "time until threshold"
  level: the effect-site concentration for a drug with an effect site, the plasma concentration
  for one without, and `0` for none. For an antibiotic, `endCe` is the plotted concentration at
  which **free** drug equals the MIC: the MIC itself if the model plots unbound drug, the MIC
  divided by the free fraction if it plots total drug. Add the antibiotic to
  `antibioticMicTable()` in `R/antibioticThresholds.R`, which records the organism, MIC, free
  fraction and sources, feeds the drug's help page, and is checked against this column by
  `test-antibiotic-thresholds.R`.
- `Class` — `IV` for an injected or swallowed drug, `gas` for an inhaled agent. The gases
  take a separate simulation path and have no `drugs_*.R` covariate function, so a new drug
  added by this procedure is `IV`.

Example row (remifentanil):

```
remifentanil,ng,mcg,mcg/kg/min,mcg/kg/min,"mcg,mcg/kg,mcg/kg/min",#0000C0,0.8,2,1.2,1,1,IV
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
