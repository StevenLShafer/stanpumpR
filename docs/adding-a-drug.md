# Adding a drug to stanpumpR

stanpumpR is designed so that adding a drug is a small, self-contained change — the goal is to
let outside investigators contribute and maintain the pharmacokinetics for individual drugs.
A new drug touches **four** places. None of the engine code needs to change.

> Prerequisite: read the [architecture map](architecture.md) first if you haven't. You only
> need to understand the *drug library* pattern, not the closed-form solver.

## 1. The model — `R/drugs_<name>.R`

Create a file named after the drug (lowercase, matching the CSV `Drug` value). Export one
function `<name>(weight, height, age, sex)` that returns a list. Even if the model ignores
covariates, keep the full signature.

Minimal, covariate-independent example (`alfentanil`):

```r
alfentanil <- function(weight, height, age, sex)
{
  # Units: time in minutes, volumes in liters

  default <- list(
    v1 = 2.1853,  v2 = 6.698864, v3 = 14.52582,
    cl1 = 0.1988623, cl2 = 1.433557, cl3 = 0.2469389
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
- `Concentration.Units` — `mcg` or `ng` per mL (sets the internal unit scaling in `simCpCe`).
- `Bolus.Units` / `Infusion.Units` / `Default.Units` — units offered in the dose grid.
- `Units` — quoted comma-separated list of all selectable units, e.g. `"mcg,mcg/kg,mcg/kg/min"`.
- `Color` — hex color for this drug's curves (e.g. `#0000C0`).
- `Lower,Upper,Typical,MEAC,endCe` — plot band bounds, MEAC, and emergence effect-site level.
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

Pin the returned values at a reference patient so future edits are intentional. Mirror the
existing drug tests:

```r
test_that("returns the correct calculations", {
  weight <- 70; height <- 171; age <- 50; sex <- "male"
  actual <- <name>(weight, height, age, sex)

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
```

`expect_equal_rounded` is defined in `tests/testthat/helpers.R`.

## Verify

```r
devtools::load_all(".")
devtools::test(filter = "drugs-<name>")   # unit test
run_app()                                  # confirm it appears in the dose-grid dropdown and plots
```

Also run the broader `test-multi-PK` / `test-single-PK` suites, which exercise the full
`getDrugPK → simCpCe` path against the library.
