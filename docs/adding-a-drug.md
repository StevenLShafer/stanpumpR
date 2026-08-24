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

## 2. The metadata — `inst/extdata/drugDefaults_global.csv`

Add one row. Columns:

```
Drug,Concentration.Units,Bolus.Units,Infusion.Units,Default.Units,Units,Color,Lower,Upper,Typical,MEAC,endCe
```

- `Drug` — must exactly match the R function name (this CSV is the source of the drug list).
- `Concentration.Units` — `mcg` or `ng` per mL (sets the internal unit scaling in `simCpCe`).
- `Bolus.Units` / `Infusion.Units` / `Default.Units` — units offered in the dose grid.
- `Units` — quoted comma-separated list of all selectable units, e.g. `"mcg,mcg/kg,mcg/kg/min"`.
- `Color` — hex color for this drug's curves (e.g. `#0000C0`).
- `Lower,Upper,Typical,MEAC,endCe` — plot band bounds, MEAC, and emergence effect-site level.

Example row (remifentanil):

```
remifentanil,ng,mcg,mcg/kg/min,mcg/kg/min,"mcg,mcg/kg,mcg/kg/min",#0000C0,0.8,2,1.2,1,1
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
