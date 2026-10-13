The simulation engine can be driven from R without the app, for research, for batch simulations, or for checking a result. A limited set of functions is exported.

## Installing as a library

```r
# from a clone of the repository
devtools::install(build_vignettes = TRUE)
library(stanpumpR)
```

## One drug

`getDrugPK()` turns a drug name and covariates into the full parameter set; `simCpCe()` simulates a dose table with it.

```r
doseTable <- data.frame(
  Drug  = c("remifentanil", "remifentanil", "remifentanil"),
  Time  = c(0, 0, 30),
  Dose  = c(60, 0.15, 0),
  Units = c("mcg", "mcg/kg/min", "mcg/kg/min")
)
eventTable <- data.frame(Time = double(), Event = character())

PK <- getDrugPK(
  drug = "remifentanil",
  weight = 70, height = 170, age = 50, sex = "male",
  getDrugDefaults("remifentanil")
)

out <- simCpCe(doseTable, eventTable, PK, maximum = 60, plotRecovery = FALSE)
results <- out$results
head(results[results$Site %in% c("Plasma", "Effect Site"), ])
```

`PK$PK$default` holds the volumes, clearances, rate constants, eigenvalues and ke0; `PK$tPeak` and `PK$reference` the time to peak effect and the citation.

`getDrugPK()` also takes the covariates of the Patient Profile, each passed only to the models that use it: `creatinine` (serum creatinine in mg/dL; the default, `NULL`, is an assumed normal value for the patient's age and sex), `cyp2d6` (`"poor"`, `"intermediate"`, `"normal"`, the default, or `"ultrarapid"`), `cyp2c19` (the same, with `"rapid"`) and `cyp2c9` (a genotype from `"*1/*1"`, the default, to `"*3/*3"`), `osmolality` (mannitol's baseline) and `adjustToFFM` (`TRUE` scales the models to fat-free mass; see [Scaling to fat-free mass](help:models/fat-free-mass)). `simulateDrugsWithCovariates()` takes the same arguments.

`PK$endCe` is the drug's recovery threshold from the library, the value the [Drug Thresholds](help:drug-library) dialog edits in the app. With `plotRecovery = TRUE`, `simCpCe()` reports at each time how long the effect site would take to fall to it if all delivery stopped there: the `Recovery` column of `out$equiSpace`, and `out$max$Recovery`. Set `PK$endCe` before calling `simCpCe()` to time a different concentration; zero means no threshold, and the times are then all zero.

## Several drugs

`simulateDrugsWithCovariates()` loops over the drugs in a dose table:

```r
out <- simulateDrugsWithCovariates(doseTable, eventTable,
                                   weight = 70, height = 170, age = 50, sex = "male",
                                   maximum = 60, plotRecovery = FALSE)
names(out)          # one element per drug
out$remifentanil$results
```

The result for each drug is a tidy table of `Time`, `Site` (Plasma, Effect Site, and the normalised and recovery series) and `Y`, plus `equiSpace`, the curves on an even grid, and `max`, the peaks. A drug with *Plasma target* or *Effect site target* rows also has `tci`, the infusion schedule, and one with scheduled doses has `scheduled`, their repeats.

## The inhaled agents

```r
gasTable <- data.frame(
  Drug  = c("oxygen", "sevoflurane", "ventilation"),
  Time  = c(0, 0, 0),
  Dose  = c(6, 2, 6),
  Units = c("L/min", "%", "L/min")
)
sim <- simulateGases(gasTable, weight = 70, age = 40, maximum = 60)
```

`getGasProperties()` returns the parameter table, `macForAge()` the age-adjusted MAC, and `gasRecoveryTime()` and `macRecoveryTime()` the time until threshold.

## The drug library

`getDrugDefaultsGlobal()` returns the library as a data frame; `getDrugDefaults(drug)` one row.

## Vignettes

Two vignettes walk through these calls with plots: `vignette("stanpumpR-single-PK")` and `vignette("stanpumpR-multi-PK")`.

## Units

Doses are converted using the drug's concentration units from the library: a drug plotted in mcg/mL has its doses converted to mg, one plotted in ng/mL to mcg, and mannitol, plotted as serum osmolality, to mOsm. Times are minutes. Weight is kilograms, height centimetres, age years.

## The simulation window

`maximum` is the end of the simulation, and the result covers 0 to `maximum` and nothing else. A dose at or after `maximum` is ignored, since it cannot change anything inside the window, and so are the repeats of a scheduled dose and the target changes of a TCI row that fall there. The returned series stop at `maximum`, and `max` (the peaks) and the normalised series are taken over the window, so a large dose after it no longer shrinks the curves before it. A dose given before `maximum` is simulated in full, including an oral dose whose absorption only starts after it. Choose `maximum` long enough to take in every dose you want simulated: the app does this for you, lengthening the plot to the last dose or event.

## The simulation API

`apiSimulate(request)` takes the patient, the dose table and the duration as one list, the shape of a JSON request, and returns the plasma and effect-site concentrations of every drug as tidy tables (`timeseries`, one row per drug and time, and `drugs`, one row per drug with its peaks):

```r
result <- apiSimulate(list(
  patient  = list(weight = 70, height = 170, age = 50, sex = "male"),
  duration = 2, timeUnit = "hours",
  doses = data.frame(
    Drug  = c("propofol", "propofol", "propofol",   "remifentanil"),
    Time  = c(0,          0,          1,            0),
    Dose  = c(2,          100,        0,            0.15),
    Units = c("mg/kg",    "mcg/kg/min", "mcg/kg/min", "mcg/kg/min")
  ),
  output = list(interval = 1/60)        # one row per minute, in hours
))
head(result$timeseries)
apiWriteResult(result, "simulation.xlsx")   # or .csv, .json, .xml
```

Unlike `simCpCe()`, it takes times in the unit named by `timeUnit`. It runs `simulateDrugsWithCovariates()`, so its numbers are the app's. `apiFormatResult()` returns the result as JSON, CSV, Excel or XML, `apiDrugCatalog()` lists the drugs it can simulate, and `run_api()` serves it over HTTP (`POST /simulate`) for other languages; like the TCI API it needs `plumber`. The request and every option are documented in `docs/api.md` in the repository.

## The TCI API

`tciNextRate(request)` is the computation behind [The TCI API](help:tci-api), the stateless web service that hands a real TCI pump its next rate. It is exported and needs no server: pass it a request body parsed with `jsonlite::fromJSON(txt, simplifyVector = FALSE)` and it returns the response as a list, so a logged request from a pump can be replayed and checked here. `tciApiDrugs()` lists the drugs the API offers, `tciApiModel()` resolves the model for a patient, and `tciApiRouter()` and `run_tci_api()` wrap them in a `plumber` router (`plumber` is an optional dependency, not loaded by the app).

## What is not exported

The Shiny server, the plotting code and Suggest Dosing are internal. They can be reached with `stanpumpR:::` or `devtools::load_all()`, with no promise of a stable interface.
