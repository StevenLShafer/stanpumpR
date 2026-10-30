# stanpumpR

![](../../workflows/R-CMD-check/badge.svg)

stanpumpR, a PK/PD simulation program
Copyright 2019-2026 Steven L. Shafer and contributors, steven.shafer@stanford.edu.
Released under the MIT License; see [LICENSE.md](LICENSE.md).

stanpumpR is open-source software for pharmacokinetic / pharmacodynamic simulation. It is intended to make pharmacokinetics accessible to facilitate perioperative patient care, teaching, and research. It is released under the MIT License: anyone may use, copy, modify and redistribute it, including commercially, provided the copyright and licence notice is kept with it.

stanpumpR is an advisory and teaching tool. It does not control drug delivery, and how its predictions are applied to an individual patient is a matter of clinical judgement. It is provided as is, without warranty of any kind; see the licence.

STANPUMP, a portmanteau of "Stanford" and "Pump", was developed in the Stanski/Shafer laboratory at Stanford University from 1987 through 1997. STANPUMP was one of many programs developed to control the delivery of intravenous anesthetics using pharmacokinetic principles. At that time there was an active exchange of concepts and algorithms among the authors. Significant contributors to this effort were Schüttler and Schwilden at the University of Bonn (CATIA), Ausems and Hug as the University of Leiden (TIAC), Reves and Alvis at the University of Alabama (CACI), Jacobs and Reves at Duke University (CACI II), Coetzee and Pina at Stellenbosch University (STELPUMP), and De Smet and Struys at the University of Ghent (RUGLOOP). This history was recently reviewed by Struys and colleagues: The History of Target-Controlled Infusion. Anesth Analg. 2016;122:56-69.

STANPUMP was distributed freely: anyone could copy and share its source and executable code, though not sell it. The STANPUMP pharmacokinetic engine was incorporated into many of the commercially available target controlled infusion devices, where it is still used today.

stanpumpR uses very little of the original STANPUMP code. However, conceptually it is identical: an open-source program to make complex pharmacokinetic algorithms available to support patient care, teaching, and research. However, stanpumpR does not control drug administration. It is simply a web-based simulator that uses the Shiny package in R to simulate the expected concentration of intravenous anesthetics from any dosing regimen.

In 2026 stanpumpR gained an engine for the inhaled anesthetics. Its structure and parameters follow GasMan®, the model of inhaled anesthetic uptake and distribution developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School, which has taught a generation of anesthesiologists how these drugs behave. Richard H. Epstein of the University of Miami validated the stanpumpR engine against GasMan, running the comparison scenarios and tracing each disagreement to its cause; that record is in [`inst/validation/VALIDATION.md`](inst/validation/VALIDATION.md). The places where stanpumpR deliberately departs from GasMan are listed in the [user's guide](docs/users-guide.md).

It is hoped that stanpumpR will encourage device manufacturers to develop the next generation of drug delivery systems and anesthesia information management systems. Companies are welcome to build on stanpumpR under the terms of the MIT License. As a courtesy, Dr. Shafer would be glad to hear from them.

stanpumpR is a collaborative research project. Individuals interested in adding drugs, pharmacokinetic data sets, or new algorithms to stanpumpR are encourage to contact Dr. Shafer. It is hoped that eventually each drug in the stanpumpR library will be maintained by an investigator, who will assume responsibility for keeping the pharmacokinetics as up-to-date as possible. See [`docs/adding-a-drug.md`](docs/adding-a-drug.md) for the technical steps.

Near-term future developments in stanpumpR will include

1. More oral models. stanpumpR simulates PO, SL, IM, IN, SC and regional (RA) delivery by first-order absorption (with zero-order, pulsed and transit-chain oral inputs where a drug needs them); the oral opioids in the library (morphine, hydromorphone, methadone, oxycodone, oxymorphone, codeine, hydrocodone, tramadol) each rest on one published fit, and better-characterised absorption models are welcome.
2. Improved models of pediatric pharmacokinetics
3. Improved models of drug interaction
4. PK changes with pregnancy beyond the 48 drugs that now read the Patient Profile's Pregnant field (`R/pregnancy.R`, `inst/extdata/pregnancyRegistry.csv`), and with renal function beyond the drugs that read the serum creatinine. (Each field reaches only the models that name it, the CYP2D6, CYP2C19, CYP2C9 and CYP2B6 phenotypes included; the help's [Cautions](inst/help/cautions.md) page lists which.)
5. Expanding the in-app help (the **Help** tab): more teaching scenarios, and screenshots.

### Documentation

- [`docs/architecture.md`](docs/architecture.md) — how the app is put together: the reactive
  pipeline, the PK/PD engine, and the file layout.
- [`docs/adding-a-drug.md`](docs/adding-a-drug.md) — the touch points for contributing a
  new drug, including its help page.
- [`docs/users-guide.md`](docs/users-guide.md) — a draft user's guide written from the code.
- [stanpumpr.io/api](https://stanpumpr.io/api) — the Simulation API: send a dose table,
  covariates and a duration (over HTTP from any language, or from R) and get the concentrations
  back as JSON, CSV, Excel or XML. The page is rendered from
  [`inst/apidocs/api.md`](inst/apidocs/api.md).
- [`docs/tci-api.md`](docs/tci-api.md) — the TCI API: a stateless web service, `/tci/v1`,
  that hands a real target-controlled infusion pump its next rate from the pump's own record
  of what it has delivered (`run_tci_api()`, or `tciNextRate()` from R).
  [FauxTCI](https://stevenlshafer.github.io/FauxTCI/) is a simulated pump in the browser
  built to it, showing every request and response as it runs
  ([source](https://github.com/StevenLShafer/FauxTCI)).
- **The in-app help** — the **Help** tab in the running app: a user's guide, a generated page
  for every drug (parameters at reference patients, citation, units), the models and methods,
  loadable teaching scenarios, the investigators, and a map of this repository. The hand-written
  pages are Markdown in [`inst/help/`](inst/help/) (see [`inst/help/README.md`](inst/help/README.md));
  the drug and scenario pages are generated from the code by `R/help-drugs.R` and
  `R/help-scenarios.R`.

### Setting up locally

1. Check the required R version by looking at the `R` `Version` field near the top of the file [`renv.lock`](renv.lock)
2. Download and install the required R version
3. Clone/download this repository to your local machine
4. Open the `stanpumpR.Rproj` file in RStudio
5. Run `renv::restore()` to install all the necessary packages
6. Make a copy of the file `config.yml.sample` as a new file named `config.yml`. This `config.yml` is a configuration file that stanpumpR needs. You can change the settings inside it.
7. Run the app from the RStudio Console:
    ```
    devtools::load_all(".")
    run_app()
    ```
8. Debug mode shows a detailed log and a performance profiler. By default, debug mode is off in production. You can turn it on by adding `?debug=1` to the URL (`&debug=1` if it already has a query).

#### Running tests

1. Install the devtools package via `install.packages("devtools")`.
2. Install all package dependencies via `devtools::install_deps(dependencies = TRUE)`
3. Run the tests via `devtools::test()`

#### Using stanpumpR as a library

A limited number of functions have been exported for use by your own code.

**On Windows, install the ready-made package.** It is rebuilt from `master` on every change and
published as the [`windows-binary` release](https://github.com/StevenLShafer/stanpumpR/releases/tag/windows-binary):
one zip for x64 and ARM, built with the current R release (install it in the same R minor version).

```r
zip <- file.path(tempdir(), "stanpumpR.zip")
download.file("https://github.com/StevenLShafer/stanpumpR/releases/download/windows-binary/stanpumpR.zip",
              zip, mode = "wb")
install.packages(zip, repos = NULL, type = "win.binary")
library(stanpumpR)
```

Install the CRAN packages in `Imports` first if you don't have them; the release notes list them.
Installing from source on Windows (`remotes::install_github()`, `pak`, `devtools::install()`) can
fail with nothing but `ERROR: lazy loading failed for package 'stanpumpR'`. On Windows 11 the
cause has been **Smart App Control**, which blocked a DLL during the build because it could not
verify where the DLL came from. The zip is already built, so installing it skips that step. It is built by
`.github/workflows/windows-binary.yaml`, which also installs it into an empty library and runs a
simulation before publishing it.

**On macOS or Linux**, install from GitHub:

```r
# install.packages("remotes")
remotes::install_github("StevenLShafer/stanpumpR")
```

or, from a clone of the repository, `devtools::install(build_vignettes = TRUE)`.

To simulate a dose table from a script, call `apiSimulate()`, or start the HTTP service with
`run_api()` (needs the `plumber` package). Both are described at
[stanpumpr.io/api](https://stanpumpr.io/api).

### Environments

When code is merged into the default branch, it is deployed to production automatically once the R-CMD-check workflow has passed on it. Pull Requests are automatically deployed to a test environment for testing.

### Dependency management (renv)

This project uses [{renv}](https://rstudio.github.io/renv/) to lock R package versions. This is mainly done so that we can take control of package versions for the production shiny app, which is a major step towards a stable app. The `renv.lock` file is committed to the repo and is used to ensure the deployed app uses the exact same package versions as those tested locally.

**Adding a new package** — add it to `DESCRIPTION` first, then install and snapshot:

```r
renv::install("pkgname")   # install into the renv library
renv::snapshot()           # update renv.lock
# commit both DESCRIPTION and renv.lock
```

**Updating an existing package:**

```r
renv::update("pkgname")
renv::snapshot()
# commit renv.lock
```

**After pulling changes** that include `renv.lock` updates:

```r
renv::restore()
```

Steven L. Shafer, MD
November 2019


