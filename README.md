# stanpumpR

![](../../workflows/R-CMD-check/badge.svg)

stanpumpR, a PK/PD simulation program
Copyright 2019-2024 Steven L. Shafer, steven.shafer@stanford.edu.
All Rights Reserved

stanpumpR is open-source software for pharmacokinetic / pharmacodynamic simulation. It is intended to make pharmacokinetics accessible to facilitate perioperative patient care, teaching, and research. stanpumpR may be freely downloaded and used without restriction for non-commericial purposes.

STANPUMP, a portmanteau of "Stanford" and "Pump", was developed in the Stanski/Shafer laboratory at Stanford University from 1987 through 1997. STANPUMP was one of many programs developed to control the delivery of intravenous anesthetics using pharmacokinetic principles. At that time there was an active exchange of concepts and algorithms among the authors. Significant contributors to this effort were Schüttler and Schwilden at the University of Bonn (CATIA), Ausems and Hug as the University of Leiden (TIAC), Reves and Alvis at the University of Alabama (CACI), Jacobs and Reves at Duke University (CACI II), Coetzee and Pina at Stellenbosch University (STELPUMP), and De Smet and Struys at the University of Ghent (RUGLOOP). This history was recently reviewed by Struys and colleagues: The History of Target-Controlled Infusion. Anesth Analg. 2016;122:56-69.

STANPUMP was placed in the public domain. The STANPUMP pharmacokinetic engine was incorporated into many of the commerially available target controlled infusion devices, where it is still used today.

stanpumpR uses very little of the original STANPUMP code. However, conceptually it is identical: an open-source program to make complex pharmacokinetic algorithms available to support patient care, teaching, and research. However, stanpumpR does not control drug administration. It is simply a web-based simulator that uses the Shiny package in R to simulate the expected concentration of intravenous anesthetics from any dosing regimen.

It is hoped that stanpumpR will encourage device manufacturers to develop the next generation of drug delivery systems and anesthesia information management systems. Companies seeking to develop such systems should contact Dr. Shafer to request written permission to incorporate stanpumpR into their products. Without written permission, stanpumpR must not be incorporated into commercially available systems.

stanpumpR is a collaborative research project. Individuals interested in adding drugs, pharmacokinetic data sets, or new algorithms to stanpumpR are encourage to contact Dr. Shafer. It is hoped that eventually each drug in the stanpumpR library will be maintained by an investigator, who will assume responsibility for keeping the pharmacokinetics as up-to-date as possible.

Near-term future developments in stanpumpR will include

1. Oral opioids. StanpumpR is programmed for PO, IM, and IN delivery. Need to get good PK for the opioids. Note that only first order absorption pk is currently supported.
2. Improved models of pediatric pharmacokinetics
3. Improved models of drug interaction
4. PK changes with pregnancy, CYP2D6, and renal function. These have been added to the UI, but no models with these are yet in the program.
5. Create of help and example pages.

### Setting up locally

1. Clone/download this repository to your local machine
1. Open the `stanpumpR.Rproj` file in RStudio
1. Install the devtools package via `install.packages("devtools")`.
1. Install all package dependencies via `devtools::install_deps(dependencies = TRUE)`
1. Make a copy of the file `config.yml.sample` as a new file named `config.yml`. This `config.yml` is a configuration file that stanpumpR needs. You can change the settings inside it.
1. Run the app from the RStudio Console:
- `library(shiny)`
- `runApp()`

#### Running tests
1. Install the devtools package via `install.packages("devtools")`.
1. Install all package dependencies via `devtools::install_deps(dependencies = TRUE)`
1. Run the tests via `devtools::test()`

#### Using stanpumpR as a library
A limited number of functions have been exported for use by your own code. To install:

1. `library(devtools)`
1. `devtools::install(build_vignettes = TRUE)`

### Environments

When code is merged into the default branch, it will automatically be deployed to production. Pull Requests are automatically deployed to a test environment for testing.

Steven L. Shafer, MD
November 2019
