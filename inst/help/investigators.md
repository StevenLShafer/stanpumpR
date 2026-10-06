stanpumpR is a thin layer of software over a thick layer of other people's work. Every curve it draws rests on a clinical study in which patients or volunteers were given a drug, blood was sampled, and a model was fitted. This page names the investigators whose work the program uses, and what of theirs is in it. Citations are on the [Bibliography](help:references) page and on each drug's page.

## The program

**Steven L. Shafer**, Stanford University. Author of STANPUMP (1987 to 1997) and of stanpumpR. The effect-site targeting algorithm (with Keith Gregg), the plasma targeting algorithm (with James Bailey), the time-to-peak-effect method for carrying ke0 between models (with John Varvel), the pooled fentanyl kinetics, and the design decisions recorded throughout the code are his.

**Dean Attali**. Co-author of the stanpumpR package. The Shiny architecture of the current app, its testing and deployment pipeline, the dose-table draft and undo mechanism and the `{undomanager}` package it uses are his work.

**Alexander Clarke** wrote the vignettes that document the scripting interface.

**Richard H. Epstein**, University of Miami, validated the inhaled-gas engine against Gas Man, running the comparison scenarios through the Gas Man API and tracing each disagreement to its cause.

Portions of the code written since 2026, including the inhaled-gas engine, the time-until-threshold calculation and this help system, were drafted with Claude Code at Dr Shafer's direction and verified by the test suite; each such file records this in a provenance header.

## STANPUMP and the first generation of TCI

STANPUMP was one of several programs that, in the late 1980s and 1990s, used pharmacokinetic models to control infusion pumps, and their authors exchanged concepts and algorithms freely:

- **Donald R. Stanski** and Steven Shafer, Stanford (STANPUMP). Stanski's laboratory also produced the fentanyl and alfentanil kinetics (with **Jeffrey C. Scott**) and, with Lewis Sheiner, the effect-compartment model itself.
- **Jürgen Schüttler** and **Helmut Schwilden**, University of Bonn (CATIA).
- **Martin Ausems** and **Carl C. Hug**, University of Leiden and Emory (TIAC).
- **Joseph G. Reves** and **J. Michael Alvis**, University of Alabama (CACI), and **James R. Jacobs** and Reves at Duke (CACI II).
- **Johan F. Coetzee** and Pina, Stellenbosch University (STELPUMP).
- **Tom De Smet** and **Michel M. R. F. Struys**, University of Ghent (RUGLOOP).

Struys and colleagues reviewed this history in *The History of Target-Controlled Infusion* (*Anesth Analg* 2016;122:56-69). See [From STANPUMP to stanpumpR](help:history).

## The pharmacokinetic models

| Drug | Investigators | What is used |
|---|---|---|
| propofol | **Douglas J. Eleveld** and colleagues, Groningen | The 2018 general-purpose propofol model, with its fat-free-mass, maturation and ageing covariates. **Thomas W. Schnider**'s 1999 time to peak effect gives ke0; his 1998 kinetic model is in the file but superseded. |
| remifentanil | Eleveld and colleagues (2017); **Tae Kyun Kim**, **Talmage D. Egan** and colleagues (2017); **Charles F. Minto**, Schnider, Shafer and colleagues (1997) | Eleveld's allometric model below BMI 30 and Kim's obesity model at and above it. Minto's 1997 model, the basis of STANPUMP's and most pumps' remifentanil kinetics, is kept in the file but not computed. |
| fentanyl, alfentanil | Scott and Stanski (1987); Shafer, **John R. Varvel** and colleagues (1990) | Alfentanil's parameters are Scott and Stanski's. Fentanyl's are the pooled analysis of Shafer and Varvel, scaled allometrically, with Scott and Stanski cited. |
| sufentanil | **Elisabeth Gepts** and colleagues (1995) | Three-compartment kinetics in surgical patients |
| morphine | **Jörn Lötsch** and colleagues (2002) | Three-compartment kinetics, weight-scaled V1 |
| pethidine | **Sven Björkman** (2003) | A physiologically based model, recast as compartments |
| hydromorphone | **David R. Drover** and colleagues (2002) | Intravenous kinetics; the oral and other routes are provisional |
| methadone | **Charles E. Inturrisi** and colleagues (1987) | Kinetics in patients with cancer pain |
| ketamine | **Edward F. Domino** and colleagues (1984) | Kinetics in volunteers |
| dexmedetomidine | **Jeffrey B. Dyck** and colleagues (1993); **Athena F. Zuppa** and colleagues (2019) | Adult kinetics; the infant model with cardiopulmonary-bypass parameters |
| midazolam | **Diane R. Mould** and colleagues (1995) | Three-compartment kinetics |
| etomidate | **John R. Arden** and colleagues (1986) | Kinetics in patients, including the elderly |
| lidocaine | Schnider and colleagues (1996) | Two-compartment kinetics during an infusion |
| rocuronium | **Bertrand Plaud** and colleagues (1995); **Luis I. Cortínez** and colleagues (2007) | Kinetics; time to peak effect |
| naloxone | **Theodoros Papathanasiou** and colleagues (2019) | Kinetics after intravenous and intranasal dosing |
| oxytocin | **James C. Eisenach** (unpublished); Tanaka and colleagues | Human kinetics from unpublished data; a rat model |
| oxycodone | **Marko Lamminsalo** and colleagues (2019); **Jaap W. Mandema**; **Anne E. Olesen**; Kokki | Intravenous kinetics; the absorption and MEAC chosen to match their observations |
| oliceridine | **Albert Dahan** and colleagues (2020) | Two-compartment kinetics and the respiratory end point used for MEAC |
| remimazolam | Eleveld and colleagues (2025) | Kinetics with size, age and sex covariates |

## Pharmacodynamics and methods

- **Lewis B. Sheiner**, Stanski and colleagues (1979): the effect compartment.
- Shafer and Varvel (1991), Minto and colleagues (2003): time to peak effect as the model-independent way to carry ke0.
- **Keith M. Gregg** and Shafer (1992): the effect-site targeting algorithm, implemented in STANPUMP and in the target-controlled infusion now in development.
- **James M. Bailey** and Shafer (1991): the plasma targeting algorithm.
- **Michael A. Hughes**, **Peter S. A. Glass** and James Jacobs (1992): the context-sensitive half-time.
- **Thomas W. Bouillon** and colleagues (2004): the propofol-remifentanil response surface.
- **Kay L. Austin**, **John V. Stapleton**, **Laurence E. Mather** (1980) and **Geoffrey K. Gourlay** and colleagues (1988): the minimum effective analgesic concentration.
- **W. P. T. James** (1976): lean body mass. **Hesham S. Al-Sallami**, **Nick Holford**, **Stephen Duffull** and colleagues (2015): fat-free mass. **Brian J. Anderson** and Holford (2008): allometry and maturation.
- **Brunner**, **Katoh**, **Lang**, **Westmoreland**, **Sebel** and their colleagues (1992 to 1999): the opioid reduction of MAC that the approximate interaction model is fitted to.

## The inhaled anesthetics

- **James H. Philip**, Brigham and Women's Hospital and Harvard Medical School: Gas Man®, whose structure and parameters the inhaled-gas engine follows and against which it is validated.
- **William W. Mapleson** (1996): the decline of MAC with age.
- **Edmond I. Eger II**: MAC itself, and the nitrogen estimate that flags Gas Man's value.
- **Jan F. A. Hendrickx**, **Hendrikus J. M. Lemmens** and Shafer (2006): tissue volumes and blood flows in the four-compartment gas model.
- **Jeffrey M. Feldman**, **Samsun Lampotang** and Hendrickx (2022): the rule that rebreathing stops when fresh gas flow reaches minute ventilation.

## Becoming a maintainer

It is hoped that each drug in the library will eventually be maintained by an investigator who keeps its pharmacokinetics up to date. If that could be you, see [Contributing a drug or a model](help:contributing).
