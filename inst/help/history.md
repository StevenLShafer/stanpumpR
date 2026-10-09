Target-controlled infusion (TCI) gives an intravenous drug to reach and hold a chosen concentration in the plasma or at the site of drug effect. The clinician sets the concentration, as with a vaporizer. A computer works out from a pharmacokinetic model the boluses and infusion rates needed, allowing for the drug already taken up by the tissues, and drives a pump to deliver them. It began in Bonn in 1979, went through a generation of research programs in the 1980s and 1990s, of which STANPUMP was one, and became a commercial product in 1996. stanpumpR is a descendant of that work that simulates the pump rather than driving one.

The history is told in three companion reviews published together in 2016, on which this page draws:

- Struys MMRF, De Smet T, Glen JB, Vereecke HEM, Absalom AR, Schnider TW. The history of target-controlled infusion. *Anesth Analg* 2016;122:56-69.
- Absalom AR, Glen JB, Zwart GJC, Schnider TW, Struys MMRF. Target-controlled infusion: a mature technology. *Anesth Analg* 2016;122:70-78.
- Schnider TW, Minto CF, Struys MMRF, Absalom AR. The safety of target-controlled infusions. *Anesth Analg* 2016;122:79-85.

## Before the computer

Widmark described the accumulation of a drug during a constant infusion in 1919, for a single compartment. In 1968 Krüger-Thiemer showed how to calculate the infusion rates that reach and hold a steady blood concentration of a drug described by two or more compartments, and Vaughan and Tucker applied his method to lidocaine. Every anesthetic accumulates in the tissues, so a fixed infusion rate gives a concentration that keeps rising; the calculation that corrects for this is simple in principle, but too laborious to do by hand at the bedside.

## CATIA: Bonn, 1979

Jürgen Schüttler and Helmut Schwilden gave the first target-controlled infusion in Bonn on 1 May 1979. In 1981 Schwilden published a general method for calculating the dosage scheme in linear pharmacokinetics, the foundation on which TCI was built. In 1983 Schüttler, Schwilden and Stoeckel reported their first clinical experience with **CATIA** (computer-assisted total intravenous anesthesia), the first practical TCI system: plasma-targeted etomidate at 0.3 mcg/mL and alfentanil at 0.45 mcg/mL to induce and maintain anesthesia, with adequate effect and short recovery.

CATIA used the **BET scheme**: a **B**olus of the target concentration times the central volume, to fill the central compartment; an infusion equal to the **E**limination rate, the target times the clearance; and an exponentially declining infusion to replace the drug **T**ransferred to the peripheral compartments, falling to zero as they equilibrate. BET has two limits. It can target only the plasma, and it assumes there is no drug on board, so it cannot be used to titrate up and down during an anesthetic.

In 1985 the Bonn group used CATIA to give etomidate at linearly increasing predicted concentrations while measuring its effect on the brain, the first use of TCI to measure a drug's pharmacodynamics. In 1988 they extended it to propofol and alfentanil, and showed smooth induction, stable hemodynamics, good intraoperative titration and rapid recovery. The group went on to write IVA-SIM and, in Erlangen, IVFEED, which target the plasma or the effect site for several drugs and can also drive a linearly increasing target.

## The research programs

CATIA inspired a series of research programs, written by investigators who knew one another and exchanged concepts and algorithms freely. Their authors gave them idiosyncratic names, until in 1997 they agreed on *target-controlled infusion* as the generic term (Glass, Glen, Kenny, Schüttler and Shafer, *Anesthesiology* 1997;86:1430). Absalom and colleagues found 14 such systems, used in 559 published studies up to 2014:

| System | Developers | Institution | Published | Studies |
|---|---|---|---|---|
| CATIA, IVA-SIM | Schwilden, Schüttler, Stoeckel | Bonn | 1982 to 2014 | 24 |
| TIAC | Ausems | Leiden | 1985 to 1992 | 10 |
| CACI, CACI II | Alvis, Jacobs, Reves | Alabama, Duke | 1985 to 2001 | 18 |
| unnamed | Tackley | Bristol Royal Infirmary | 1989 to 1999 | 2 |
| MINA, Infusion Toolbox | Barvais | Université Libre de Bruxelles | 1989 to 2006 | 15 |
| Diprifusor prototypes | Kenny, White | Glasgow | 1991 to 2013 | 43 |
| STANPUMP | Shafer | Stanford | 1990 to 2014 | 268 |
| Leiden platform | Engbers | Leiden | 1992 to 2003 | 13 |
| PAMO | Viviand | Hôpital Nord | 1997 to 2005 | 9 |
| RUGLOOP | Struys, De Smet | Ghent | 1998 to 2014 | 93 |
| STELPUMP | Coetzee | Stellenbosch | 1999 to 2012 | 51 |
| Bonn platform | Hoeft, Brauer | Bonn | 2006 | 1 |
| AnestFusor | Stutzin, Brinckmann, Muñoz | University of Chile | 2009 to 2014 | 7 |
| Asan Pump | Noh | Asan | 2010 to 2013 | 5 |

A few threads run through them.

- **Leiden.** Martin Ausems and Carl Hug, with support from Janssen, built TIAC to test their alfentanil kinetics under plasma-targeted infusion. Measured concentrations differed from predicted by 22 to 32 per cent between patients. The same spread appeared with boluses and manual infusions, showing that it was the biology, not the infusion method. Compared with repeated boluses, TCI gave steadier concentrations, fewer responses to stimulation and more stable hemodynamics. Frank Engbers later built portable two-pump TCI systems on an Atari Portfolio and a Psion palmtop.
- **Alabama and Duke.** Jerry Reves saw the Bonn work as a visiting professor. With J. Michael Alvis at Alabama he wrote CACI, in Pascal on an Apple II Plus driving an IMED 929 pump, to titrate fentanyl and sufentanil in cardiac surgery. At Duke, with James Jacobs, he built CACI II, with models for fentanyl, alfentanil, sufentanil, midazolam and propofol.
- **Glasgow and Bristol.** Tackley and colleagues at Bristol built a BET propofol system. Martin White and Gavin Kenny at Glasgow built an Atari-controlled propofol system whose algorithms became those of the Diprifusor.
- **Stellenbosch, Ghent, Brussels, Santiago.** Johan Coetzee and Ralph Pina wrote STELPUMP in Turbo Pascal, one of the first with a graphical interface, and gave it away; it has been used especially widely in Asia. Tom De Smet and Michel Struys wrote RUGLOOP in C++ for Windows, named, like STANPUMP and STELPUMP, for its university, and the engine of their closed-loop propofol system. Luc Barvais in Brussels wrote TOOLBOX, and a group at the University of Chile wrote AnestFusor, later sold as ezFUSOR.

## STANPUMP

Donald Stanski spent a sabbatical in Leiden working on the TIAC studies. On returning to Stanford he recruited Steven Shafer to study pharmacokinetics and TCI. Shafer and colleagues first tested the CACI device by simulation (*Anesthesiology* 1988;68:261) and found limitations. Unable to use CACI II, Shafer wrote STANPUMP (STANford PUMP) in C, to run on any MS-DOS computer and drive a range of pumps (IMED 929, BARD Chronofusor, Harvard 22, Graseby 3400 and others). It was developed in the Stanski/Shafer laboratory from 1987 through 1997.

The early versions advanced the compartments numerically, step by step (Euler's method). The final version used an exact analytical solution of the three-compartment model.

- **Exact control of the plasma.** Jacobs published an analytical solution to the three-compartment model in 1988 and the exactly correct infusion algorithm in 1990. Bailey and Shafer published a simplified exact algorithm in 1991 (*IEEE Trans Biomed Eng* 1991;38:522).
- **Effect-site targeting.** Shafer and Gregg extended it to the effect site in 1992 (*J Pharmacokinet Biopharm* 1992;20:147). Their algorithm gives the bolus that makes the effect-site concentration peak exactly at the target, with no overshoot. Struys and colleagues describe this algorithm, with Bailey and Shafer's, as the basis of all TCI systems.

STANPUMP also served as a test bed for:

- carrying **time to peak effect** rather than ke0 between models (Shafer and Varvel, 1991), so that a pharmacodynamic study done with one kinetic model could be used with another;
- a real-time Bayesian update of the pharmacokinetic parameters from observations made during drug administration;
- output files ready for pharmacokinetic and pharmacodynamic model fitting, drug files read from disk, multiple syringe sizes and a batch mode for research. Its library included the hypnotics, opioids, benzodiazepines, local anesthetics and muscle relaxants, and kinetics for dogs, rats and horses as well as humans.

STANPUMP was placed in the public domain. It was used in 268 of the 559 studies that Absalom and colleagues counted, nearly half. Its principles, and its pharmacokinetic engine, were taken into later research systems and into the commercial open TCI pumps. The original C source is kept in the repository, in `Original Stanpump/Stanpump.zip`, with its documentation; see [What is in the repository](help:repository).

## The Diprifusor

Propofol was launched in Europe in 1986. In 1990 ICI Pharmaceuticals brought the research groups together to consider a commercial TCI system, and a symposium at the 1992 World Congress in The Hague convinced it to proceed. ICI, later Zeneca, chose not to build pumps. Instead it built the **Diprifusor** module and supplied it to pump makers, so that every pump delivered propofol in the same way. The module contained:

- the Marsh propofol model;
- White and Kenny's control algorithms, run on two processors, an exact solution on a 16-bit processor checked by an independent Euler approximation on an 8-bit processor;
- a reader for electronically tagged, prefilled Diprivan syringes, so that it could give nothing else.

The first pump containing it received the CE mark in 1996. TCI was launched commercially at the World Congress in Sydney that year, and the Diprifusor pumps from Graseby, Alaris and Fresenius, and later Terumo, followed. About 25,000 were sold, and approval came in more than 50 countries. Their two limits were that they could not target the effect site, which arrived after the design was settled, and that they would give only branded propofol.

## Open TCI

Once generic propofol appeared, clinicians wanted pumps that would take any syringe, give other drugs (remifentanil above all) and target the effect site. Carefusion and Fresenius launched the first "open" TCI pumps, beginning in 2002; second-generation pumps were first approved in 2003. They carry a library of published models for propofol, remifentanil, sufentanil, alfentanil and, in some, fentanyl and midazolam, in plasma and effect-site modes. More than 36,000 were sold from 2004 to 2013. By 2015 TCI was approved or available in at least 96 countries, and Absalom and colleagues estimated that about 2.6 million patients a year in Europe, and perhaps 5 million worldwide, received a drug by TCI. Their verdict was in their title: a mature technology.

## The United States

TCI is not approved in the United States. Discussions with the Food and Drug Administration began in 1993. Zeneca submitted the Diprifusor in 1995, the review moved between the drug and device divisions, and in 2001 the FDA declared it not approvable, holding the device responsible for the difference between predicted and measured concentrations. That difference is pharmacokinetic variability, which exists however a drug is given. AstraZeneca withdrew the application in 2004, nine years after submission. In the United States TCI is possible only with research software in studies approved by an institutional review board.

Two arguments since then bear on this. Hu, Horstman and Shafer proved that the variability of concentrations under TCI is necessarily less than after a bolus (*Anesthesiology* 2005;102:639). Schnider and colleagues proposed that a TCI device should be judged, like any pump, on whether it delivers the infusion profile its model specifies for a given target and patient, which can be verified on the bench, rather than on whether the patient's concentration matches the target.

## Safety

After about 20 years and millions of patients, Schnider and colleagues searched the literature, regulators' notices and manufacturers' reports. They found seven published reports, all of syringe or pump faults except one: a clinician who selected the propofol model for a pump containing remifentanil. Choosing the wrong model, a "TCI drug swap", is the one hazard specific to TCI, and is the counterpart of a syringe swap. The field safety notices concerned:

- the James lean-body-mass formula, which fails at high body mass index, used by the Schnider propofol and Minto remifentanil models; pumps were limited to a BMI below 35 in women and 42 in men;
- two implementations of the same Schnider model that gave different amounts of propofol during induction.

No report found an adverse event caused by the TCI algorithm. They concluded that TCI is at least as safe as a constant-rate infusion, with an upper 95 per cent bound on a serious TCI-related event of less than 1 in 7 million.

## stanpumpR

stanpumpR uses very little of the original STANPUMP code. Conceptually it is identical: an open-source program that makes complex pharmacokinetic algorithms available to support patient care, teaching and research. The difference is that **stanpumpR does not control drug administration**. It is a web-based simulator, written in R with the Shiny framework, that shows the expected concentrations from any dosing regimen.

The R package was begun by Steven Shafer in 2019. Dean Attali rebuilt it as a maintainable Shiny application with a test suite and automated deployment. In 2026 it gained an engine for the inhaled anesthetics following Gas Man®, validated by Richard Epstein. The same year STANPUMP's targeting algorithm returned as two dose-table units, *Plasma target* and *Effect site target*, which simulate a TCI pump using Shafer and Gregg's method; see [Target-controlled infusion](help:tci).

## Licence and use

stanpumpR may be freely downloaded and used without restriction for non-commercial purposes. It is hoped that it will encourage device manufacturers to develop the next generation of drug delivery systems and anesthesia information management systems; companies seeking to develop such systems should contact Dr Shafer for written permission before incorporating stanpumpR into a product.

## Gas Man

Gas Man® was developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School, and has taught a generation of anesthesiologists how the inhaled agents behave: a four-compartment model of uptake and distribution with a breathing circuit in front, animated on screen. stanpumpR's inhaled-gas engine follows its structure and parameters and departs from it only deliberately; see [The inhaled-gas engine](help:models/gas-engine).
