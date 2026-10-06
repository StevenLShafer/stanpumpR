## STANPUMP

STANPUMP, a portmanteau of "Stanford" and "Pump", was developed in the Stanski/Shafer laboratory at Stanford University from 1987 through 1997. It was one of many programs written in those years to control the delivery of intravenous anesthetics using pharmacokinetic principles: given a pharmacokinetic model and a target concentration, the program computed, every few seconds, the infusion rate that would reach and hold the target, and drove a syringe pump to deliver it. This is target-controlled infusion (TCI).

There was an active exchange of concepts and algorithms among the authors of these programs: Schüttler and Schwilden at Bonn (CATIA), Ausems and Hug at Leiden (TIAC), Reves and Alvis at Alabama (CACI), Jacobs and Reves at Duke (CACI II), Coetzee and Pina at Stellenbosch (STELPUMP), and De Smet and Struys at Ghent (RUGLOOP). Struys and colleagues reviewed this history: *The History of Target-Controlled Infusion*, *Anesth Analg* 2016;122:56-69.

STANPUMP was placed in the public domain. Its pharmacokinetic engine was incorporated into many of the commercially available TCI devices, where it is still used today. The original C source is kept in the repository, in `Original Stanpump/Stanpump.zip`, with its documentation; see [What is in the repository](help:repository).

## What STANPUMP contributed

- The **closed-form** approach to multi-compartment simulation that stanpumpR still uses: solve the eigenvalues once, then evaluate sums of exponentials, so that a pump could keep up in real time on the computers of 1990.
- The **effect-site targeting algorithm** of Shafer and Gregg (1992), which gives the bolus that makes the effect-site concentration peak exactly at the target, with no overshoot, and the plasma-targeting algorithm of Bailey and Shafer (1991).
- The practice of carrying **time to peak effect** rather than ke0 between models (Shafer and Varvel, 1991), so that a pharmacodynamic observation made with one kinetic model could be used with another.
- Drug files with the kinetics of many anesthetics, most of which are still in stanpumpR's library.

## stanpumpR

stanpumpR uses very little of the original STANPUMP code. Conceptually it is identical: an open-source program that makes complex pharmacokinetic algorithms available to support patient care, teaching and research. The difference is that **stanpumpR does not control drug administration**. It is a web-based simulator, written in R with the Shiny framework, that shows the expected concentrations from any dosing regimen.

The R package was begun by Steven Shafer in 2019. Dean Attali rebuilt it as a maintainable Shiny application with a test suite and automated deployment. In 2026 it gained an engine for the inhaled anesthetics following Gas Man®, validated by Richard Epstein, and the target-controlled infusion algorithm of the original STANPUMP is being returned to it as a dose-table unit; see [In development](help:in-development).

## Licence and use

stanpumpR may be freely downloaded and used without restriction for non-commercial purposes. It is hoped that it will encourage device manufacturers to develop the next generation of drug delivery systems and anesthesia information management systems; companies seeking to develop such systems should contact Dr Shafer for written permission before incorporating stanpumpR into a product.

## Gas Man

Gas Man® was developed by James H. Philip at Brigham and Women's Hospital and Harvard Medical School, and has taught a generation of anesthesiologists how the inhaled agents behave: a four-compartment model of uptake and distribution with a breathing circuit in front, animated on screen. stanpumpR's inhaled-gas engine follows its structure and parameters and departs from it only deliberately; see [The inhaled-gas engine](help:models/gas-engine).
