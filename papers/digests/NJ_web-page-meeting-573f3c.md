---
file: NJ_web-page-meeting-573f3c.pdf
pmid: null
doi: null
citation: "Gaohua L, Chetty M, Jamei M, Rostami-Hodjegan A. Application of the Simcyp PBPK/PD model to simulate PK and PD of nifedipine in Japanese and Caucasian populations. Poster, PAGE meeting 2012 (Simcyp Ltd, Sheffield and University of Manchester). https://www.page-meeting.org/pdf_assets/4374-GL%202012%20PAGE%20nifedipine.pdf"
kind: other
drugs: []
source_read: "page image (the single poster page rendered; Table 1 read on a zoomed crop) plus text layer. The text layer scrambles Table 1: Kon and Koff are shifted down one row and the Rtotal*Slope/Rmax/N/tau values are misaligned. Table 1 is taken from the image."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Gaohua 2012 (PAGE poster): Simcyp PBPK/PD simulation of nifedipine and systolic pressure

## Study
A one-page conference poster. The PDF metadata give PowerPoint 2007, created 2012-05-29, and the URL names
"2012 PAGE". This is a simulation study, not a fit: Simcyp Simulator V11, Mini-PBPK model, 100 virtual subjects
per population ("Virtual subjects (n=100)", Figures 2-3). The aim was to see whether Simcyp's PD modules
reproduce published nifedipine PK and systolic blood pressure (SBP) data in Japanese and Caucasian populations.
- The poster assumes that PK and PD in healthy virtual populations equal those in hypertensive patients.
- The PK parameters are Simcyp defaults. The PD parameters are "collated from" Shimada et al., Biol Pharm Bull
  1996;19:430-437.
- Comparison data are clinical studies in essential hypertensive patients that Shimada did not use: Japanese,
  Takekoshi 1981 (n=14) and Shimada 1996 (n=5), Figure 2; Caucasian, Hirasawa 1985 (n=10), Figure 3.
- **Dose and formulation are not stated** anywhere on the poster. The route is oral (Figure 1: "Nifedipine Oral
  dose").
- No assay, no fitting software beyond Simcyp, and no demographics are given.

## Model and parameters
Structure (Figure 1): oral dose, then fa and ka into the gut, gut-wall metabolism (Fg), the portal vein, the liver
(CYP3A4 and CYP3A5 Vmax/Km), and a systemic compartment. An effect compartment (keo) drives one of three PD
models: (1) a Hill function (Emax, EC50, n); (2) empirical transduction, with receptor binding (Kon, Koff) and
Rtotal*Slope; (3) operational transduction (Rmax, N, tau as a function of efficacy and Rtotal).

Table 1 (poster, right-hand lower panel; read from the image). The header reads "Parameters0.0", an artefact on
the poster.
```yaml
parameters:
  - {name: fa, value: 1, unit: "-", source: "Table 1", note: "CV 30%; Simcyp default"}
  - {name: ka, value: 3.67, unit: 1/h, source: "Table 1", note: "CV 30%; Simcyp default"}
  - {name: Fg, value: 0.68, unit: "-", source: "Table 1", note: "Simcyp default"}
  - {name: Vss, value: 0.57, unit: L/kg, source: "Table 1", note: "CV 30%; Simcyp default"}
  - {name: Vmax-3A4, value: 22, unit: "uL/min/mg as printed", source: "Table 1", note: "Simcyp default; the printed unit is a clearance unit, not a rate"}
  - {name: Km-3A4, value: 10.95, unit: uM, source: "Table 1", note: "Simcyp default"}
  - {name: Vmax-3A5, value: 3.5, unit: "uL/min/mg as printed", source: "Table 1", note: "Simcyp default"}
  - {name: Km-3A5, value: 31.9, unit: uM, source: "Table 1", note: "Simcyp default"}
  - {name: keo, value: 0.88, unit: 1/h, source: "Table 1", note: "CV 32%; Shimada et al."}
  - {name: Emax, value: -35, unit: mmHg, source: "Table 1", note: "CV 14%; Shimada et al.; change in SBP, negative = fall"}
  - {name: EC50, value: 0.035, unit: uM, source: "Table 1", note: "CV 49%; Shimada et al."}
  - {name: n (Hill), value: 1, unit: "-", source: "Table 1", note: "Shimada et al."}
  - {name: Kon, value: 19, unit: "1/um/h as printed", source: "Table 1", note: "CV 40%; Shimada et al."}
  - {name: Koff, value: 0.47, unit: 1/h, source: "Table 1", note: "CV 36%; Shimada et al."}
  - {name: Kd, value: 0.025, unit: uM, source: "Table 1", note: "= Koff/Kon; Shimada et al."}
  - {name: Rtotal*Slope, value: -33, unit: mmHg, source: "Table 1", note: "CV 10%; Shimada et al."}
  - {name: Rmax, value: -33, unit: mmHg, source: "Table 1", note: "CV 10%; Shimada et al."}
  - {name: N (transducer), value: 1, unit: "-", source: "Table 1", note: "Shimada et al."}
  - {name: tau, value: 6, unit: "-", source: "Table 1", note: "CV 30%; assumed"}
```
Derived checks:
- Kd = 0.47 / 19 = 0.0247 uM, which matches the printed 0.025.
- t1/2 of keo = ln 2 / 0.88 = 0.79 h, about 47 min.
- EC50 0.035 uM = 35 nmol/L, about 12 ng/mL at nifedipine's 346.3 g/mol. The molecular weight is mine, not the
  poster's.
- "CV" is not defined. For the Simcyp defaults it is presumably population variability; for the Shimada values
  the poster does not say.

Results are given in figures only, with no tabulated numbers:
- Figure 2 (Japanese): mean simulated plasma concentration peaks near 100 ng/mL at about 1 h, and individual
  curves reach about 300 ng/mL. Mean SBP falls by about 28-30 mmHg under all three PD models.
- Figure 3 (Caucasian): a mean peak near 90 ng/mL and an SBP fall of about 25-27 mmHg.
These are read from the figures and are approximate.

## Pharmacodynamics and end points
End point: change in systolic blood pressure (mmHg) from baseline. The effect compartment keo is 0.88 /h. The Hill
model has Emax -35 mmHg, EC50 0.035 uM and n 1; Table 1 also lists receptor-binding and transduction parameters,
all "Shimada et al.". The authors say that adding an effect compartment improved the PD prediction. They call the
operational model more mechanistic but report no statistic comparing the models. The poster gives no therapeutic
or toxic concentration.

## Covariates
None modelled explicitly. Population differences, Japanese against Caucasian, come only from Simcyp's built-in
system parameters (blood flows, enzyme abundance).

## Caveats
- The PD values are a secondary transcription of Shimada 1996. The poster does not reproduce that paper's tables
  or say which formulation or population Shimada fitted.
- No dose, formulation, n per comparison arm beyond the figure legends, or goodness-of-fit statistic is given.
- The healthy virtual subjects are compared with data from hypertensive patients, under the assumption that their
  PK and PD are equal. The authors themselves note that healthy volunteers show no consistent PD effect.
- Units are printed loosely: the "Vmax" values carry a clearance unit (uL/min/mg), and Kon reads "1/um/h".

## Used by stanpumpR
The context file names `inst/extdata/antihypertensiveParameters.csv:121-122`. At 8baa733 the rows are at
**lines 138-139** (located by content). Both rows belong to model
`nifedipine__unspecified__oral__essential_hypertension__sbp__shimada1996`, with `source_url` set to this poster,
and are generated by `data-raw/antihypertensiveRegistry.R:1818-1829`. The model's registry row
(`inst/extdata/antihypertensiveRegistry.csv:96`) points to Shimada 1996 on J-STAGE. Its limitations text quotes
"rate constant 0.88 /h, source Emax -35 mmHg, EC50 0.035 microM, Hill 1" as a secondary transcription from a
conference poster. The model is gated: `bp_simulation_enabled` FALSE, status `requires_source_extraction`.

## Verification against stanpumpR
- `antihypertensiveParameters.csv:138`, EC50 0.035 micromol/L, "Equivalently 35 nmol/L": Table 1 prints 0.035 uM
  (CV 49%), and the conversion is correct. The note that the formulation is not identified is correct: the poster
  names none. **matches**
- `antihypertensiveParameters.csv:139`, "Emax (source sign)" -35 mmHg: Table 1 prints -35 mmHg (CV 14%), a change
  in SBP. The note's warning is consistent with the poster's sign convention: the drop should be stored as
  35 mmHg with direction "decrease". **matches**. The note's "later -50 mmHg figure was an overdose scenario" is
  not on this poster and cannot be verified from it.
- `antihypertensiveRegistry.csv:96`, limitations: keo 0.88 /h, Emax -35, EC50 0.035 uM and Hill 1 all match
  Table 1, and "secondary transcription from a conference poster" describes this document correctly. The row's
  population ("essential hypertension") is not stated as Shimada's population on the poster. The poster only says
  that its comparison data come from essential hypertensive patients and assumes the same PD; **cannot verify**
  from this source.
- No implemented drug uses these numbers (nifedipine has no `R/drugs_` file at 8baa733). Nothing else to check.
