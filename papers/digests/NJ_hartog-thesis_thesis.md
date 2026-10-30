---
file: NJ_hartog-thesis_thesis.pdf
pmid: none
doi: none
citation: "den Hartog YM. Optimizing Perioperative Care for Primary Total Hip Arthroplasty Patients. PhD thesis, Erasmus University Rotterdam, defended 15 June 2016 (ISBN 978-94-6169-875-9). Chapter 8: den Hartog YM, Rahman RN, Vehmeijer SBW, Proost JH, Mathijssen NMC, van Dasselaar NT, Langendijk PNJ. Clinical pharmacokinetics of ropivacaine using local infiltration analgesia technique in total hip arthroplasty: a pilot study (submitted), pp. 115-125. https://repub.eur.nl/pub/93139/y-hartog-2.pdf"
kind: other
drugs: [ropivacaine]
source_read: partial - text layer; title pages, table of contents, and all of chapter 8 (pp. 115-125, abstract, methods, Tables 1-3, results, discussion). Chapters 1-7 and 9-12 (orthopaedic outcomes, not cited by the repo) were not digested. Table 2 is scrambled in the text layer (values shifted up one row); it is reconstructed below from the row order and the results text.
digest_by: Claude Opus 5.5, 2026-10-12
checked_against: stanpumpR master 7bc2719
verification: matches
---

# den Hartog 2016 (thesis ch. 8): Ropivacaine after local infiltration analgesia for total hip arthroplasty

## Study
- Prospective cohort pilot, n = 12 (7 men, 5 women) with hip osteoarthritis having primary THA (anterior
  approach programme). Exclusions (Table 1): amide allergy, BMI >= 40, liver failure (bilirubin > 34
  umol/L, albumin < 35 g/L), CYP1A2 or CYP3A4 inhibitors.
- Demographics (Table 2, reconstructed; mean (SD), range): age 68 (11) y, 47-85 (results text: 68.2, SD 10.9);
  BMI 26.9 (2.1) kg/m2, 22.1-30.3; eGFR (Cockcroft-Gault) 94 (34) mL/min, 50-154; total bilirubin 6.8 (1.7)
  umol/L, 5-10; albumin 37 (4) g/L, 30-42; ALAT 30 (22) U/L, 15-88. **Weight not reported.**
- Dose: one-shot LIA, ropivacaine 2 mg/mL + epinephrine 1 mg/mL mixed 99:1, three 50 mL syringes
  (periacetabular after reaming; gluteus minimus/medius and vastus lateralis after the femoral component;
  subcutaneous before closure). (derived: 150 mL x 0.99 x 2 mg/mL = about 297 mg ropivacaine; epinephrine
  about 10 mcg/mL.) The total mg dose is not stated in the chapter.
- Sampling: peripheral **venous** serum before infiltration and at 0.5, 1, 2, 4, 8, 12, 24, 36, 48 h after
  the first syringe (plus at discharge if before 48 h).
- Assay: **total** serum ropivacaine, reversed-phase HPLC with diode-array detection (Reinier de Graaf
  hospital pharmacy). LLOQ not given.
- Analysis: MwPharm 3.83, Iterative Two-Stage Bayesian. Disposition fixed from Simon et al. (ref 12,
  intravenous deuterium-labelled ropivacaine, group 3, patients over 60 y); absorption models compared by AIC.

## Model and parameters
Disposition (fixed, from Simon 2006 Table 2 after unit conversion; V2 and V3 derived by Hartog from Vss and
elimination half-life; p. 119), normalised to body weight:
```yaml
parameters:
  - {name: CL,   value: 18,   unit: L/h/70 kg, source: "p.119", note: "from Simon Table 2, group >60 y; assumed IIV CV 40%"}
  - {name: CL12, value: 58,   unit: L/h/70 kg, source: "p.119", note: "fast distribution clearance CLd(r); CV 40% assumed"}
  - {name: CL13, value: 24,   unit: L/h/70 kg, source: "p.119", note: "slow distribution clearance CLd(s); CV 40% assumed"}
  - {name: V1,   value: 0.10, unit: L/kg,      source: "p.119", note: "Vc from Simon; CV 30% assumed"}
  - {name: V2,   value: 0.18, unit: L/kg,      source: "p.119", note: "derived by Hartog from Vss and t1/2"}
  - {name: V3,   value: 0.41, unit: L/kg,      source: "p.119", note: "derived by Hartog from Vss and t1/2"}
  - {name: kd (Weibull rate),  value: 0.0394, unit: 1/h, source: "Table 3", note: "mean of 12 individual estimates, SD 0.0109; range 0.0298-0.0540"}
  - {name: n (Weibull shape),  value: 1.03,   unit: none, source: "Table 3", note: "mean, SD 0.18; range 0.75-1.19"}
  - {name: F, value: 1, unit: fraction, source: "p.120", note: "fixed; the F=1 model had the lowest AIC"}
  - {name: proportional residual error, value: 29, unit: "%", source: "p.120"}
  - {name: Cmax, value: 0.52, unit: mg/L, source: "p.120", note: "observed mean, SD 0.19, range 0.27-0.87"}
  - {name: Tmax, value: 6.0,  unit: h,    source: "p.120", note: "observed mean, SD 2.0, range 2.8-9.3"}
```
- AIC by absorption model (p. 120): Weibull -73, Gamma -19, first-order with lag -11, inverse Gaussian -6,
  zero-order with lag +30.
- The Weibull equation itself (whether kd multiplies t inside or outside the power) is not written out.
- Most patients showed a second concentration peak (Figure 1); authors attribute it to mobilisation and
  call the profile flip-flop.
- (derived: with n about 1, an absorption half-time of ln2/0.0394 = 17.6 h.)

## Pharmacodynamics and end points
No PK/PD model, ke0 or analgesic concentration. Toxicity threshold quoted from the literature (ref 18):
2.2 mg/L in venous serum for CNS/cardiovascular toxicity; no patient exceeded it. Not an opioid.

## Covariates
None tested. Authors mention CYP3A4/CYP1A2 variability and infiltration technique as possible sources of the
threefold Cmax range.

## Caveats
- n = 12, pilot; individual Bayesian estimates with disposition fixed from another study (Simon, arterial
  sampling, epidural patients) while Hartog sampled venous serum.
- IIV for the disposition parameters was assumed (40% / 30%), not taken from Simon.
- Weight not reported, so the per-kg parameters were applied to each patient's weight; the thesis gives no
  absolute 70 kg values other than the "/70 kg" clearances.
- Chapter marked "Submitted"; not peer reviewed in this form. TKA comparison relies on unpublished data.

## Used by stanpumpR
- R/drugs_ropivacaine.R:13,19 - source of the transcribed/completed Simon 2006 older-group parameters at 70 kg
  (Vc 7, Vp1 12.6, Vp2 28.7 L; CL 0.300, Q1 0.967, Q2 0.400 L/min); Vp1, Vp2 said to be derived by Hartog.
- R/drugs_ropivacaine.R:46 - periarticular hip infiltration Weibull (shape 1.03, rate 0.0394/h, half-time 17.6 h),
  with epinephrine 10 mcg/mL; described, not used.
- R/drugs_ropivacaine.R:79-80 - reference entry; :119 - reference string "as completed by Hartog".
- inst/help/drugs/ropivacaine.md:3 and :9 - help text of the same.

## Verification against stanpumpR
- **Disposition (R/drugs_ropivacaine.R:16-17, :101-106; help :3)**: 0.10 x 70 = 7.0 L, 0.18 x 70 = 12.6 L,
  0.41 x 70 = 28.7 L; 18/60 = 0.300, 58/60 = 0.9667, 24/60 = 0.400 L/min. Code cl2 = 0.9666666667. Per-kg to
  absolute and h to min conversions correct. **matches**.
- **"Hartog transcribed CL, Q1, Q2 and Vc and derived Vp1 and Vp2"** (:19): p. 119 says exactly this. **matches**.
- **"oldest group (61 years and over)"** (:12, help :3): Hartog says "patients over 60 years (group 3)". Same
  group; the ">=61" wording comes from Simon, not Hartog. **matches** (not contradicted).
- **Weibull shape 1.03, rate 0.0394/h** (:46-48): Table 3 population means. **matches**. Half-time 17.6 h is a
  derived value (ln2/kd, valid as n is about 1) and is labelled approximately in the help ("about 17.6 hours").
  **matches**; note the thesis does not print the Weibull equation, so the half-time is an interpretation.
- **"epinephrine 10 mcg/mL"** (:47): derived from 1 mg/mL mixed 99:1. **matches** (derived, not stated).
- **Weibull not used; F = 1 assumption** (:50-53): Hartog also fixed F = 1, but for hip infiltration; the repo
  correctly says no site-specific F has been measured. **matches**.
- Reference string (:115-119) and help (:9): consistent with the chapter. **matches**.
- No discrepancies found.
