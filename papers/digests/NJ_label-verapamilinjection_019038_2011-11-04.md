---
file: NJ_label-verapamilinjection_019038_2011-11-04.pdf
pmid: null
doi: null
citation: "CALAN (verapamil hydrochloride) for Intravenous Injection, G.D. Searle LLC, Division of Pfizer Inc. Prescribing information LAB-0421-2.0, Revised October 2011; NDA 019038 (Drugs@FDA file 2011/019038s004lbl.pdf, Reference ID 3038095)."
kind: label
drugs: [verapamil]
source_read: "text layer, all 9 PDF pages (one-column old-format label, no TOC): Description p1, Clinical Pharmacology (mechanism, hemodynamics, pharmacokinetics) p1-2, Indications p2-3, Contraindications p3, Warnings p3-5, Precautions incl. Pregnancy, Labor, Nursing, Pediatric p5-7, Adverse Reactions p7-8, Overdosage p8, Dosage and Administration p8-9, How Supplied p9; page image of p9"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# CALAN injection label (NDA 019038), October 2011

## Study
US prescribing information for the brand intravenous verapamil, CALAN, distributed by G.D. Searle LLC, Division of
Pfizer (PDF p9, rendered). Every page carries FDA's "may not be the latest approved" banner. Sterile solution of
verapamil HCl 2.5 mg/mL with NaCl 8.5 mg/mL, pH 4.1-6.0; 5 mg (2 mL) ampules, vials and syringes, 10 mg (4 mL)
vials and syringes. Verapamil HCl C27H38N2O4·HCl, MW 491.08 (p1). The label has no numbered sections, no Geriatric
Use section and no PK table.

## Model and parameters
Clinical Pharmacology, Pharmacokinetics (PDF p2): rapidly metabolised; after IV infusion elimination is
bi-exponential, distribution half-life about 4 min and terminal half-life 2-5 h. Oral data quoted: 12 metabolites,
mostly trace; about 70% of a dose in urine and 16% or more in faeces within 5 days; 3-4% unchanged. Highly
protein-bound (Precautions, p6, no number). Cimetidine has no effect on IV kinetics. No volumes or clearances.

```yaml
parameters:
  - {name: t_half_distribution, value: 4, unit: min, source: "Clinical Pharmacology, PDF p2", note: "about; IV"}
  - {name: t_half_terminal, value: "2-5", unit: h, source: "Clinical Pharmacology, PDF p2", note: "IV, range"}
  - {name: time_to_peak_therapeutic_effect, value: "3-5", unit: min, source: "Hemodynamics, PDF p2", note: "after a bolus injection; effect not named"}
  - {name: duration_single_injection, value: "30-60", unit: min, source: "Indications, PDF p3", note: "when conversion to sinus rhythm does not occur"}
```

## Pharmacodynamics and end points
- Hemodynamics paragraph (PDF p2): peak therapeutic effects occur within 3 to 5 minutes after a bolus injection.
  The sentence follows statements on afterload, contractility and heart failure and precedes one on the transient
  fall in arterial pressure with 5-10 mg; it does not name the effect (rate, conversion or pressure).
- Efficacy (PDF p2-3): about 60% of patients with SVT converted within 10 min in US controlled studies (about 80% in
  uncontrolled reports); about 70% of patients with AF or flutter and a fast ventricular rate had a heart-rate fall
  of at least 20%; conversion of AF/flutter about 10%, similar to placebo; a single injection lasts 30-60 min when it
  does not convert.
- Adverse: symptomatic hypotension 1.5%, bradycardia 1.2%, severe tachycardia 1.0% (324 patients); SBP < 90 and/or
  DBP < 60 mmHg in 5-10% (SVT) and about 10% (AF/flutter).
- No concentration-effect relation, ke0 or therapeutic range.

## Dosing (Dosage and Administration, PDF p8-9)
Slow IV injection over at least 2 min (at least 3 min in older patients) with continuous ECG and BP monitoring.
Adult: 5-10 mg (0.075-0.15 mg/kg), repeat 10 mg (0.15 mg/kg) at 30 min if needed. Paediatric 0-1 year 0.1-0.2 mg/kg
(usual 0.75-2 mg); 1-15 years 0.1-0.3 mg/kg (usual 2-5 mg, not over 5 mg first dose, 10 mg repeat), repeat at 30
min. Precipitates above pH 6.0.

## Covariates
Specific populations (Warnings, PDF p5; Precautions, p6-7): hepatic and renal failure should not increase the effect of a single dose but may
prolong it; repeated doses may accumulate. Pregnancy Category C; crosses the placenta; excreted in milk. Paediatric:
uncontrolled experience in more than 250 patients, rare severe haemodynamic events in neonates and infants.

## Caveats
An old-format brand label from 2011; the PK is two summary half-lives. The "3 to 5 minutes" is a clinical statement
without a stated end point, study or n.

## Used by stanpumpR
- `R/drugs_verapamil.R:81-91` (header), `:131` (`VERAPAMIL_TPEAK <- 4`), `:157-158` (reference string): tPeak 4 min,
  the midpoint of the label's 3-5 min to peak therapeutic effect, taken as heart rate.
- `inst/help/drugs/verapamil.md:19`; `inst/help/models/vasoactive.md:3` (verapamil 4 minutes).
- `tests/testthat/test-drugs-verapamil.R:59-75`: tPeak 4, effect-site peak at 4 min after a bolus.
- Also `inst/extdata/vasoactiveParameters.csv:122` (from `data-raw/vasoactiveRegistry.R:865`): tPeak 4 min,
  evidence "abstract", URL labeling.pfizer.com ShowLabeling id=4652, note that the label text was seen in a search
  excerpt, not read in full.

## Verification against stanpumpR
- **Quoted sentence** (`R/drugs_verapamil.R:83-84`, help `:19`): the quotation is verbatim on PDF p2. **Matches.**
- **tPeak 4 min** (`:131`, test `:65`, `:70-74`, vasoactive.md `:3`): midpoint of 3-5 min, a choice the repo states
  as its own. **Matches.**
- **End point** (`:85-86`, "the slowing of the ventricular rate and termination of PSVT"; "Heart rate is the
  endpoint used here"): the label does not say which effect peaks at 3-5 min; the attribution to rate slowing is the
  repo's reading. Not contradicted, but an inference, and the archived label is the CALAN brand label from 2011
  rather than the Pfizer labeling page the CSV links; I could not confirm that page's text is identical.
- **Dose basis** (`:61-65`, help `:13`): the injection is labelled as verapamil HCl 2.5 mg/mL. **Matches.**
- The CSV's "label text ... not read in full" and evidence "abstract" are now superseded by this full label, which
  confirms the sentence.
