---
file: NJ_dailymed-38a8b23a.pdf
pmid: ""
doi: ""
citation: "BREVIBLOC (esmolol hydrochloride) injection, for intravenous use. Prescribing information (Baxter Healthcare), DailyMed SPL setid 38a8b23a-fca8-45f4-a09a-9c8b21f522cd, repackaged by Henry Schein (100 mg/10 mL vial, NDC 0404-9825-10), NDA 019386; Highlights revised 6/2023, SPL revised 8/2026; retrieved 2026-10-21."
kind: label
drugs: [esmolol]
source_read: "text layer, all 19 PDF pages: Highlights and contents (p1-2), 1 Indications (p2-3), 2 Dosage and Administration (p3-4), 5 Warnings (p6-10), 8 Use in Specific Populations (p10-11), 10 Overdosage (p11-12), 11 Description, 12.1-12.3 Clinical Pharmacology (p13-15), 14 Clinical Studies (p15-16), 16 How Supplied (p16-17); page-to-text mapping checked with pypdfium2 for p3, p14, p15. No PK table in the label"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Brevibloc (esmolol) prescribing information, DailyMed SPL 38a8b23a

## Study
A US label (Henry Schein repackaging of the Baxter 100 mg/10 mL vial; the PI also describes the 2500 mg/250 mL and
2000 mg/100 mL premixed bags). Indications (1.1, 1.2): rapid control of ventricular rate in AF/flutter and
noncompensatory sinus tachycardia; short-term treatment of intraoperative and postoperative tachycardia **and
hypertension**.

Dosing (2.1-2.2, PDF p3):
- SVT / sinus tachycardia: optional 500 mcg/kg over 1 min, then 50 mcg/kg/min for 4 min, titrated; maintenance 50-200
  mcg/kg/min (as low as 25); above 200 adds little HR effect. Without loading, PK and PD steady state in about 30 min.
  Up to 48 h.
- Perioperative, immediate control: 1 mg/kg bolus over 30 s, then 150 mcg/kg/min if needed. Gradual control: 500
  mcg/kg over 1 min, then 50 mcg/kg/min for 4 min. Maximum 200 (tachycardia) or 250-300 mcg/kg/min (hypertension);
  above 300 not studied.

## Model and parameters
12.3 Pharmacokinetics (PDF p14-15), no table, no compartmental parameters:
- Hydrolysed by red-cell cytosolic esterases (not plasma cholinesterase). Total body clearance about 20 L/kg/h
  (derived: 333 mL/kg/min), greater than cardiac output. Distribution t1/2 about 2 min, elimination t1/2 about 9 min.
- With a loading dose, steady-state blood levels within 5 min for 50-300 mcg/kg/min; about 30 min without. Linear,
  dose-independent over that range. Under 2% excreted unchanged; acid metabolite 73-88% of dose in urine in 24 h.
- Acid metabolite: t1/2 about 3.7 h, about 48 h in ESRD (12-fold), peak doubled in ESRD; parent levels in ESRD similar
  after a 4-h 150 mcg/kg infusion. Protein binding 55% (metabolite 10%). Methanol near endogenous levels.

```yaml
parameters:
  - {name: CL, value: 20, unit: L/kg/h, source: "12.3, PDF p15", note: "about; = 333 mL/kg/min (derived)"}
  - {name: t_half_distribution, value: 2, unit: min, source: "12.3, PDF p15", note: about}
  - {name: t_half_elimination, value: 9, unit: min, source: "12.3, PDF p15; 10.2; 11; 12.1", note: about}
  - {name: protein_binding, value: 0.55, unit: fraction, source: "12.3, PDF p15", note: "human plasma protein"}
  - {name: t_half_acid_metabolite, value: 3.7, unit: h, source: "12.3, PDF p15", note: "about; 48 h in ESRD"}
```

## Pharmacodynamics and end points
12.2 (PDF p14): HR reduction at rest and on exercise, attenuation of isoproterenol tachycardia; blood levels correlate
with the extent of blockade; substantial recovery 10-20 min after stopping; haemodynamics back to baseline 30 min after
stopping. 5.12: HR rises moderately above pretreatment 30 min after discontinuation. 14: average effective dose about
100 mcg/kg/min in SVT trials; hypotension in 20-50% of patients. No ke0, time to peak effect or EC50.

## Covariates
8.6 hepatic impairment: no precautions (RBC metabolism). 8.7 renal: no adjustment for 150 mcg/kg/min over 4 h; no data
beyond. 8.4: paediatric safety not established. 8.5: insufficient patients 65 and over.

## Caveats
Descriptive values only ("about"); blood sampling site not stated; the metabolite half-life (3.7 h) differs from
Yacobi 1983's 223 min (both in normal subjects, different studies).

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:49`: esmolol catalogue row, `official_label_descriptive`,
  `descriptive_only`, matrix blood.

## Verification against stanpumpR
- Row 49 identity (esmolol, IV, injection for infusion, analyte esmolol, matrix blood, label source): **matches** (the
  label speaks of blood levels and RBC esterase metabolism).
- Row 49's limitations text is not about the label: it describes the separate Sum 1983 record (ill-conditioned) and the
  exercise EC50 infusion-rate study, and says nothing of the de Bruijn 1987 model now implemented as `esmolol` under
  Vasoactive and resuscitation (`vasoactiveRegistry.csv:79`). Not a transcription error; possibly stale. The label's
  CL (about 333 mL/kg/min) and 2 / 9 min half-lives are close to Sum's awake volunteers, not de Bruijn's 128 mL/kg/min.
- Also supports `docs/antihypertensives.md:1291` (esmolol's label covers pressure as well as rate): section 1.2 and
  2.2 (maintenance up to 300 mcg/kg/min for hypertension). **matches**.
