---
file: NJ_label-nimodipine_018869_2006-01-27.pdf
pmid: null
doi: null
citation: "FDA. NIMOTOP (nimodipine) capsules for oral use, prescribing information. NDA 18-869/S-014, Bayer Pharmaceuticals; label text dated 12/05, electronically signed 1/20/2006 (Drugs@FDA letter date 2006-01-27)."
kind: label
drugs: [nimodipine]
source_read: "text layer, whole label (8 PDF pages: Description and Clinical Pharmacology p. 1-2, Clinical Trials p. 2-3, Indications, Contraindications, Warnings and Precautions p. 3-4, Adverse Reactions p. 5, Overdosage and Dosage and Administration p. 6, distributor p. 7, signature page p. 8); adverse-reaction table on p. 5 from the rendered page image"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# FDA 2006: Nimotop (nimodipine) capsules label

## Study
An old-format (pre-PLR) US label for Nimotop 30 mg soft gelatin capsules, oral only, NDA 18-869 supplement 014. It is the product named in the file: identity confirmed on PDF p. 1 (NDA 18-869/S-014 header) and p. 8 (the signature page, signed 1/20/2006). No sections 12.x numbering; the PK is a paragraph under Clinical Pharmacology.

- **Indication** (p. 3): improvement of neurological outcome by reducing the incidence and severity of ischaemic deficits in subarachnoid haemorrhage (SAH) from ruptured intracranial berry aneurysm, Hunt and Hess grades I-V. No other indication; hypertension is not one.
- **Dosage** (p. 6): 60 mg (two 30 mg capsules) orally every 4 h for 21 consecutive days, starting within 96 h of the SAH, preferably at least 1 h before or 2 h after meals. Capsule contents may be drawn up with an 18 G needle and given by nasogastric tube (syringe labelled "Not for IV Use"). Hepatic cirrhosis: 30 mg every 4 h.
- **Boxed-style warning** (p. 1 and p. 3): never give the capsule contents intravenously or parenterally; deaths, cardiac arrest, cardiovascular collapse, hypotension and bradycardia have followed.
- **Clinical trials** (p. 2-3): four randomized double-blind placebo-controlled SAH trials, doses 20-30 mg to 90 mg every 4 h for 18-21 days (US, French, UK n = 278 vs 276 at 60 mg q4h, Canadian 90 mg q4h in grades III-V). A 30/60/90 mg dose-ranging study found no dose-response relationship for spasm-related deficits.

## Model and parameters
No compartmental model. Descriptive PK (Clinical Pharmacology, p. 1-2):

```yaml
parameters:
  - {name: Tmax, value: "within 1", unit: h, source: "Clinical Pharmacology, p. 1", note: "generally; rapid oral absorption"}
  - {name: terminal t1/2, value: "8-9", unit: h, source: "Clinical Pharmacology, p. 1", note: "approximate; earlier phase equivalent to a half-life of 1-2 h, hence q4h dosing"}
  - {name: oral bioavailability, value: 0.13, unit: fraction, source: "Clinical Pharmacology, p. 1", note: "average; high first-pass metabolism"}
  - {name: protein binding, value: "> 0.95", unit: fraction, source: "Clinical Pharmacology, p. 1", note: "concentration independent over 10 ng/mL to 10 ug/mL"}
  - {name: urinary unchanged, value: "< 0.01", unit: fraction of dose, source: "Clinical Pharmacology, p. 1", note: "eliminated almost entirely as inactive or less active metabolites"}
  - {name: CSF concentration, value: 12.5, unit: ng/mL, source: "Mechanism of Action, p. 1", note: "highest detected, SAH patients; not a target"}
  - {name: fed vs fasted Cmax, value: -0.68, unit: fractional change, source: "p. 2", note: "standard breakfast, 24 healthy men; bioavailability -38%"}
  - {name: elderly vs young AUC and Cmax, value: "about 2-fold", unit: ratio, source: "p. 2", note: "24 aged 59-79 vs 24 aged 22-40, 30 mg single and 30 mg tid to steady state"}
  - {name: cirrhosis Cmax, value: "about 2-fold", unit: ratio, source: "p. 2 and p. 6", note: "bioavailability raised, clearance substantially reduced"}
  - {name: cimetidine interaction, value: "+50% Cmax, +90% AUC", unit: change, source: "Precautions, p. 4", note: "8 healthy volunteers, cimetidine 1000 mg/day with nimodipine 90 mg/day for a week"}
```
No clearance or volume is printed.

## Pharmacodynamics and end points
- **Blood pressure as an adverse effect** (Adverse Reactions, p. 5, rendered): adverse events in 92 of 823 SAH patients on nimodipine (11.2%) vs 29 of 479 on placebo (6.1%); decreased blood pressure was the most frequent, 4.4% overall. By dose every 4 h: 0.35 mg/kg 1/82 (1.2%), 30 mg 0/71, 60 mg 19/494 (3.8%), 90 mg 14/172 (8.1%), 120 mg 2/4 (50.0%), placebo 6/479 (1.2%). The label counts decreased blood pressure, oedema and headache as dose-related effects of a calcium-channel blocker. "Hypertension" appears among the <1% events at 60 mg.
- Precautions (p. 3): haemodynamic effects "generally not marked"; about 5% of SAH trial patients had a fall in pressure and about 1% left the study for it. In Europe it occasionally intensified concomitant antihypertensives; not seen in the North American trials (p. 4).
- Outcome end points (p. 2-3): spasm-related neurological deficits, delayed ischaemic deficits, Glasgow Outcome Scale at 3 months. No concentration-effect relation, no target concentration.

## Covariates
Qualitative only: hepatic cirrhosis (dose halved), age (about 2-fold exposure in the elderly; response "not considered significant"), food, cimetidine.

## Caveats
A 2005-2006 label in the old format; the current Nimotop/nimodipine labelling (and the oral solution, Nymalize) may differ. The PK is a summary with no variability and no sources. The adverse-event rates are from SAH patients, in whom altered consciousness under-reports symptoms (label, p. 5). The 120 mg group had n = 4.

## Used by stanpumpR
- docs/antihypertensives.md:1292 (at 8baa733; the context file's line 1280 is stale): the exclusions table. Nimodipine is not a registry record because it is "indicated for subarachnoid haemorrhage only" and its fall in pressure is "an adverse effect in its label, not an indication". The context notes that this is a passing mention of the label's content, not a formal citation.

## Verification against stanpumpR
- docs/antihypertensives.md:1292, "indicated for subarachnoid haemorrhage only": the label's single indication is neurological outcome after aneurysmal SAH (p. 3). **matches**.
- Same line, lowering pressure is an adverse effect in the label: decreased blood pressure is the most frequent adverse experience (4.4%), dose-related (3.8% at 60 mg, 8.1% at 90 mg vs 1.2% placebo; p. 5), and blood pressure monitoring is a Precaution (p. 3). **matches**.
- Not stated in the repo but in the label: no pharmacokinetic model or clearance is given, so the label could not have supplied a registry record in any case.
