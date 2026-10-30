---
file: NJ_dailymed-ff2c4d1e.pdf
pmid: ""
doi: ""
citation: "HydrALAZINE Hydrochloride Tablets USP (25 mg as packaged). Bryant Ranch Prepack (repackager; source NDC 50228-183, ANDA205236). DailyMed SPL setid ff2c4d1e-33ee-40d7-955b-d736f0559f39; revised 12/2024; retrieved 2026-10-21."
kind: label
drugs: [hydralazine]   # not a stanpumpR drug model; antihypertensive registry catalogue row only
source_read: "text layer, all 8 PDF pages: Description and Clinical Pharmacology incl. PK paragraph (p1-2), Indications to Precautions incl. food interaction (p2-3), Pregnancy, Nursing, Pediatric Use (p4), Adverse Reactions and Overdosage (p4-5), Dosage and Administration (p5-6), How Supplied and SPL data (p6-8). Old-format label without numbered 12.x sections"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Hydralazine hydrochloride tablets (Bryant Ranch repackage), DailyMed SPL ff2c4d1e

## Study
Generic US label for oral hydralazine HCl (the description lists 10, 25, 50 and 100 mg; this SPL packages only 25 mg
orange round tablets, imprint SG 183). Indication: essential hypertension, alone or as an adjunct (PDF p2).

Dosing (PDF p5-6): start 10 mg four times daily for 2-4 days, 25 mg four times daily for the rest of week 1, 50 mg four
times daily from week 2, then the lowest effective maintenance dose; up to 300 mg/day in resistant patients; toxicity
(L.E. cell syndrome) more frequent at large doses. Paediatric (PDF p4): 0.75 mg/kg/day in four doses, increased over
3-4 weeks to at most 7.5 mg/kg or 200 mg daily.

## Model and parameters
Clinical Pharmacology (PDF p2), descriptive only:
- rapidly absorbed; peak plasma levels at 1-2 h.
- plasma levels of "apparent hydralazine" decline with a half-life of 3-7 h.
- plasma protein binding 87%; levels vary widely between individuals.
- polymorphic acetylation: slow acetylators have higher levels and need lower doses.
- extensive hepatic metabolism; excreted mainly as urinary metabolites.
- food raises plasma levels (Drug/Food Interactions, PDF p3).
No clearance, volume or bioavailability values.

```yaml
parameters:
  - {name: tmax, value: "1-2", unit: h, source: "Clinical Pharmacology, PDF p2", note: oral}
  - {name: t_half_apparent, value: "3-7", unit: h, source: "Clinical Pharmacology, PDF p2", note: "'apparent hydralazine'"}
  - {name: protein_binding, value: 87, unit: "%", source: "Clinical Pharmacology, PDF p2"}
```

## Pharmacodynamics and end points
Direct arteriolar vasodilator, diastolic more than systolic fall, reflex tachycardia and renin rise (PDF p1-2). No
time course or dose-response numbers for the oral form.

## Covariates
Acetylator phenotype (above); advanced renal damage, caution (PDF p2); excreted in breast milk (PDF p4); pregnancy
category C.

## Caveats
Ranges only; "apparent hydralazine" signals a non-specific assay; acetylator status is the dominant source of
variability and is not quantified.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:66` (generator `data-raw/antihypertensiveRegistry.R:393`): model
  `hydralazine__ir_tablet__oral__catalog__none__label`, oral IR tablet, analyte "hydralazine (specific assay
  required)", plasma, `official_label_descriptive`, `descriptive_only`, BP simulation FALSE; limitations: the label's
  apparent 3-7 h half-life is assay-dependent; NAT2 acetylator phenotype affects exposure; apparent hydralazine from a
  non-specific assay is a different analyte; the primary proportional BP relation has no retrievable slope.

## Verification against stanpumpR
- "apparent 3-7 h half-life": **matches** (PDF p2). "Assay-dependent" and "a different analyte" are the registry's
  reading of the label's word "apparent" (the label names no assay): consistent, not stated outright.
- "NAT2 acetylator phenotype affects exposure": the label says polymorphic acetylation, slow acetylators higher
  levels; it does not name NAT2: **matches** in substance.
- "The primary proportional BP relation has no retrievable slope": not from this label (not checked here).
- The row records neither tmax 1-2 h nor 87% binding; omissions, not errors.
