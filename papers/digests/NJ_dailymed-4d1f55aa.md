---
file: NJ_dailymed-4d1f55aa.pdf
pmid: ""
doi: ""
citation: "Hydralazine Hydrochloride Injection, USP, 20 mg/mL (1 mL fill in a 2 mL single-dose vial), for intravenous or intramuscular use. Henry Schein, Inc. (repackager; Fresenius Kabi USA, ANDA040388). DailyMed SPL setid 4d1f55aa-e924-4540-ae9f-bd7362a82f33; labelling text revised February 2022, SPL revised 5/2025; retrieved 2026-10-21."
kind: label
drugs: [hydralazine]   # not a stanpumpR drug model; antihypertensive registry catalogue row only
source_read: "text layer, all 8 PDF pages: Description and Clinical Pharmacology (p1-2), Indications, Contraindications, Warnings and Precautions (p2-3), Pregnancy, Nursing, Pediatric Use (p4), Adverse Reactions and Overdosage (p4-5), Dosage and Administration (p5), How Supplied (p5-6), SPL product data (p7-8). Old-format label: no numbered 12.x sections, no Use in Specific Populations section"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Hydralazine hydrochloride injection (Henry Schein / Fresenius Kabi), DailyMed SPL 4d1f55aa

## Study
Generic US label, 20 mg/mL hydralazine HCl with methyl- and propylparaben and propylene glycol (103.6 mg/mL), pH
3.4-4.4, IV or IM. Indication: severe essential hypertension when oral therapy is not possible or blood pressure must
fall urgently (PDF p2). Contraindications: hypersensitivity, coronary artery disease, mitral valvular rheumatic heart
disease.

Dosing (PDF p5): IM or rapid IV bolus in hospital, usual dose 20-40 mg, repeated as necessary; lower doses may be
needed with marked renal damage; most patients can move to oral hydralazine within 24-48 h; do not add to infusion
solutions; discolours on contact with metal. Paediatric (PDF p4): 1.7-3.5 mg/kg/day IM or IV in four to six divided
doses (not established in controlled trials).

## Model and parameters
None. Clinical Pharmacology (PDF p2): the average maximal fall in blood pressure usually occurs 10-80 min after the
injection, and the label states that no other pharmacokinetic data on the injection are available.

```yaml
parameters:
  - {name: time_to_max_BP_fall, value: "10-80", unit: min, source: "Clinical Pharmacology, PDF p2; Dosage and Administration, PDF p5", note: "average maximal decrease after IV/IM injection"}
```

## Pharmacodynamics and end points
Direct arteriolar vasodilator; diastolic falls more than systolic; reflex rises in heart rate, stroke volume, cardiac
output and plasma renin (PDF p1-2). Blood pressure may begin to fall within a few minutes (PDF p5). Profound
hypotension with concomitant diazoxide (PDF p3). No dose-response or concentration-effect data.

## Covariates
"Marked renal damage" may need a lower dose (PDF p5); no numbers. Pregnancy category C; excretion in milk unknown for
this product (PDF p4).

## Caveats
No PK in the label at all. The acetylator dependence described in the oral label is not mentioned here.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:64` (generator `data-raw/antihypertensiveRegistry.R:443`): model
  `hydralazine__injection__iv__catalog__none__label`, route iv, analyte "hydralazine (specific assay)", plasma,
  `official_label_descriptive`, `descriptive_only`, BP simulation FALSE; limitations: "Label maximum BP reduction
  averages 10-80 min"; the specific-assay bolus record (Ludden 1980) is a separate, underdetermined row.

## Verification against stanpumpR
- "Label maximum BP reduction averages 10-80 min": **matches** (PDF p2 and p5).
- Route iv: the label is for IV and IM; the row catalogues the IV use only (not an error).
- Analyte "specific assay": the label names no assay (it has no PK); the qualifier comes from the separate primary
  record, not from this label (not checked here).
