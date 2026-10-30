---
file: NJ_label-aptiom_022416_2013-11.pdf
pmid: none
doi: none
citation: "APTIOM (eslicarbazepine acetate) tablets, for oral use. Full prescribing information, NDA 022416 original approval labeling (022416s000lbl.pdf), Revised 11/2013. Sunovion Pharmaceuticals."
kind: label
drugs: [eslicarbazepine]
source_read: "text layer; Highlights, sections 2, 8.6-8.7 and 12 (12.1-12.3) read in full; other sections skimmed"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 4ea0cab
verification: matches
---

# Aptiom label, original approval (Revised 11/2013)

The first US prescribing information for Aptiom (the archive's 2024 file, `NJ_label-aptiom_022416_2024-09-06.pdf`,
holds only container labels). Each page carries FDA's note that it may not be the latest label.

## Study
Not a study. Indication at approval: adjunctive treatment of partial-onset seizures. Dosing (Highlights, PDF p. 1):
400 mg once daily, after one week 800 mg (maintenance), maximum 1200 mg; CrCl < 50 mL/min: 200 mg, then 400 mg
after two weeks, maximum 600 mg. Tablets 200, 400, 600, 800 mg of eslicarbazepine acetate.

## Model and parameters
Section 12.3 (PDF p. 15) and its Excretion paragraph (PDF p. 16):
- Linear, dose-proportional PK, 400-1200 mg once daily; steady state in 4-5 days.
- Apparent half-life of eslicarbazepine 13-20 h in epilepsy patients (stated twice).
- Eslicarbazepine acetate almost undetectable (0.01% of exposure); eslicarbazepine 91% of exposure, (R)-licarbazepine
  5%, oxcarbazepine 1%, glucuronides ~3%. Cmax at 1-4 h. Urinary recovery > 90% of the dose; food has no effect.
- Protein binding < 40%; apparent V 61 L at 70 kg (population PK).
- Renal clearance of eslicarbazepine about 20 mL/min against GFR 80-120 (tubular reabsorption).
- No autoinduction.

```yaml
parameters:
  - {name: eslicarbazepine half-life, value: "13-20", unit: h, source: "12.3, PDF pp. 15-16", note: "apparent, epilepsy patients"}
  - {name: V/F, value: 61, unit: L, source: "12.3 Distribution, PDF p. 15", note: "70 kg, population PK"}
  - {name: Tmax, value: "1-4", unit: h, source: "12.3 Absorption, PDF p. 15"}
  - {name: protein binding, value: "<40", unit: "%", source: "12.3 Distribution, PDF p. 15"}
  - {name: renal clearance, value: 20, unit: mL/min, source: "12.3 Excretion, PDF p. 16", note: "approximately, healthy subjects"}
```

## Pharmacodynamics and end points
No concentration-effect relation or therapeutic range. QT study (12.2): 1200 and 2400 mg once daily for 5 days, no
significant QTc effect.

## Covariates
- Renal: exposure after 800 mg single dose +62% (CrCl 50-80), 2-fold (30-49), 2.5-fold (< 30) vs CrCl > 80;
  dose reduction below 50 mL/min; removed by haemodialysis (PDF p. 16).
- Moderate hepatic impairment (Child-Pugh 7-9): no effect; severe not studied.
- Age (>= 65, CrCl > 60), sex, race: no effect.
- Enzyme inducers: carbamazepine, phenytoin, phenobarbital/primidone may need higher Aptiom doses (Highlights).

## Caveats
- This is the 2013 labeling. stanpumpR's "label 13-20 h" does not say which version; the half-life is shown here
  for the original label, and the 2024 prescribing information itself is still not archived.

## Used by stanpumpR
- `R/drugs_eslicarbazepine.R:36`: CHECKS line, model half-life 17 h against "label 13-20 h".

## Verification against stanpumpR
- "label 13-20 h" (`R/drugs_eslicarbazepine.R:36`): 12.3 gives an apparent half-life of 13-20 h in epilepsy
  patients. **matches** (for the 2013 label).
- The model's V/F 61.3 L (Falcao 2012; help page) agrees with the label's population-PK 61 L at 70 kg. **matches**.
