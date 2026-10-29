---
file: NJ_dailymed-58935d94.pdf
pmid: null
doi: null
citation: "ZAROXOLYN (metolazone tablets, USP) 2.5 mg, prescribing information (UCB text Rev. 01/2014), repackager SPL by Carilion Materials Management, NDA 017386, DailyMed setid 58935d94-5bf1-4065-b8b5-ac61292436a7, SPL revised 8/2016 (accessed 2026-10-21)."
kind: label
drugs: [metolazone]
source_read: "text layer, all 11 pages (old-format label, no numbered sections): Description and Clinical Pharmacology p1-2, Indications incl. Usage in Pregnancy p2, Contraindications and Warnings p3, Precautions incl. drug interactions p4-6, Pregnancy, Labor, Nursing, Pediatric and Geriatric Use p6-7, Adverse Reactions p7-8, Overdosage p8, Dosage and Administration p8-9, How Supplied p9, SPL product data p10-11"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# ZAROXOLYN (metolazone) label: repackager SPL of the UCB tablet label

The file is what its name says: the DailyMed SPL of ZAROXOLYN 2.5 mg tablets repackaged by Carilion Materials
Management (NDC 68151-2943, source NDC 53014-975), carrying UCB's label text Rev. 01/2014; the listing shows a
marketing end date of 07/01/2016 for this package (PDF p10-11). Original NDA 017386, marketed since 11/27/1973.

## Study
Not a study. Product: metolazone, a quinazoline diuretic, MW 365.83, sparingly water-soluble; tablets 2.5 and 5 mg
(this SPL lists only the 2.5 mg pink round tablet). Boxed-style caution (p1, repeated under Precautions): ZAROXOLYN
and formulations sharing its slow, incomplete bioavailability are not to be interchanged with MYKROX, a more rapidly
and completely bioavailable metolazone product.

## Model and parameters
The label gives no PK parameters (no half-life, clearance, volume, bioavailability fraction, Cmax or AUC). Clinical
Pharmacology (p2) states only: maximum blood levels about eight hours after dosing; a small fraction metabolized;
most excreted unchanged in the urine. Onset of diuresis within one hour, lasting 24 h or more.

```yaml
parameters:
  - {name: tmax (blood), value: 8, unit: h, source: "Clinical Pharmacology p2", note: "approximately; no dose, population or Cmax given"}
  - {name: onset of diuresis, value: 1, unit: h, source: "Clinical Pharmacology p2; Dosage and Administration p9", note: "usually within one hour; may persist 24 h or more"}
```

## Pharmacodynamics and end points
Diuretic potency at maximum dosage approximately equal to thiazides; may produce diuresis with GFR below 20 mL/min;
marked diuresis with concurrent furosemide in refractory oedema (p2). Hypertension response may take 3-4 days to 3-6
weeks (p9). Dosing (p8-9): oedema of cardiac failure or renal disease 5-20 mg once daily; mild to moderate
hypertension 2.5-5 mg once daily; switching to MYKROX by titration from 0.5 mg daily. Paediatric experience (p6-7):
0.05-0.1 mg/kg once daily, not established in controlled trials. No therapeutic or toxic concentration range.

## Covariates
Renal impairment: caution, accumulation may occur since most is renally excreted (p4-5; geriatric use p7). No
quantitative adjustment. Pregnancy Category B; crosses the placenta and appears in cord blood; appears in breast
milk (p6).

## Caveats
- A repackager SPL of a discontinued package, not the current originator label.
- Descriptive only; PK beyond tmax is absent.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:83` (built by `data-raw/antihypertensiveRegistry.R:255-259`): the
  metolazone catalogue record (ZAROXOLYN oral tablet, `official_label_descriptive`, `descriptive_only`, BP
  simulation disabled, `stanpumpr_drug` NA).

## Verification against stanpumpR
- **antihypertensiveRegistry.csv:83:** source URL is this setid; formulation "tablet (ZAROXOLYN)"; matrix "blood"
  (the label speaks of blood levels); limitations "Slow, incomplete absorption with a peak near 8 h", ZAROXOLYN and
  MYKROX not interchangeable milligram for milligram. All supported by the label (p1-2). The record correctly makes
  no PK claim beyond what the label gives; the "primary two-compartment study" it mentions is not in this document
  and cannot be checked here. **matches**.
