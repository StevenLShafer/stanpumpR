---
file: NJ_dailymed-93b4b34c.pdf
pmid: null
doi: null
citation: "Primidone tablets USP, 50 mg and 250 mg (generic). DailyMed SPL setid 93b4b34c-6dba-4aec-b98e-b2b18feb86a9, labeler Bryant Ranch Prepack (repackager, NDC 63629-7811, 50 mg); made by Amneal Pharmaceuticals, ANDA040866; Medication Guide Rev. 02-2022-05, SPL revised 6/2026; downloaded 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 11 pages: Description and Clinical Pharmacology (PDF p1), Indications, Contraindications (p1-2), Warnings incl. pregnancy (p2-4), Precautions (p4), Adverse Reactions and Dosage and Administration (p5), How Supplied (p6), Medication Guide (p7-10), SPL product data (p10-11). Page 5 rendered (dosing and the serum level)."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Primidone tablets (generic, Amneal/Bryant Ranch) DailyMed label

An old-format (non-PLR) generic label: there are no numbered sections and **no pharmacokinetics section at all**.
The SPL lists only the 50 mg tablet (debossed AN/44), although the text covers 50 and 250 mg.

## Study
Not a study. Regulatory label; indication grand mal, psychomotor and focal seizures, alone or with other
anticonvulsants.

## Model and parameters
None. The label gives no absorption, half-life, clearance, volume, protein binding or metabolite fraction.
What it does say about disposition (Clinical Pharmacology, PDF p1): primidone and its two metabolites,
phenobarbital and phenylethylmalonamide (PEMA), all have anticonvulsant activity, and PEMA potentiates
phenobarbital in animals. It gives no fraction of primidone converted to phenobarbital.

```yaml
parameters: []  # the label prints no pharmacokinetic parameter
```

## Pharmacodynamics and end points
- "Clinically effective serum level" for primidone 5 to 12 mcg/mL (Dosage and Administration, PDF p5, rendered).
  The label does not say whether trough or steady state, and gives no phenobarbital target.
- Total daily dose not to exceed 2 g (Precautions, p4); breast milk contains substantial quantities (p4).

## Dosing (PDF p5)
- 8 years and older, untreated: days 1-3 100-125 mg at bedtime; days 4-6 twice daily; days 7-9 three times
  daily; day 10 on 250 mg three times daily. Usual maintenance 750-1000 mg/day; maximum 500 mg four times daily.
- Under 8 years: 50 mg at bedtime days 1-3, 50 mg twice daily, 100 mg twice daily, then 125-250 mg three times
  daily (10-25 mg/kg/day).
- Conversion from other anticonvulsants over at least two weeks.

## Covariates
None stated (no renal, hepatic, age or interaction pharmacokinetics).

## Caveats
- A repackager's SPL of a generic, old label format; it cannot support any pharmacokinetic number.
- The 5-12 mcg/mL range is stated without sampling time, matrix detail or source.

## Used by stanpumpR
Registry source for `inst/extdata/antiseizureRegistry.csv:32` (row Primidone; generator
`data-raw/antiseizureRegistry.R:205-209`).

## Verification against stanpumpR
- `inst/extdata/antiseizureRegistry.csv:32` (8baa733): routes "oral IR tablet" **matches**; dose basis "primidone +
  phenobarbital + PEMA" and analyte "primidone and derived phenobarbital" **match** the label's two active
  metabolites; structural model "parent -> phenobarbital + PEMA (formation fractions unknown)" and note "No
  identified modern joint model; ~15-25% to phenobarbital from secondary sources only" are consistent with this
  label, which gives no fraction (so the 15-25% cannot come from it, as the note says). Tier
  `proposed_unestimated`, `stanpumpr_drug` NA: consistent. Not recorded in the row: the label's 5-12 mcg/mL
  effective serum level. **Matches.**
