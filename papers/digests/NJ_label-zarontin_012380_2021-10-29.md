---
file: NJ_label-zarontin_012380_2021-10-29.pdf
pmid: none
doi: none
citation: "ZARONTIN (ethosuximide capsules, USP) prescribing information and Medication Guide, Parke-Davis (Pfizer); NDA 012380 (s037), revised October 2021 (FDA Reference ID 4880076; posted 2021-10-29). https://www.accessdata.fda.gov/drugsatfda_docs/label/2021/012380s037lbl.pdf"
kind: label
drugs: [ethosuximide]
source_read: "text layer, all 11 pages (one form-feed chunk per PDF page): prescribing information pp. 1-6 read in full (it has no pharmacokinetics section), Medication Guide pp. 7-11 skimmed; PDF p5 (Overdosage, Dosage and Administration) rendered and read"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master ca8ce42
verification: matches
---

# Zarontin label 2021: ethosuximide capsules, the 40-100 mcg/mL range

## Study
Regulatory label (pre-PLR format), revised October 2021. Product: 250 mg ethosuximide capsules (the oral solution
is mentioned only in the Medication Guide title). Indication: absence (petit mal) epilepsy.

## Model and parameters
None. The label has **no pharmacokinetics section**: no half-life, clearance, volume, bioavailability or tmax.
```yaml
parameters: []  # no PK values in this label; the range and dosing are under PD and dosing below
```
Dosing (Dosage and Administration, PDF p5): start 250 mg/day at 3-6 years, 500 mg/day at 6 years and older;
increase by 250 mg every 4-7 days; above 1.5 g/day only under strict supervision; optimal paediatric dose 20
mg/kg/day.

## Pharmacodynamics and end points
- Overdosage (PDF p5, rendered): therapeutic range of serum levels 40 to 100 mcg/mL; levels up to 150 mcg/mL
  reported without toxicity; no established relation between toxicity and plasma level.
- Dosage and Administration (PDF p5): 20 mg/kg/day has given average plasma levels within the accepted
  therapeutic range of 40 to 100 mcg/mL.
- No sampling time stated. Not an opioid.

## Covariates
None quantified. Drug interactions (Precautions, PDF p4): ethosuximide may raise phenytoin levels; valproic acid
reported both to increase and to decrease ethosuximide levels. Pregnancy: registry only, no PK.

## Caveats
Old-format label with no PK; the range is stated twice (Overdosage, Dosage) without source or sampling time.

## Used by stanpumpR
- docs/therapeutic-ranges.md:44 (same line at ca8ce42) - ethosuximide row: band 40-100 mg/L (ILAE 2008), "Zarontin
  label: 40-100 mcg/mL".
- docs/therapeutic-ranges.md:240 (context gave :175) - Zarontin among the labels stating a range, "read through
  search excerpts".
- inst/extdata/therapeuticRanges.csv:7 (label_statement; not in the context file) - "Zarontin (FDA): 20 mg/kg/day
  gives average plasma levels within the accepted therapeutic range of 40 to 100 mcg/mL".

## Verification against stanpumpR
- therapeutic-ranges.md:44 and :240, 40-100 mcg/mL: **matches** (PDF p5, both statements).
- therapeuticRanges.csv:7 label_statement: **matches** the Dosage and Administration sentence (PDF p5); the row's
  band 40-100 mg/L (Patsalos 2008) agrees with the label.
- Context, not a repo citation of this label: the repo applies valproate -> ethosuximide as an inhibition
  (antiseizureInteractions.csv:35, CL x0.85); this label says valproate has been reported to raise and to lower
  ethosuximide levels, consistent with the repo's "variable" description but not with a single direction.
- R/drugs_ethosuximide.R:12 says the Diezi 2023 subjects took Zarontin syrup; this capsule label gives no PK to
  compare.
