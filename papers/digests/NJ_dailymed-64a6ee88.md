---
file: NJ_dailymed-64a6ee88.pdf
pmid: null
doi: null
citation: "Parke-Davis Div of Pfizer Inc. CELONTIN (methsuximide capsules, USP) 300 mg. US prescribing information LAB-0156-7.0, Revised March 2023; Medication Guide LAB-0404-3.0; NDA010596, marketing start 02/08/1957; SPL revised 4/2026. DailyMed SPL setid 64a6ee88-c6b1-4e13-8208-b6772ef65a74; archived 2026-10-21."
kind: label
drugs: []
source_read: "text layer, all 12 PDF pages: Description, Clinical Pharmacology, Indications, Contraindications and Warnings p1-4; Precautions incl. Drug Interactions, Pregnancy and Pediatric Use p4-5; Adverse Reactions p5; Overdosage p5-6; Dosage and Administration and How Supplied p6; Medication Guide p7-11; carton panel and SPL product data p11-12. Pages 5 and 6 checked on rendered images; page 1 rendered to confirm identity."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Celontin (methsuximide) US label, Revised March 2023

## Study
Not a study: an old-format (non-PLR) US label of a succinimide anticonvulsant, 300 mg capsules (Description, p1;
How Supplied, p6), NDA010596, marketed since 1957 (SPL data, p12). Indication: absence (petit mal) seizures refractory
to other drugs (p1). No PK study, population, n, assay or sampling is described.

## Model and parameters
None. The Clinical Pharmacology section (p1) describes only the mechanism (suppression of the 3-per-second spike
and wave, raised seizure threshold). The label gives no absorption, bioavailability, Tmax, volume, clearance,
protein binding or half-life for methsuximide or its metabolite.

What it does give:
- An active metabolite, N-desmethylmethsuximide, held responsible for the biphasic course of poisoning (coma,
  waking, relapse into coma within 24 h) (Overdosage, p5-6).
- Concentrations (Overdosage, p6, rendered): N-desmethylmethsuximide above 40 µg/mL has caused toxicity; coma seen
  at 150 µg/mL. No therapeutic range.
- Charcoal hemoperfusion may remove the N-desmethyl metabolite; forced diuresis and exchange transfusion are
  ineffective (p6).

```yaml
parameters:
  - {name: toxic_N_desmethylmethsuximide, value: 40, unit: ug/mL, source: "Overdosage, PDF p6", note: "levels greater than this have caused toxicity; plasma"}
  - {name: coma_N_desmethylmethsuximide, value: 150, unit: ug/mL, source: "Overdosage, PDF p6", note: "coma has been seen at this level"}
```

## Pharmacodynamics and end points
No concentration-effect relationship and no therapeutic concentration. Dosing (p6): 300 mg/day for the first week,
increased by 300 mg/day at weekly intervals for three weeks to 1.2 g/day if needed; individualised to the smallest
dose that controls seizures.

## Covariates
None quantified. Caution in liver or renal disease (Warnings, p2). Methsuximide may raise phenytoin and
phenobarbital concentrations (Drug Interactions, p5). Pediatric use refers to Dosage and Administration only.

## Caveats
- No pharmacokinetics at all; the toxic levels are from poisoning reports, with no n or assay given.
- The capsule melts at 124 °F; melted or partly empty capsules may be less effective (Precautions, p4).

## Used by stanpumpR
- `inst/extdata/antiseizureRegistry.csv:24` (at 8baa733): moiety Methsuximide, "oral capsule (Celontin)",
  `US_SEIZURE_LABELED`, `proposed_unestimated`, dose basis "methsuximide + N-desmethylmethsuximide", analyte
  N-desmethylmethsuximide, model "parent -> active metabolite (half-lives only)", `pk_only`, `stanpumpr_drug` NA,
  this setid as source; notes "No modern population parameter set; only half-lives known (parent ~1.4 h,
  metabolite 28-80 h)." Built at `data-raw/antiseizureRegistry.R:164-168`. Methsuximide is not a library drug.

## Verification against stanpumpR
- Status `US_SEIZURE_LABELED`, oral capsule (Celontin): label indication refractory absence seizures, 300 mg
  capsules, current SPL (revised 4/2026). **matches**.
- Analyte N-desmethylmethsuximide and "parent -> active metabolite": the label names that metabolite as active and
  as the one to follow in poisoning (p5-6). **matches**.
- "No modern population parameter set": the label has no PK. **matches**.
- "only half-lives known (parent ~1.4 h, metabolite 28-80 h)": neither half-life is in this label, which is the
  row's only source. The numbers may be right, but they are not supported by the cited document. **cannot verify**;
  flagged for the maintainer (the row needs a source for the half-lives).
- Not used by the repo: the 40 and 150 µg/mL metabolite toxicity levels.
