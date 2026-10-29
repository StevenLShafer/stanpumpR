---
file: NJ_dailymed-33c68d5e.pdf
pmid: null
doi: null
citation: "Clonidine hydrochloride tablets USP 0.1 mg (generic, ANDA070925; manufactured by Frontida BioPharm, distributed by TruPharma, repackaged by Proficient Rx LP) prescribing information; DailyMed SPL setid 33c68d5e-6a68-40fd-a326-bf36a7e01d5b, label Rev00 March 2017, SPL revised 9/2022."
kind: label
drugs: [clonidine]
source_read: "text layer, all 10 pages: Description and Clinical Pharmacology incl. Pharmacokinetics (PDF p1-2), Warnings incl. withdrawal and Precautions incl. perioperative use (p3-4), Pediatric Use (p5), Overdosage (p6-7), Dosage and Administration and Renal Impairment (p7), How Supplied (p7-8), SPL product data (p9-10)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Clonidine HCl tablets (generic, DailyMed SPL)

## Study
This is a label, not a study. It is a **generic** clonidine HCl 0.1 mg tablet (ANDA070925, Proficient Rx repackage), not the Boehringer Catapres brand. Its Clinical Pharmacology and Dosage text is the same as the 2011 Catapres label (`NJ_label-catapres_017407_2012-06-06`). The 0.1 mg tablet equals 0.087 mg of free base (Description, p1).

## Model and parameters
No model. These are statements from Pharmacokinetics (p2):

```yaml
parameters:
  - {name: F_oral, value: "0.70-0.80", unit: fraction, source: "Pharmacokinetics p2", note: "absolute"}
  - {name: tmax_oral, value: "1-3", unit: h, source: "Pharmacokinetics p2", note: approximately}
  - {name: dose_proportional_range, value: "100-600", unit: mcg, source: "Pharmacokinetics p2"}
  - {name: t_half_distribution_IV, value: 20, unit: min, source: "Pharmacokinetics p2", note: about}
  - {name: t_half_elimination_IV, value: "12-16", unit: h, source: "Pharmacokinetics p2", note: "up to 41 h in severe renal impairment"}
  - {name: fraction_unchanged_urine_24h, value: "0.40-0.60", unit: "fraction of absorbed dose", source: "Pharmacokinetics p2"}
  - {name: fraction_hepatic_metabolism, value: 0.5, unit: "fraction of absorbed dose", source: "Pharmacokinetics p2", note: about}
  - {name: antihypertensive_concentration_range, value: "0.2-2.0", unit: ng/mL, source: "Pharmacokinetics p2", note: "normal excretory function; higher levels do not add effect"}
```

- Food and race do not influence the PK (p2).
- This label gives no clearance or volume. The transdermal label gives CL 177 mL/min and Vz 197 L.

## Pharmacodynamics and end points
- Blood pressure: it falls within 30-60 min of an oral dose, with the maximum fall within 2-4 h (Clinical Pharmacology, p1-2).
- The antihypertensive effect is reached at plasma 0.2-2.0 ng/mL, and a further rise does not enhance it (p2). That is a plateau statement, not a fitted Emax.
- Dosing (p7): start 0.1 mg twice daily; usual 0.2-0.6 mg/day in divided doses; 2.4 mg/day is the maximum effective dose.
- Overdose case (p7): 370 ng/mL at 2 h after 100 mg of powder.

## Covariates
- Renal: lower initial dose; half-life up to 41 h; dialysis removes little (p2, p7).
- Elderly patients may need a lower initial dose (p7).
- Paediatric safety is not established. The label warns that children who vomit may be at risk of rebound hypertension (p3, p5).

## Caveats
- This is a generic, repackaged SPL, not the brand label. Its PK text is the class text inherited from Catapres.
- The PK statements are summary ranges without study n.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:38`: the `source_url` of `clonidine__ir_tablet__oral__catalog__none__label` (catalog only).

## Verification against stanpumpR
- **"Label F 70-80%"**: matches (p2).
- **"IV distribution half-life about 20 min, elimination 12-16 h"**: matches (p2).
- **"BP nadir 2-4 h"**: matches; the maximum decrease is within 2-4 h (p1-2).
- **"The primary records show a NON-MONOTONE concentration-BP relation that plateaus and then attenuates above about 1.5-2 ng/mL"**: the registry attributes this to the primary records, not this label. The label supports the plateau ("a further rise ... will not enhance") but not the attenuation. Consistent as attributed.
- **Source type** `official_label`: the linked document is a generic repackager's SPL, not the Catapres NDA label. Its content is the same as the brand text, so there is no numeric consequence.
