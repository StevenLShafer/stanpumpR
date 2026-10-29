---
file: NJ_dailymed-b8da2015.pdf
pmid: null
doi: null
citation: "Miglitol Tablets 25, 50, 100 mg; ANDA203965 (marketed from 01/15/2021), manufactured by Orient Pharma Co. (Taiwan), distributed by Westminster Pharmaceuticals (NDC 69367-305 and related). DailyMed SPL setid b8da2015-d254-2425-e053-2995a90acf0e, label revised 12/2021; retrieved 2026-10-21."
kind: label
drugs: [miglitol]
source_read: "text layer, all 19 PDF pages: Description and Clinical Pharmacology incl. Mechanism, Pharmacokinetics and Special Populations (p. 1-2), Clinical Studies incl. Tables 1-2 (p. 2-7, tables scrambled in the text layer, numbers not recorded), Indications (p. 7), Contraindications, Precautions incl. Drug Interactions, Pregnancy, Pediatric and Geriatric Use (p. 8-11), Overdosage and Dosage and Administration (p. 12-14), How Supplied and SPL data (p. 14-19). Glyset is the reference brand; this is a generic SPL."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Miglitol tablets (generic of Glyset, ANDA203965), DailyMed SPL

Old-format label. Alpha-glucosidase inhibitor (desoxynojirimycin derivative, MW 207.2, pKa 5.9) for type 2 diabetes; acts by reversible inhibition of membrane-bound intestinal alpha-glucoside hydrolases; does not enhance insulin secretion; minor lactase inhibition (p. 1).

## Study
Not a study.

## Model and parameters
No population model. Pharmacokinetics (PDF p. 2):

```yaml
parameters:
  - {name: F_25mg, value: 1.0, unit: fraction, source: "Pharmacokinetics, Absorption (p. 2)", note: "25 mg completely absorbed"}
  - {name: F_100mg, value: "0.50-0.70", unit: fraction, source: "Absorption (p. 2)", note: "saturable absorption at high doses"}
  - {name: tmax, value: "2-3", unit: h, source: "Absorption (p. 2)", note: "all doses"}
  - {name: V, value: 0.18, unit: L/kg, source: "Distribution (p. 2)", note: "extracellular fluid"}
  - {name: protein_binding, value: "<4.0", unit: "%", source: "Distribution (p. 2)"}
  - {name: urinary_recovery_25mg, value: ">95", unit: "% in 24 h", source: "Excretion (p. 2)", note: "unchanged drug"}
  - {name: t_half, value: 2, unit: h, source: "Excretion (p. 2)", note: "plasma elimination half-life, approximately"}
```

- Not metabolised; renal excretion of unchanged drug. CrCl < 25 mL/min on 25 mg three times daily: plasma levels more than twofold those at CrCl > 60 mL/min; dose adjustment not feasible because the drug acts locally; use not recommended. Cirrhosis: PK unchanged. Sex: no difference after weight; Japanese similar to Caucasians (p. 2).
- Systemic absorption is stated not to contribute to the therapeutic effect (p. 2).
- Interactions (p. 9-10): glyburide Cmax -17%, AUC -25% (healthy volunteers); digoxin mean plasma concentrations -19% and -28% in healthy volunteers (not altered in diabetic patients); ranitidine and propranolol bioavailability -60% and -40%; no effect on warfarin or nifedipine; intestinal adsorbents and carbohydrate-splitting enzyme preparations may reduce effect.

## Pharmacodynamics and end points
End points are one-hour postprandial plasma glucose and HbA1c (Clinical Studies; Dose-Response, p. 2-7). Dose response from 25 to 100 mg three times daily for postprandial glucose. Dosing (p. 12-14): start 25 mg three times daily at the start of each main meal (or once daily, increasing), maintenance 50 mg three times daily, maximum 100 mg three times daily; 200 mg three times daily improved control but increased GI effects. No therapeutic plasma concentration range (action is local).

## Covariates
Renal function (descriptive).

## Caveats
Pediatric safety and effectiveness not established; 24% of US trial subjects over 65 (p. 11). Clinical-study tables are scrambled in the text layer and were not transcribed.

## Used by stanpumpR
- inst/extdata/antidiabeticRegistry.csv:75 (8baa733): key miglitol, product Glyset, PO, core, US; pk_status and pd_status PK-TEMPLATE / PD-TEMPLATE, PD end point "meal carbohydrate hydrolysis", not implemented, structure_verified FALSE; notes "Local gut action."; source_url this SPL.

## Verification against stanpumpR
- "Local gut action": Mechanism (p. 1) and the statement that systemic absorption does not contribute to the effect (p. 2). **matches**.
- PD end point "meal carbohydrate hydrolysis": mechanism is inhibition of intestinal hydrolysis of oligo- and disaccharides; the label's measured end points are postprandial glucose and HbA1c. **matches** as a mechanism label.
- Product named "Glyset": this SPL is a generic (ANDA203965, Westminster/Orient), not the Glyset brand label; the ingredient, route and indication are the same. Noted only. Nothing numeric is taken from the label.
