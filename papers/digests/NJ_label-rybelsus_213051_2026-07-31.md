---
file: NJ_label-rybelsus_213051_2026-07-31.pdf
pmid: null
doi: null
citation: "Novo Nordisk. RYBELSUS (semaglutide) tablets and OZEMPIC (semaglutide) tablets, for oral use: full prescribing information and Medication Guide. NDA 213051 (URL 213051Orig1s030lbl.pdf). Revised 1/2026, FDA Reference ID 5737308 (signed 01/30/2026); archived file dated 2026-07-31."
kind: label
drugs: [semaglutideOral]
source_read: "text layer, all 38 PDF pages scanned; read in full: Highlights and contents (pp. 1-2), 1-2 Indications and Dosage and Administration (pp. 3-4), 3 Dosage forms (p. 4), 7 Drug interactions and 8 Use in specific populations (pp. 10-12), 10 Overdosage, 11 Description (p. 12-13), 12.1-12.3 Clinical pharmacology (pp. 13-15). Page image of PDF p. 14 (12.3 opening) rendered. Sections 5, 6, 13, 14, 16, 17, Medication Guide and carton art (to p. 38) skimmed only."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Rybelsus / Ozempic tablets label (NDA 213051, revised 1/2026)

The document is a combined label for two oral semaglutide products: RYBELSUS 3, 7, 14 mg (with SNAC, magnesium
stearate, microcrystalline cellulose, povidone) and the newer OZEMPIC tablets 1.5, 4, 9 mg (SNAC, magnesium
stearate). Highlights "Revised: 1/2026"; recent major changes dated 10/2025 and 12/2025; electronic signature
01/30/2026. The file name's date (2026-07-31) is not printed in the document. PDF pages run to the Medication Guide
and carton labels (p. 38).

## Study
Not a study. Dosing (2.1-2.2, PDF pp. 3-4): one tablet once daily on an empty stomach in the morning with up to
4 ounces of water (no other liquids); swallow whole; wait at least 30 minutes before food, beverages or other oral
medicines; skip a missed dose. Rybelsus 3 mg days 1-30 (not effective for glycaemia), 7 mg days 31-60, then 7 or
14 mg. Ozempic tablets 1.5 mg, then 4 mg, then 4 or 9 mg. The two products are not substitutable mg for mg; after
day 30 Rybelsus 7 = Ozempic tablets 4 mg and 14 = 9 mg (Table 1, 2.3). From Ozempic injection 0.5 mg, start tablets
one week after the last injection (2.4).

## Model and parameters
No model. Label PK values (12.3, PDF pp. 14-15; p. 14 rendered):
```yaml
parameters:
  - {name: Css mean at 7 mg Rybelsus, value: 6.7, unit: nmol/L, source: "12.3", note: "estimated mean steady-state concentration, patients with T2D"}
  - {name: Css mean at 14 mg Rybelsus, value: 14.6, unit: nmol/L, source: "12.3", note: "estimated mean steady-state concentration, patients with T2D"}
  - {name: time to steady state, value: "4-5", unit: weeks, source: "12.3", note: ""}
  - {name: F Rybelsus, value: "0.4-1", unit: "%", source: "12.3", note: "absolute, 3/7/14 mg"}
  - {name: F Ozempic tablets, value: "1-2", unit: "%", source: "12.3", note: "absolute, 1.5/4/9 mg"}
  - {name: tmax, value: 1, unit: h, source: "12.3", note: "approximately"}
  - {name: V, value: 8, unit: L, source: "12.3", note: "absolute volume of distribution, T2D; >99% albumin bound"}
  - {name: CL, value: 0.04, unit: L/h, source: "12.3", note: "absolute clearance, T2D"}
  - {name: t1/2, value: 1, unit: week, source: "12.3; 10", note: "approximately; present about 5 weeks after the last dose"}
  - {name: urinary excretion intact, value: 3, unit: "% of absorbed dose", source: "12.3", note: ""}
  - {name: molecular weight, value: 4113.58, unit: g/mol, source: "11", note: "C187H291N45O59"}
```
Formulations: steady-state AUC0-24 and Cmax not clinically different between Rybelsus 3/7/14 mg and Ozempic tablets
1.5/4/9 mg in healthy subjects (12.3). Water: single doses with 50 mL gave higher AUC and Cmax than 240 mL (8-h
fast, 4-h post-dose fast); over 10 days, a longer post-dose fast (15, 30, 60, 120 min) raised absorption and 50 vs
120 mL did not differ (12.3).

## Pharmacodynamics and end points
12.2 describes semaglutide injection 1 mg weekly (12 weeks): fasting glucose -29 mg/dL (22%), 2-h postprandial
-74 mg/dL (36%), 24-h mean -30 mg/dL (22%) vs placebo; glucose-dependent insulin/glucagon effects; delayed early
gastric emptying; no QTc effect at 4 x the exposure of the maximum tablet dose. No concentration-effect values.

## Covariates
12.3 Specific populations: no clinically significant PK differences by age (>= 18), sex, race, ethnicity, body
weight (40-188 kg), upper GI disease, hepatic impairment (Child-Pugh) or renal impairment including ESRD. 8.6/8.7:
no dose adjustment. Interactions: levothyroxine total T4 AUC +33% (90% CI 1.25-1.42), Cmax unchanged; omeprazole no
clinically significant effect on semaglutide (7.2, 12.3). 8.2: breastfeeding not recommended (SNAC in milk); 8.3:
stop at least 2 months before a planned pregnancy.

## Caveats
- The "no clinically significant difference" by body weight is a clinical-relevance statement; the population
  models (Overgaard 2021) still give exposure falling with weight.
- Css values are "estimated mean"; the label does not say arithmetic or geometric, or which model.

## Used by stanpumpR
- `R/drugs_semaglutideOral.R:31-34`: the curve is drawn for the label's dosing conditions, 30 min post-dose fasting
  and up to 120 mL of water.
- `inst/help/drugs/semaglutideOral.md:8`: the same conditions in the help text.

## Verification against stanpumpR
- `semaglutideOral.R:32-33`: "30 minutes of fasting and up to 120 mL of water": label 2.1 says wait at least 30
  minutes and up to 4 ounces of water (4 US fl oz = 118 mL; Overgaard 2021 equates 120 mL with 4 fl oz). **matches.**
- `semaglutideOral.md:8`: "taken fasting, with up to 120 mL of water, at least 30 minutes before food": label 2.1
  (empty stomach in the morning; also no beverages or other oral medicines for 30 min). **matches.**
- Not a listed location, checked for consistency: `semaglutideOral.R:78-80` and `semaglutideOral.md:29` (Ozempic
  tablets 1.5/4/9 mg not substitutable mg for mg with Rybelsus 3/7/14 mg; "bioequivalent"): label 2.1, Table 1 and
  12.3 (no clinically significant AUC/Cmax differences between the paired strengths). **matches.** The label's mean
  14.6 nmol/L at 14 mg Rybelsus in T2D is the same figure the code quotes from Overgaard 2021b (PIONEER) at
  `semaglutideOral.R:71-72`; the label's CL about 0.04 L/h and V about 8 L in T2D agree with the model's 0.038 L/h
  and 7.7 L at 85 kg (derived).
