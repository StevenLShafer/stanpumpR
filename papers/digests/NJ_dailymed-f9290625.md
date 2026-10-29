---
file: NJ_dailymed-f9290625.pdf
pmid: null
doi: null
citation: "CLEVIPREX (clevidipine) injectable emulsion, for intravenous use. US prescribing information, Chiesi USA, Inc. NDA022156. DailyMed setid f9290625-b723-4eda-bab8-a97bef6041f6; Highlights revised 9/2022, SPL revised 11/2025; downloaded 2026-10-21."
kind: label
drugs: [clevidipine]
source_read: "text layer, 21 PDF pages (form-feed chunks = pages). Read: Highlights (p1), contents (p2), 1-2 Indications, Dosage and Administration (p3-4), 3-5 (p4-5), 6 Adverse Reactions (p5-7), 8 Use in Specific Populations (p7-8), 10 Overdosage (p8-9), 11 Description (p9), 12.1-12.3 Clinical Pharmacology (p9-10), 13 (p10-11), 14 Clinical Studies (p11-15), 16-17 (p15), carton and SPL data (p16-21). All sections used are clean prose; no page rendered."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# CLEVIPREX (clevidipine) US label, 2022/2025

## Study
This is the current US prescribing information for clevidipine, a dihydropyridine L-type calcium channel blocker
given IV as a 0.5 mg/mL lipid emulsion (initial US approval 2008, NDA022156). PDF page 1 is the Cleviprex label,
so there is no source file problem. The SPL header misspells the product as "clevipidine emulsion".

**Dosage (section 2.2, p3):**
- Start at 1-2 mg/h. Double the dose every 90 s initially; near goal, increase by less than doubling every
  5-10 min. Each 1-2 mg/h increase generally lowers SBP by a further 2-4 mmHg.
- Maintenance is usually 4-6 mg/h. Most patients received at most 16 mg/h, and experience up to 32 mg/h is
  limited.
- Lipid limit: no more than 1000 mL per 24 h, an average of 21 mg/h. There is little experience beyond 72 h.
- Monitor for rebound hypertension for at least 8 h after a prolonged infusion (2.1, 5.6).

**Formulation (5.3, 11):**
- Each mL has about 0.2 g of lipid (2.0 kcal), from soybean oil 200 mg/mL and egg phospholipids.
- Racemate, MW 456.3 g/mol. The enantiomers are described as equipotent.
- Contraindicated with soy or egg allergy, defective lipid metabolism and severe aortic stenosis (4).

## Model and parameters
Section 12.3 (p10) is narrative only. It gives no population model and no table.
```yaml
parameters:
  - {name: t1/2 initial phase, value: 1, unit: min, source: "12.3, p10", note: "approximately; arterial blood; accounts for 85-90% of elimination"}
  - {name: t1/2 terminal, value: 15, unit: min, source: "12.3, p10", note: approximately}
  - {name: Vss, value: 0.17, unit: L/kg, source: "12.3, p10", note: "arterial blood"}
  - {name: protein binding, value: ">99.5", unit: "%", source: "12.3, p10", note: "plasma, 37 C"}
  - {name: metabolite (carboxylic acid) CL, value: 0.03, unit: L/h/kg, source: "12.3, p10", note: "terminal t1/2 approximately 9 h; inactive"}
  - {name: radiolabel recovery, value: 83, unit: "% of dose", source: "12.3, p10", note: "urine 63-74%, faeces 7-22%; >90% of recovered within 72 h"}
  - {name: "Emax (plateau)", value: 25, unit: "% of baseline SBP", source: "12.2, p9; 14.3, p14", note: "essential hypertension, 72-h study"}
  - {name: infusion rate for half-maximal effect, value: 10, unit: mg/h, source: "12.2, p9; 14.3, p14-15", note: approximately}
```
- No clearance value is printed.
- Elimination is by ester hydrolysis in blood and extravascular tissue, so it is "unlikely" to be affected by
  hepatic or renal dysfunction. It does not inhibit or induce CYP enzymes.

## Pharmacodynamics and end points
- **Onset:** a 4-5% fall in SBP within 2-4 min of starting 0.4 µg/kg/min (about 1-2 mg/h) in perioperative
  patients (12.2).
- **Offset:** full BP recovery in 5-15 min in most patients.
- **Maintenance:** no tolerance or hysteresis up to 72 h, with some evidence of rebound when patients were not
  transitioned to oral therapy.
- **Dose-response:** SBP effect plateaus at about 25% of baseline, with half-maximal effect at about 10 mg/h (12.2,
  14.3). This is from the 72-h essential-hypertension study (Smith 2012).
- No therapeutic or toxic concentration range is given. Overdose experience runs to 106 mg/h and 1153 mg total
  (10).
- **Trials (14):**
  - ESCAPE-1 (preoperative, mean 15.3 mg/h) and ESCAPE-2 (postoperative, 5.1 mg/h), both placebo-controlled for
    30 min.
  - ECLIPSE: 752 clevidipine patients, mean 8 h at 4.5 mg/h, against NTG, SNP or nicardipine.
  - VELOCITY: 126 patients with severe hypertension, mean 9.5 mg/h for 21 h.

## Covariates
- None quantified.
- Geriatric (8.5): no overall differences, but start at the low end.
- Pediatric (8.4): not established under 18.
- Pregnancy (8.1): human data insufficient. Animal embryo-fetal toxicity appeared at 7.6-12 times the human dose
  of 16 mg/h.
- Lactation (8.2): no data.

## Caveats
- Narrative PK only: approximate half-lives and Vss, with no clearance and no study identification.
- The matrix is stated as arterial blood for the concentration decline and Vss.

## Used by stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:37` (line confirmed at 8baa733): record
  `clevidipine__injectable_emulsion__iv__catalog__none__label`, `official_label_descriptive`, `descriptive_only`,
  matrix "whole blood (arterial in the label)".
- Its limitations field reads: "Label arterial steady-state volume about 0.17 L/kg with an initial half-life about
  1 min and a terminal about 15 min ... The implemented model is the healthy-volunteer whole-blood record."

## Verification against stanpumpR
- **Vss 0.17 L/kg arterial, initial t1/2 about 1 min, terminal about 15 min:** match 12.3. **matches**
- **Matrix "whole blood (arterial in the label)":** the label says arterial blood concentration. **matches**
- **`descriptive_only`, PD unfitted:** consistent with a label that gives no model. **matches**
- **Context:** the label figures agree with the arterial study (Ericsson 2000: Vss 0.14-0.22 L/kg, first half-life
  0.6-0.8 min). The implemented venous-blood model (Ericsson 1999 BJCP) has Vss 0.56 L/kg and half-lives of 1.8
  and 9.5 min. The registry's explanation, that matrix and sampling site differ, is consistent.
