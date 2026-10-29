---
file: NJ_label-cardizem_018602_2025-04-24.pdf
pmid: null
doi: null
citation: "CARDIZEM (diltiazem hydrochloride) tablets prescribing information. Bausch Health US. NDA 018602 (supplement s072), Rev. 4/2024 (FDA Reference ID 5572936; accessdata label dated 2025-04-24)."
kind: label
drugs: [diltiazem]
source_read: "text layer, all 11 pages (whole label: Description, Clinical Pharmacology incl. Pharmacokinetics and Metabolism, Contraindications, Warnings, Precautions/Drug Interactions, Adverse Reactions, Overdosage, Dosage and Administration, How Supplied); page image of PDF page 3 rendered and read"
digest_by: Claude (unattended digest_run.py), 2026-10-28
checked_against: stanpumpR master 50bb4219
verification: matches
---

# Cardizem label 2024: diltiazem tablets (Bausch)

## Study
Not a study: US prescribing information (old, non-PLR format) for immediate-release oral diltiazem HCl tablets
30, 60, 90 and 120 mg, indicated for chronic stable angina and vasospastic angina (p. 3). MW 450.98 (p. 1).
The printed revision is 4/2024 (last page); the FDA file is dated 2025-04-24.

## Model and parameters
No compartmental model. Pharmacokinetics and Metabolism (PDF p. 3, rendered and read):
- Absolute oral bioavailability about 40% (extensive first pass); tablet about 98% of a reference solution.
- 2 to 4% excreted unchanged in urine; 70 to 80% bound to plasma proteins (in vitro).
- Plasma elimination half-life about 3.0 to 4.5 h after single or multiple dosing.
- Desacetyl diltiazem at 10 to 20% of parent levels, 25 to 50% as potent a coronary vasodilator.
- Nonlinear: AUC rises 2.3-fold from 120 to 240 mg/day and 1.8-fold from 240 to 360 mg/day.
- Single oral 30 to 120 mg: detectable at 30 to 60 min, peak at 2 to 4 h.
- Cirrhosis: longer half-life and 69% higher AUC; severe renal impairment (n=9): no PK difference.
- No clearance, volume, Cmax values given.

```yaml
parameters:
  - {name: oral bioavailability, value: 40, unit: "%", source: "PK and Metabolism, p. 3 (image read)", note: approximate}
  - {name: protein binding, value: "70-80", unit: "%", source: "p. 3", note: in vitro}
  - {name: t1/2, value: "3.0-4.5", unit: h, source: "p. 3", note: "single or multiple dose"}
  - {name: fraction unchanged in urine, value: "2-4", unit: "%", source: "p. 3"}
  - {name: tmax (tablets), value: "2-4", unit: h, source: "p. 3"}
  - {name: minimum therapeutic plasma level, value: "50-200", unit: ng/mL, source: "p. 3 (image read)", note: "label says 'appear to be'"}
  - {name: AUC increase in cirrhosis, value: 69, unit: "%", source: "p. 3"}
  - {name: usual dose, value: "180-360", unit: mg/day, source: "Dosage and Administration, p. 10"}
```

## Pharmacodynamics and end points
- Minimum therapeutic plasma levels "appear to be in the range of 50 to 200 ng/mL" (p. 3, rendered page). No
  population, end point, sampling time or study is given; context is chronic oral angina therapy.
- IV 20 mg prolongs AH conduction and AV nodal refractory periods by about 20%; single oral 300 mg in 6 volunteers
  gave mean maximum PR prolongation of 14% (p. 2). No concentrations attached.
- Overdose: no toxic dose or concentration in man; blood levels after a standard dose vary over tenfold (p. 9).
- No ke0. Not an opioid; opioid audit not applicable.

## Covariates
Hepatic impairment (AUC +69%); renal impairment no change; CYP3A4 and P-gp interactions (midazolam/triazolam AUC
3- to 4-fold, cimetidine raises diltiazem AUC 53%, rifampin lowers diltiazem to undetectable), pp. 5-6.

## Caveats
Oral immediate-release label; the 50 to 200 ng/mL figure is a hedged, unreferenced statement of a minimum level, not
a validated therapeutic window, and does not apply directly to IV rate control of atrial fibrillation.

## Used by stanpumpR
- `inst/extdata/therapeuticRanges.csv:185` — diltiazem row: `not_applicable`, no band (NA bounds, ng/mL), source
  "CARDIZEM (diltiazem) tablets label, rev. 04/2024, Clinical Pharmacology", `label_statement` quoting the 50 to
  200 ng/mL minimum therapeutic level, and a note that it is for chronic oral treatment, not the IV rate control
  the model describes.

## Verification against stanpumpR
- `therapeuticRanges.csv:185`: label statement matches p. 3 word for word (rendered page); units ng/mL agree; the
  revision date "rev. 04/2024" matches the printed "Rev. 4/2024" (the archive file name carries the FDA posting
  date 2025-04-24, not a different label). The section cited (Clinical Pharmacology) is right. The note's
  characterisation (oral, chronic; not IV rate control) is consistent with the label, whose only indication is
  angina with oral tablets. Leaving the band undrawn is a defensible reading of a hedged statement. **matches**
