---
file: NJ_dailymed-5ac32c20.pdf
pmid: null
doi: null
citation: "COZAAR (losartan potassium) tablets, for oral use. DailyMed SPL setid 5ac32c20-169d-475a-fc8a-934f758d6ab0, Merck Sharp & Dohme Corp., NDA020386; prescribing information revised 11/2019, SPL revised 12/2021 (listing marketing end date 12/31/2021); downloaded 2026-10-21."
kind: label
drugs: [losartan]
source_read: "text layer, 33 pages: Highlights and contents (PDF p1-3), 2 Dosage and Administration and 3 Dosage Forms (p4-5), 8 Use in Specific Populations (p9-11), 10 Overdosage (p11-12), 12.1-12.3 Clinical Pharmacology including Special Populations and Drug Interactions (p12-15), SPL product data (p32-33). Page 14 (Table 2) rendered."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Cozaar (losartan) label, Merck SPL, 2019/2021

The document is the Cozaar (Merck) tablet label as its name implies (25, 50, 100 mg). It is an older Merck SPL:
the product listing shows marketing end 12/31/2021, so it may not be the current Cozaar labeling.

## Study
Not a study. Regulatory label. Indications include hypertension (adults and children 6 years and over),
hypertensive LV hypertrophy (stroke risk), and diabetic nephropathy.

## Model and parameters
No model. Section 12.3 (PDF p13-15):
- Absorption: well absorbed, substantial first pass; systemic bioavailability about 33%. Mean peak of losartan
  at 1 h and of the active metabolite (E-3174, unnamed in the label) at 3-4 h. Peak concentrations of the two
  approximately equal; metabolite AUC about 4 times parent. Food slows absorption, lowers Cmax, AUC changes minor
  (about 10% for metabolite). Linear up to 200 mg.
- Distribution: volume about 34 L (losartan) and 12 L (metabolite); free fractions 1.3% and 0.2%.
- Metabolism: about 14% of an oral dose converted to the active metabolite; CYP2C9 and 3A4 (conversion mainly
  2C9).
- Elimination: total plasma clearance about 600 and 50 mL/min, renal about 75 and 25 mL/min; terminal half-life
  about 2 h (losartan) and 6-9 h (metabolite); about 4% of an oral dose unchanged and 6% as metabolite in urine;
  14C oral: 35% urine, 60% faeces; 14C IV: 45% urine, 50% faeces. No accumulation once daily.

Table 2 (PDF p14, rendered; the text calls it "Table 4"), multiple dosing for 7 days:

| | Adults 50 mg qd, n = 12, parent | metabolite | Age 6-16, 0.7 mg/kg qd, n = 25, parent | metabolite |
|---|---|---|---|---|
| AUC0-24 (ng·h/mL), mean ± SD | 442 ± 173 | 1685 ± 452 | 368 ± 169 | 1866 ± 1076 |
| Cmax (ng/mL), mean ± SD | 224 ± 82 | 212 ± 73 | 141 ± 88 | 222 ± 127 |
| t1/2 (h), harmonic mean ± SD | 2.1 ± 0.70 | 7.4 ± 2.4 | 2.3 ± 0.8 | 5.6 ± 1.2 |
| Tpeak (h), median | 0.9 | 3.5 | 2.0 | 4.1 |
| CLren (mL/min), mean ± SD | 56 ± 23 | 20 ± 3 | 53 ± 33 | 17 ± 8 |

```yaml
parameters:
  - {name: oral bioavailability, value: 33, unit: "%", source: "12.3 p13", note: approximately}
  - {name: V losartan, value: 34, unit: L, source: "12.3 p13"}
  - {name: V metabolite, value: 12, unit: L, source: "12.3 p13"}
  - {name: CL losartan, value: 600, unit: mL/min, source: "12.3 p14", note: "total plasma, about"}
  - {name: CL metabolite, value: 50, unit: mL/min, source: "12.3 p14"}
  - {name: CLren losartan, value: 75, unit: mL/min, source: "12.3 p14"}
  - {name: CLren metabolite, value: 25, unit: mL/min, source: "12.3 p14"}
  - {name: t1/2 losartan, value: 2, unit: h, source: "12.3 p14", note: about}
  - {name: t1/2 metabolite, value: "6-9", unit: h, source: "12.3 p14"}
  - {name: fraction converted, value: 14, unit: "% of oral dose", source: "12.3 p14"}
  - {name: Cmax adults 50 mg qd day 7 losartan, value: 224, unit: ng/mL, source: "Table 2 p14", note: "mean, n=12"}
  - {name: Cmax adults 50 mg qd day 7 metabolite, value: 212, unit: ng/mL, source: "Table 2 p14"}
```

## Pharmacodynamics and end points
- Metabolite 10 to 40 times more potent than losartan by weight, non-competitive at AT1 (12.1, p13).
- 100 mg inhibits the angiotensin II pressor response by about 85% at peak, 25-40% persisting at 24 h (12.2,
  p13). Antihypertensive effect substantially present within one week, maximal in some studies at 3-6 weeks; no
  rebound on withdrawal; no change in heart rate.
- No therapeutic concentration range.

## Covariates
- Women: losartan concentrations about twice those of men; metabolite similar (p15).
- Mild/moderate renal insufficiency: concentrations and AUCs up 50-90%; not dialysed (p15).
- Mild/moderate alcoholic cirrhosis: losartan 5 times and metabolite 1.7 times; clearance about 50% lower and
  bioavailability about doubled; start 25 mg (2.4, 8.8, 12.3).
- Interactions: rifampin lowers parent and metabolite AUC 30% and 40%; fluconazole raises parent AUC about 70% and
  lowers metabolite AUC about 40% (p15).

## Dosing (section 2, p4-5)
Adults 50 mg once daily (25 mg if volume depleted), maximum 100 mg once daily; children 6 and over 0.7 mg/kg once
daily up to 50 mg, doses above 1.4 mg/kg or 100 mg not studied.

## Caveats
- Older Merck SPL, listing ended 12/31/2021.
- Table numbered 2 but cited in the text as "Table 4".
- Label's metabolite volume (12 L) differs from Lo 1995's measured Vss (10.3 L, n = 6).

## Used by stanpumpR
Registry source: `inst/extdata/antihypertensiveRegistry.csv` row `losartan__ir_tablet__oral__catalog__none__label`
(generator `data-raw/antihypertensiveRegistry.R`). Losartan is not an implemented drug.

## Verification against stanpumpR
- `inst/extdata/antihypertensiveRegistry.csv:78` (8baa733; same text at `data-raw/antihypertensiveRegistry.R:175-179`):
  "losartan and EXP3174 (both active)" **matches** (the label says the metabolite gives most of the AT1
  antagonism); oral immediate-release tablet **matches**; "terminal half-lives about 2 h and 6-9 h" **matches**
  12.3 (p14). "The day-7 BP effect exceeded day-1 with little PK change" is **not in this label**: the label says
  the effect is substantially present within one week and maximal at 3-6 weeks, and that losartan PK do not change
  over time, which is compatible but is not the day-1 versus day-7 comparison (that appears to come from the
  separate temporal record, `antihypertensiveRegistry.csv:79`, PubMed 8739018). "the 2026 systems model" is also not
  from this label. **Cannot verify from this source**; the PK parts match.
