---
file: NJ_label-aldactone_012151_2025-11-17.pdf
pmid: null
doi: null
citation: "Aldactone (spironolactone) tablets 25/50/100 mg prescribing information, NDA 012151 (s080), Pfizer (Searle); revised 11/2025, FDA Reference ID 5695854."
kind: label
drugs: [spironolactone]
source_read: "text layer (OCR-like artefacts, e.g. ':55.0' for '<=5.0', 'STRENGIBS'), all 13 pages: Highlights and contents (PDF p1), Indications (p2-3), Dosage and Administration 2.1-2.5 and Dosage Forms (p3, rendered), Warnings (p4), Use in Specific Populations 8.1-8.7 (p7-8), Overdosage, Description, Clinical Pharmacology 12.1-12.3 (p8-10; 12.3 on p9 rendered), Clinical Studies 14.1-14.2 (p10-12), How Supplied and Patient Counseling (p12-13)"
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Aldactone FDA label, NDA 012151 (revised 11/2025)

## Study
This is the brand label (PLR format), not a study. Tablets (3, p3, rendered):
- 25 mg round light yellow, SEARLE 1001
- 50 mg oval light orange scored, SEARLE 1041
- 100 mg round peach scored, SEARLE 1031

Indications: heart failure, hypertension, edema (cirrhosis or nephrotic syndrome), primary hyperaldosteronism (1.1-1.4).

## Model and parameters
No model. Section 12.3 (p9, rendered):

```yaml
parameters:
  - {name: tmax_spironolactone, value: 2.6, unit: h, source: "12.3 p9", note: "mean, healthy volunteers"}
  - {name: tmax_canrenone, value: 4.3, unit: h, source: "12.3 p9", note: mean}
  - {name: food_effect_AUC, value: 95.4, unit: "% increase", source: "12.3 p9"}
  - {name: protein_binding, value: ">90", unit: "%", source: "12.3 p9", note: "parent and metabolites"}
  - {name: t_half_spironolactone, value: 1.4, unit: h, source: "12.3 p9", note: mean}
  - {name: t_half_canrenone, value: 16.5, unit: h, source: "12.3 p9", note: mean}
  - {name: t_half_TMS, value: 13.8, unit: h, source: "12.3 p9", note: mean}
  - {name: t_half_HTMS, value: 15, unit: h, source: "12.3 p9", note: mean}
```

- TMS and 7-alpha-thiospirolactone are about one third as potent as the parent against fludrocortisone, with their serum levels unmeasured.
- Metabolites are excreted in urine, then bile.
- The effects of age, sex, race and renal impairment on the PK have not been studied. The half-life is longer in cirrhotic ascites (12.3).

## Pharmacodynamics and end points
- Hypertension (2.3, p3, rendered): 25-100 mg/day, single or divided, titrated every 2 weeks; above 100 mg/day there is generally no further BP reduction.
- Section 14.2 says the BP dose response is not well characterised; systolic falls were seen at 25-100 mg/day.
- Heart failure (2.2): 25 mg once daily if K+ is 5.0 mEq/L or less and eGFR above 50 mL/min/1.73 m2; may rise to 50 mg; 25 mg every other day if eGFR is 30-50 or for hyperkalemia.
- Edema (2.4): 100 mg/day, range 25-200 mg; give at least 5 days before increasing.
- Primary hyperaldosteronism (2.5): 100-400 mg/day before surgery.
- No concentration-effect relation is given.

## Covariates
- Renal impairment: hyperkalemia risk (8.6).
- Elderly: monitor renal function (8.5).
- Cirrhosis: start in hospital, titrate slowly, clearance reduced (8.7).
- Paediatric safety and effectiveness are not established (8.4).
- Take consistently with respect to food (2.1).

## Caveats
- The text layer has OCR-type errors. The numbers used here were confirmed on the rendered pages 3 and 9.
- No clearance or volume is given.
- The 95.4% food effect is for the tablet. Tatipalli 2021 (`PMID_34201093`) reports about 90% for the suspension, from its fed study.

## Used by stanpumpR
- `tests/testthat/test-antihypertensive-registry.R:176-177` (context gives line 66): the pair `spironolactone__tablet__oral__catalog__none__label` / the Tatipalli suspension record must both exist.
- `docs/antihypertensives.md:31`: the spironolactone tablet is kept separate from the study suspension.
- The registry record's `source_url` (`antihypertensiveRegistry.csv:116`) is a DailyMed generic SPL, not this PDF.

## Verification against stanpumpR
- **test:176-177 and docs:31**: this is a tablet label. Its PK (Tmax 2.6/4.3 h, food +95.4%) is a tablet's, and Tatipalli p2 reports the suspension's higher relative bioavailability. Separate records are justified. **Matches.**
- **Registry:116** limitations, checked against this brand text: active metabolites beyond canrenone (TMS, HTMS) and the food effect. Both match 12.1 and 12.3.
