---
file: NJ_dailymed-b00570ff.pdf
pmid: null
doi: null
citation: "NORVASC (amlodipine besylate) tablets 2.5, 5 and 10 mg (Pfizer; NDA019787), repackaged by Rebel Distributors Corp (5 mg, NDC 21695-151). DailyMed SPL setid b00570ff-4081-4514-b45b-eb6f753450f5, prescribing information LAB-0014-8.0 January 2010 (Highlights revised 1/2010), SPL revised 12/2010; retrieved 2026-10-21."
kind: label
drugs: [amlodipine]
source_read: "text layer, all 18 PDF pages: Highlights (p. 1), contents (p. 2), 1 Indications and 2 Dosage and Administration (p. 3), 3-5 incl. 5.4 hepatic failure (p. 4), 6 Adverse Reactions (pp. 4-6), 7 Drug Interactions (pp. 6-7), 8 Use in Specific Populations (pp. 7-8), 10 Overdosage and 11 Description (pp. 8-9), 12.1-12.2 (pp. 9-10), 12.3 Pharmacokinetics (p. 10), 12.4 Pediatric (p. 11), 13-14 (pp. 11-14), 16-17 and patient information (pp. 14-16), SPL product data (pp. 17-18). Clean prose; nothing rendered, no PK table."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: matches
---

# Norvasc (amlodipine) label, DailyMed SPL (Rebel Distributors repackage, 2010)

PLR-format brand label, initial US approval 1987. This is a 2010 relabeler SPL (the Pfizer text dated January 2010), not the current Pfizer label. Its section 8.1 still uses Pregnancy Category C, for example. Product and route (amlodipine tablet, oral) match the registry record.

## Study
Not a study. Clinical Studies (section 14): 15 double-blind placebo-controlled hypertension studies (800 on Norvasc, 538 on placebo). Placebo-corrected falls at 24 h were about 12/6 mm Hg standing and 13/7 mm Hg supine. One paediatric study had 268 patients aged 6-17 (p. 11).

## Model and parameters
No population model. Section 12.3 (p. 10), 5.4 (p. 4), 8.5 (p. 8), 12.4 (p. 11):

```yaml
parameters:
  - {name: Tmax, value: "6-12", unit: h, source: "12.3 p. 10"}
  - {name: F_absolute, value: "64-90", unit: "%", source: "12.3 p. 10", note: "not altered by food"}
  - {name: hepatic_metabolism, value: 90, unit: "%", source: "12.3 p. 10", note: "about; to inactive metabolites; urine 10% parent, 60% metabolites"}
  - {name: protein_binding, value: 93, unit: "%", source: "12.3 p. 10", note: "approximately, ex vivo, hypertensive patients"}
  - {name: t_half_terminal, value: "30-50", unit: h, source: "12.3 p. 10", note: "biphasic elimination"}
  - {name: time_to_steady_state, value: "7-8", unit: days, source: "12.3 p. 10", note: "consecutive daily dosing"}
  - {name: t_half_hepatic_impairment, value: 56, unit: h, source: "5.4 p. 4"}
  - {name: AUC_increase_elderly_hepatic, value: "40-60", unit: "%", source: "12.3 p. 10; 8.5 p. 8", note: "similar in moderate-severe heart failure"}
  - {name: pKa, value: 8.6, unit: "", source: "12.1 p. 9"}
```

- Renal impairment does not significantly alter PK; the usual initial dose applies (p. 10).
- Weight-adjusted CL and V are similar in children aged 6-17 and adults (p. 11).
- Not removed by haemodialysis because highly protein bound (p. 8).
- Cimetidine, grapefruit juice (240 mL with 10 mg, n = 20) and antacids do not change amlodipine PK (pp. 6-7).

## Pharmacodynamics and end points
- Plasma concentrations correlate with effect in young and elderly patients. The antihypertensive effect is maintained for at least 24 h (p. 10).
- Moderate hypertension (DBP 105-114) responds about 50% more than mild (90-104). Normotensive subjects change by +1/-2 mm Hg (p. 10).
- Onset is gradual: slow association with and dissociation from the receptor (p. 9).
- No concentration-effect parameters or therapeutic range.

## Covariates
Age and hepatic impairment (lower CL, AUC +40-60%) and heart failure (similar AUC rise), all descriptive.

## Caveats
- Dosage (2.1-2.2, p. 3):
  - Adults: start 5 mg once daily, maximum 10 mg. Small, fragile or elderly patients and those with hepatic insufficiency may start at 2.5 mg. Titrate over 7-14 days.
  - Angina and CAD: 5-10 mg.
  - Children 6-17: 2.5-5 mg; above 5 mg not studied.
- An old label edition; values here should be checked against the current label before being cited as current.

## Used by stanpumpR
- inst/extdata/antihypertensiveRegistry.csv:5 (8baa733): source_url of record amlodipine__tablet__oral__catalog__none__label (official_label_descriptive, proposed_unfitted, descriptive_only). Built from data-raw/antihypertensiveRegistry.R:214-218.
- Limitations text: label Tmax 6-12 h, F 64-90%, biphasic terminal half-life 30-50 h, steady state in 7-8 days; the fitted erect-SBP slope record is separate.
- The drug is also listed in tests/testthat/test-antihypertensive-registry.R:520. There is no amlodipine drug model; the Donnelly 1993 slope record in R/antihypertensives.R:120 is a different source.

## Verification against stanpumpR
- "Tmax 6-12 h": **matches** (12.3, p. 10).
- "F 64-90%": **matches** (absolute bioavailability, p. 10).
- "biphasic terminal half-life 30-50 h": **matches** (p. 10).
- "steady state in 7-8 days": **matches** (p. 10).
- Product and route identity: **matches**. The registry cites a 2010 repackager edition rather than the current Pfizer label; the four quoted numbers are long-standing label text.
