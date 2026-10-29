---
file: NJ_dailymed-91d114ce.pdf
pmid: null
doi: null
citation: "Bisoprolol Fumarate Tablets USP 5 mg (label text covers 5 and 10 mg). Prescribing information, DailyMed SPL setid 91d114ce-5551-455e-82df-d058eb222ac1. Repackager Bryant Ranch Prepack (NDC 71335-1678); source product NDC 52817-270, ANDA075474; SPL revised 11/2023."
kind: label
drugs: [bisoprolol]
source_read: "text layer, all 14 PDF pages. Description and Clinical Pharmacology incl. Pharmacokinetics and Metabolism p1-2; Pharmacodynamics p2-3; Clinical Studies p3; Indications, Contraindications, Warnings p3-5; Precautions incl. Impaired Renal or Hepatic Function, Pregnancy, Nursing, Pediatric, Geriatric p5-7; Adverse Reactions and Laboratory p7-10; Overdosage p10; Dosage and Administration incl. renal/hepatic impairment p11; How Supplied and SPL product data p11-14. Rendered page 1 to confirm the document's identity. This old-format label has no numbered section 12."
digest_by: Claude Opus 5.5, 2026-10-27
checked_against: stanpumpR master 8baa733
verification: partly-verified
---

# Bisoprolol fumarate tablets label (DailyMed, Bryant Ranch repack of ANDA075474)

## Study
- **Document:** a US prescribing information in the old format, for generic bisoprolol fumarate tablets. It is a
  repackager's SPL (Bryant Ranch Prepack, 5 mg red tablets debossed MP711) of ANDA075474. The source NDC is
  52817-270.
- **Version:** SPL revised 11/2023.
- **Identity:** the document is what its name says (rendered page 1). The heading names 5 and 10 mg, but this SPL
  lists only the 5 mg tablet.
- **Chemistry:** racemic; the S(−) enantiomer carries most of the β-blockade. Fumarate (2:1 salt) MW 766.97 (p1).
- **Clinical studies:** two US placebo-controlled trials in mild to moderate hypertension, 24 h after dosing (p3):

| Study | Weeks | Placebo | Bisoprolol arms |
|---|---|---|---|
| A | 3-4 | n = 61 | 5, 10 and 20 mg, n = 61 each |
| B | 12 | n = 56 | 2.5 mg n = 59, 10 mg n = 62 |

## Model and parameters
No PK model and no parameter table. Statements from Pharmacokinetics and Metabolism (pp1-2):

```yaml
parameters:
  - {name: absolute bioavailability, value: 80, unit: "%", source: "PK and Metabolism, p1", note: "after 10 mg oral; food has no effect; first-pass about 20%"}
  - {name: protein binding, value: 30, unit: "%", source: "p1", note: "approximately"}
  - {name: Tmax, value: "2-4", unit: h, source: "p1", note: "doses 5-20 mg"}
  - {name: Cmax, value: "16 (5 mg) to 70 (20 mg)", unit: ng/mL, source: "p1", note: "mean peak values"}
  - {name: plasma elimination half-life, value: "9-12", unit: h, source: "p1", note: "slightly longer in the elderly"}
  - {name: accumulation factor (once daily), value: "1.1-1.3", unit: ratio, source: "p1", note: "steady state within 5 days; dose-proportional 5-20 mg"}
  - {name: fraction excreted unchanged in urine, value: 50, unit: "% dose", source: "p1", note: "'eliminated equally by renal and non-renal pathways'; the rest as inactive metabolites; feces < 2%"}
  - {name: half-life in CLcr < 40 mL/min, value: "about 3-fold", unit: "x normal", source: "p1"}
  - {name: half-life in cirrhosis, value: "8.3-21.7", unit: h, source: "p1"}
```

- Not metabolised by CYP2D6.
- The two enantiomers have similar pharmacokinetics.
- Rifampin increases metabolic clearance and shortens the half-life (Precautions, p6).
- Bisoprolol is probably not dialysable (pp10-11).

## Pharmacodynamics and end points
- **Healthy volunteers (p2):** the reduction of exercise- and isoproterenol-induced tachycardia peaked at 1-4 h
  after a dose and lasted 24 h at doses of 5 mg or more.
- **β1-selectivity:** not absolute at 20 mg or more (p1).
- **Placebo-subtracted sitting BP / HR at 24 h** (Clinical Studies table, p3):
  - Study A: 5.0/4.8 mm Hg and 6.7 bpm (5 mg); 5.8/7.7 and 8.2 (10 mg); 7.4/8.7 and 10.8 (20 mg).
  - Study B: 4.6/4.4 and 2.2 (2.5 mg); 10.5/7.5 and 9.1 (10 mg).
  - Baseline about 150/100 mm Hg and 76 bpm.
- **Time course of the BP response:** seen within a week and sustained for 12 weeks and over a year. BP returned to
  baseline after a 2-week taper (p3).
- No concentration-effect relation is given.

## Covariates
- **Renal (CLcr < 40 mL/min) or hepatic impairment:** start at 2.5 mg daily and titrate cautiously (p11).
- **Elderly:** no adjustment unless renal or hepatic function is impaired.
- **Paediatric:** not established.
- **Usual adult dose:** 5 mg once daily, raised to 10 and then 20 mg. 2.5 mg is an option, as in bronchospastic
  disease.

## Caveats
- Qualitative PK only: a half-life range, F and the renal fraction, with no clearance or volume.
- A repackager's generic SPL, not the innovator (Zebeta) label, though the text is the standard one.

## Used by stanpumpR
`inst/extdata/antihypertensiveRegistry.csv:16` (row `bisoprolol__ir_tablet__oral__catalog__none__label`, from
`data-raw/antihypertensiveRegistry.R:285`): a descriptive-only catalogue row, not simulated. Its limitations text
reads: "Label half-life 9-12 h with roughly equal renal and hepatic clearance. Population PK with heart rate exists
in acute-coronary patients; no adult hypertensive concentration-BP fit."

## Verification against stanpumpR
- **"Label half-life 9-12 h"**: label p1, 9 to 12 hours. **matches**
- **"Roughly equal renal and hepatic clearance"**: the label says equal renal and *non-renal* elimination, with
  about 50% unchanged in the urine. Non-renal is read as hepatic, a reasonable gloss. The label does not say
  "hepatic" except through the cirrhosis data. **matches**
- **"Population PK with heart rate exists in acute-coronary patients; no adult hypertensive concentration-BP
  fit"**: not in this label. It gives no concentration-effect data, which is consistent with the second half of
  the claim. **cannot verify** the first half here.
- **Route, formulation and analyte (oral IR tablet, bisoprolol in plasma)**: correct. **matches**
