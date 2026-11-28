---
file: BK_9781108565011-ch17-oxygen-delivery-and-demand.pdf
pmid: none
doi: 10.1017/9781108565011.020
citation: "Chapter 17, Oxygen Delivery and Demand, pp 74-76, in a Cambridge University Press physiology-for-anaesthesia textbook (ISBN 9781108565011), Section 2 Respiratory Physiology. Authors and editors not on the 3 archived pages."
kind: other
drugs: []
source_read: "text layer (all 3 PDF pages, printed pp 74-76); no page image rendered"
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Textbook ch 17: Oxygen delivery and demand

## Study
Textbook teaching chapter; no study, no subjects, no assay. Worked examples use a resting adult: CO 5 L/min, [Hb] 15 g/dL, SaO2 98%, PaO2 13 kPa.

## Model and parameters
Equations (printed pp 74-75):
- Reverse Fick: VO2 = CO x (CaO2 - CvO2) x 10 (CO L/min, contents mL O2/100 mL, factor 10 converts to mL/min).
- CaO2 = 1.34 x [Hb] x SaO2/100 + 0.023 x PaO2 (Hb g/dL, PaO2 kPa; mL O2/100 mL).
- DO2 = CO x CaO2 x 10. Worked: CaO2 20.0, DO2 1000 mL/min. Hb 8 g/dL, CO unchanged: CaO2 10.8, DO2 540 mL/min (the text notes CO would rise in real anaemia).
- OER = VO2/DO2; normal 0.2-0.3 (mixed venous saturation 70-80%); heart about 0.6.
- Resting VO2 of a 70-kg man about 250 mL/min. Post-major-surgery inflammation raises resting VO2 by 50%.
- Fig 17.1: VO2 vs DO2, with a critical DO2 ("anaerobic threshold") below which VO2 becomes supply dependent; shifted to higher DO2 in sepsis/ARDS. The text layer of the figure label reads "anabolic threshold" (apparently a typo for anaerobic; not checked on a page image).

```yaml
parameters:
  - {name: Hufner_constant, value: 1.34, unit: "mL O2/g Hb", source: "p 74", note: textbook}
  - {name: dissolved_O2, value: 0.023, unit: "mL/100 mL/kPa", source: "p 74", note: textbook}
  - {name: resting_VO2, value: 250, unit: "mL/min", source: "p 74", note: 70-kg man}
  - {name: resting_DO2, value: 1000, unit: "mL/min", source: "p 74", note: worked example}
  - {name: OER_normal, value: "0.2-0.3", unit: ratio, source: "p 75", note: textbook}
```

## Pharmacodynamics and end points
None (not a drug paper). Anaesthesia effects on VO2 are qualitative: hypnotics and paralysis lower it.

## Covariates
Hb, CO, PaO2/SaO2 enter the content equation. No others.

## Caveats
Textbook, no primary data; author and edition not on the archived pages. Numbers are text-layer only.

## Used by stanpumpR
Not cited anywhere in the repo (context locations empty).

## Verification against stanpumpR
Not cited, so there is no citation to verify. As an independent check of R/arterialOxygen.R: OXYGEN_HUFNER 1.34 mL/g (line 120) matches. OXYGEN_DISSOLVED 0.003 mL/dL/mmHg (line 124) equals 0.0225 mL/dL/kPa (derived: 0.003 x 7.5006), consistent with the book's 0.023 per kPa. The repo computes content only, not DO2 or VO2. Worked arithmetic recomputed: correct. Verdict not-applicable.
