---
file: BK_9781108565011-ch18-alveolar-gas-equation.pdf
pmid: none
doi: 10.1017/9781108565011.021
citation: 'Chapter 18, Alveolar Gas Equation, Section 2 Respiratory Physiology, printed pp. 77-79. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: other
drugs: []
source_read: text layer (all 3 PDF pages, printed pp 77-79); no page image rendered
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Textbook ch 18: Alveolar gas equation

## Study
Textbook teaching chapter; no subjects or assay.

## Model and parameters
Simplified alveolar gas equation (printed p 77): PAO2 = FiO2 x (PB - PSVP water) - PaCO2/R, in kPa.
- FiO2 dry air 20.93%; PB sea level 101.325 kPa; PSVP water 6.3 kPa at body temperature; R usually 0.8.
- R: carbohydrate 1.0, fat 0.7, protein 0.9 (pp 77-78).
- Worked (pp 78-79): room air, PaCO2 5.3 kPa gives PAO2 13.3 kPa. PaCO2 9 kPa (opioid overdose) gives 8.6 kPa. PB 65 kPa: PaCO2 5.3 gives 5.7 kPa; PaCO2 3.0 gives 8.5 kPa. All recomputed (derived) and correct.
- Normal A-a gradient in healthy young adults < 1.5 kPa; rises with age (p 78).
- Fig 18.1: PAO2 vs minute ventilation for FiO2 0.21, 0.3, 0.4; plateau beyond about 10 L/min.

```yaml
parameters:
  - {name: FiO2_air, value: 0.2093, unit: fraction, source: "p 77", note: dry air}
  - {name: PB_sea_level, value: 101.325, unit: kPa, source: "p 77", note: textbook}
  - {name: PSVP_water, value: 6.3, unit: kPa, source: "p 77", note: at body temperature}
  - {name: R, value: 0.8, unit: ratio, source: "p 77-78", note: balanced Western diet}
```

## Pharmacodynamics and end points
No PK/PD data. Opioid content is one qualitative example (overdose: PaCO2 9 kPa, SaO2 about 91%, PAO2 8.6 kPa). No MEAC, ke0, tPeak or potency ratio.

## Covariates
FiO2, barometric pressure (altitude), PaCO2 (alveolar ventilation), R.

## Caveats
Textbook simplification of the full equation (the chapter says the derivation is out of scope); no primary data. Numbers are text-layer only.

## Used by stanpumpR
Not cited anywhere in the repo (context locations empty).

## Verification against stanpumpR
Not cited; independent comparison only. R/altitude.R:176 WATER_VAPOUR_PRESSURE 47 Torr = 6.27 kPa (derived), matching the book's 6.3 kPa. R/altitude.R:193 GAS_ALVEOLAR_TORR = 760 - 47 = 713 Torr = 95.06 kPa (derived), matching the book's 101.325 - 6.3 = 95.0 kPa. R/arterialOxygen.R:183-184 converts an alveolar percentage to PAO2 with that 713 Torr share and starts PaO2 equal to PAO2 (shunt applied afterwards). The repo has no PaCO2/R term on that path, so PAO2 is the dry-gas share only; the book's equation subtracts PaCO2/R. Whether the repo's alveolar fractions already account for CO2 is not settled by this chapter. Verdict not-applicable.
