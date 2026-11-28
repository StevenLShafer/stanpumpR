---
file: BK_9781108565011-ch19-oxygen-cascade.pdf
pmid: null
doi: 10.1017/9781108565011.022
citation: 'Chapter 19, Oxygen Cascade, Section 2 Respiratory Physiology, printed pp. 80-81. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: other
drugs: []
source_read: text layer (both PDF pages; Figure 19.1 labels are scrambled in the text layer, so values below are taken from the equations in the prose)
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Oxygen cascade (textbook chapter 19)

## Study
Two-page textbook chapter (printed pp. 80-81, PDF pages 1-2). No data, no drug. Textbook summary of the stepwise fall in PO2 from air to mitochondria.

## Model and parameters
Worked sea-level values (PDF p.1-2, printed 80-81):
```yaml
parameters:
  - {name: PB, value: 101.325, unit: kPa, source: "p.80", note: sea level}
  - {name: FiO2_dry_air, value: 20.93, unit: "%", source: "p.80"}
  - {name: PO2_atmosphere, value: 21.2, unit: kPa, source: "p.80"}
  - {name: PSVP_water_37C, value: 6.3, unit: kPa, source: "p.80"}
  - {name: PO2_carina_humidified, value: 19.9, unit: kPa, source: "p.80"}
  - {name: PaCO2_typical, value: 5.3, unit: kPa, source: "p.81"}
  - {name: R_respiratory_quotient, value: 0.8, unit: ratio, source: "p.81"}
  - {name: PAO2, value: 13.3, unit: kPa, source: "p.81", note: alveolar gas equation}
  - {name: PaO2_with_shunt, value: 13.0, unit: kPa, source: "p.81", note: normal physiological shunt 2-5%}
  - {name: Pasteur_point, value: "0.15-0.3", unit: kPa, source: "p.81", note: critical mitochondrial PO2}
```
Alveolar gas equation as printed: PAO2 = FiO2 (PB - PSVP water) - PaCO2/R. The chapter's numeric example, 20.93% x (101.325 - 6.3) - 5.3/0.8, gives 13.3 kPa, so the printed form divides only the PaCO2 term by R (the usual form also has an FiO2 correction term that is negligible at air). Figure 19.1 also shows tissue-capillary and venous values, but the text layer scrambles them and they are not recorded.

## Pharmacodynamics and end points
Not applicable (no drug). No MEAC, ke0, tPeak or potency ratio.

## Covariates
None. Altitude, hypoventilation and V/Q mismatch are named qualitatively as disturbances of the cascade.

## Caveats
Teaching summary, not primary data; values are typical.

## Used by stanpumpR
Not cited anywhere in the repo (context locations empty).

## Verification against stanpumpR
No repo citation. For orientation only: `R/arterialOxygen.R:183` takes alveolar O2 as a percentage times a constant `GAS_ALVEOLAR_TORR` rather than the cascade equations here, so nothing in the repo is checked against this chapter. Verdict: not-applicable.
