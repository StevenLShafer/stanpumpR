---
file: BK_9781108565011-ch10-alveolar-diffusion.pdf
pmid: none
doi: 10.1017/9781108565011.013
citation: 'Chapter 10, Alveolar Diffusion, Section 2 Respiratory Physiology, printed pp. 40-44. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer, all 5 pages; figures 10.1-10.3 are schematics with no numbers beyond axis ticks; no page images rendered
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
---

# Alveolar Diffusion (textbook chapter 10)

## Study
Teaching chapter, no original data. PDF pages 1-5 = printed pages 40-44. Topics: determinants of diffusion, perfusion- vs diffusion-limited gas transfer (O2, N2O, volatile agents, CO), DLCO, and diffusion impairment.

## Model and parameters
No pharmacokinetic model. Textbook values (PDF p1-2, printed 40-41, unless noted):
- Diffusion rate is proportional to surface area x gradient x solubility, and inversely to thickness x sqrt(MW) (key equation, p1).
- About 300 million alveoli, 70 m2 surface area, barrier as thin as 200 nm.
- RBC capillary transit 0.75 s at rest; O2 equilibrates by about 0.25 s (threefold safety factor); extreme exercise can cut transit to about 0.25 s (p3, printed 42).
- CO2 diffuses about 20x faster than O2 (solubility).
- CO affinity for Hb is 250x that of O2; CO is diffusion limited.
- N2O is perfusion limited and equilibrates within 0.075 s; volatile anaesthetics are similar, equilibrating slightly later (Figure 10.1).
- DLCO test: single vital-capacity breath of 0.3% CO, held 10 s (p3, printed 42).
- Pneumonectomy mortality up to 8%, lobectomy about 2% (p4, printed 43).

```yaml
parameters:
  - {name: RBC_capillary_transit_rest, value: 0.75, unit: s, source: "PDF p1", note: textbook average}
  - {name: O2_equilibration_time, value: 0.25, unit: s, source: "PDF p1", note: textbook}
  - {name: N2O_equilibration_time, value: 0.075, unit: s, source: "PDF p2", note: textbook}
  - {name: DLCO_inspired_CO, value: 0.3, unit: "%", source: "PDF p3", note: test gas}
```

## Pharmacodynamics and end points
None. Not an opioid paper: no MEAC, ke0, tPeak or potency ratio.

## Covariates
DLCO is corrected for Hb and affected by altitude, age and sex (stated, no equations).

## Caveats
Secondary teaching source; the round numbers carry no citations on these pages.

## Used by stanpumpR
Context lists no locations and `cited_in_repo` is false. It gives only qualitative support for the gas engine's treatment of N2O and volatile agents as perfusion limited.

## Verification against stanpumpR
No repo location cites this chapter, so there is nothing to verify. Not applicable.
