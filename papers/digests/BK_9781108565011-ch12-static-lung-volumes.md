---file: BK_9781108565011-ch12-static-lung-volumes.pdf
pmid: none
doi: 10.1017/9781108565011.015
citation: 'Chapter 12, Static Lung Volumes, Section 2 Respiratory Physiology, printed pp. 50-55. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: other
drugs: []
source_read: 'text layer for all 6 PDF pages; rendered page images read for PDF pages 1-2 (printed 50-51), which hold the typical volumes and capacities. PDF pages 3-6 (printed 52-55: pre-oxygenation figures, gas dilution, plethysmography, N2 washout, closing capacity) text layer only.'
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: partly-verified
open_questions:
- '[decision] R/gasProperties.R:269 sets alveolar gas volume (FRC) to 2.5 L at 70 kg (GasMan''s standard, comment at :212); this chapter gives FRC 3000 mL for a 70-kg man standing (PDF p.1, Fig 12.1) and about 2000 mL supine (printed p.52 text layer; PDF p.2 says supine lowers FRC by 1000 mL). The engine''s patient is anaesthetised and supine, so which posture''s FRC (and whether anaesthesia''s further fall) should the default represent? The chapter gives no anaesthetised value.'
---

# Static Lung Volumes (textbook chapter 12)

## Study
Textbook chapter, not a study. Typical values are "for a 70-kg man when standing" (PDF p.1, printed p.50, text and Fig 12.1); the chapter explains how FRC is measured. No data, population or assay.

## Model and parameters
No pharmacokinetic model. Typical lung volumes and capacities, standing 70-kg man (PDF p.1, printed p.50; confirmed on the rendered page):
```yaml
parameters:
  - {name: tidal_volume, value: 500, unit: mL, source: "p.50, Fig 12.1", note: "typical; about 7 mL/kg"}
  - {name: IRV, value: 2500, unit: mL, source: "p.50"}
  - {name: ERV, value: 1500, unit: mL, source: "p.50"}
  - {name: residual_volume, value: 1500, unit: mL, source: "p.50"}
  - {name: FRC, value: 3000, unit: mL, source: "p.50", note: "residual volume + ERV; standing"}
  - {name: VC, value: 4500, unit: mL, source: "p.50"}
  - {name: IC, value: 3000, unit: mL, source: "p.50"}
  - {name: TLC, value: 6000, unit: mL, source: "p.50"}
  - {name: FRC_supine, value: 2000, unit: mL, source: "printed p.52 (pre-oxygenation example, text layer)", note: "p.51 says supine lowers FRC by 1000 mL"}
```
Other facts (printed p.52, text layer): example O2 consumption 250 mL/min; supine lungs on air exhaust their O2 in just over 1 min; after pre-oxygenation about 1800 mL O2 in the lungs gives about 8 min before hypoxaemia. Respiratory quotient 0.8. FRC is measured by helium dilution (FRC = V1 x (C1 - C2)/C2), body plethysmography (Boyle's law) or multiple-breath N2 washout; dilution and washout underestimate with gas trapping (printed pp.52-55). Closing capacity is measured by Fowler's single-breath N2 method and encroaches on FRC at age 45 supine, 60 standing (printed p.55).

## Pharmacodynamics and end points
None. No drug, ke0, MEAC or potency ratio. Opioid audit: not an opioid paper.

## Covariates
FRC is reduced by supine position (by 1000 mL), raised intra-abdominal pressure (obesity, pregnancy, laparoscopy), anaesthesia (spontaneous or controlled), young age and lung disease; increased by PEEP, emphysema, increasing age and asthma (PDF p.2, printed p.51). No equations; the only magnitude is the 1000 mL supine fall. Lung-protective VT 6 mL/kg versus traditional 12 mL/kg (printed p.51).

## Caveats
Single typical man; no ranges, no body-size scaling, no anaesthetised FRC value, no source given for the typical numbers.

## Used by stanpumpR
The context file lists no citation of this chapter (`cited_in_repo: false`, no locations). It bears on the gas engine's alveolar volume, which the code labels functional residual capacity.

## Verification against stanpumpR
- `R/gasProperties.R:269` `V_alveolar = 2.5 * scale  # functional residual capacity` (scale = weight/70; scaled with fat-free mass when body composition is known, lines 197-203); `inst/app-help/models/gas-engine.md:11` lists 2.5 L. The 2.5 L is GasMan's standard (comment :212), not this chapter. Chapter: 3.0 L standing, about 2.0 L supine. The code's value is 17% below the standing value and 25% above the supine value (derived). No citation mismatch since the code does not cite the chapter, but the posture choice is open (see open_questions).
- Dead space in the code is 30% of minute ventilation (`R/gasProperties.R:381`); the chapter gives no dead-space value (that is chapter 11).
- Tidal volume is not a stanpumpR input (minute ventilation is).
Verdict: partly-verified, one open decision.
