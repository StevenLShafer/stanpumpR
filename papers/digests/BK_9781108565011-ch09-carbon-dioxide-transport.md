---file: BK_9781108565011-ch09-carbon-dioxide-transport.pdf
pmid: none
doi: 10.1017/9781108565011.012
citation: 'Chapter 9, Carbon Dioxide Transport, Section 2 Respiratory Physiology, printed pp. 37-39. In: Chambers D, Huang C, Matthews G. Basic Physiology for Anaesthetists. 2nd edition. Cambridge: Cambridge University Press, 2019. ISBN 9781108565011, doi 10.1017/9781108565011.'
kind: review
drugs: []
source_read: text layer (PDF pages 1-3); no page images rendered
digest_by: Claude (unattended digest_run.py), 2026-11-27
checked_against: stanpumpR master d7801725
verification: not-applicable
open_questions:
- '[source] The repo''s gas-engine notes say no source was identified for the body''s CO2 stores (docs/gas-engine-expansion.md:202); this chapter gives about 120 L stored (PDF p1, printed p37) and 2.5 L immediately available (PDF p3, printed p39) without a primary reference. Only a primary source (e.g. Farhi and Rahn) or the Nunn text it cites would settle whether to use either number.'
---

# Textbook ch 9 (2019): Carbon dioxide transport

## Study
Teaching chapter on CO2 production, the three transport forms, chloride shift, the Haldane and Bohr effects, the CO2 dissociation curve and gas changes in apnoea. No data set or methods. PDF pages 1-3 = printed pp 37-39. Text layer only; figures and Table 9.1 were not checked on page images.

## Model and parameters
Teaching values, no SE.
```yaml
parameters:
  - {name: basal_CO2_production, value: 200, unit: "mL/min", source: "p37 (PDF p1)", note: "STP; basal O2 consumption 250 mL/min"}
  - {name: max_CO2_production_exercise, value: 4000, unit: "mL/min", source: "p37"}
  - {name: body_CO2_store, value: 120, unit: L, source: "p37", note: "estimated, various forms; O2 store 1.5 L"}
  - {name: immediately_available_CO2, value: 2.5, unit: L, source: "p39 (PDF p3)", note: "circulation and lungs"}
  - {name: CO2_vs_O2_solubility, value: 20, unit: "x", source: "p37"}
  - {name: PaCO2, value: 5.3, unit: kPa, source: "p39", note: "arterial CO2 content 48 mL/100 mL"}
  - {name: PvCO2, value: 6.1, unit: kPa, source: "p39", note: "mixed venous CO2 content 52 mL/100 mL"}
  - {name: RBC_volume_venous_vs_arterial, value: 3, unit: "% larger", source: "p38"}
  - {name: PCO2_rise_apnoea, value: "0.4-0.8", unit: "kPa/min", source: "p39"}
```
Table 9.1 (p39), arterial blood: dissolved 5 %, carbamino 5 %, bicarbonate 90 %. Additional CO2 in venous blood: dissolved 10 %, carbamino 30 %, bicarbonate 60 %. Text states venous blood has six times the carbaminohaemoglobin of arterial.

Apnoea (p39): SaO2 falls to 70 % (PO2 5.0 kPa) after 2 min on air. After pre-oxygenation the O2 store exceeds 3 L, SaO2 stays 100 % through 5 min and PaCO2 approaches 10 kPa. Arterial-to-venous CO2 content difference 4 mL/100 mL (derived: 52 - 48).

## Pharmacodynamics and end points
Not a drug paper. No MEAC, ke0, tPeak or potency ratio. Not an opioid paper.

## Covariates
None quantified. Haldane effect (deoxyHb carries more CO2) is described qualitatively; the dissociation curve is drawn for oxygenated and deoxygenated blood (Fig 9.2a).

## Caveats
Secondary textbook source. The PCO2 rise in apnoea is a range, not a model. Internal check: 5.3 kPa arterial and 6.1 kPa venous with the content figures quoted. The PCO2 values are read from text; the rise rate was not derived from the stated 200 mL/min.

## Used by stanpumpR
The context file lists no locations (`cited_in_repo: false`, no drugs).

## Verification against stanpumpR
Nothing cites this file. For orientation only: `docs/gas-engine-expansion-mathematics.md` lines 26-27 use VCO2 = RQ x VO2 with RQ default 0.8, which at 250 mL/min gives 200 mL/min, matching the chapter's basal CO2 production. The repo's body CO2 store is flagged as unsourced (see open question). Verdict: not-applicable (no citing location).
